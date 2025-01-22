use crate::name_space::NameSpace;
use std::collections::BTreeSet;
use std::collections::BTreeMap;
use proc_macro2::Literal;
use proc_macro2::Group;
use proc_macro2::Punct;
use proc_macro2::TokenTree;
use syn::buffer::Cursor;
use std::fmt::Debug;
use crate::basic_parsing::Combinator;
use std::collections::HashMap;
use std::rc::Rc;
use std::any::Any;
use std::sync::atomic::{AtomicU64, Ordering};
use std::collections::btree_map::Entry as BEntry;
use proc_macro2::{Ident};


//for aliasing we need a unique id
#[derive(Debug,Clone, PartialEq, Eq, Hash,PartialOrd,Ord)]
pub struct Unique(u64);

static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Unique {
    pub fn new() -> Self {
        let id = UNIQUE_COUNTER.fetch_add(1, Ordering::Relaxed);
        //if some idiot runs a loop that makes lots of these multithreaded this would crash them all
        assert!(id < u64::MAX / 2, "Unique ID counter has overflowed...");
        Unique(id)
    }
    #[inline]
    pub fn id(&self) -> u64 {
        self.0
    }
    #[inline]
    pub fn get_counter() -> u64{
    	UNIQUE_COUNTER.fetch_add(0,Ordering::Relaxed)
    }

    //this specifcly takes u32 to avoid wrap around risks
    //if for some reason u need more than a full u32 something went horibly wrong
    #[allow(clippy::unnecessary_fallible_conversions)]
    pub fn ensure_size(size: u32) {
        let size = size.try_into().unwrap();
        let id = UNIQUE_COUNTER.load(Ordering::Relaxed);
        if id >= size {
            return;
        }

        let id = UNIQUE_COUNTER.fetch_add(size - id, Ordering::Relaxed);
        assert!(id < u64::MAX / 2, "Unique ID counter has wrapped around!");
    }
}

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug,Clone, PartialEq, Eq,Hash,PartialOrd,Ord)]
pub enum BasicType {
    Tree,//token tree
    Int,
    None,

    Literal,
    Word,
    Punc,
    Group,
    /*might want:
    	Token
    	...
    */
}

///# Type info for Object.
///
/// This type is used by signatures and objects. 
///
/// Unions would not be found directly on objects. Instead the type of the specific member. 
///
/// We hold these in ordered sets to allow for good hashing (larger collections can be heled in a hash table).
#[derive(Debug,Clone, PartialEq, Eq,Hash)]
#[repr(i8)]
pub enum Type{
	Basic(BasicType)=0,
	Array(Rc<Type>) ,
	Struct(Rc<StructTypes>),
	Alias(Rc<Type>,Unique),
	Union(Rc<BTreeSet<Type>>),
	Parser(Rc<Type>),
	External(Unique),
}

impl Type{
	pub fn get_id(&self) -> i8 {
		use Type::*;
		match self{
			Basic(_)=>0,
			Array(_)=>1,
			Struct(_)=>2,
			Alias(_, _)=>3,
			Union(_)=>4,
			Parser(_)=>5,
			External(_)=>6,
		}
	}
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use Type::*;

        match (self, other) {
            //things with id
            (Basic(a), Basic(b)) => a.cmp(b),
            (Alias(_, u1), Alias(_, u2)) => u1.cmp(u2),//aliases have unique ids
            (External(u1), External(u2)) => u1.cmp(u2),


            //recursivly compare
            (Array(a), Array(b)) => a.cmp(b),
            (Struct(a), Struct(b)) => a.cmp(b),
            (Parser(a), Parser(b)) => a.cmp(b),
            (Union(u1), Union(u2)) => u1.cmp(u2),
            
            //this is an arbitrary choice but it is consistent
            _ => self.get_id().cmp(&other.get_id())

           
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}



impl Type{
	pub fn new_external() ->Self{
		Type::External(Unique::new())
	}

	pub fn new_union<I:Iterator<Item=Type>>(input:I) ->Self{
		let mut set = BTreeSet::new();
		for i in input{
			set.insert(i);
		}
		Type::Union(set.into())
	}
}

impl From<BasicType> for Type{

fn from(t: BasicType) -> Self { Type::Basic(t) }
}


///Data heled by an object (usually passed by value). 
///
///This type is cheap to clone and is intended to move around a lot.
///
///Structs are Rc since a lot of the time get_mut would work for essentially free. For other cases having Rc here is best. 
///
///Note that the alias and union information is lost here.
#[derive(Debug,Clone)]
pub enum ObjData{
	Basic(BasicData),
	Array(Rc<[Object]>),
	Struct(Rc<StructData>),
	Parser(Rc<dyn ObjectParser>),

	External(Rc<dyn Any>)
}

#[derive(Debug,Clone)]
pub enum BasicData {
    Tree(TokenTree),//token tree
    Int(i64),
    None,

    Literal(Literal),
    Word(Ident),
    Punc(Punct),
    Group(Group),
    /*might want:
    	Token
    	...
    */
}

macro_rules! impl_from_for_basicdata {
    ($($ty:ty => $variant:path),* $(,)?) => {
        $(
            impl From<$ty> for ObjData {
                fn from(data: $ty) -> Self {
                    ObjData::Basic($variant(data))
                }
            }
        )*
    };
}

impl_from_for_basicdata! {
    TokenTree => BasicData::Tree,
    Literal => BasicData::Literal,
    Ident => BasicData::Word,
    Punct => BasicData::Punc,
    Group => BasicData::Group,
    i64 => BasicData::Int,
}

impl From<()> for ObjData {
    fn from(_data: ()) -> Self {
        ObjData::Basic(BasicData::None)
    }
}
impl From<Vec<Object>> for ObjData {
    fn from(data: Vec<Object>) -> Self {
        ObjData::Array(Rc::from(data.into_boxed_slice()))
    }
}

impl From<StructData> for ObjData {
    fn from(data: StructData) -> Self {
        ObjData::Struct(Rc::new(data))
    }
}

impl From<Rc<dyn ObjectParser>> for ObjData {
    fn from(data: Rc<dyn ObjectParser>) -> Self {
        ObjData::Parser(data)
    }
}

impl From<TokenTree> for Object {
    fn from(data: TokenTree) -> Self {
        Object::new(data,BasicType::Tree.into())
    }
}

impl From<Rc<dyn ObjectParser>> for Object {
    fn from(data: Rc<dyn ObjectParser>) -> Self {
    	let t = Type::Parser(data.type_info().into());
        Object::new(data,t)
    }
}



///All data heled by the runtime.
///
///Unions type information is lost here
#[derive(Debug,Clone)]
pub struct Object{
	pub data: ObjData,
	///missing union info
	pub type_info: Type,
}



impl Object {
	pub fn new<T:Into<ObjData>>(data:T,type_info: Type) -> Self {
		Object{
			data:data.into(),
			type_info
		}
	}

	pub fn from_iter<I : Iterator<Item: Into<Object>>>(data:I,type_info: Type) -> Self{
		Object{
			data:ObjData::Array(data.map(|x| x.into()).collect()),
			type_info
		}

	}

	pub fn none() -> Self{
		Object{
			data:().into(),
			type_info:BasicType::None.into()
		}
	}
}

pub type StructData = HashMap<Ident,Object>;
/// We hold these in ordered set to allow for good hashing
pub type StructTypes = BTreeMap<Ident,Type>;

#[test]
fn proc_macro2_equals(){
	use proc_macro2::Span;

	let a = Ident::new("data",Span::call_site());
	let b = Ident::new("data",Span::mixed_site());
	assert_eq!(a,b);
}

pub trait ObjectParser<E = syn::Error>: Combinator<Object, E> + Debug
where
    E: std::error::Error,
{
    fn type_info(&self) -> Type;
}

#[derive(Debug)]
pub struct StructParser(pub Box<[(Option<Ident>,Rc<dyn ObjectParser>)]>,pub Rc<StructTypes>);

impl StructParser{
	pub fn new(fields:Box<[(Option<Ident>,Rc<dyn ObjectParser>)]>) -> Result<Self,syn::Error>{
		let mut types = Rc::new(StructTypes::new());
		let r = Rc::get_mut(&mut types).unwrap();
		let mut error_map :HashMap<Ident,Vec<Ident>> = HashMap::new();

		for (ident,parser) in fields.iter()
		.filter(|(i,_)| !i.is_none())
		.map(|(i,parser)| (i.as_ref().unwrap(),parser)) 
		{
			match r.entry(ident.clone()) {
				BEntry::Vacant(spot) => {spot.insert(parser.type_info());},
			    BEntry::Occupied(spot) => {
			    	error_map.entry(ident.clone())
			    		.or_insert_with(||vec![spot.key().clone()])
			    		.push(ident.clone())
			    },
			};
		}
		// If duplicates exist, create a combined error
        if !error_map.is_empty() {
            let mut error: Option<syn::Error> = None;

            for (_first, all) in error_map.drain() {
                for ident in all {
                    let span = ident.span(); // Get the span of the duplicate identifier
                    let new_error = syn::Error::new(span, format!("Duplicate key: `{}`", ident));
                    if let Some(ref mut existing_error) = error {
                        existing_error.combine(new_error);
                    } else {
                        error = Some(new_error);
                    }
                }
            }

            return Err(error.unwrap());
        }
        Ok(Self(fields, types))
	}
}

impl Combinator<Object> for StructParser{
	fn parse<'a>(&self, mut input: Cursor<'a>,name_space:&NameSpace) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut data = StructData::new();
		for (opt,parser) in &self.0 {
			let (new_cursor,obj) = parser.parse(input,name_space)?;
			input = new_cursor;
			if let Some(ident)=opt{
				data.insert(ident.clone(),obj);
			}

		}
		Ok((input,Object::new(data,self.type_info())))
	}
}

impl ObjectParser for StructParser{
	fn type_info(&self) ->Type { Type::Struct(self.1.clone())}
}

#[cfg(test)]
mod tests {
use crate::basic_parsing::AnyParser;
use crate::basic_parsing::GroupParser;
use crate::basic_parsing::PuncParser;
use crate::basic_parsing::WordParser;
use crate::basic_parsing::LiteralParser;
use super::*;
    use proc_macro2::TokenStream;
    use syn::buffer::TokenBuffer;
    use syn::parse_quote;
    use std::rc::Rc;

	#[test]
	fn test_struct_parser_duplicate_fields() {
	    use syn::parse_quote;

	    // Define fields with duplicates
	    let fields = Box::new([
	        (parse_quote! { field1 }, Rc::new(LiteralParser) as Rc<dyn ObjectParser>),
	        (parse_quote! { field2 }, Rc::new(WordParser)),
	        (parse_quote! { field1 }, Rc::new(PuncParser)),
	        (parse_quote! { field3 }, Rc::new(GroupParser)),
	        (parse_quote! { field2 }, Rc::new(AnyParser)),
	    ]);

	    // Attempt to create the StructParser
	    let result = StructParser::new(fields);

	    // Expect an error
	    assert!(result.is_err());

	    // Retrieve the error
	    let error = result.unwrap_err();


	    //syn truncates error messages so we need to
	    // Concatenate all errors into a single string
	    let mut error_messages = String::new();
	    for child_error in error.clone().into_iter() {
	        error_messages.push_str(&format!("{child_error}\n"));
	    }

	    // Check that the concatenated error message contains all duplicate fields
	    assert!(error_messages.contains("Duplicate key: `field1`"));
	    assert!(error_messages.contains("Duplicate key: `field2`"));
	}


	#[test]
	fn struct_parser_basic_parse() {
		let name_space = NameSpace::new_global();

	    // Define fields in the struct
	    let fields = Box::new([
	        (parse_quote! { field1 }, Rc::new(LiteralParser) as Rc<dyn ObjectParser>),
	        (parse_quote! { field2 }, Rc::new(WordParser)),
	        (None, Rc::new(WordParser)),
	        (parse_quote! { field3 }, Rc::new(PuncParser)),
	    ]);

	    // Create the StructParser
	    let struct_parser = StructParser::new(fields).unwrap();

	    // Input token stream: 42 foo !
	    let token_stream: TokenStream = syn::parse_quote! { 42 foo bar! };
	    let token_buffer = TokenBuffer::new2(token_stream);
	    let cursor = token_buffer.begin();

	    // Parse the token stream using StructParser
	    let (remaining_cursor, object) = struct_parser
	        .parse(cursor,&name_space)
	        .unwrap(); // Unwrap for better diagnostics

	    // Ensure the remaining cursor is at the end
	    assert!(
	        remaining_cursor.eof(),
	        "Expected remaining cursor to be at the end, but found more tokens."
	    );

	    // Check the captured fields and their types
	    let captured_fields: StructData = match object.data {
	        ObjData::Struct(ref struct_data) => (**struct_data).clone(),
	        _ => panic!("Expected ObjData::Struct, found {:?}", object.data),
	    };

	    // Verify the captured fields contain the expected keys
	    assert!(captured_fields.contains_key(&parse_quote! { field1 }));
	    assert!(captured_fields.contains_key(&parse_quote! { field2 }));
	    assert!(captured_fields.contains_key(&parse_quote! { field3 }));

	    // Verify the types of the captured fields
	    match &captured_fields[&parse_quote! { field1 }].data {
	        ObjData::Basic(BasicData::Literal(_)) => {}
	        _ => panic!("Expected BasicData::Literal for field1"),
	    }
	    match &captured_fields[&parse_quote! { field2 }].data {
	        ObjData::Basic(BasicData::Word(_)) => {}
	        _ => panic!("Expected BasicData::Word for field2"),
	    }
	    match &captured_fields[&parse_quote! { field3 }].data {
	        ObjData::Basic(BasicData::Punc(_)) => {}
	        _ => panic!("Expected BasicData::Punc for field3"),
	    }
	}



    #[test]
    fn struct_parser_fail_parse() {
		let name_space = NameSpace::new_global();


        // Define fields in the struct
        let fields = Box::new([
            (parse_quote! { field1 }, Rc::new(LiteralParser) as Rc<dyn ObjectParser>),
            (parse_quote! { field2 }, Rc::new(WordParser)),
            (parse_quote! { field3 }, Rc::new(PuncParser)),
        ]);

        // Create the StructParser
        let struct_parser = StructParser::new(fields).unwrap();

        // Input token stream with mismatched sequence: 42 !
        let token_stream: TokenStream = syn::parse_quote! { 42 ! };
        let token_buffer = TokenBuffer::new2(token_stream);
        let cursor = token_buffer.begin();

        // Parse the token stream using StructParser
        let result = struct_parser.parse(cursor,&name_space);

        // Expect an error because the input doesn't match the sequence
        assert!(result.is_err());
    }
}
