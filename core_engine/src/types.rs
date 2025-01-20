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
use std::collections::hash_map::Entry;
use proc_macro2::{Ident};


//for aliasing we need a unique id
#[derive(Debug,Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum BasicType {
    Tree,//token tree
    Number,
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

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum Type{
	Basic(BasicType),
	Array(Rc<Type>),
	Struct(Rc<StructTypes>),
	Alias(Rc<Type>,Unique),
	Union(Rc<[Type]>),
	External(Unique)
}

impl Type{
	pub fn new_external() ->Self{
		Type::External(Unique::new())
	}
}

impl From<BasicType> for Type{

fn from(t: BasicType) -> Self { Type::Basic(t) }
}

#[derive(Debug,Clone)]
pub enum ObjData{
	Basic(BasicData),
	Array(Rc<[Object]>),
	Struct(Rc<StructData>),
	External(Rc<dyn Any>)
}

#[derive(Debug,Clone)]
pub enum BasicData {
    Tree(TokenTree),//token tree
    Number(i64),
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
    i64 => BasicData::Number,
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

// impl<T, I> From<I> for ObjData where T : Into<Object>,I:Iterator<Item = T> {
//     fn from(data: Vec<Object>) -> Self {
//     	ObjData::Array(Rc::from(data.iter().map(|x| x.into)))
//     }
// }

impl From<StructData> for ObjData {
    fn from(data: StructData) -> Self {
        ObjData::Struct(Rc::new(data))
    }
}

impl From<TokenTree> for Object {
    fn from(data: TokenTree) -> Self {
        Object::new(data,BasicType::Tree.into())
    }
}



#[derive(Debug,Clone)]
pub struct Object{
	pub data: ObjData,//Rc<dyn Any>,
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
}

pub type StructData = HashMap<Ident,Object>;
pub type StructTypes = HashMap<Ident,Type>;
pub type ArrayData = Vec<Object>;

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

#[derive(Debug,Clone)]
pub struct StructParser(pub Box<[(Ident,Rc<dyn ObjectParser>)]>,pub Rc<StructTypes>);

impl StructParser{
	pub fn new(fields:Box<[(Ident,Rc<dyn ObjectParser>)]>) -> Result<Self,syn::Error>{
		let mut types = Rc::new(StructTypes::with_capacity(fields.len()));
		let r = Rc::get_mut(&mut types).unwrap();
		let mut error_map :HashMap<Ident,Vec<Ident>> = HashMap::new();

		for (ident,parser) in &fields {
			match r.entry(ident.clone()) {
				Entry::Vacant(spot) => {spot.insert(parser.type_info());},
			    Entry::Occupied(spot) => {
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
	fn parse<'a>(&self, mut input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut data = StructData::new();
		for (ident,parser) in &self.0 {
			let (new_cursor,obj) = parser.parse(input)?;
			input = new_cursor;
			data.insert(ident.clone(),obj);

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
	    // Define fields in the struct
	    let fields = Box::new([
	        (parse_quote! { field1 }, Rc::new(LiteralParser) as Rc<dyn ObjectParser>),
	        (parse_quote! { field2 }, Rc::new(WordParser)),
	        (parse_quote! { field3 }, Rc::new(PuncParser)),
	    ]);

	    // Create the StructParser
	    let struct_parser = StructParser::new(fields).unwrap();

	    // Input token stream: 42 foo !
	    let token_stream: TokenStream = syn::parse_quote! { 42 foo ! };
	    let token_buffer = TokenBuffer::new2(token_stream);
	    let cursor = token_buffer.begin();

	    // Parse the token stream using StructParser
	    let (remaining_cursor, object) = struct_parser
	        .parse(cursor)
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
        let result = struct_parser.parse(cursor);

        // Expect an error because the input doesn't match the sequence
        assert!(result.is_err());
    }
}
