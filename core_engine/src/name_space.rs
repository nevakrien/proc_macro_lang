use std::collections::BTreeMap;
use crate::types::BasicType;
use crate::types::Type;
use crate::combinator::State;
use syn::buffer::Cursor;
use crate::combinator::Combinator;
use crate::Object;
use crate::basic_parsing::{AnyParser,LiteralParser,EndParser,IntParser,GroupParser,PuncParser,WordParser};
use proc_macro2::Span;
use crate::ObjectParser;
use std::rc::Rc;
use proc_macro2::Ident;
use std::collections::HashMap;

///used to store program name space. note that things should not really refer to this directly
#[derive(Debug)]
pub struct Scope<'a,T> where T : Clone{
	pub map:HashMap<Ident,T>,
	pub parent:Option<&'a Scope<'a, T>>,
}

pub type TypeScope<'a> = Scope<'a,Type>;
pub type ObjectScope<'a> = Scope<'a,Object>;
// pub type ParserScope<'a> = Scope<'a,Rc<dyn ObjectParser>>;

///deafualt here is a place holder!!!
#[derive(Debug,Default)]
pub struct TypeGlobalData{
	parsers:BTreeMap<NonNanFloat,Rc<dyn ObjectParser>>
}

#[derive(Debug)]
pub struct NameSpace<'a> {
	types : TypeScope<'a>,
	objects : ObjectScope<'a>,
	type_info:HashMap<Type,TypeGlobalData>,
}

impl NameSpace<'_>{
	pub fn new_global() -> Self {
		NameSpace{
			types : TypeScope::new_global(),
			type_info: default_type_info_map(),
			objects : ObjectScope::new_global(),
		}
	}
}

#[derive(Debug,Clone)]
pub struct DeferedParse(pub Type,pub Ident);
impl Combinator<Object> for DeferedParse{
	fn parse<'a>(&self, input: Cursor<'a>, state: &mut State) -> Result<(Cursor<'a>, Object), syn::Error> {
		let info = state.name_space.type_info.entry(self.0.clone()).or_default();

		let mut error = syn::Error::new(input.span(),format!("errored on global parse of type {}",self.1));

		for parser in info.parsers.values().rev()
		//collect so that others may modify the type cache as they wish
		.map(Rc::clone).collect::<Vec<Rc<dyn ObjectParser>>>() {
			
			match parser.parse(input,state){
				Err(e) => {error.combine(e)}
				Ok(x) => {return Ok(x)},
			}

		}
		Err(error)
	}
}

macro_rules! insert_type_parsers {
    ($map:expr, { $($basic_type:expr => $parser:expr),* $(,)? }) => {
        $(
            {
                let mut parsers = BTreeMap::new();
                parsers.insert(NonNanFloat::new(1.0).unwrap(), Rc::new($parser) as Rc<dyn ObjectParser>);
                $map.insert(Type::Basic($basic_type), TypeGlobalData { parsers });
            }
        )*
    };
}

pub fn default_type_info_map() -> HashMap<Type, TypeGlobalData> {
    let mut type_info = HashMap::new();

    insert_type_parsers! {
        type_info, {
            BasicType::Tree => AnyParser,
            BasicType::Int => IntParser,
            BasicType::None => EndParser,
            BasicType::Literal => LiteralParser,
            BasicType::Word => WordParser,
            BasicType::Punc => PuncParser,
            BasicType::Group => GroupParser,
        }
    };

    type_info
}

macro_rules! insert_parsers {
    ($map:expr, { $($name:ident => $parser:expr),* $(,)? }) => {
        $(
            let obj : Rc<dyn ObjectParser> = Rc::new($parser);
            $map.insert(
                Ident::new(stringify!($name), Span::mixed_site()),
                obj.into(),
            );
        )*
    };
}


impl ObjectScope<'_>{
	pub fn new_global()->ObjectScope<'static>{
		let mut map :HashMap<Ident,Object>= HashMap::new();
		
		 insert_parsers!(map, {
            any => AnyParser,
            lit => LiteralParser,
            word => WordParser,
            punc => PuncParser,
            group => GroupParser,
            end => EndParser,
            int => IntParser,
        });
		
		Scope{map,parent:None}
	}
}


macro_rules! insert_types {
    ($map:expr, { $($name:ident => $type:expr),* $(,)? }) => {
        $(
            
            $map.insert(
                Ident::new(stringify!($name), Span::mixed_site()),
                ($type).into(),
            );
        )*
    };
}


impl TypeScope<'_>{
	pub fn new_global()->TypeScope<'static>{
		let mut map :HashMap<Ident,Type>= HashMap::new();
		
		 insert_types!(map, {
            any => BasicType::Tree,
            lit => BasicType::Literal,
            word => BasicType::Word,
            punc => BasicType::Punc,
            group => BasicType::Group,
            end => BasicType::None,
            int => BasicType::Int,
        });
		
		Scope{map,parent:None}
	}
}


impl<T> Scope<'_, T> where T : Clone {
	pub fn get(&self, key:&Ident) -> Option<T>{
		match self.map.get(key){
			Some(x)=>Some(x.clone()),
			None=>match self.parent{
				Some(p)=> p.get(key),
				None=>None,
			}
		}
	}

	pub fn make_child<'b>(&'b self) -> Scope<'b, T>{
		Scope{
			map:HashMap::new(),
			parent:Some(self)
		}
	}

	

	pub fn capture_scope(&self) ->Scope<'static,T>{
		let mut map = self.map.clone();
		let mut cur : Option<&Scope<T>> = self.parent;
		while let Some(scope) = cur {
			for (k,v) in scope.map.iter(){
				map.entry(k.clone()).or_insert(v.clone());
			}
			cur=scope.parent;
		}
		Scope{map,parent:None}
	}
}

#[cfg(test)]
mod tests {
    use crate::combinator::initialize_state;
use crate::types::ObjData;
use crate::types::BasicData;
use super::*;
    
    use proc_macro2::Span;
    
    

    #[test]
    fn test_deferred_parse() {
        // Create a global namespace
        let (input,mut state) = initialize_state("42").unwrap();

        // Test parsing with an existing parser (int parser in this case)
        let deferred_parser = DeferedParse(BasicType::Int.into(),Ident::new("int", Span::call_site()));
        let result = deferred_parser.parse(input.begin(), &mut state);

        // Assert the result is correct
        match result {
            Ok((remaining, object)) => {
                // Assuming `Object::Integer` exists, otherwise adapt this to your actual implementation
                if let ObjData::Basic(BasicData::Int(value)) = object.data {
                    assert_eq!(value, 42, "Parsed integer value is incorrect");
                } else {
                    panic!("Parsed object is not an integer");
                }

                // Ensure no remaining tokens after parsing
                assert!(remaining.eof(), "Remaining tokens after parsing");
            }
            Err(err) => panic!("Deferred parser failed: {}", err),
        }
    }

    #[test]
    fn test_deferred_parse_unrecognized_parser() {
        let (input,mut state) = initialize_state("42").unwrap();

        // Test parsing with a non-existent parser
        let deferred_parser = DeferedParse(Type::new_external(),Ident::new("nonexistent", Span::call_site()));
        let result = deferred_parser.parse(input.begin(), &mut state);

        // Assert the result is an error
        assert!(result.is_err(), "Expected error for unrecognized parser");
    }
}


use std::cmp::Ordering;

// Wrapper for f32 that ensures no NaN values
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct NonNanFloat(f32);

impl NonNanFloat {
    // Constructor that checks for NaN
    pub fn new(value: f32) -> Option<Self> {
        if value.is_nan() {
            None
        } else {
            Some(Self(value))
        }
    }

    // Unsafe constructor if you're absolutely sure the value is not NaN
    pub unsafe fn new_unchecked(value: f32) -> Self {
        Self(value)
    }
}

impl Eq for NonNanFloat {}

impl Ord for NonNanFloat {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).expect("Comparison failed; invalid float values detected")
    }
}


impl TryFrom<f32> for NonNanFloat {
    type Error = &'static str;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        if value.is_nan() {
            Err("NaN values are not allowed in NonNanFloat")
        } else {
            Ok(Self(value))
        }
    }
}

impl From<NonNanFloat> for f32 {

fn from(x: NonNanFloat) -> f32 { x.0 }
}