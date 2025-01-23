use crate::combinator::PakeratError;
use crate::combinator::Pakerat;
use std::fmt;
use std::cell::RefCell;
use std::rc::Weak;
use syn::buffer::TokenBuffer;
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
pub struct TypeGlobalData<'a>{
	parsers:BTreeMap<NonNanFloat,Rc<dyn ObjectParser>>,
	cache: HashMap<usize,CacheState<'a>>,
}

pub enum CacheState<'a>{
	Pending,
	Err(syn::Error),
	Ok(Cursor<'a>,Object)
}
// Implement Debug for CacheState
impl<'a> fmt::Debug for CacheState<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CacheState::Pending => write!(f, "CacheState::Pending"),
            CacheState::Err(error) => write!(f, "CacheState::Err({:?})", error),
            CacheState::Ok ( _,obj) => {
                write!(f, "CacheState::Ok {{ cursor: <opaque>, obj: {:?} }}", obj)
            }
        }
    }
}

///used for implementing pakerat parsing
#[derive(Debug)]
pub struct FileNameSpace<'a>{
	pub type_info: HashMap<Type,TypeGlobalData<'a>>,
	pub types : TypeScope<'static>,
	pub objects : ObjectScope<'static>,
} 

impl Default for FileNameSpace<'_>{

fn default() -> Self { 
	FileNameSpace{
		type_info:default_type_info_map(),
		types: TypeScope::new_global(),
		objects: ObjectScope::new_global(),
	}
}
}




///parses a type based on the callers file scope.
///
///this function allows for recursive calling which is very powerful.
///
///it implements caching which will never break any valid peg parser and will never return wrong results.
///
///it detects all potential infinite loops by keeping track of all the pending calls.
///
///however it will still reject some grammers that are premisible in cfg.
///
///for instance "expr + term => expr" would not work because it causes an infinite left recursive loop. 
#[derive(Debug,Clone)]
pub struct DeferedParse(pub Type,pub Ident);
impl Combinator<Object> for DeferedParse{
	fn parse_pakerat<'a>(&self, input: Cursor<'a>, state: &mut State<'a>) -> Pakerat<(Cursor<'a>, Object), syn::Error> {
		// !!!Contract we can not leave pending entries that we created in the cache
		//we also have to respect all recursive errors in parsers we call

		let byte_idx = input.span().byte_range().start;
		let file = state.file.clone();

		//ref cell shananigans to get file
		let to_iter = {
			
			let mut file = file.borrow_mut();
			

			let info = file.type_info.entry(self.0.clone()).or_default();



			match info.cache.entry(byte_idx) {
			    std::collections::hash_map::Entry::Occupied(entry) => match entry.get() {
			        CacheState::Ok(cursor, obj) => return Ok((*cursor, obj.clone())),
			        CacheState::Err(error) => return Err(PakeratError::Regular(error.clone())),
			        CacheState::Pending => {
			        	//this may be inserted multiple times as we call recursivly. which is fine but anoying
			        	let error = syn::Error::new(input.span(),format!("infinite loop while parsing type {}",self.1));
			        	entry.remove();
			        	// entry.insert(CacheState::Err(error.clone()));
			        	return Err(PakeratError::Regular(error));
			        },
			    },
			    std::collections::hash_map::Entry::Vacant(entry) => {
			        entry.insert(CacheState::Pending);
			    }
			}

			info.parsers.values().rev()
			//collect so that others may modify the type cache as they wish
			.map(Rc::clone).collect::<Vec<Rc<dyn ObjectParser>>>()
		};

		//note that even tho we dont hold the info we still know its in the table
		//that assumbtion is only broken for recursive errors

		let mut error = syn::Error::new(input.span(),format!("errored in parse of type {}",self.1));
		for parser in to_iter {
			
			match parser.parse_pakerat(input,state){
				Err(e) => match e {
					PakeratError::Regular(e) => {error.combine(e)},
					PakeratError::Recursive(e) => {
						error.combine(e);

						//removing the pending entry as it is now wrong
						let mut file = file.borrow_mut();
						if let Some(info) = file.type_info.get_mut(&self.0){
							info.cache.remove(&byte_idx);
						}
						
						return Err(PakeratError::Recursive(error))
					}
				}
				Ok((cursor,obj)) => {
					let mut file = file.borrow_mut();
					let info = file.type_info.get_mut(&self.0).unwrap();
					info.cache.insert(byte_idx,CacheState::Ok(cursor,obj.clone()));
					return Ok((cursor,obj))	

				},
			}

		}

		let mut file = file.borrow_mut();
		let info = file.type_info.get_mut(&self.0).unwrap();
		info.cache.insert(byte_idx,CacheState::Err(error.clone()));
		Err(PakeratError::Regular(error))
	}
}


impl ObjectParser for DeferedParse{

fn type_info(&self) -> Type { self.0.clone()}
}


macro_rules! insert_type_parsers {
    ($map:expr, { $($basic_type:expr => $parser:expr),* $(,)? }) => {
        $(
            {
                let mut parsers = BTreeMap::new();
                parsers.insert(NonNanFloat::new(1.0).unwrap(), Rc::new($parser) as Rc<dyn ObjectParser>);
                $map.insert(Type::Basic($basic_type), TypeGlobalData { parsers,cache:HashMap::new() });
            }
        )*
    };
}

pub fn default_type_info_map() -> HashMap<Type, TypeGlobalData<'static>> {
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
            any_parser => AnyParser,
            lit_parser => LiteralParser,
            word_parser => WordParser,
            punc_parser => PuncParser,
            group_parser => GroupParser,
            end_parser => EndParser,
            int_parser => IntParser,
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

    //this test would go into an infinite loop if the detection method is broken
    #[test]
	fn test_deferred_infinite_parse_detection() {
	   // Define a DeferedParse pointing to the int type
	    let int_type :Type = BasicType::Int.into();
	    let deferred_parser = Rc::new(DeferedParse(int_type.clone(), Ident::new("int", Span::call_site())));


	    // Create a global namespace and initialize state
	    let (input, mut state) = initialize_state("42").unwrap();

	    
	    // Insert the DeferedParse into the existing type info for int\
	    {
	    	let type_info = &mut state.file.borrow_mut().type_info;
	    	let int_data = type_info.get_mut(&int_type).expect("Int type data should exist");
	    	int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());
		}	
	    // Test parsing a valid integer
	    let valid_result = deferred_parser.parse(input.begin(), &mut state);

	    match valid_result {
	        Ok((remaining, object)) => {
	            if let ObjData::Basic(BasicData::Int(value)) = object.data {
	                assert_eq!(value, 42, "Parsed integer value is incorrect");
	            } else {
	                panic!("Parsed object is not an integer");
	            }
	            assert!(remaining.eof(), "Remaining tokens after parsing");
	        }
	        Err(err) => panic!("Parsing valid input failed: {}", err),
	    }

	    // Test parsing invalid input (non-integer)
	    let (invalid_input, mut state) = initialize_state("not_a_number").unwrap();
	    // Insert the DeferedParse into the existing type info for int\
	    {
	    	let type_info = &mut state.file.borrow_mut().type_info;
	    	let int_data = type_info.get_mut(&int_type).expect("Int type data should exist");
	    	int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());
		}	

	    let invalid_result = deferred_parser.parse(invalid_input.begin(), &mut state);

	    assert!(invalid_result.is_err(), "Expected error for invalid input, but got Ok result");
	}

	    //this test would go into an infinite loop if the detection method is broken
    #[test]
	fn test_deferred_infinite_parse_detection2() {
	   	let int_type :Type = BasicType::Int.into();
	    let deferred_parser = Rc::new(DeferedParse(int_type.clone(), Ident::new("int", Span::call_site())));

	    

	    // Test parsing invalid input (non-integer)
	    let (invalid_input, mut state) = initialize_state("not_a_number").unwrap();
	    // Insert the DeferedParse into the existing type info for int\
	    {
	    	let type_info = &mut state.file.borrow_mut().type_info;
	    	let int_data = type_info.get_mut(&int_type).expect("Int type data should exist");
	    	int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());
		}	

	    let invalid_result = deferred_parser.parse(invalid_input.begin(), &mut state);

	    assert!(invalid_result.is_err(), "Expected error for invalid input, but got Ok result");

	    // Create a global namespace and initialize state
	    let (input, mut state) = initialize_state("42").unwrap();

	    // Insert the DeferedParse into the existing type info for int\
	    {
	    	let type_info = &mut state.file.borrow_mut().type_info;
	    	let int_data = type_info.get_mut(&int_type).expect("Int type data should exist");
	    	int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());
		}	
	    // Test parsing a valid integer
	    let valid_result = deferred_parser.parse(input.begin(), &mut state);

	    match valid_result {
	        Ok((remaining, object)) => {
	            if let ObjData::Basic(BasicData::Int(value)) = object.data {
	                assert_eq!(value, 42, "Parsed integer value is incorrect");
	            } else {
	                panic!("Parsed object is not an integer");
	            }
	            assert!(remaining.eof(), "Remaining tokens after parsing");
	        }
	        Err(err) => panic!("Parsing valid input failed: {}", err),
	    }
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