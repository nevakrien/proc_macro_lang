use std::cell::RefCell;
use crate::combinator::CacheState;
use crate::basic_parsing::DelTokenParser;
use proc_macro2::Delimiter;
use crate::combinator::PakeratError;
use crate::combinator::Pakerat;
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

#[derive(Debug)]
pub struct NameSpace<'a>{
	pub map:HashMap<Ident,Object>,
	pub parent:Option<&'a NameSpace<'a>>,
}

/// This is heavier than it may seem. See [`NameSpace::new_global`].
impl Default for NameSpace<'_>{

fn default() -> Self { 
	NameSpace::new_global()
}
}

pub type FileNameSpace = NameSpace<'static>;

///holds a type to be parsed later in the **callers** scope using [DeferedParse::parse_pakerat]
#[derive(Debug,Clone)]
pub struct DeferedParse(pub Type,pub Ident);
impl Combinator<Object> for DeferedParse{
	///this function allows for recursive calling which is very powerful.
	///
	///it implements caching which will never break any valid peg parser and will never return wrong results.
	///
	///it detects all potential infinite loops by keeping track of all the pending calls.
	///
	///however it will still reject some grammers.
	///for instance "expr + term => expr" would not work because it causes an infinite left recursive loop. 
	///but "term + expr => expr" will work
	fn parse_pakerat<'a>(&self, input: Cursor<'a>, state: &mut State<'a>) -> Pakerat<(Cursor<'a>, Object), syn::Error> {
		// !!!Contract we can not leave pending entries that we created in the cache
		//we also have to respect all recursive errors in parsers we call

		let byte_idx = input.span().byte_range().start;

		//ref cell shananigans to get file
			
		// let mut file = file.borrow_mut();
		

		let info = state.type_info.entry(self.0.clone()).or_default();



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


		//note that even tho we dont hold the info we still know its in the table
		//that assumbtion is only broken for recursive errors

		let mut error = syn::Error::new(input.span(),format!("errored in parse of type {}",self.1));
		for parser in info.parsers.values().rev()
			//collect so that others may modify the type cache as they wish
			.map(Rc::clone).collect::<Vec<Rc<dyn ObjectParser>>>() {
			
			match parser.parse_pakerat(input,state){
				Err(e) => match e {
					PakeratError::Regular(e) => {error.combine(e)},
					PakeratError::Recursive(e) => {
						error.combine(e);

						//removing the pending entry as it is now wrong
						if let Some(info) = state.type_info.get_mut(&self.0){
							info.cache.remove(&byte_idx);
						}
						
						return Err(PakeratError::Recursive(error))
					}
				}
				Ok((cursor,obj)) => {
					let info = state.type_info.get_mut(&self.0).unwrap();
					info.cache.insert(byte_idx,CacheState::Ok(cursor,obj.clone()));
					return Ok((cursor,obj))	

				},
			}

		}

		let info = state.type_info.get_mut(&self.0).unwrap();
		info.cache.insert(byte_idx,CacheState::Err(error.clone()));
		Err(PakeratError::Regular(error))
	}
}


impl ObjectParser for DeferedParse{

fn type_info(&self) -> Type { self.0.clone()}
}

#[derive(Debug,Clone)]
pub struct ModuledParser<P:ObjectParser>{
	pub file: Rc<RefCell<FileNameSpace>>,
	pub parser: P
}

impl<P:ObjectParser> Combinator<Object> for ModuledParser< P>{
	fn parse_pakerat<'a>(&self, input: Cursor<'a>, state: &mut State<'a>) -> Pakerat<(Cursor<'a>, Object)>{
		let current_file = std::mem::replace(&mut state.file, self.file.clone());
		let ans = self.parser.parse_pakerat(input,state);
		state.file=current_file;
		ans
	}
}

impl<P:ObjectParser> ObjectParser for ModuledParser< P>{

fn type_info(&self) -> Type { self.parser.type_info() }
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

macro_rules! insert_types {
    ($map:expr, { $($name:ident => $type:expr),* $(,)? }) => {
        $(
            let t : Type = ($type).into();
            $map.insert(
                Ident::new(stringify!($name), Span::mixed_site()),
                t.into(),
            );
        )*
    };
}



impl NameSpace<'_> {
	///this function is relativly heavy. it constructs all the basic parsers types etc that should be in the global scope.
	///
	///it should only be called once per file
	pub fn new_global()->NameSpace<'static>{
		let mut map :HashMap<Ident,Object>= HashMap::new();
		
		 insert_types!(map, {
            any => BasicType::Tree,
            lit => BasicType::Literal,
            word => BasicType::Word,
            punc => BasicType::Punc,
            group => BasicType::Group,
            end => BasicType::None,
            int => BasicType::Int,
        });

		insert_parsers!(map, {
            any_parser => AnyParser,
            lit_parser => LiteralParser,
            word_parser => WordParser,
            punc_parser => PuncParser,
            group_parser => GroupParser,
            end_parser => EndParser,
            int_parser => IntParser,

            bracket_token_parser => DelTokenParser(Delimiter::Bracket),
            paren_token_parser => DelTokenParser(Delimiter::Parenthesis),
            brace_token_parser => DelTokenParser(Delimiter::Brace),
        });
		
		NameSpace{map,parent:None}
	}

	pub fn get(&self, key:&Ident) -> Option<Object>{
		match self.map.get(key){
			Some(x)=>Some(x.clone()),
			None=>match self.parent{
				Some(p)=> p.get(key),
				None=>None,
			}
		}
	}

	//wana make sure the lifetimes are what i think they are
	#[allow(clippy::needless_lifetimes)]
	pub fn make_child<'b>(&'b self) -> NameSpace<'b>{
		NameSpace{
			map:HashMap::new(),
			parent:Some(self)
		}
	}

	

	pub fn capture_scope(&self) ->NameSpace<'static>{
		let mut map = self.map.clone();
		let mut cur : Option<&NameSpace> = self.parent;
		while let Some(scope) = cur {
			for (k,v) in scope.map.iter(){
				map.entry(k.clone()).or_insert(v.clone());
			}
			cur=scope.parent;
		}
		NameSpace{map,parent:None}
	}
}

#[cfg(test)]
mod tests {
    use crate::combinator::NonNanFloat;
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

	    
	    let int_data = state.type_info.get_mut(&int_type).expect("Int type data should exist");
	    int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());	
	    
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
	    let int_data = state.type_info.get_mut(&int_type).expect("Int type data should exist");
	    int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());

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
	    let int_data = state.type_info.get_mut(&int_type).expect("Int type data should exist");
	    int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());

	    let invalid_result = deferred_parser.parse(invalid_input.begin(), &mut state);

	    assert!(invalid_result.is_err(), "Expected error for invalid input, but got Ok result");

	    // Create a global namespace and initialize state
	    let (input, mut state) = initialize_state("42").unwrap();

	    let int_data = state.type_info.get_mut(&int_type).expect("Int type data should exist");
	    int_data.parsers.insert(NonNanFloat::new(0.2).unwrap(), deferred_parser.clone());
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


