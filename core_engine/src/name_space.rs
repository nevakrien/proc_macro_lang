use syn::buffer::Cursor;
use crate::basic_parsing::Combinator;
use crate::Object;
use crate::basic_parsing::{AnyParser,LiteralParser,EndParser,IntParser,GroupParser,PuncParser,WordParser};
use proc_macro2::Span;
use crate::ObjectParser;
use std::rc::Rc;
use proc_macro2::Ident;
use std::collections::HashMap;

///used to store program name space. note that things should not really refer to this directly
pub struct Scope<'a,T> where T : Clone,  T: From<Rc<dyn ObjectParser>>,{
	pub map:HashMap<Ident,T>,
	pub parent:Option<&'a Scope<'a, T>>,
}

pub type ObjectScope<'a> = Scope<'a,Object>;
pub type ParserScope<'a> = Scope<'a,Rc<dyn ObjectParser>>;

pub struct NameSpace<'a> {
	parsers : ParserScope<'a>,
	objects : ObjectScope<'a>
}

impl NameSpace<'_>{
	pub fn new_global() -> Self {
		NameSpace{
			parsers : ParserScope::new_global(),
			objects : ObjectScope::new_global(),
		}
	}
}

#[derive(Debug,Clone)]
pub struct DeferedParse(pub Ident);
impl Combinator<Object> for DeferedParse{
	fn parse<'a>(&self, input: Cursor<'a>, name_space: &NameSpace) -> Result<(Cursor<'a>, Object), syn::Error> {
		match name_space.parsers.get(&self.0) {
		    None => Err(syn::Error::new(self.0.span(),format!("unrecognized parser name {}",self.0))),
		    Some(parser) => parser.parse(input,name_space),
		}
	}
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


impl<T> Scope<'_, T> where T : Clone , T: From<Rc<dyn ObjectParser>>{
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

	pub fn new_global()->Scope<'static,T>{
		let mut map :HashMap<Ident,T>= HashMap::new();
		
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
    use crate::types::ObjData;
use crate::types::BasicData;
use super::*;
    use syn::buffer::TokenBuffer;
    use proc_macro2::{Span, TokenStream};
    
    use std::str::FromStr;

    #[test]
    fn test_deferred_parse() {
        // Create a global namespace
        let namespace = NameSpace::new_global();

        // Define some input to parse
        let input = "42";
        let tokens = TokenStream::from_str(input).expect("Failed to parse input as TokenStream");
        let token_buffer = TokenBuffer::new2(tokens);
        let cursor = token_buffer.begin();

        // Test parsing with an existing parser (int parser in this case)
        let deferred_parser = DeferedParse(Ident::new("int", Span::call_site()));
        let result = deferred_parser.parse(cursor, &namespace);

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
        // Create a global namespace
        let namespace = NameSpace::new_global();

        // Define some input to parse
        let input = "42";
        let tokens = TokenStream::from_str(input).expect("Failed to parse input as TokenStream");
        let token_buffer = TokenBuffer::new2(tokens);
        let cursor = token_buffer.begin();

        // Test parsing with a non-existent parser
        let deferred_parser = DeferedParse(Ident::new("nonexistent", Span::call_site()));
        let result = deferred_parser.parse(cursor, &namespace);

        // Assert the result is an error
        assert!(result.is_err(), "Expected error for unrecognized parser");
        if let Err(error) = result {
            assert_eq!(
                error.to_string(),
                "unrecognized parser name nonexistent",
                "Error message does not match"
            );
        }
    }
}
