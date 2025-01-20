
use proc_macro2::{TokenStream,TokenTree,Delimiter};
use syn::buffer::{Cursor,TokenBuffer};

use std::rc::Rc;
use std::fmt;
use crate::basic_parsing::Combinator;
use crate::types::{BasicType,Object};
use crate::types::{ObjectParser,Type};


#[derive(Debug, Clone)]
pub struct MatchParser(pub RcTokenBuffer);
impl Combinator<Object> for MatchParser {

    fn parse<'a>(&self, actual: syn::buffer::Cursor<'a>) 
    -> Result<(syn::buffer::Cursor<'a>, Object), syn::Error> 
    {   
        let mut v = Vec::with_capacity(3);
        let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,Some(&mut v))?;
        Ok((cursor,Object::new(v,self.type_info())))
    }
}

impl ObjectParser for MatchParser{

fn type_info(&self) ->Type { Type::Array(Rc::new(BasicType::Tree.into()))}
}


#[derive(Clone)]
pub struct RcTokenBuffer(pub Rc<TokenBuffer>);
impl RcTokenBuffer{
    pub fn begin(&self) -> Cursor{
        self.0.begin()
    }
}

impl fmt::Debug for RcTokenBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tokens = Vec::new();
        let mut cursor = self.0.begin();
        while let Some((token, next)) = cursor.token_tree() {
            tokens.push(format!("{:?}", token));
            cursor = next;
        }
        f.debug_struct("RcTokenBuffer")
            .field("tokens", &tokens)
            .finish()
    }
}

impl From<std::vec::IntoIter<TokenTree>> for RcTokenBuffer {
    fn from(iter: std::vec::IntoIter<TokenTree>) -> Self {
        let token_buffer = TokenBuffer::new2(proc_macro2::TokenStream::from_iter(iter));
        RcTokenBuffer(Rc::new(token_buffer))
    }
}

impl From<TokenBuffer> for RcTokenBuffer {
    fn from(buffer: TokenBuffer) -> Self {
        RcTokenBuffer(Rc::new(buffer))
    }
}

impl Default for RcTokenBuffer{

	fn default() -> Self { RcTokenBuffer(Rc::new(TokenBuffer::new2(TokenStream::new()))) }
}


#[derive(Clone,Copy,Debug)]
enum SpotType{
	Regular,
	End(Delimiter),
	Start(Delimiter)
}

impl SpotType{
	fn from_cursor_del(c:&Cursor,last_del:Delimiter) -> Self{
		match c.token_tree(){
			None =>  SpotType::End(last_del),
			Some((tree,_)) => match tree{
				TokenTree::Group(g) => SpotType::Start(g.delimiter()),
				_ => SpotType::Regular
			}
		}
	}
}

#[derive(Clone,Copy)]
struct CodeSpot<'a>{
	cursor: Cursor<'a>,
	spot_type: SpotType,
}

impl std::fmt::Display for CodeSpot<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.spot_type{
        		SpotType::Regular => write!(f, "{}", self.cursor.span().source_text().unwrap_or_else(||"<MISSING (likely parser bug)>".to_string())),
        		SpotType::Start(d) => match d {
        			Delimiter::None => write!(f, "[EMPTY DELIMITER (this is some weirdness with rust and may be a bug in this program)]"),
        			Delimiter::Parenthesis=> write!(f,"("),
        			Delimiter::Bracket=> write!(f,"["),
        			Delimiter::Brace => write!(f,"{{"),
        		}
        		SpotType::End(d) => match d{
        			Delimiter::None => write!(f, "<EOF>"),
        			Delimiter::Parenthesis=> write!(f,")"),
        			Delimiter::Bracket=> write!(f,"]"),
        			Delimiter::Brace => write!(f,"}}"),
        		}
        }
        
    }
}

//originaly was an errorreturn type now its just a way to make a syn error
#[derive(Clone,Copy)]
struct ExactMatchError<'a,'b> {
    expected: CodeSpot<'b>,
    actual: CodeSpot<'a>,
}

#[allow(clippy::from_over_into)]
impl Into<syn::Error> for ExactMatchError<'_, '_> {
    fn into(self) -> syn::Error {
        syn::Error::new(self.actual.cursor.span(), format!(
            "Expected token at {}, found token at {}",
            self.expected, self.actual
        ))
    }
}

impl std::fmt::Display for ExactMatchError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Into::<syn::Error>::into(*self))
    }
}

impl std::fmt::Debug for ExactMatchError<'_, '_>{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "ExactMatchError({:?})", Into::<syn::Error>::into(*self))
	}
}

impl std::error::Error for ExactMatchError<'_, '_> {}

macro_rules! return_match_err {
     ($expected:expr, $actual:expr, $del:ident) => {
        return Err(ExactMatchError{
				expected:CodeSpot{
					cursor:$expected,
					spot_type:SpotType::from_cursor_del(&$expected,$del),

				},
				actual:CodeSpot{
					cursor:$actual,
					spot_type:SpotType::from_cursor_del(&$actual,$del),

				},
			}.into())
    };
}


fn parse_exact_match<'a>(actual: Cursor<'a>,expected:Cursor,del:Delimiter,mut acc:Option<&mut Vec<TokenTree>>) 
-> Result<Cursor<'a>, syn::Error>{
	let (e_tree,expected) = match expected.token_tree() {
		Some(pair) => pair,
		None => {
			return Ok(actual);
		} 
	};

	let (a_tree,a_next) = match actual.token_tree() {
		Some(pair) => pair,
		None => {
			return_match_err!(expected,actual,del);
		} 
	};
	let actual = a_next;
	
	let matches = match (a_tree,e_tree) {
		(TokenTree::Ident(x),TokenTree::Ident(y)) => x==y,
		(TokenTree::Punct(x),TokenTree::Punct(y)) => x.to_string()==y.to_string(),
		(TokenTree::Literal(x),TokenTree::Literal(y)) => x.to_string()==y.to_string(),
		(TokenTree::Group(e),TokenTree::Group(a)) => {
			if e.delimiter()!=a.delimiter() {
				return_match_err!(expected,actual,del);
			}
			let buff_a = TokenBuffer::new2(a.stream());
			let buff_e = TokenBuffer::new2(e.stream());
			//we dont need the output if they match since its to an internal buffer
			parse_exact_match(buff_a.begin(),buff_e.begin(),a.delimiter(),None)?;
			
            if let Some(v) = acc.as_mut(){
                v.push(TokenTree::Group(a))
            };
			true
		},
	    _ => return_match_err!(expected,actual,del),
	};

	if !matches {
		return_match_err!(expected,actual,del);
	} else {
		parse_exact_match(actual,expected,del,acc)
	}
}



#[repr(transparent)]
pub struct ExactTokens(pub TokenBuffer);
impl Combinator<Vec<TokenTree>> for ExactTokens {

	fn parse<'a>(&self, actual: syn::buffer::Cursor<'a>) 
	-> Result<(syn::buffer::Cursor<'a>, Vec<TokenTree>), syn::Error> 
	{ 	
		let mut v = Vec::with_capacity(10);
		let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,Some(&mut v))?;
		Ok((cursor,v))
	}
}

impl fmt::Debug for ExactTokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tokens = Vec::new();
        let mut cursor = self.0.begin();
        while let Some((token, next)) = cursor.token_tree() {
            tokens.push(format!("{:?}", token));
            cursor = next;
        }
        f.debug_struct("ExactTokens")
            .field("tokens", &tokens)
            .finish()
    }
}

// // #[derive(Debug, Clone)]
// pub struct ExactTokensIgnore(pub TokenBuffer);
// impl<'a> Combinator<'a,()> for ExactTokensIgnore {

// 	fn parse(&self, actual: syn::buffer::Cursor<'a>) 
// 	-> Result<(syn::buffer::Cursor<'a>, ()), syn::Error> 
// 	{ 	
// 		let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,None)?;
// 		Ok((cursor,()))
// 	}
// }


#[test]
fn test_exact_tokens_combinator() {
    use proc_macro2::TokenStream;
    

    // Macro to test success
    macro_rules! test_success {
        ($expected:expr, $actual:expr) => {
            {
                let expected_stream: TokenStream = $expected.parse().unwrap();
                let actual_stream: TokenStream = $actual.parse().unwrap();

                let token_buffer = syn::buffer::TokenBuffer::new2(expected_stream);
                let combinator = ExactTokens(token_buffer);

                let actual_buff = syn::buffer::TokenBuffer::new2(actual_stream);

                if let Err(err) = combinator.parse(actual_buff.begin()) {
                    panic!(
                        "Expected parse to succeed, but it failed:\nExpected: {:?}\nActual: {:?}\nError: {:?}",
                        $expected, $actual, err
                    );
                }
            }
        };
    }

    // Macro to test failure
    macro_rules! test_failure {
        ($expected:expr, $actual:expr) => {
            {
                let expected_stream: TokenStream = $expected.parse().unwrap();
                let actual_stream: TokenStream = $actual.parse().unwrap();

                let token_buffer = syn::buffer::TokenBuffer::new2(expected_stream);
                let combinator = ExactTokens(token_buffer);

                let actual_buff = syn::buffer::TokenBuffer::new2(actual_stream);
                if combinator.parse(actual_buff.begin()).is_ok() {
                    panic!(
                        "Expected parse to fail, but it succeeded:\nExpected: {:?}\nActual: {:?}",
                        $expected, $actual
                    );
                }
            }
        };
    }

    // Test cases

    // Test 1: Simple token match
    test_success!(
        "a b c;",
        "a b c;"
    );

    // Test 2: Extra spaces should not affect matching
    test_success!(
        "a b c;",
        "a     b c ;"
    );

    // Test 3: Comments should not affect matching
    test_success!(
        "a b c;",
        "a /* comment */ b // inline comment\n c ;"
    );

    // Test 4: Mismatched tokens
    test_failure!(
        "a b c;",
        "a b d;"
    );

    // Test 5: Nested groups with exact match
    test_success!(
        "a (b {c}) d;",
        "a (b {c}) d;"
    );

    // Test 6: Nested groups with mismatched delimiters
    test_failure!(
        "a (b {c}) d;",
        "a [b {c}] d;"
    );

    // Test 7: Nested groups with mismatched inner content
    test_failure!(
        "a (b {c}) d;",
        "a (b {x}) d;"
    );

    // Test 8: Complex non-Rust syntax
    test_success!(
        "custom_function(arg1,arg2,arg3);",
        "custom_function(arg1, arg2 /* comment */, arg3);"
    );

    // Test 9: Different spacing and comments within nested groups
    test_success!(
        "outer(inner1{inner2[data]});",
        "outer ( inner1 /* inline */ { inner2 // comment\n [ data ] } ) ;"
    );

    // Test 10: Unexpected EOF in actual tokens
    test_failure!(
        "a b c;",
        "a b"
    );

    // Test 11: Extra tokens in actual tokens
    test_success!(
        "a b",
        "a b c;"
    );
}