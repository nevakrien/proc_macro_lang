use syn::buffer::TokenBuffer;
use syn::buffer::Cursor;
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use std::rc::Rc;
// use syn::parse::Parse;

#[derive(Debug)]
pub enum Pattern {
    Exact(TokenStream),
    Delimited(Rc<Pattern>, Delimiter),
    

    //rusts core tokens
    Literal,
    Word,
    Punc,
    Paren,


    //optionals
    Any,
    Ignore(Rc<Pattern>),//ignore the tokens of this pattern but still parse it
    Not(Rc<Pattern>),
    Maybe(Rc<Pattern>),

    //composite
    Sequnce(Box<[Rc<Pattern>]>),
    OneOf(Box<[Rc<Pattern>]>),

    //unbounded
    Many0(Rc<Pattern>),
    Many1(Rc<Pattern>),

    EOF,
}

// ///currently a place holder
// #[derive(Debug)]
// pub struct MatchError;

// impl Pattern {
//     pub fn matches<I>(&self, input: &mut I) -> Result<(), MatchError>
//     where
//         I: Iterator<Item = TokenTree> + Clone,
//     {
//         match self {
//             Pattern::Exact(stream) => {
//                 parse_exact_match(stream.clone(), input).map_err(|_| MatchError)
//             }
//             Pattern::Any => {
//                 if input.next().is_some() {
//                     Ok(())
//                 } else {
//                     Err(MatchError)
//                 }
//             }
//             Pattern::Maybe(inner) => {
//                 let mut clone_iter = input.clone();
//                 if inner.matches(&mut clone_iter).is_ok() {
//                     *input = clone_iter;
//                 }
//                 Ok(())
//             }
//             Pattern::Literal => match input.next() {
//                 Some(TokenTree::Literal(_)) => Ok(()),
//                 _ => Err(MatchError),
//             },
//             Pattern::Word => match input.next() {
//                 Some(TokenTree::Ident(_)) => Ok(()),
//                 _ => Err(MatchError),
//             },
//             Pattern::Punc(expected_char) => match input.next() {
//                 Some(TokenTree::Punct(punct)) if punct.as_char() == *expected_char => Ok(()),
//                 _ => Err(MatchError),
//             },
//             Pattern::Paren(inner, delimiter) => match input.next() {
//                 Some(TokenTree::Group(group)) if group.delimiter() == *delimiter => {
//                     let mut inner_iter = group.stream().into_iter();
//                     inner.matches(&mut inner_iter)
//                 }
//                 _ => Err(MatchError),
//             },
//             Pattern::Sequnce(patterns) => {
//                 for pattern in patterns.iter() {
//                     pattern.matches(input)?;
//                 }
//                 Ok(())
//             }
//             Pattern::OneOf(patterns) => {
//                 for pattern in patterns.iter() {
//                     let mut clone_iter = input.clone();
//                     if pattern.matches(&mut clone_iter).is_ok() {
//                         *input = clone_iter;
//                         return Ok(());
//                     }
//                 }
//                 Err(MatchError)
//             }
//             Pattern::Many0(inner) => {
//                 while inner.matches(input).is_ok() {}
//                 Ok(())
//             }
//             Pattern::Many1(inner) => {
//                 if inner.matches(input).is_err() {
//                     return Err(MatchError);
//                 }
//                 while inner.matches(input).is_ok() {}
//                 Ok(())
//             }
//             Pattern::Not(inner) => {
//                 if inner.matches(input).is_err() {
//                     Ok(())
//                 } else {
//                     Err(MatchError)
//                 }
//             }
//             Pattern::EOF => {
//                 if input.next().is_none() {
//                     Ok(())
//                 } else {
//                     Err(MatchError)
//                 }
//             }
//         }
//     }
// }

// ///Compares an expected `TokenStream` to an actual `IntoIter`
// //consuming from the iter in the process.
// ///Note that on fail this could overconsume the end of a Group.
// pub fn parse_exact_match<I: Iterator<Item = TokenTree>>(
//     expected: TokenStream,
//     actual_iter: &mut I,
// ) -> Result<(), (TokenTree, TokenTree)> {
//     for expected_token in expected.into_iter() {
//         match actual_iter.next() {
//             Some(actual_token) => {
//                 let matches = match (&expected_token, &actual_token) {
//                     (TokenTree::Group(expected_group), TokenTree::Group(actual_group)) => {
//                         if expected_group.delimiter() == actual_group.delimiter() {
//                             //this can overconsume on fail because we are making a new stream
//                             //however on sucesses it will allways consume exacly enough
//                             parse_exact_match(
//                                 expected_group.stream(),
//                                 &mut actual_group.stream().into_iter(),
//                             )?;
//                             true
//                         } else {
//                             false
//                         }
//                     }
//                     (TokenTree::Literal(expected_lit), TokenTree::Literal(actual_lit)) => {
//                         expected_lit.to_string() == actual_lit.to_string()
//                     }
//                     (TokenTree::Ident(expected_ident), TokenTree::Ident(actual_ident)) => {
//                         expected_ident == actual_ident
//                     }
//                     (TokenTree::Punct(expected_punct), TokenTree::Punct(actual_punct)) => {
//                         expected_punct.as_char() == actual_punct.as_char()
//                     }
//                     _ => false,
//                 };

//                 if !matches {
//                     return Err((expected_token, actual_token));
//                 }
//             }
//             None => {
//                 return Err((
//                     expected_token,
//                     TokenTree::Literal(proc_macro2::Literal::string("<EOF>")),
//                 ));
//             }
//         }
//     }

//     Ok(())
// }

// // impl Parse for Pattern {
// //     ///tries to find something
// //     fn parse(input: &ParseBuffer<'_>) -> Result<Self, syn::Error> {
// //         //[tokens]
// //         if let Ok(lit) = TokenLiteral::parse(input) {
// //             return Ok(Pattern::Exact(lit.0));
// //         }

// //         todo!()
// //     }
// // }

// #[test]
// fn test_pattern_matches() {
//     // Test: Exact match
//     {
//         let pattern = Pattern::Exact("a b c".parse::<TokenStream>().unwrap());
//         let mut input = "a b c".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//     }

//     // Test: Any match
//     {
//         let pattern = Pattern::Any;
//         let mut input = "any_token".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//     }

//     // Test: Maybe match
//     {
//         let pattern = Pattern::Maybe(Rc::new(Pattern::Exact("optional".parse().unwrap())));
//         let mut input = "optional".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//         assert!(input.next().is_none());

//         let mut input = "something_else".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//         assert!(input.next().is_some());
//     }

//     // Test: Literal match
//     {
//         let pattern = Pattern::Literal;
//         let mut input = "42".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "not_literal".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected Literal match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Word match
//     {
//         let pattern = Pattern::Word;
//         let mut input = "identifier".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "123".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected Word match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Punctuation match
//     {
//         let pattern = Pattern::Punc('+');
//         let mut input = "+".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "-".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected Punctuation match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Parenthesized pattern
//     {
//         let pattern = Pattern::Paren(
//             Rc::new(Pattern::Exact("inner".parse().unwrap())),
//             Delimiter::Parenthesis,
//         );
//         let mut input = "(inner)".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "{inner}".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected Parenthesized match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Sequence pattern
//     {
//         let pattern = Pattern::Sequnce(Box::new([
//             Rc::new(Pattern::Word),
//             Rc::new(Pattern::Punc('=')),
//             Rc::new(Pattern::Literal),
//         ]));
//         let mut input = "var_Word = 42".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//     }

//     // Test: OneOf pattern
//     {
//         let pattern = Pattern::OneOf(Box::new([
//             Rc::new(Pattern::Exact("a".parse().unwrap())),
//             Rc::new(Pattern::Exact("b".parse().unwrap())),
//         ]));
//         let mut input = "a".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "b".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "c".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected OneOf match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Many0 pattern
//     {
//         let pattern = Pattern::Many0(Rc::new(Pattern::Literal));
//         let mut input = "1 2 3".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();
//     }

//     // Test: Many1 pattern
//     {
//         let pattern = Pattern::Many1(Rc::new(Pattern::Literal));
//         let mut input = "1 2 3".parse::<TokenStream>().unwrap().into_iter();
//         pattern.matches(&mut input).unwrap();

//         let mut input = "".parse::<TokenStream>().unwrap().into_iter();
//         let result = pattern.matches(&mut input);
//         assert!(
//             result.is_err(),
//             "Expected Many1 match to fail, got {:?}",
//             result
//         );
//     }

//     // Test: Complex hierarchy (3 levels deep and 2 branches wide)
//     {
//         let pattern = Pattern::Sequnce(Box::new([
//             Rc::new(Pattern::Paren(
//                 Rc::new(Pattern::Sequnce(Box::new([
//                     Rc::new(Pattern::Word),
//                     Rc::new(Pattern::Punc(',')),
//                     Rc::new(Pattern::Literal),
//                 ]))),
//                 Delimiter::Parenthesis,
//             )),
//             Rc::new(Pattern::OneOf(Box::new([
//                 Rc::new(Pattern::Exact("branch1".parse().unwrap())),
//                 Rc::new(Pattern::Exact("branch2".parse().unwrap())),
//             ]))),
//         ]));
//         let mut input = "(var_word, 42) branch1"
//             .parse::<TokenStream>()
//             .unwrap()
//             .into_iter();
//         pattern.matches(&mut input).unwrap();
//     }
// }


use crate::basic_parsing::Combinator;

#[derive(Clone,Copy,Debug)]
pub enum SpotType{
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
pub struct CodeSpot<'a>{
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

#[derive(Clone,Copy)]
pub struct ExactMatchError<'a,'b> {
    pub expected: CodeSpot<'b>,
    pub actual: CodeSpot<'a>,
}

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


fn parse_exact_match<'a,'b>(actual: Cursor<'a>,expected:Cursor<'b>,del:Delimiter,mut acc:Option<&mut Vec<TokenTree>>) 
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
			acc.as_mut().map(|v| v.push(TokenTree::Group(a)));
			true
		},
	    _ => return_match_err!(expected,actual,del),
	};

	if !matches {
		return_match_err!(expected,actual,del);
	} else {

		return parse_exact_match(actual,expected,del,acc)
	}
}

// #[derive(Debug, Clone)]
pub struct ExactTokens(pub TokenBuffer);
impl<'a> Combinator<'a,Vec<TokenTree>> for ExactTokens {

	fn parse(&mut self, actual: syn::buffer::Cursor<'a>) 
	-> Result<(syn::buffer::Cursor<'a>, Vec<TokenTree>), syn::Error> 
	{ 	
		let mut v = Vec::with_capacity(10);
		let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,Some(&mut v))?;
		Ok((cursor,v))
	}
}

// #[derive(Debug, Clone)]
pub struct ExactTokensIgnore(pub TokenBuffer);
impl<'a> Combinator<'a,()> for ExactTokensIgnore {

	fn parse(&mut self, actual: syn::buffer::Cursor<'a>) 
	-> Result<(syn::buffer::Cursor<'a>, ()), syn::Error> 
	{ 	
		let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,None)?;
		Ok((cursor,()))
	}
}

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
                let mut combinator = ExactTokens(token_buffer);

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
                let mut combinator = ExactTokens(token_buffer);

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