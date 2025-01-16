use syn::parse::discouraged::Speculative;
use syn::parse::{Parse,ParseBuffer};
use std::rc::Rc;
use proc_macro2::{TokenStream, TokenTree,Delimiter};
use crate::basic_parsing::TokenLiteral;


#[derive(Debug)]
pub enum Pattern{
	Exact(TokenStream),
	
	//rusts core tokens
	Literal,
	Name,
	Punc(char),
	Paren(Rc<Pattern>,Delimiter),
	
	//optionals
	Any,
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

///currently a place holder 
#[derive(Debug)]
pub struct MatchError;


impl Pattern {

   pub fn matches<I>(&self, input: &mut I) -> Result<(), MatchError> 
   where I: Iterator<Item = TokenTree> + Clone,
   {
     match self {
         Pattern::Exact(stream) => {
             parse_exact_match(stream.clone(), input).map_err(|_| MatchError)
         }
         Pattern::Any => {
             if input.next().is_some() {
                 Ok(())
             } else {
                 Err(MatchError)
             }
         }
         Pattern::Maybe(inner) => {
             let mut clone_iter = input.clone();
             if inner.matches(&mut clone_iter).is_ok() {
                 *input = clone_iter;
             }
             Ok(())
         }
         Pattern::Literal => {
             match input.next() {
                 Some(TokenTree::Literal(_)) => Ok(()),
                 _ => Err(MatchError),
             }
         }
         Pattern::Name => {
             match input.next() {
                 Some(TokenTree::Ident(_)) => Ok(()),
                 _ => Err(MatchError),
             }
         }
         Pattern::Punc(expected_char) => {
             match input.next() {
                 Some(TokenTree::Punct(punct)) if punct.as_char() == *expected_char => Ok(()),
                 _ => Err(MatchError),
             }
         }
         Pattern::Paren(inner, delimiter) => {
             match input.next() {
                 Some(TokenTree::Group(group)) if group.delimiter() == *delimiter => {
                     let mut inner_iter = group.stream().into_iter();
                     inner.matches(&mut inner_iter)
                 }
                 _ => Err(MatchError),
             }
         }
         Pattern::Sequnce(patterns) => {
             for pattern in patterns.iter() {
                 pattern.matches(input)?;
             }
             Ok(())
         }
         Pattern::OneOf(patterns) => {
             for pattern in patterns.iter() {
                 let mut clone_iter = input.clone();
                 if pattern.matches(&mut clone_iter).is_ok() {
                     *input = clone_iter;
                     return Ok(());
                 }
             }
             Err(MatchError)
         }
         Pattern::Many0(inner) => {
             while inner.matches(input).is_ok() {}
             Ok(())
         }
         Pattern::Many1(inner) => {
             if inner.matches(input).is_err() {
                 return Err(MatchError);
             }
             while inner.matches(input).is_ok() {}
             Ok(())
         }
         Pattern::Not(inner) => {
             if inner.matches(input).is_err() {
                 Ok(())
             } else {
                 Err(MatchError)
             }
         }
         Pattern::EOF => {
             if input.next().is_none() {
                 Ok(())
             } else {
                 Err(MatchError)
             }
         }
      }
   }
}

///Compares an expected `TokenStream` to an actual `IntoIter`
//consuming from the iter in the process.
///Note that on fail this could overconsume the end of a Group.
pub fn parse_exact_match<I: Iterator<Item = TokenTree>>(expected: TokenStream, actual_iter: &mut I) -> Result<(), (TokenTree, TokenTree)> {
    for expected_token in expected.into_iter() {
        match actual_iter.next() {
            Some(actual_token) => {
                let matches = match (&expected_token, &actual_token) {
                    (TokenTree::Group(expected_group), TokenTree::Group(actual_group)) => {
                        if expected_group.delimiter() == actual_group.delimiter() {
                            //this can overconsume on fail because we are making a new stream
                            //however on sucesses it will allways consume exacly enough
                            parse_exact_match(expected_group.stream(), &mut actual_group.stream().into_iter())?;
                            true
                        } else {
                            false
                        }
                    }
                    (TokenTree::Literal(expected_lit), TokenTree::Literal(actual_lit)) => {
                        expected_lit.to_string() == actual_lit.to_string()
                    }
                    (TokenTree::Ident(expected_ident), TokenTree::Ident(actual_ident)) => {
                        expected_ident == actual_ident
                    }
                    (TokenTree::Punct(expected_punct), TokenTree::Punct(actual_punct)) => {
                        expected_punct.as_char() == actual_punct.as_char()
                    }
                    _ => false,
                };

                if !matches {
                    return Err((expected_token, actual_token));
                }
            }
            None => {
                return Err((
                    expected_token,
                    TokenTree::Literal(proc_macro2::Literal::string("<EOF>")),
                ));
            }
        }
    }

    Ok(())
}

impl Parse for Pattern {
	///tries to find something 
	fn parse(input: &ParseBuffer<'_>) -> Result<Self, syn::Error> { 
		//[tokens]
		if let Ok(lit) = TokenLiteral::parse(&input){
			return Ok(Pattern::Exact(lit.0));
		}


		todo!() 
	}
}


#[test]
fn test_pattern_matches() {
    // Test: Exact match
    {
        let pattern = Pattern::Exact("a b c".parse::<TokenStream>().unwrap());
        let mut input = "a b c".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
    }

    // Test: Any match
    {
        let pattern = Pattern::Any;
        let mut input = "any_token".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
    }

    // Test: Maybe match
    {
        let pattern = Pattern::Maybe(Rc::new(Pattern::Exact("optional".parse().unwrap())));
        let mut input = "optional".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
        assert!(input.next().is_none());

        let mut input = "something_else".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
        assert!(input.next().is_some());

    }

    // Test: Literal match
    {
        let pattern = Pattern::Literal;
        let mut input = "42".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "not_literal".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected Literal match to fail, got {:?}", result);
    }

    // Test: Name match
    {
        let pattern = Pattern::Name;
        let mut input = "identifier".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "123".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected Name match to fail, got {:?}", result);
    }

    // Test: Punctuation match
    {
        let pattern = Pattern::Punc('+');
        let mut input = "+".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "-".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected Punctuation match to fail, got {:?}", result);
    }

    // Test: Parenthesized pattern
    {
        let pattern = Pattern::Paren(
            Rc::new(Pattern::Exact("inner".parse().unwrap())),
            Delimiter::Parenthesis,
        );
        let mut input = "(inner)".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "{inner}".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected Parenthesized match to fail, got {:?}", result);
    }

    // Test: Sequence pattern
    {
        let pattern = Pattern::Sequnce(Box::new([
            Rc::new(Pattern::Name),
            Rc::new(Pattern::Punc('=')),
            Rc::new(Pattern::Literal),
        ]));
        let mut input = "var_name = 42".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
    }

    // Test: OneOf pattern
    {
        let pattern = Pattern::OneOf(Box::new([
            Rc::new(Pattern::Exact("a".parse().unwrap())),
            Rc::new(Pattern::Exact("b".parse().unwrap())),
        ]));
        let mut input = "a".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "b".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "c".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected OneOf match to fail, got {:?}", result);
    }

    // Test: Many0 pattern
    {
        let pattern = Pattern::Many0(Rc::new(Pattern::Literal));
        let mut input = "1 2 3".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();
    }

    // Test: Many1 pattern
    {
        let pattern = Pattern::Many1(Rc::new(Pattern::Literal));
        let mut input = "1 2 3".parse::<TokenStream>().unwrap().into_iter();
        pattern.matches(&mut input).unwrap();

        let mut input = "".parse::<TokenStream>().unwrap().into_iter();
        let result = pattern.matches(&mut input);
        assert!(result.is_err(), "Expected Many1 match to fail, got {:?}", result);
    }

    // Test: Complex hierarchy (3 levels deep and 2 branches wide)
    {
        let pattern = Pattern::Sequnce(Box::new([
            Rc::new(Pattern::Paren(
                Rc::new(Pattern::Sequnce(Box::new([
                    Rc::new(Pattern::Name),
                    Rc::new(Pattern::Punc(',')),
                    Rc::new(Pattern::Literal),
                ]))),
                Delimiter::Parenthesis,
            )),
            Rc::new(Pattern::OneOf(Box::new([
                Rc::new(Pattern::Exact("branch1".parse().unwrap())),
                Rc::new(Pattern::Exact("branch2".parse().unwrap())),
            ]))),
        ]));
        let mut input = "(var_name, 42) branch1"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        pattern.matches(&mut input).unwrap();
    }
}

#[test]
fn test_parse_exact_match() {
	use proc_macro2::Delimiter;

    // Test 1: Simple token match
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b c;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 2: Extra spaces should not affect matching
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a     b c ;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 3: Comments should not affect matching
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a /* comment */ b // inline comment\n c ;"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 4: Mismatched tokens
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b d;".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Ident(left), TokenTree::Ident(right)) => {
                assert_eq!(left, "c");
                assert_eq!(right, "d");
            }
            _ => panic!("Expected a mismatch of identifiers, got {:?}", err),
        }
    }

    // Test 5: Nested groups with exact match
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a (b {c}) d;".parse::<TokenStream>().unwrap().into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 6: Nested groups with mismatched delimiters
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a [b {c}] d;".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Group(left), TokenTree::Group(right)) => {
                assert_eq!(left.delimiter(), Delimiter::Parenthesis);
                assert_eq!(right.delimiter(), Delimiter::Bracket);
            }
            _ => panic!("Expected a mismatch of group delimiters, got {:?}", err),
        }
    }

    // Test 7: Nested groups with mismatched inner content
    {
        let expected: TokenStream = "a (b {c}) d;".parse().unwrap();
        let mut actual_iter = "a (b {x}) d;".parse::<TokenStream>().unwrap().into_iter();
        let _err = parse_exact_match(expected, &mut actual_iter).unwrap_err();
    }

    // Test 8: Complex non-Rust syntax
    {
        let expected: TokenStream = "custom_function(arg1,arg2,arg3);".parse().unwrap();
        let mut actual_iter = "custom_function(arg1, arg2 /* comment */, arg3);"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 9: Different spacing and comments within nested groups
    {
        let expected: TokenStream = "outer(inner1{inner2[data]});".parse().unwrap();
        let mut actual_iter = "outer ( inner1 /* inline */ { inner2 // comment\n [ data ] } ) ;"
            .parse::<TokenStream>()
            .unwrap()
            .into_iter();
        assert!(parse_exact_match(expected, &mut actual_iter).is_ok());
    }

    // Test 10: Unexpected EOF in actual tokens
    {
        let expected: TokenStream = "a b c;".parse().unwrap();
        let mut actual_iter = "a b".parse::<TokenStream>().unwrap().into_iter();
        let err = parse_exact_match(expected, &mut actual_iter).unwrap_err();
        match err {
            (TokenTree::Ident(left), TokenTree::Literal(right)) => {
                assert_eq!(left, "c");
                assert_eq!(right.to_string(), "\"<EOF>\"");
            }
            _ => panic!("Expected an EOF mismatch, got {:?}", err),
        }
    }

    // Test 11: Extra tokens in actual tokens
    {
        let expected: TokenStream = "a b".parse().unwrap();
        let mut actual_iter = "a b c;".parse::<TokenStream>().unwrap().into_iter();
			parse_exact_match(expected, &mut actual_iter).unwrap();
    }
}
