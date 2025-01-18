use crate::basic_parsing::DelParser;
use crate::basic_parsing::Combinator;
use crate::basic_parsing::BasicCombinator;
use crate::basic_parsing::{LiteralCombinator,WordCombinator,PuncCombinator,GroupCombinator,AnyCombinator};


use std::fmt;
use syn::buffer::TokenBuffer;
use syn::buffer::Cursor;
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use std::rc::Rc;
// use syn::parse::Parse;

#[derive(Debug)]
pub enum Pattern {
	Exact(ExactTokens),
	Delimited(Rc<Pattern>, Delimiter),


	//rusts core tokens
	Literal,
	Word,
	Punc,
	Group,


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
}

impl<'a> Combinator<'a,Vec<TokenTree>> for Pattern {

	fn parse(&self, input: syn::buffer::Cursor<'a>) -> syn::Result<(syn::buffer::Cursor<'a>, Vec<TokenTree>)> { 
		match self {
			Pattern::Exact(parser) => parser.parse(input),
		   Pattern::Delimited(parser, del) => {
		   	let (rest,tree)=DelParser(*del).parse(input)?;
		   	let buff = TokenBuffer::new2(tree);
		   	let (inner_c,v) = parser.parse(buff.begin())?;
		   	if !inner_c.eof(){
		   		let s = match del {
		   			Delimiter::None =>"<EOF (empty delim)>",
	        			Delimiter::Parenthesis=> ")",
	        			Delimiter::Bracket=> "]",
	        			Delimiter::Brace => "}",
		   		};
		   		return Err(syn::Error::new(
		   			inner_c.span(),format!("expected {} found: {}",
		   				s,inner_c.span().source_text()
		   					.unwrap_or_else(|| "<missing source>".to_string()))
		   			)
		   		)
		   	}
		   	Ok((rest,v))
		   },


			Pattern::Literal => LiteralCombinator::parse(input).map(|(c,x)| {(c,vec![TokenTree::Literal(x.0)])}),
			Pattern::Word => WordCombinator::parse(input).map(|(c,x)| {(c,vec![TokenTree::Ident(x.0)])}),
			Pattern::Punc => PuncCombinator::parse(input).map(|(c,x)| {(c,vec![TokenTree::Punct(x.0)])}),
			Pattern::Group => GroupCombinator::parse(input).map(|(c,x)| {(c,vec![TokenTree::Group(x.0)])}),
			Pattern::Any => AnyCombinator::parse(input).map(|(c,x)| {(c,vec![x.0])}),
		   
		   Pattern::Ignore(parser) => parser.parse(input).map(|(c,_)| {(c,vec![])}),
		   Pattern::Not(parser) => match parser.parse(input){
		   	Err(_) => Ok(input.token_tree()
		   		.map(|(tree,c)| (c,vec![tree]))
		   		.unwrap_or_else(||(input,vec![]))
		   	),
		   	Ok((_,wrong)) => {
		   		Err(
		   			syn::Error::new_spanned(wrong.into_iter().collect::<TokenStream>(),"found unexpected sequnce (Not)")
		   		)
		   	}
		   },
		   Pattern::Maybe(parser) => match parser.parse(input){
		   	Ok(x) => Ok(x),
		   	Err(_) => Ok((input,vec![]))
		   },

		   Pattern::Sequnce(seq) => {
		   	let mut input = input;
		   	let mut ans = Vec::with_capacity(seq.len());
		   	for p in seq{
		   		let (new_input,v) = p.parse(input)?;
		   		input = new_input;
		   		ans.extend(v);
		   	}

		   	Ok((input,ans))
		   },
		   Pattern::OneOf(seq) => {
		   	let mut error = syn::Error::new(input.span(),format!("errored on {} things (OneOf):",seq.len()));
		   	for p in seq{
		   		match p.parse(input){
		   			Ok(x) => {return Ok(x)},
		   			Err(e) =>{error.combine(e)}
		   		}
		   	}

		   	Err(error)
		   },
		   Pattern::Many0(parser) => {
		   	let mut input = input;
		   	let mut ans = Vec::new();
		   	while let Ok((new_input,v)) = parser.parse(input){
		   		input=new_input;
		   		ans.extend(v);
		   	}
		   	Ok((input,ans))

		   },
		   Pattern::Many1(parser) => {
		   	let (mut input,mut ans) = parser.parse(input)?;
		   	while let Ok((new_input,v)) = parser.parse(input){
		   		input=new_input;
		   		ans.extend(v);
		   	}
		   	Ok((input,ans))

		   },
		}
	}
	
}

#[test]
fn test_pattern_matches() {
    use Pattern::*;
    
    // Test: Exact match
    {
        let pattern = Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
            "a b c".parse::<TokenStream>().unwrap()
        )));
        let input = syn::buffer::TokenBuffer::new2("a b c".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();
    }

    // Test: Any match
    {
        let pattern = Any;
        let input = syn::buffer::TokenBuffer::new2("any_token".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();
    }

    // Test: Maybe match
    {
        let pattern = Maybe(Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
            "optional".parse::<TokenStream>().unwrap()
        )))));

        let input = syn::buffer::TokenBuffer::new2("optional something_else".parse::<TokenStream>().unwrap());
        let cursor = input.begin();
        let (cursor,_) = pattern.parse(cursor).unwrap();
        pattern.parse(cursor).unwrap();
    }

    // Test: Literal match
    {
        let pattern = Literal;

        let input = syn::buffer::TokenBuffer::new2("42".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();

        let input = syn::buffer::TokenBuffer::new2("not_literal".parse::<TokenStream>().unwrap());
        assert!(pattern.parse(input.begin()).is_err());
    }

    // Test: Word match
    {
        let pattern = Word;

        let input = syn::buffer::TokenBuffer::new2("identifier".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();

        let input = syn::buffer::TokenBuffer::new2("123".parse::<TokenStream>().unwrap());
        assert!(pattern.parse(input.begin()).is_err());
    }

    // Test: Punctuation match
    {
        let pattern = Punc;

        let buffer = syn::buffer::TokenBuffer::new2("+@".parse::<TokenStream>().unwrap());
        let(cursor,_)=pattern.parse(buffer.begin()).unwrap();
        pattern.parse(cursor).unwrap();
    }

        // Test: Ignore pattern
    {
        let pattern = Ignore(Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
            "ignore_this".parse::<TokenStream>().unwrap()
        )))));

        let input = syn::buffer::TokenBuffer::new2("ignore_this keep_this".parse::<TokenStream>().unwrap());
        let (cursor, result) = pattern.parse(input.begin()).unwrap();

        // Ensure the ignored tokens are not in the result
        assert!(result.is_empty());

        // The remaining cursor should still contain "keep_this"
        assert_eq!(
            cursor.token_tree().unwrap().0.to_string(),
            "keep_this"
        );
    }

    // Test: Not pattern
    {
        let pattern = Not(Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
            "not_this".parse::<TokenStream>().unwrap()
        )))));

        // Input that does NOT match "not_this"
        let input = syn::buffer::TokenBuffer::new2("valid_token".parse::<TokenStream>().unwrap());
        let (_cursor, result) = pattern.parse(input.begin()).unwrap();

        // Ensure the token is parsed as it doesn't match "not_this"
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_string(), "valid_token");

        // Input that matches "not_this"
        let input = syn::buffer::TokenBuffer::new2("not_this".parse::<TokenStream>().unwrap());
        assert!(pattern.parse(input.begin()).is_err());
    }
    

    // Test: Sequence pattern
    {
        let pattern = Sequnce(Box::new([
            Rc::new(Word),
            Rc::new(Punc),
            Rc::new(Literal),
        ]));

        let input = syn::buffer::TokenBuffer::new2("var = 42".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();
    }

    // Test: OneOf pattern
    {
        let pattern = OneOf(Box::new([
            Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
                "a".parse::<TokenStream>().unwrap()
            )))),
            Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
                "b".parse::<TokenStream>().unwrap()
            )))),
        ]));

        let input = syn::buffer::TokenBuffer::new2("a".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();

        let input = syn::buffer::TokenBuffer::new2("b".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();

        let input = syn::buffer::TokenBuffer::new2("c".parse::<TokenStream>().unwrap());
        assert!(pattern.parse(input.begin()).is_err());
    }

    // Test: Many0 pattern
    {
        let pattern = Many0(Rc::new(Literal));

        let input = syn::buffer::TokenBuffer::new2("1 2 3".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();
    }

    // Test: Many1 pattern
    {
        let pattern = Many1(Rc::new(Literal));

        let input = syn::buffer::TokenBuffer::new2("1 2 3".parse::<TokenStream>().unwrap());
        pattern.parse(input.begin()).unwrap();

        let input = syn::buffer::TokenBuffer::new2("".parse::<TokenStream>().unwrap());
        assert!(pattern.parse(input.begin()).is_err());
    }

    // Test: Complex hierarchy (3 levels deep and 2 branches wide)
    {
        let pattern = Sequnce(Box::new([
            Rc::new(Delimited(
                Rc::new(Sequnce(Box::new([
                    Rc::new(Word),
                    Rc::new(Punc),
                    Rc::new(Literal),
                ]))),
                Delimiter::Parenthesis
            )),
            Rc::new(OneOf(Box::new([
                Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
                    "branch1".parse::<TokenStream>().unwrap()
                )))),
                Rc::new(Exact(ExactTokens(syn::buffer::TokenBuffer::new2(
                    "branch2".parse::<TokenStream>().unwrap()
                )))),
            ]))),
        ]));

        let input = syn::buffer::TokenBuffer::new2(
            "(var_word, 42) branch1".parse::<TokenStream>().unwrap()
        );
        pattern.parse(input.begin()).unwrap();
    }
}


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

	fn parse(&self, actual: syn::buffer::Cursor<'a>) 
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

	fn parse(&self, actual: syn::buffer::Cursor<'a>) 
	-> Result<(syn::buffer::Cursor<'a>, ()), syn::Error> 
	{ 	
		let cursor = parse_exact_match(actual,self.0.begin(),Delimiter::None,None)?;
		Ok((cursor,()))
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