#![allow(clippy::from_over_into)]

use crate::types::{Type,Object,ObjectParser};
use crate::types::BasicType;

use std::rc::Rc;
use crate::syn::buffer::{Cursor,TokenBuffer};
use proc_macro2::{TokenStream,TokenTree,Delimiter, Group, Ident, Literal, Punct};


pub trait Combinator<T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), E>;

}

pub trait CombinatorUtils<T, E = syn::Error>{
	fn parse_into<'a,U:From<T>>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>,  U), E>;
}

impl<T, E,C > CombinatorUtils<T,E> for C 
where C : Combinator<T,E>,
E: std::error::Error, {
	fn parse_into<'a,U:From<T>>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>,  U), E>{
    	let (cursor,ans) = self.parse(input)?;
    	Ok((cursor,ans.into()))
    }
}
    

fn parse_literal(input: Cursor) -> Result<(Cursor, Literal), syn::Error> {
    match input.token_tree() {
        Some((TokenTree::Literal(lit), next)) => Ok((next, lit)),
        Some((other, _)) => Err(syn::Error::new(other.span(), "Expected a literal (number or string)")),
        None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting a literal")),
    }
}

fn parse_word(input: Cursor) -> Result<(Cursor, Ident), syn::Error> {
    match input.token_tree() {
        Some((TokenTree::Ident(ident), next)) => Ok((next, ident)),
        Some((other, _)) => Err(syn::Error::new(other.span(), "Expected an identifier")),
        None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting an identifier")),
    }
}

fn parse_punc(input: Cursor) -> Result<(Cursor, Punct), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Punct(punct), next)) => Ok((next,punct)),
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected one of +!#?.'& etc")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting punctuation")),
        }
    }

fn parse_group(input: Cursor) -> Result<(Cursor, Group), syn::Error> {
    match input.token_tree() {
        Some((TokenTree::Group(group), next)) => {
            Ok((next, group))
        }
        Some((other, _)) => Err(syn::Error::new(other.span(), "Expected one of [...] (...) {...}")),
        None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting a group")),
    }
}



fn parse_any(input: Cursor) -> Result<(Cursor, TokenTree), syn::Error> {
    match input.token_tree() {
        Some((tree, next)) => {
            Ok((next, tree))
        }

        None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF")),
    }
}

macro_rules! define_parser {
    ($name:ident, $parse_fn:ident, $basic_type:expr) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name;

        impl Combinator<Object> for $name {
            fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
                let (next, token) = $parse_fn(input)?;
                Ok((next, Object::new(token,self.type_info())))
            }
        }

        impl ObjectParser for $name {
            fn type_info(&self) -> Type {
            	use BasicType::*;
                Type::Basic($basic_type)
            }
        }
    };
}

define_parser!(LiteralParser, parse_literal, Literal);
define_parser!(WordParser, parse_word, Word);
define_parser!(PuncParser, parse_punc, Punc);
define_parser!(GroupParser, parse_group, Group);
define_parser!(AnyParser, parse_any, Tree);

#[test]
fn test_dumby_dyn_structs(){
	let _any :Rc<dyn ObjectParser> = Rc::new(AnyParser);
}


#[test]
fn test_basic_combinators() {
    use syn::buffer::TokenBuffer;
    use proc_macro2::TokenStream;

    let tokens: TokenStream = "42 identifier + (inner) invalid".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let mut cursor = buffer.begin();

    // Test LiteralCombinator (success)
    match parse_literal(cursor) {
        Ok((next,literal)) => {
            assert_eq!(literal.to_string(), "42");
            cursor = next;
        }
        Err(err) => panic!("LiteralCombinator failed: {}", err),
    }

    // Test WordCombinator (success)
    match parse_word(cursor) {
        Ok((next, ident)) => {
            assert_eq!(ident.to_string(), "identifier");
            cursor = next;
        }
        Err(err) => panic!("WordCombinator failed: {}", err),
    }

    // Test PuncCombinator (success)
    match parse_punc(cursor) {
        Ok((next, punct)) => {
            assert_eq!(punct.as_char(), '+');
            cursor = next;
        }
        Err(err) => panic!("PuncCombinator failed: {}", err),
    }

    // Test ParenCombinator (success)
    match parse_group(cursor) {
        Ok((next, group)) => {
            assert_eq!(group.delimiter(), Delimiter::Parenthesis);
            assert_eq!(group.stream().to_string(), "inner");
            cursor = next;
        }
        Err(err) => panic!("ParenCombinator failed: {}", err),
    }

    // Test LiteralCombinator (failure)
    match parse_literal(cursor) {
        Ok((_,x)) => panic!("Expected LiteralCombinator to fail, but it succeeded with {:?}",x),
        Err(_) => (),
    }

    // Move the cursor forward to ensure trailing data works
    match parse_any(cursor) {
        Ok((_, tree)) => {
            assert_eq!(tree.to_string(), "invalid");
        }
        Err(err) => panic!("WordCombinator failed on trailing data: {}", err),
    }
}

#[derive(Debug,Clone,Copy)]
pub struct DelTokenParser (pub Delimiter);

impl DelTokenParser{
	pub fn get_start_del(&self) -> &str {
		match self.0 {
            Delimiter::Parenthesis => "(",
            Delimiter::Bracket => "[",
            Delimiter::Brace => "{",
            Delimiter::None => "<empty delim (likely bug)>",
        }
	}

	pub fn get_end_del(&self) -> &str {
		match self.0 {
            Delimiter::Parenthesis => ")",
            Delimiter::Bracket => "]",
            Delimiter::Brace => "}",
            Delimiter::None => "<EOF>",
        }
	}
}

impl Combinator<TokenStream> for DelTokenParser {
    fn parse<'a>(&self, input: Cursor<'a>) -> syn::Result<(Cursor<'a>, TokenStream)> {
        match input.group(self.0) {
            Some((group, _, next)) => {
                Ok((next, group.token_stream()))
            }
            None => {
                let delimiter_name = self.get_start_del();
                Err(syn::Error::new(input.span(), format!("Expected delimited group starting with '{}'", delimiter_name)))
            }
        }
    }
}

fn parse_delimited<'a, T>(
    input: Cursor<'a>,
    del_token_parser: DelTokenParser,
    parse_inner: impl for<'b> FnOnce(Cursor<'b>) -> Result<(Cursor<'b>, T), syn::Error>,
) -> Result<(Cursor<'a>, T), syn::Error> {
    let (cursor, inner) = del_token_parser.parse(input)?;
    let buff = TokenBuffer::new2(inner);

    let (inner_cursor, result) = parse_inner(buff.begin())?;

    if !inner_cursor.eof() {
        let delimiter_name = del_token_parser.get_end_del();
        return Err(syn::Error::new(
            input.span(),
            format!("Expected delimited group starting with '{}'", delimiter_name),
        ));
    }

    Ok((cursor, result))
}

/// Generic parser for delimited groups with typed output.
#[derive(Clone)]
pub struct DelCombParser<T>(pub DelTokenParser, pub Rc<dyn Combinator<T>>);

impl<T> Combinator<T> for DelCombParser<T> {
    fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), syn::Error> {
        // Pass the inner parser as a closure
        parse_delimited(input,self.0, |inner_input| self.1.parse(inner_input))
    }
}

/// Generic object parser for delimited groups.
#[derive(Debug,Clone)]
pub struct DelParser(pub DelTokenParser, pub Rc<dyn ObjectParser>);

impl Combinator<Object> for DelParser {
    fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
        // Pass the inner parser as a closure
        parse_delimited(input,self.0, |inner_input| self.1.parse(inner_input))
    }
}

impl ObjectParser for DelParser{
fn type_info(&self) -> Type { self.1.type_info()}
}

impl DelParser{
	pub fn new(del:Delimiter,parser:Rc<dyn ObjectParser>) -> Self{
		DelParser(DelTokenParser(del),parser)
	}
}


// pub struct DelObjectParser()

#[test]
fn test_delimited_sequence_combinator() {
    let tokens: TokenStream = "[1, 2, 3]".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let cursor = buffer.begin();

    let combinator = DelTokenParser(Delimiter::Bracket);

    // Test parsing a delimited sequence
    match combinator.parse(cursor) {
        Ok((next, token_stream)) => {
            assert_eq!(token_stream.to_string(), "1 , 2 , 3");
            assert!(next.token_tree().is_none(), "Expected no tokens after the delimited group");
        }
        Err(err) => panic!("DelimitedSequence failed: {}", err),
    }

    // Test parsing with wrong delimiter
    let tokens: TokenStream = "(1, 2, 3)".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let cursor = buffer.begin();

    match combinator.parse(cursor) {
        Ok(_) => panic!("Expected DelimitedSequence to fail, but it succeeded"),
        Err(err) => {
            assert!(err.to_string().contains("Expected delimited group starting with '['"));
        }
    }

    // Test the DelCombParser for nested delimiters (e.g., "([])")
    let tokens: TokenStream = "([text])".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let cursor = buffer.begin();

    let inner_combinator = Rc::new(DelTokenParser(Delimiter::Bracket));
    let nested_combinator = DelCombParser(DelTokenParser(Delimiter::Parenthesis), inner_combinator);

    match nested_combinator.parse(cursor) {
        Ok((next, token_stream)) => {
            assert_eq!(token_stream.to_string(), "text");
            assert!(next.token_tree().is_none(), "Expected no tokens after the delimited group");
        }
        Err(err) => panic!("Nested DelimitedSequence failed: {}", err),
    }

    // Test DelCombParser failure with mismatched delimiters (e.g., "{[]}")
    let tokens: TokenStream = "{[]}".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let cursor = buffer.begin();

    match nested_combinator.parse(cursor) {
        Ok(_) => panic!("Expected Nested DelimitedSequence to fail, but it succeeded"),
        Err(err) => {
            assert!(err.to_string().contains("Expected delimited group starting with '('"));
        }
    }
}


pub enum PreOp{
	Many0,
	Many1,
	Maybe,
	Not,
	Ignore,
}

// pub enum GenericPattern{
// 	Terminal(TerminalPatern),
// 	Delimited(Rc<GenericPattern>, Delimiter),
// 	PreOp(Rc<GenericPattern>,PreOp),
// 	Sequence(Rc<[GenericPattern]>),
// 	OneOf(Rc<[GenericPattern]>),
// }