
use crate::combinator::Pakerat;
use crate::combinator::PakeratError;
use crate::combinator::{Combinator,State};


use crate::types::{Type,Object,ObjectParser};
use crate::types::BasicType;

use std::rc::Rc;
use crate::syn::buffer::{Cursor};
use proc_macro2::{TokenTree,Delimiter, Group, Ident, Literal, Punct};


#[cfg(test)]
use syn::buffer::TokenBuffer;

#[cfg(test)]
use crate::combinator::initialize_state;

#[cfg(test)]
use proc_macro2::TokenStream;

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

fn parse_empty(input: Cursor) -> Result<(Cursor,()), syn::Error> {
    match input.token_tree() {
        None => Ok((input,())), // No more tokens, end of block reached
        Some((unexpected, _)) => Err(syn::Error::new(
            unexpected.span(),
            "Expected the end of the block, but found unexpected token",
        )),
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

fn parse_int(input: Cursor) -> Result<(Cursor, i64), syn::Error> {
    match input.token_tree() {
        Some((proc_macro2::TokenTree::Literal(lit), next)) => {
            lit.to_string()
                .parse::<i64>()
                .map(|value| (next, value))
                .map_err(|e| syn::Error::new(lit.span(), format!("Failed to parse integer: {}", e)))
        }
        Some((other, _)) => Err(syn::Error::new(
            other.span(),
            "Expected an integer literal, but found something else",
        )),
        None => Err(syn::Error::new(
            input.span(),
            "Unexpected EOF while expecting an integer literal",
        )),
    }
}


macro_rules! define_parser {
    ($name:ident, $parse_fn:ident, $basic_type:expr) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name;

        impl Combinator<Object> for $name {
            fn parse_pakerat<'a>(&self, input: Cursor<'a>,_state:&mut State) -> Pakerat<(Cursor<'a>, Object), syn::Error> {
                let (next, token) = $parse_fn(input).map_err(|e| PakeratError::Regular(e))?;
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
define_parser!(EndParser, parse_empty, None);
define_parser!(IntParser, parse_int, Int);

#[test]
fn test_dumby_dyn_structs(){
	let any :Rc<dyn ObjectParser> = Rc::new(AnyParser);
	let _delim = DelParser::new(Delimiter::Bracket,any); 
}


#[test]
fn test_basic_combinators() {
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
        Ok((next, tree)) => {
            assert_eq!(tree.to_string(), "invalid");
            cursor = next;


        }
        Err(err) => panic!("WordCombinator failed on trailing data: {}", err),
    }

    //empty
    match parse_empty(cursor) {
        Ok(_) => {
            // Successfully detected the end of block
        }
        Err(err) => panic!("Expected success, but got error: {}", err),
    }
}

#[test]
fn test_parse_int() {
    let tokens: TokenStream = "42 invalid_int".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let mut cursor = buffer.begin();

    // Test parse_int success
    match parse_int(cursor) {
        Ok((next, value)) => {
            assert_eq!(value, 42);
            cursor = next;
        }
        Err(err) => panic!("parse_int failed: {}", err),
    }

    // Test parse_int failure with non-integer literal
    match parse_int(cursor) {
        Ok((_, value)) => panic!(
            "Expected parse_int to fail, but it succeeded with {:?}",
            value
        ),
        Err(err) => {
            assert!(err.to_string().contains("Expected an integer"));
        }
    }
}

pub fn get_start_del(del:Delimiter) -> &'static str {
	match del {
        Delimiter::Parenthesis => "(",
        Delimiter::Bracket => "[",
        Delimiter::Brace => "{",
        Delimiter::None => "<empty delim (likely bug)>",
    }
}

pub fn get_end_del(del:Delimiter) -> &'static str {
	match del {
        Delimiter::Parenthesis => ")",
        Delimiter::Bracket => "]",
        Delimiter::Brace => "}",
        Delimiter::None => "<EOF>",
    }
}

//used as a tester
#[derive(Debug,Clone,Copy)]
pub struct DelTokenParser (Delimiter);

impl Combinator<TokenStream> for DelTokenParser {
    fn parse_pakerat<'a>(&self, input: Cursor<'a>,_state:&mut State) -> Pakerat<(Cursor<'a>, TokenStream)> {
        match input.group(self.0) {
            Some((group, _, next)) => {
                Ok((next, group.token_stream()))
            }
            None => {
                let delimiter_name = get_start_del(self.0);
                let message = format!("Expected delimited group starting with '{}'", delimiter_name);
                let error = syn::Error::new(input.span(),message); 
                Err(PakeratError::Regular(error))
            }
        }
    }
}

impl Combinator<Object> for DelTokenParser{
	fn parse_pakerat<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Pakerat<(Cursor<'a>, Object)>{
		let (cursor,stream): (syn::buffer::Cursor<'a>, TokenStream) = self.parse_pakerat(input,state)?;
		let v :Vec<Object>= stream.into_iter().map(|x| x.into()).collect();
		let t: Type = BasicType::Tree.into();
		let obj = Object::new(v,Type::Array(t.into()));
		Ok((cursor,obj))
	}
}

impl ObjectParser for DelTokenParser {

fn type_info(&self) -> Type {
	let t: Type = BasicType::Tree.into();
	Type::Array(t.into())
}
}


fn parse_delimited<'a, T>(
    input: Cursor<'a>,
    state: &mut State<'a>,
    del: Delimiter,
    parse_inner: &(impl Combinator<T, syn::Error> + ?Sized),
) -> Pakerat<(Cursor<'a>, T), syn::Error>
{
    let (group, next) = match input.group(del) {
        None => {
            let delimiter_name = get_start_del(del);
            return Err(PakeratError::Regular(syn::Error::new(
                input.span(),
                format!("Expected delimited group starting with '{}'", delimiter_name),
            )));
        }
        Some((group, _, next)) => (group, next),
    };

    let (inner_cursor, result) = parse_inner.parse_pakerat(group, state)?;

    if !inner_cursor.eof() {
        let delimiter_name = get_end_del(del);
        return Err(PakeratError::Regular(syn::Error::new(
            input.span(),
            format!("Expected delimited group ending with '{}'", delimiter_name),
        )));
    }

    Ok((next, result))
}

/// Generic parser for delimited groups with typed output.
#[derive(Clone)]
pub struct DelCombParser<T>(pub Delimiter, pub Rc<dyn Combinator<T>>);

impl<T> Combinator<T> for DelCombParser<T> {
    fn parse_pakerat<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Pakerat<(Cursor<'a>, T), syn::Error> {
        // Pass the inner parser as a closure
        parse_delimited(input,state,self.0, &*self.1)
    }
}

/// Generic object parser for delimited groups.
#[derive(Debug,Clone)]
pub struct DelParser(pub Delimiter, pub Rc<dyn ObjectParser>);

impl Combinator<Object> for DelParser {
    fn parse_pakerat<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Pakerat<(Cursor<'a>, Object), syn::Error> {
        parse_delimited(input,state,self.0, &*self.1)
    }
}

impl ObjectParser for DelParser{
fn type_info(&self) -> Type { self.1.type_info()}
}

impl DelParser{
	pub fn new(del:Delimiter,parser:Rc<dyn ObjectParser>) -> Self{
		DelParser(del,parser)
	}
}


// pub struct DelObjectParser()


#[test]
fn test_delimited_sequence_combinator() {
    let (buffer,mut state) = initialize_state("[1, 2, 3]").unwrap();
    let cursor = buffer.begin();
    let combinator = DelTokenParser(Delimiter::Bracket);

    // Test parsing a delimited sequence
    match combinator.parse(cursor,&mut state) {
        Ok((next, token_stream)) => {
            assert_eq!(token_stream.to_string(), "1 , 2 , 3");
            assert!(next.token_tree().is_none(), "Expected no tokens after the delimited group");
        }
        Err(err) => panic!("DelimitedSequence failed: {}", err),
    }

    // Test parsing with wrong delimiter
    let (buffer,mut state) = initialize_state("(1, 2, 3)").unwrap();
    let cursor = buffer.begin();


    match combinator.parse(cursor,&mut state) {
        Ok(_) => panic!("Expected DelimitedSequence to fail, but it succeeded"),
        Err(err) => {
            assert!(err.to_string().contains("Expected delimited group starting with '['"));
        }
    }

    // Test the DelCombParser for nested delimiters (e.g., "([])")
    let (buffer,mut state) = initialize_state("([text])").unwrap();
    let cursor = buffer.begin();

    let inner_combinator = Rc::new(DelTokenParser(Delimiter::Bracket));
    let nested_combinator = DelCombParser(Delimiter::Parenthesis, inner_combinator);

    match nested_combinator.parse(cursor,&mut state) {
        Ok((next, token_stream)) => {
            assert_eq!(token_stream.to_string(), "text");
            assert!(next.token_tree().is_none(), "Expected no tokens after the delimited group");
        }
        Err(err) => panic!("Nested DelimitedSequence failed: {}", err),
    }

    // Test DelCombParser failure with mismatched delimiters (e.g., "{[]}")
    let (buffer,mut state) = initialize_state("{[]}").unwrap();
    let cursor = buffer.begin();


    match nested_combinator.parse(cursor,&mut state) {
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