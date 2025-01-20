
use std::rc::Rc;
use crate::syn::buffer::Cursor;
use proc_macro2::{TokenStream};
use proc_macro2::{TokenTree,Delimiter, Group, Ident, Literal, Punct};


#[cfg(test)]
use syn::parse_str;
#[cfg(test)]
use crate::syn::buffer::TokenBuffer;



pub trait Combinator<'a, T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), E>;
}

pub trait MutCombinator<'a, T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse_mut(&mut self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), E>;
}

// Automatically implement MutCombinator for all Combinators
impl<'a, T, E, C> MutCombinator<'a, T, E> for C
where
    C: Combinator<'a, T, E>,
    E: std::error::Error,
{
    fn parse_mut(&mut self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), E> {
        // Delegate to the immutable version of parse
        Combinator::parse(self, input)
    }
}

pub trait BasicCombinator<'a, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), E>
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub struct TokenLiteral(pub TokenStream);

impl<'a> BasicCombinator<'a,  syn::Error> for TokenLiteral {
    #[inline]
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, TokenLiteral), syn::Error> {
        if let Some((ans,_, next)) = input.group(proc_macro2::Delimiter::Bracket) {
            Ok((next, TokenLiteral(ans.token_stream())))
        } else {
            Err(syn::Error::new(input.span(), "Expected [tokens]"))
        }
    }
}

impl Into<TokenStream> for TokenLiteral {
    fn into(self) -> proc_macro2::TokenStream {
        self.0
    }
}

#[test]
fn literal_tokens_parse() {
	let tokens = TokenBuffer::new2("[let x [=] 42;]".parse().unwrap());
    let cursor = tokens.begin();

    TokenLiteral::parse(cursor).expect("Failed to parse");
}

pub enum TerminalPatern {
	Exact(TokenStream),
	Any,

    Literal,
    Word,
    Punc,
    Group

}

impl<'a> BasicCombinator<'a, syn::Error> for TerminalPatern {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, TerminalPatern), syn::Error> {
        static ERROR_MESSAGE: &str = "expected one of [tokens] 'any 'group 'literal 'word 'punc";

        // Attempt to parse exact tokens first
        if let Ok((input, ans)) = TokenLiteral::parse(input) {
            return Ok((input, TerminalPatern::Exact(ans.into())));
        }

        // Attempt to parse a lifetime (e.g., 'any, 'literal)
        match input.lifetime() {
            Some((lifetime, new_input)) => {
                match lifetime.ident.to_string().as_str() {
                    "any" => Ok((new_input, TerminalPatern::Any)),
                    "word" => Ok((new_input, TerminalPatern::Word)),
                    "literal" => Ok((new_input, TerminalPatern::Literal)),
                    "punc" => Ok((new_input, TerminalPatern::Punc)),
                    "group" => Ok((new_input, TerminalPatern::Group)),
                    _ => Err(syn::Error::new(lifetime.span(), ERROR_MESSAGE)),
                }
            },
            None => Err(syn::Error::new(input.span(), ERROR_MESSAGE)),
        }
    }
}


#[test]
fn match_terminals() {
    let binding = TokenBuffer::new2(parse_str("[5 aadsw]").unwrap());
    let input = binding.begin();
    let parsed = TerminalPatern::parse(input).expect("Failed to parse exact token");
    match parsed {
        (_,TerminalPatern::Exact(_)) => (),
        _ => panic!("Expected TerminalPatern::Exact"),
    }

    let binding = TokenBuffer::new2(parse_str("'word").unwrap());
    let input = binding.begin();
    let (_,parsed)= TerminalPatern::parse(input).expect("Failed to parse 'word'");
    assert!(matches!(parsed, TerminalPatern::Word));


    let binding = TokenBuffer::new2(parse_str("123invalid").unwrap());
    let input = binding.begin();
    let result = TerminalPatern::parse(input);
    assert!(result.is_err(), "Expected parsing error for invalid input");
}


#[derive(Debug)]
pub struct LiteralCombinator(pub Literal);

impl<'a> BasicCombinator<'a> for LiteralCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Literal(lit), next)) => Ok((next, LiteralCombinator(lit))),
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected a literal (number or string)")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting a literal")),
        }
    }
}

#[derive(Debug)]
pub struct WordCombinator(pub Ident);

impl<'a> BasicCombinator<'a> for WordCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Ident(ident), next)) => Ok((next, WordCombinator(ident))),
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected an identifier")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting an identifier")),
        }
    }
}

#[derive(Debug)]
pub struct PuncCombinator(pub Punct);

impl<'a> BasicCombinator<'a> for PuncCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Punct(punct), next)) => Ok((next, PuncCombinator(punct))),
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected one of +!#?.'& etc")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting punctuation")),
        }
    }
}

#[derive(Debug)]
pub struct GroupCombinator(pub Group);

impl<'a> BasicCombinator<'a> for GroupCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Group(group), next)) => {
                Ok((next, GroupCombinator(group)))
            }
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected one of [...] (...) {...}")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting a group")),
        }
    }
}

#[derive(Debug)]
pub struct AnyCombinator(pub TokenTree);

impl<'a> BasicCombinator<'a> for AnyCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((tree, next)) => {
                Ok((next, AnyCombinator(tree)))
            }

            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF")),
        }
    }
}

#[test]
fn test_basic_combinators() {
    use syn::buffer::TokenBuffer;
    use proc_macro2::TokenStream;

    let tokens: TokenStream = "42 identifier + (inner) invalid".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let mut cursor = buffer.begin();

    // Test LiteralCombinator (success)
    match LiteralCombinator::parse(cursor) {
        Ok((next, LiteralCombinator(literal))) => {
            assert_eq!(literal.to_string(), "42");
            cursor = next;
        }
        Err(err) => panic!("LiteralCombinator failed: {}", err),
    }

    // Test WordCombinator (success)
    match WordCombinator::parse(cursor) {
        Ok((next, WordCombinator(ident))) => {
            assert_eq!(ident.to_string(), "identifier");
            cursor = next;
        }
        Err(err) => panic!("WordCombinator failed: {}", err),
    }

    // Test PuncCombinator (success)
    match PuncCombinator::parse(cursor) {
        Ok((next, PuncCombinator(punct))) => {
            assert_eq!(punct.as_char(), '+');
            cursor = next;
        }
        Err(err) => panic!("PuncCombinator failed: {}", err),
    }

    // Test ParenCombinator (success)
    match GroupCombinator::parse(cursor) {
        Ok((next, GroupCombinator(group))) => {
            assert_eq!(group.delimiter(), Delimiter::Parenthesis);
            assert_eq!(group.stream().to_string(), "inner");
            cursor = next;
        }
        Err(err) => panic!("ParenCombinator failed: {}", err),
    }

    // Test LiteralCombinator (failure)
    match LiteralCombinator::parse(cursor) {
        Ok((_,x)) => panic!("Expected LiteralCombinator to fail, but it succeeded with {:?}",x),
        Err(_) => (),
    }

    // Move the cursor forward to ensure trailing data works
    match AnyCombinator::parse(cursor) {
        Ok((_, AnyCombinator(ident))) => {
            assert_eq!(ident.to_string(), "invalid");
        }
        Err(err) => panic!("WordCombinator failed on trailing data: {}", err),
    }
}

#[derive(Debug,Clone,Copy)]
pub struct DelParser (pub Delimiter);

impl<'a> Combinator<'a, TokenStream> for DelParser {
    fn parse(&self, input: Cursor<'a>) -> syn::Result<(Cursor<'a>, TokenStream)> {
        match input.group(self.0) {
            Some((group, _, next)) => {
                Ok((next, group.token_stream()))
            }
            None => {
                let delimiter_name = match self.0 {
                    Delimiter::Parenthesis => "(",
                    Delimiter::Bracket => "[",
                    Delimiter::Brace => "{",
                    Delimiter::None => "<empty delim (likely bug)>",
                };
                Err(syn::Error::new(input.span(), format!("Expected delimited group starting with '{}'", delimiter_name)))
            }
        }
    }
}

#[test]
fn test_delimited_sequence_combinator() {
    use syn::buffer::TokenBuffer;
    use proc_macro2::TokenStream;

    let tokens: TokenStream = "[1, 2, 3]".parse().unwrap();
    let buffer = TokenBuffer::new2(tokens);
    let cursor = buffer.begin();

    let combinator = DelParser(Delimiter::Bracket);

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