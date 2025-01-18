
use crate::syn::buffer::Cursor;
use proc_macro2::{TokenStream};

#[cfg(test)]
use syn::parse_str;
#[cfg(test)]
use crate::syn::buffer::TokenBuffer;


pub trait Combinator<'a, T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse(&mut self, input: Cursor<'a>) -> Result<(Cursor<'a>, T), E>;
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
    EOF,

}

impl<'a> BasicCombinator<'a,  syn::Error> for TerminalPatern {
	fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, TerminalPatern), syn::Error>{
		static  ERROR_MESSAGE: &str = "expected one of [tokens] 'any' 'literal' 'word' 'punc' 'eof'";
		
		if let Ok((input,ans)) = TokenLiteral::parse(input) {
			return Ok((input,TerminalPatern::Exact(ans.into())));
		}
		match input.ident() {
			None => Err(syn::Error::new(input.span(),ERROR_MESSAGE)),
			Some((word,new_input)) => {
				match word.to_string().as_str() {
					"any" => Ok((new_input,TerminalPatern::Any)),
					"word" => Ok((new_input,TerminalPatern::Word)),
					"literal" => Ok((new_input,TerminalPatern::Literal)),
					"punc" => Ok((new_input,TerminalPatern::Punc)),
					"eof" => Ok((new_input,TerminalPatern::EOF)),
					_ => Err(syn::Error::new(input.span(),ERROR_MESSAGE)),
				}
			},
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

    let binding = TokenBuffer::new2(parse_str("word").unwrap());
    let input = binding.begin();
    let (_,parsed)= TerminalPatern::parse(input).expect("Failed to parse 'word'");
    assert!(matches!(parsed, TerminalPatern::Word));

    let binding = TokenBuffer::new2(parse_str("eof").unwrap());
    let input = binding.begin();
    let (_,parsed) = TerminalPatern::parse(input).expect("Failed to parse 'eof'");
    assert!(matches!(parsed, TerminalPatern::EOF));

    let binding = TokenBuffer::new2(parse_str("123invalid").unwrap());
    let input = binding.begin();
    let result = TerminalPatern::parse(input);
    assert!(result.is_err(), "Expected parsing error for invalid input");
}

use proc_macro2::{Delimiter, TokenTree, Group, Ident, Literal, Punct};

#[derive(Debug)]
pub struct LiteralCombinator(pub Literal);

impl<'a> BasicCombinator<'a> for LiteralCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Literal(lit), next)) => Ok((next, LiteralCombinator(lit))),
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected a literal")),
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
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected punctuation")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting punctuation")),
        }
    }
}

#[derive(Debug)]
pub struct ParenCombinator(pub Group);

impl<'a> BasicCombinator<'a> for ParenCombinator {
    fn parse(input: Cursor<'a>) -> Result<(Cursor<'a>, Self), syn::Error> {
        match input.token_tree() {
            Some((TokenTree::Group(group), next)) => {
                if group.delimiter() == Delimiter::Parenthesis {
                    Ok((next, ParenCombinator(group)))
                } else {
                    Err(syn::Error::new(
                        group.span(),
                        format!("Expected parentheses, found {:?}.", group.delimiter()),
                    ))
                }
            }
            Some((other, _)) => Err(syn::Error::new(other.span(), "Expected a group with parentheses")),
            None => Err(syn::Error::new(proc_macro2::Span::call_site(), "Unexpected EOF while expecting a group")),
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
    match ParenCombinator::parse(cursor) {
        Ok((next, ParenCombinator(group))) => {
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
    match WordCombinator::parse(cursor) {
        Ok((_, WordCombinator(ident))) => {
            assert_eq!(ident.to_string(), "invalid");
        }
        Err(err) => panic!("WordCombinator failed on trailing data: {}", err),
    }
}