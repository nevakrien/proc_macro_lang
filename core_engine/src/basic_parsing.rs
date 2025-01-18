
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