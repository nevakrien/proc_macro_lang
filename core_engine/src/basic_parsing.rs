use proc_macro2::{TokenStream};
use syn::parse::{Parse, ParseBuffer};

#[cfg(test)]
use syn::parse_str;

#[derive(Debug,Clone)]
pub struct TokenLiteral(pub TokenStream);

impl Parse for TokenLiteral {
    fn parse(input: &ParseBuffer<'_>) -> syn::Result<Self> {
        if !input.peek(syn::token::Bracket){
        	return Err(input.error("expected [tokens]"));
        }
        let content;
        let _brackets = syn::bracketed!(content in input);

        // Capture the tokens inside the brackets as a TokenStream
        Ok(TokenLiteral(content.parse()?))
 
    }
}

impl Into<TokenStream> for TokenLiteral {

	fn into(self) -> proc_macro2::TokenStream { self.0 }
}

#[test]
fn literal_tokens_parse() {
    parse_str::<TokenLiteral>("[let x [=] 42;]").unwrap();
    // let x = parse_str::<TokenLiteral>("[let x [=] 42;]").unwrap();
    // println!("parsed {}",x.0.to_string());
    parse_str::<TokenLiteral>("let x = 42;").unwrap_err();
    parse_str::<TokenLiteral>("(let x = 42; [])").unwrap_err();
}

pub enum TerminalPatern {
	Exact(TokenStream),
	Any,

    Literal,
    Word,
    Punc(char),
    EOF,

}

impl Parse for TerminalPatern {

	fn parse(input: &ParseBuffer<'_>) -> Result<Self, syn::Error> {
    	//first try for exact
    	if let Ok(s) = TokenLiteral::parse(input){
			return Ok(TerminalPatern::Exact(s.into()))
		}

		//now the keywords
    	syn::custom_keyword!(word);
    	syn::custom_keyword!(literal);
    	syn::custom_keyword!(eof);
    	syn::custom_keyword!(any);
		
		if let Ok(_) = input.parse::<word>() {
			return Ok(TerminalPatern::Word); 
		}

		if let Ok(_) = input.parse::<any>() {
			return Ok(TerminalPatern::Any); 
		}

		if let Ok(_) = input.parse::<literal>() {
			return Ok(TerminalPatern::Literal); 
		}

		if let Ok(_) = input.parse::<eof>() {
			return Ok(TerminalPatern::EOF); 
		}

		//now punctioation
		let res = input.step(|cursor| {
			match cursor.punct() {
				Some(p) => Ok(p),
				None => Err(cursor.error("expected *&!@$.."))
			}
		});

		if let Ok(p) = res {
			return Ok(TerminalPatern::Punc(p.as_char()))
		}
        

		Err(input.error("expected one of: [tokens], 'word', 'any',*&!@$..  literal or EOF"))
	}
}

#[test]
fn test_terminal_pattern_exact() {
    let input = "[5 aadsw]";
    let parsed: TerminalPatern = parse_str(input).expect("Failed to parse exact token");
    match parsed {
        TerminalPatern::Exact(_) => (),
        _ => panic!("Expected TerminalPatern::Exact"),
    }
}

#[test]
fn test_terminal_pattern_word() {
    let input = "word";
    let parsed: TerminalPatern = parse_str(input).expect("Failed to parse 'word'");
    assert!(matches!(parsed, TerminalPatern::Word));
}

#[test]
fn test_terminal_pattern_eof() {
    let input = "eof";
    let parsed: TerminalPatern = parse_str(input).expect("Failed to parse 'eof'");
    assert!(matches!(parsed, TerminalPatern::EOF));
}

#[test]
fn test_terminal_pattern_any() {
    let input = "any";
    let parsed: TerminalPatern = parse_str(input).expect("Failed to parse 'any'");
    assert!(matches!(parsed, TerminalPatern::Any));
}

#[test]
fn test_terminal_pattern_punctuation() {
    let input = "!";
    let parsed: TerminalPatern = parse_str(input).expect("Failed to parse punctuation");
    match parsed {
        TerminalPatern::Punc('!') => (),
        _ => panic!("Expected TerminalPatern::Punc('!')"),
    }
}

#[test]
fn test_terminal_pattern_invalid_input() {
    let input = "invalid";
    let result: Result<TerminalPatern, syn::Error> = parse_str(input);
    assert!(result.is_err(), "Expected parsing error for invalid input");
}