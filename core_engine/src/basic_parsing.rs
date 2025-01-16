use syn::parse::{Parse,ParseBuffer};
use proc_macro2::TokenStream;

#[cfg(test)]
use syn::parse_str;

#[derive(Debug)]
pub struct TokenLiteral(pub TokenStream);

impl Parse for TokenLiteral {
    fn parse(input: &ParseBuffer<'_>) -> syn::Result<Self> {
        // Parse the bracketed content
        let content;
        let _brackets = syn::bracketed!(content in input);

        // Capture the tokens inside the brackets as a TokenStream
        Ok(TokenLiteral(content.parse()?))
    }
}

#[test]
fn literal_tokens_parse() {
    parse_str::<TokenLiteral>("[let x [=] 42;]").unwrap();
    // let x = parse_str::<TokenLiteral>("[let x [=] 42;]").unwrap();
    // println!("parsed {}",x.0.to_string());
    parse_str::<TokenLiteral>("let x = 42;").unwrap_err();
    parse_str::<TokenLiteral>("(let x = 42; [])").unwrap_err();

}