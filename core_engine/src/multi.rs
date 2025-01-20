use crate::basic_parsing::Combinator;
use crate::types::{Object,Type};
use crate::types::ObjectParser;
use std::rc::Rc;
use syn::buffer::Cursor;

pub fn parse_many<'a,T,E>(
    mut input: Cursor<'a>,
	parser: impl for<'b> Fn(Cursor<'b>) -> Result<(Cursor<'b>, T), E>,
	ans:&mut Vec<T>
)-> Cursor<'a>{
	while let Ok((new_input,item)) = parser(input){
		ans.push(item);
		input = new_input;
	}
	input
}

#[derive(Debug,Clone)]
pub struct Many0(pub Rc<dyn ObjectParser>);

impl Combinator<Object> for Many0{

	fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut ans = Vec::new();
		let cursor = parse_many(input,|c| {self.0.parse(c)},&mut ans);
		Ok((cursor,Object::new(ans,self.type_info())))
	}
}

impl ObjectParser for Many0{

	fn type_info(&self) -> Type {Type::Array(self.0.type_info().into())}
}

#[derive(Debug,Clone)]
pub struct Many1(pub Rc<dyn ObjectParser>);

impl Combinator<Object> for Many1{
	fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let (cursor,first) = self.0.parse(input)?;
		let mut ans = vec![first];
		let cursor = parse_many(cursor,|c| {self.0.parse(c)},&mut ans);
		Ok((cursor,Object::new(ans,self.type_info())))
	}
}

impl ObjectParser for Many1{

	fn type_info(&self) -> Type {Type::Array(self.0.type_info().into())}
}

#[derive(Debug)]
pub struct OneOf(pub Box<[Rc<dyn ObjectParser>]>,Type);

impl OneOf{
	pub fn new (parsers:Box<[Rc<dyn ObjectParser>]>) -> Self {
		let t = parsers.iter().map(|x| x.type_info()).collect();
		OneOf(parsers,Type::Union(t))
	}
}

impl Combinator<Object> for OneOf{
	fn parse<'a>(&self,input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut error = syn::Error::new(input.span(),format!("errored on {} things (OneOf):",self.0.len()));
		for parser in &self.0 {
			match parser.parse(input){
				Err(e) => {error.combine(e)}
				Ok(x) => {return Ok(x);}
			}
		}
		Err(error)
	}
}

impl ObjectParser for OneOf{
	fn type_info(&self) -> Type {self.1.clone()}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exact::{MatchParser, RcTokenBuffer};
    use syn::buffer::{Cursor, TokenBuffer};

    #[test]
    fn test_many0_no_error() {
        let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { mock_input})));
        let match_parser = Rc::new(MatchParser(token_buffer.clone()));
        let many0 = Many0(match_parser.clone());

        let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {mock_input mock_input.})));
        let (cursor,_) = many0.parse(input.begin()).unwrap();
        assert!(cursor.punct().is_some())

    }

    #[test]
    fn test_many1_no_error() {
        let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { mock_input })));
        let match_parser = Rc::new(MatchParser(token_buffer.clone()));
        let many1 = Many1(match_parser.clone());

	    let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {mock_input mock_input.})));
        let (cursor,_) = many1.parse(input.begin()).unwrap();
        assert!(cursor.punct().is_some())

    }

	#[test]
	fn test_many1_with_error() {
	    let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {this})));
	    let match_parser = Rc::new(MatchParser(token_buffer.clone()));
	    let many1 = Many1(match_parser.clone());

	    let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {not this})));
	    let input: Cursor = token_buffer.begin();

	    let result = many1.parse(input);

	    // Should error (Many1 requires at least one match)
	    assert!(result.is_err());
	}

	#[test]
	fn test_oneof_with_first_match() {
	    let token_buffer1 = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { first })));
	    let match_parser1: Rc<dyn ObjectParser> = Rc::new(MatchParser(token_buffer1.clone()));

	    let token_buffer2 = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { second })));
	    let match_parser2: Rc<dyn ObjectParser> = Rc::new(MatchParser(token_buffer2.clone()));

	    let parsers: Box<[Rc<dyn ObjectParser>]> = Box::new([match_parser1, match_parser2]);
	    let one_of = OneOf::new(parsers);

	    let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { first })));
	    let (cursor, _) = one_of.parse(input.begin()).unwrap();
	    assert!(cursor.eof()); // All input should be consumed
	}

	#[test]
	fn test_oneof_with_error() {
	    let token_buffer1 = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { first })));
	    let match_parser1: Rc<dyn ObjectParser> = Rc::new(MatchParser(token_buffer1.clone()));

	    let token_buffer2 = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { second })));
	    let match_parser2: Rc<dyn ObjectParser> = Rc::new(MatchParser(token_buffer2.clone()));

	    let parsers: Box<[Rc<dyn ObjectParser>]> = Box::new([match_parser1, match_parser2]);
	    let one_of = OneOf::new(parsers);

	    let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { third })));
	    let result = one_of.parse(input.begin());

	    // Should error as no parser matches
	    assert!(result.is_err());
	}

}
