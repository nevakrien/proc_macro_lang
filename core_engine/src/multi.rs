use crate::types::BasicType;
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
		let t = parsers.iter().map(|x| x.type_info());
		let t = Type::new_union(t);
		OneOf(parsers,t)
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

#[derive(Debug)]
pub struct Maybe(pub Rc<dyn ObjectParser>,Type);

impl Maybe{
	pub fn new (parser:Rc<dyn ObjectParser>) -> Self {
		let t = parser.type_info();
		Maybe(parser,Type::new_union([t,BasicType::None.into()].into_iter()))
	}
}

impl Combinator<Object> for Maybe{
	fn parse<'a>(&self,input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		match self.0.parse(input){
			Ok(x) => Ok(x),
			Err(_e) => Ok((input,Object::none()))
		}
	}
}

impl ObjectParser for Maybe{
	fn type_info(&self) -> Type {self.1.clone()}
}

#[derive(Debug)]
pub struct Recognize(pub Box<[Rc<dyn ObjectParser>]>);

impl Combinator<Object> for Recognize{
	fn parse<'a>(&self, input: Cursor<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut cursor = input;
		for parser in &self.0 {
			let (new_cursor,_obj) = parser.parse(cursor)?;
			cursor = new_cursor;

		}

		let mut cursor2 = input;
		let mut ans = Vec::with_capacity(self.0.len());
		while cursor2!=cursor{
			let (tree,new) = cursor2.token_tree().unwrap();
			ans.push(tree);
			cursor2=new;
		}
		
		Ok((cursor,Object::from_iter(ans.into_iter(),self.type_info())))
	}
}

impl ObjectParser for Recognize{
	fn type_info(&self) ->Type { Type::Array(Rc::new(BasicType::Tree.into()))}
}

#[cfg(test)]
mod tests {
	use crate::types::ObjData;
	use proc_macro2::TokenTree;
	use crate::types::BasicData;
	use proc_macro2::TokenStream;
	use crate::basic_parsing::PuncParser;
	use crate::basic_parsing::LiteralParser;
	use crate::basic_parsing::WordParser;
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
    fn test_maybe_no_error() {
        let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { mock_input })));
        let match_parser = Rc::new(MatchParser(token_buffer.clone()));
        let maybe = Maybe::new(match_parser.clone());

	    let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {mock_input.})));
        let (cursor,_) = maybe.parse(input.begin()).unwrap();
        assert!(cursor.punct().is_some())

    }

	#[test]
	fn test_maybe_with_error() {
	    let token_buffer = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! { mock_input })));
        let match_parser = Rc::new(MatchParser(token_buffer.clone()));
        let maybe = Maybe::new(match_parser.clone());

	    let input = RcTokenBuffer(Rc::new(TokenBuffer::new2(syn::parse_quote! {.})));
        let (cursor,_) = maybe.parse(input.begin()).unwrap();
        assert!(cursor.punct().is_some())
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

	#[test]
	fn test_recognize_combination() {
	    // Create Recognize combinator with Literal, Word, and Punct parsers
	    let recognize = Recognize(Box::new([
	        Rc::new(LiteralParser) as Rc<dyn ObjectParser>,
	        Rc::new(WordParser),
	        Rc::new(PuncParser),
	    ]));

	    // Input token stream: 42 foo !
	    let token_stream: TokenStream = syn::parse_quote! { 42 foo ! };
	    let token_buffer = Rc::new(TokenBuffer::new2(token_stream));
	    let cursor = token_buffer.begin();

	    // Parse the token stream using Recognize
	    let (remaining_cursor, object) = recognize
	        .parse(cursor)
	        .unwrap(); // Unwrap here for better diagnostics in case of failure

	    // Ensure the remaining cursor is at the end
	    assert!(
	        remaining_cursor.eof(),
	        "Expected remaining cursor to be at the end, but found more tokens."
	    );

	    // Extract ObjData::Array from the Object
	    let matched_tokens: &[Object] = match &object.data {
	        ObjData::Array(array) => array,
	        _ => panic!("Expected ObjData::Array, found {:?}", object.data),
	    };

	    // Verify the number of matched tokens
	    assert_eq!(
	        matched_tokens.len(),
	        3,
	        "Expected 3 matched tokens, but found {}",
	        matched_tokens.len()
	    );

	    // Verify each matched token is correctly converted to TokenTree
	    match &matched_tokens[0].data {
	        ObjData::Basic(BasicData::Tree(TokenTree::Literal(_))) => {}
	        _ => panic!("Expected first token to be a Literal, but found {:?}", matched_tokens[0].data),
	    }
	    match &matched_tokens[1].data {
	        ObjData::Basic(BasicData::Tree(TokenTree::Ident(ident))) if ident == "foo" => {}
	        _ => panic!("Expected second token to be Ident(\"foo\"), but found {:?}", matched_tokens[1].data),
	    }
	    match &matched_tokens[2].data {
	        ObjData::Basic(BasicData::Tree(TokenTree::Punct(punct))) if punct.as_char() == '!' => {}
	        _ => panic!("Expected third token to be Punct('!'), but found {:?}", matched_tokens[2].data),
	    }
	}



    #[test]
    fn test_recognize_fail() {
        // Create Recognize combinator with Literal, Word, and Punct parsers
        let recognize = Recognize(Box::new([
            Rc::new(LiteralParser) as Rc<dyn ObjectParser>,
            Rc::new(WordParser),
            Rc::new(PuncParser),
        ]));

        // Input token stream: 42 !
        let token_stream: TokenStream = syn::parse_quote! { 42 ! };
        let token_buffer = Rc::new(TokenBuffer::new2(token_stream));
        let cursor = token_buffer.begin();

        // Parse the token stream using Recognize
        let result = recognize.parse(cursor);

        // Expect an error because the input doesn't match the sequence
        assert!(result.is_err());
    }

}
