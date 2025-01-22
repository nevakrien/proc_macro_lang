use crate::combinator::State;
use crate::types::BasicType;
use crate::combinator::Combinator;
use crate::types::{Object,Type};
use crate::types::ObjectParser;
use std::rc::Rc;
use syn::buffer::Cursor;

pub fn parse_many<'a,T,E>(
    mut input: Cursor<'a>,
    state:&mut State<'a>,
	parser: impl for<'b> Fn(Cursor<'b>,&mut State<'b>) -> Result<(Cursor<'b>, T), E>,
	ans:&mut Vec<T>
)-> Cursor<'a>{
	while let Ok((new_input,item)) = parser(input,state){
		ans.push(item);
		input = new_input;
	}
	input
}

#[derive(Debug,Clone)]
pub struct Many0(pub Rc<dyn ObjectParser>);

impl Combinator<Object> for Many0{

	fn parse<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut ans = Vec::new();
		let cursor = parse_many(input,state,|c,n| {self.0.parse(c,n)},&mut ans);
		Ok((cursor,Object::new(ans,self.type_info())))
	}
}

impl ObjectParser for Many0{

	fn type_info(&self) -> Type {Type::Array(self.0.type_info().into())}
}

#[derive(Debug,Clone)]
pub struct Many1(pub Rc<dyn ObjectParser>);

impl Combinator<Object> for Many1{
	fn parse<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let (cursor,first) = self.0.parse(input,state)?;
		let mut ans = vec![first];
		let cursor = parse_many(cursor,state,|c,n| {self.0.parse(c,n)},&mut ans);
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
	fn parse<'a>(&self,input: Cursor<'a>,state:&mut State<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut error = syn::Error::new(input.span(),format!("errored on {} things (OneOf):",self.0.len()));
		for parser in &self.0 {
			match parser.parse(input,state){
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
	fn parse<'a>(&self,input: Cursor<'a>,state:&mut State<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		match self.0.parse(input,state){
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
	fn parse<'a>(&self, input: Cursor<'a>,state:&mut State<'a>) -> Result<(Cursor<'a>, Object), syn::Error> {
		let mut cursor = input;
		for parser in &self.0 {
			let (new_cursor,_obj) = parser.parse(cursor,state)?;
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
    use crate::combinator::initialize_state;
    use crate::types::{BasicData, ObjData};
    use crate::basic_parsing::{LiteralParser, PuncParser, WordParser};
    use crate::exact::{MatchParser};
    use proc_macro2::TokenTree;
    use syn::buffer::TokenBuffer;
    use super::*;
    use std::rc::Rc;

    fn setup_parser(input: &str) -> Rc<MatchParser> {
        let token_buffer = TokenBuffer::new2(input.parse().unwrap());
        Rc::new(MatchParser(token_buffer))
    }

    #[test]
    fn test_many0_no_error() {
        let match_parser = setup_parser("mock_input");
        let many0 = Many0(match_parser.clone());

        let (input, mut state) = initialize_state("mock_input mock_input.").unwrap();
        let (cursor, _) = many0.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.punct().is_some());
    }

    #[test]
    fn test_many1_no_error() {
        let match_parser = setup_parser("mock_input");
        let many1 = Many1(match_parser.clone());

        let (input, mut state) = initialize_state("mock_input mock_input.").unwrap();
        let (cursor, _) = many1.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.punct().is_some());
    }

    #[test]
    fn test_many1_with_error() {
        let match_parser = setup_parser("this");
        let many1 = Many1(match_parser.clone());

        let (input, mut state) = initialize_state("not this").unwrap();
        let result = many1.parse(input.begin(), &mut state);

        assert!(result.is_err()); // Many1 requires at least one match
    }

    #[test]
    fn test_maybe_no_error() {
        let match_parser = setup_parser("mock_input");
        let maybe = Maybe::new(match_parser.clone());

        let (input, mut state) = initialize_state("mock_input.").unwrap();
        let (cursor, _) = maybe.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.punct().is_some());
    }

    #[test]
    fn test_maybe_with_error() {
        let match_parser = setup_parser("mock_input");
        let maybe = Maybe::new(match_parser.clone());

        let (input, mut state) = initialize_state(".").unwrap();
        let (cursor, _) = maybe.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.punct().is_some());
    }

    #[test]
    fn test_oneof_with_first_match() {
        let match_parser1 = setup_parser("first");
        let match_parser2 = setup_parser("second");
        let parsers: Box<[Rc<dyn ObjectParser>]> = Box::new([match_parser1, match_parser2]);

        let one_of = OneOf::new(parsers);
        let (input, mut state) = initialize_state("first").unwrap();
        let (cursor, _) = one_of.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.eof());
    }

    #[test]
    fn test_oneof_with_error() {
        let match_parser1 = setup_parser("first");
        let match_parser2 = setup_parser("second");
        let parsers: Box<[Rc<dyn ObjectParser>]> = Box::new([match_parser1, match_parser2]);

        let one_of = OneOf::new(parsers);
        let (input, mut state) = initialize_state("third").unwrap();
        let result = one_of.parse(input.begin(), &mut state);

        assert!(result.is_err());
    }

    #[test]
    fn test_recognize_combination() {
        let recognize = Recognize(Box::new([
            Rc::new(LiteralParser) as Rc<dyn ObjectParser>,
            Rc::new(WordParser),
            Rc::new(PuncParser),
        ]));

        let (input, mut state) = initialize_state("42 foo !").unwrap();
        let (cursor, object) = recognize.parse(input.begin(), &mut state).unwrap();

        assert!(cursor.eof());

        let matched_tokens: &[Object] = match &object.data {
            ObjData::Array(array) => array,
            _ => panic!("Expected ObjData::Array, found {:?}", object.data),
        };

        assert_eq!(matched_tokens.len(), 3);

        match &matched_tokens[0].data {
            ObjData::Basic(BasicData::Tree(TokenTree::Literal(_))) => {}
            _ => panic!("Expected first token to be a Literal, found {:?}", matched_tokens[0].data),
        }
        match &matched_tokens[1].data {
            ObjData::Basic(BasicData::Tree(TokenTree::Ident(ident))) if ident == "foo" => {}
            _ => panic!("Expected second token to be Ident(\"foo\"), found {:?}", matched_tokens[1].data),
        }
        match &matched_tokens[2].data {
            ObjData::Basic(BasicData::Tree(TokenTree::Punct(punct))) if punct.as_char() == '!' => {}
            _ => panic!("Expected third token to be Punct('!'), found {:?}", matched_tokens[2].data),
        }
    }

    #[test]
    fn test_recognize_fail() {
        let recognize = Recognize(Box::new([
            Rc::new(LiteralParser) as Rc<dyn ObjectParser>,
            Rc::new(WordParser),
            Rc::new(PuncParser),
        ]));

        let (input, mut state) = initialize_state("42  !").unwrap();
        let result = recognize.parse(input.begin(), &mut state);

        assert!(result.is_err());
    }
}
