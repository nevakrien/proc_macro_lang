use crate::multi::Recognize;
use crate::basic_parsing::get_end_del;
use syn::__private::quote::spanned::Spanned;
use crate::basic_parsing::DelParser;
use crate::exact::MatchParser;
use syn::buffer::TokenBuffer;
use crate::name_space::DeferedParse;
use crate::types::BasicData;
use crate::types::ObjData;
use crate::multi::Many0;
use crate::multi::Many1;
use crate::multi::Maybe;
use proc_macro2::Delimiter;
use crate::multi::OneOf;
use crate::types::StructPair;
use crate::name_space::FileNameSpace;
use crate::ObjectParser;
use std::rc::Rc;
use proc_macro2::Ident;
use crate::types::{StructParser};
use syn::buffer::Cursor;

pub fn parse_pattern<'a>(mut input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<(Cursor<'a>,StructParser)>{
	let mut ans = Vec::new();
	while let Some((cursor,pair)) = parse_pair(input,name_space)?{
		input=cursor;
		ans.push(pair)
	}
	let parser = StructParser::new(ans.into())?;
	Ok((input,parser))
}

pub fn parse_pair<'a>(mut input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<Option<(Cursor<'a>,StructPair)>>{
	let capture = match parse_name_capture(input){
		None => None,
		Some((cursor,x)) =>{
			input = cursor;
			Some(x)
		}
	};

	match parse_parser(input,name_space)?{
		Some((cursor,parser)) =>  Ok(Some((cursor,StructPair{capture,parser}))),
		None => {
			if let Some(_ident) = capture{
				Err(syn::Error::new(input.span(),"expected parser after capture decleration"))
			}else{
				Ok(None)
			}
		}
	}
}

pub fn parse_name_capture<'a>(input:Cursor<'a>) ->Option<(Cursor<'a>,Ident)>{
	let (ans,cursor) = match input.ident(){
		None => return None,
		Some(x) => x
	};

	match cursor.punct(){
		None =>  None,
		Some((p,cursor)) => {
			if p.as_char()==':'{
				Some((cursor,ans))
			} else {
				None
			}
		}
	}	
}

//looks for an | clause
pub fn parse_or<'a>(input:Cursor<'a>) ->Option<Cursor<'a>>{
	match input.punct(){
		None =>  None,
		Some((p,cursor)) => {
			if p.as_char()=='|'{
				Some(cursor)
			} else {
				None
			}
		}
	}	
}

pub fn parse_parser<'a>(mut input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<Option<(Cursor<'a>,Rc<dyn ObjectParser>)>>{
	let mut or_stack = Vec::new();
	
	match parse_internal_parser(input,name_space)?{
		Some((cursor,x)) => {
			input = cursor;
			or_stack.push(x);
		},
		None => return Ok(None)
	}

	while let Some(cursor) = parse_or(input) {
		input = cursor;
		match parse_internal_parser(input,name_space)?{
			Some((cursor,x)) => {
				or_stack.push(x);
				input = cursor;
			},
			None=> return Err(syn::Error::new(input.span(),"expected a parser after |"))
		}
	}

	match or_stack.len() {
		0 => return Ok(None),
		1 => return Ok(Some((input,or_stack.into_iter().next().unwrap()))),
		2_usize.. => Ok(Some((input,Rc::new(OneOf::new(or_stack.into())))))
	}

}

#[derive(Debug,Clone,Copy)]
enum Prefix{
	Maybe,
	Many0,
	Many1,
	Recognize
}

fn parse_internal_parser<'a>(mut input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<Option<(Cursor<'a>,Rc<dyn ObjectParser>)>>{
	let mut prefixes = Vec::new();

	while let Some((punc,cursor)) = input.punct(){		
		match punc.as_char() {
			'?' => prefixes.push(Prefix::Maybe),
			'*' => prefixes.push(Prefix::Many0),
			'+' => prefixes.push(Prefix::Many1),
			'$' => prefixes.push(Prefix::Recognize),
		    _ => break, //error handled by terminal parser
		}

		input=cursor;
	}

	let mut parser = match input.group(Delimiter::Parenthesis) {
	    None => match parse_terminal_parser(input,name_space)?{
	    	None => return if !prefixes.is_empty() {
	    		 Err(syn::Error::new(input.span(),"expected parser"))
	    	} else {
	    		Ok(None)
	    	},
	    	Some((cursor,parser)) => {
	    		input = cursor;
	    		parser
	    	}
	    },
	    Some((x,_,cursor)) => {
	    	match parse_parser(x,name_space)?{
	    		Some((spot,parser)) => {
	    			if !spot.eof(){
	    				return Err(syn::Error::new(spot.span(),"expected )"))
	    			}
	    			input = cursor;
	    			parser
	    		},
	    		None => return Err(syn::Error::new(x.span(),"expected parser"))
	    	}
	    },
	};

	for pre in prefixes.iter().rev(){
		match pre{
			Prefix::Maybe=> parser = Rc::new(Maybe::new(parser)),
			Prefix::Many0=> parser = Rc::new(Many0(parser)),
			Prefix::Many1=> parser = Rc::new(Many1(parser)),
			Prefix::Recognize => parser = Rc::new(Recognize(parser)),
		}
	}

	Ok(Some((input,parser)))
}

pub fn parse_terminal_parser<'a>(input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<Option<(Cursor<'a>,Rc<dyn ObjectParser>)>>{
	if let Some((punc,cursor)) = input.punct() {
		if punc.as_char() == '#'{
			if let Some((x,del,del_span,cursor)) = cursor.any_group(){
				match parse_parser(x,name_space)?{
					None => return Err(syn::Error::new(del_span.__span(),"expected a parser")),
					Some((end,parser)) => {
						if !end.eof(){
		    				return Err(syn::Error::new(end.span(),format!("expected {}",get_end_del(del))))
		    			}
						let parser = Rc::new(DelParser(del,parser));
						return Ok(Some((cursor,parser)))
					}
				}
			}
		}else {
			return Ok(None)
		}
	}
	
	if let Some((id,cursor)) = input.ident(){
		match name_space.get(&id) {
		    None => return Err(syn::Error::new(id.span(),format!("unrecognized name {}",id))),
		    Some(obj) => match obj.data {
		    	ObjData::Basic(BasicData::TypeRef(t)) =>{
		    		let parser = Rc::new(DeferedParse(t,id));
		    		return Ok(Some((cursor,parser)))
		    	}
		    	ObjData::Parser(parser) => return Ok(Some((cursor,parser))),
		        _ => return Err(syn::Error::new(id.span(),format!("{} is of type {:?} which is not a type alias for Type or Parser",id,obj.type_info))),
		    }
		}
	}

	if let Some((x,_,cursor)) = input.group(Delimiter::Bracket){
		let parser = MatchParser(TokenBuffer::new2(x.token_stream()));
		return Ok(Some((cursor,Rc::new(parser))))
	}

	return Ok(None)
}

#[cfg(test)]
mod tests {
    use crate::combinator::Combinator;
use super::*;
    use crate::combinator::initialize_state;

    #[test]
    fn test_zero_or_more_parser() {
        let (pattern_input, state) = initialize_state("*[42]").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input, mut valid_state) = initialize_state("42 42 42").unwrap();
        let (remaining_cursor, _) = struct_parser.parse(valid_input.begin(), &mut valid_state).unwrap();
        assert!(remaining_cursor.eof());

        let (empty_input, mut empty_state) = initialize_state("").unwrap();
        struct_parser.parse(empty_input.begin(), &mut empty_state).unwrap();

        let (invalid_input, mut invalid_state) = initialize_state("lol invalid").unwrap();
        struct_parser.parse(invalid_input.begin(), &mut invalid_state).unwrap();
    }

    #[test]
    fn test_one_or_more_parser() {
        let (pattern_input, state) = initialize_state("+[42]").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input, mut valid_state) = initialize_state("42 42 42").unwrap();
        let (remaining_cursor, _) = struct_parser.parse(valid_input.begin(), &mut valid_state).unwrap();
        assert!(remaining_cursor.eof());

        let (empty_input, mut empty_state) = initialize_state("").unwrap();
        assert!(struct_parser.parse(empty_input.begin(), &mut empty_state).is_err());
    }

    #[test]
    fn test_choice_parser_hard() {
        let (pattern_input, state) = initialize_state("[42] | name").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input_1, mut valid_state_1) = initialize_state("42").unwrap();
        struct_parser.parse(valid_input_1.begin(), &mut valid_state_1).unwrap();

        let (valid_input_2, mut valid_state_2) = initialize_state("Carlos").unwrap();
        struct_parser.parse(valid_input_2.begin(), &mut valid_state_2).unwrap();

        let (invalid_input, mut invalid_state) = initialize_state(";invalid").unwrap();
        assert!(struct_parser.parse(invalid_input.begin(), &mut invalid_state).is_err());
    }

    #[test]
    fn test_choice_parser() {
        let (pattern_input, state) = initialize_state("([42] | name)").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input_1, mut valid_state_1) = initialize_state("42").unwrap();
        struct_parser.parse(valid_input_1.begin(), &mut valid_state_1).unwrap();

        let (valid_input_2, mut valid_state_2) = initialize_state("Carlos").unwrap();
        struct_parser.parse(valid_input_2.begin(), &mut valid_state_2).unwrap();

        let (invalid_input, mut invalid_state) = initialize_state(";invalid").unwrap();
        assert!(struct_parser.parse(invalid_input.begin(), &mut invalid_state).is_err());
    }

    #[test]
    fn test_parenthesized_choice_parser() {
        let (pattern_input, state) = initialize_state("?([a] | [b])").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input_1, mut valid_state_1) = initialize_state("a").unwrap();
        struct_parser.parse(valid_input_1.begin(), &mut valid_state_1).unwrap();

        let (valid_input_2, mut valid_state_2) = initialize_state("b").unwrap();
        struct_parser.parse(valid_input_2.begin(), &mut valid_state_2).unwrap();

        let (empty_input, mut empty_state) = initialize_state("").unwrap();
        struct_parser.parse(empty_input.begin(), &mut empty_state).unwrap();
    }

    #[test]
    fn test_exact_token_literal() {
        let (pattern_input, state) = initialize_state("[exact]").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input, mut valid_state) = initialize_state("exact").unwrap();
        let (remaining_cursor, _) = struct_parser.parse(valid_input.begin(), &mut valid_state).unwrap();
        assert!(remaining_cursor.eof());

        let (invalid_input, mut invalid_state) = initialize_state("different").unwrap();
        assert!(struct_parser.parse(invalid_input.begin(), &mut invalid_state).is_err());
    }

    #[test]
    fn test_delimited_parser() {
        let (pattern_input, state) = initialize_state("$#([42])").unwrap();

        let pattern_cursor = pattern_input.begin();
        let file_name_space = state.file.borrow();
        let (remaining_cursor, struct_parser) = parse_pattern(pattern_cursor, &file_name_space).unwrap();
        assert!(remaining_cursor.eof());

        let (valid_input, mut valid_state) = initialize_state("(42)").unwrap();
        let (remaining_cursor, _) = struct_parser.parse(valid_input.begin(), &mut valid_state).unwrap();
        assert!(remaining_cursor.eof());

        let (invalid_input, mut invalid_state) = initialize_state("[42]").unwrap();
        assert!(struct_parser.parse(invalid_input.begin(), &mut invalid_state).is_err());
    }
}
