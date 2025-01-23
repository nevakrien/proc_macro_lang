use crate::combinator::PakeratError;
use crate::combinator::Pakerat;
use crate::combinator::State;
use crate::ObjectParser;
use std::rc::Rc;
use proc_macro2::Ident;
use crate::types::{StructParser};
use syn::buffer::Cursor;

pub fn parse_pattern<'a>(_input:Cursor<'a>,_state: &mut State<'a>) -> Pakerat<(Cursor<'a>,StructParser)>{
	todo!()
}

///looks for "name :" definitions are expected to come after.
pub fn parse_name_capture<'a>(input:Cursor<'a>) ->Pakerat<(Cursor<'a>,Option<Ident>)>{
	let (ans,cursor) = match input.ident(){
		None => return Ok((input,None)),
		Some(x) => x
	};

	let (dots_exist,cursor) = match cursor.punct(){
		None => (false,cursor),
		Some((p,cursor)) => (p.as_char()==':',cursor),
	};

	if !dots_exist {
		let error = syn::Error::new(cursor.span(),"expected : after name");
		return Err(PakeratError::Regular(error));
	}

	Ok((cursor,Some(ans)))
}

pub fn parse_parser<'a>(_input:Cursor<'a>,_state: &mut State<'a>) -> Pakerat<(Cursor<'a>,Rc<dyn ObjectParser>)>{
	todo!()
}