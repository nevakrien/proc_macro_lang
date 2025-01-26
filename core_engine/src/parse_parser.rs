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

	match parse_internal_parser(input,name_space)?{
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


pub fn parse_internal_parser<'a>(_input:Cursor<'a>,_name_space:&FileNameSpace) -> syn::Result<Option<(Cursor<'a>,Rc<dyn ObjectParser>)>>{
	todo!()
}

// pub fn parse_parser<'a>(input:Cursor<'a>,name_space:&FileNameSpace) -> syn::Result<(Cursor<'a>,Rc<dyn ObjectParser>)>{
// 	match input.ident() {
// 	    None => {} 
// 	    Some((i,cursor)) => {
// 	    	match name_space.objects.get(&i) {
// 	    	    None => {
// 	    	    	let error = format!("unrecognized parser name {}",i);
// 	    	    	return Err(syn::Error::new(i.span(),error))
// 	    	    } 
// 	    	    Some(obj) => {
// 	    	    	match obj.data {
// 	    	    		ObjData::Parser(parser) => return Ok((cursor,parser)),
// 	    	    	    _ => {
// 			    	    	let error = format!("expected a type alias of parser found {:?}",obj.type_info);
// 			    	    	return Err(syn::Error::new(i.span(),error))
// 			    	    } 
// 	    	    	}
// 	    	    },
// 	    	}
// 	    },
// 	};

// 	match input.lifetime() {
// 	    None => {} 
// 	    Some((life,cursor)) => {
// 	    	match name_space.types.get(&life.ident) {
// 	    	    None => {
// 	    	    	let error = format!("unrecognized type name {}",life.ident);
// 	    	    	return Err(syn::Error::new(life.span(),error))
// 	    	    } 
// 	    	    Some(t) => return Ok((cursor,Rc::new(DeferedParse(t,life.ident)))),
// 	    	}
// 	    },
// 	};
	
// 	//need to also cover token literals and similar things
// 	todo!()

// 	// Err(
// 	// 	syn::Error::new(input.span(),"expected a parser or type")
// 	// )
// }