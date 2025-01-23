use crate::name_space::DeferedParse;
use crate::types::ObjData;
use crate::name_space::FileNameSpace;
use crate::combinator::State;
use crate::ObjectParser;
use std::rc::Rc;
use proc_macro2::Ident;
use crate::types::{StructParser};
use syn::buffer::Cursor;

pub fn parse_pattern<'a>(_input:Cursor<'a>,_state: &mut State<'a>) -> syn::Result<(Cursor<'a>,StructParser)>{
	todo!()
}


///looks for "name :" definitions are expected to come after.
///would ignore any lone "name" thats not imidiatly flanked by punctioation
pub fn parse_name_capture<'a>(input:Cursor<'a>) ->syn::Result<(Cursor<'a>,Option<Ident>)>{
	let (ans,cursor) = match input.ident(){
		None => return Ok((input,None)),
		Some(x) => x
	};

	//this here is sketchy since a fail here is not necirly a faily everywhere
	//for instance a one of would raise this error since a | b is read as potentially a : b
	let (dots_exist,cursor) = match cursor.punct(){
		None => return Ok((input,None)),
		Some((p,cursor)) => (p.as_char()==':',cursor),
	};

	if !dots_exist {
		let error = syn::Error::new(cursor.span(),"expected : after capture name");
		return Err(error);
	}

	Ok((cursor,Some(ans)))
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