use crate::ObjectParser;
use std::collections::HashMap;
use std::rc::Rc;
use proc_macro2::Ident;
use crate::types::{StructParser};
use syn::buffer::Cursor;

pub fn parse_pattern(_input:Cursor) -> syn::Result<(Cursor,StructParser)>{
	todo!()
}

///looks for "name :" definitions are expected to come after.
pub fn parse_name_capture(_input:Cursor) -> syn::Result<(Cursor,Ident)>{
	todo!()
}

pub fn parse_parser(_input:Cursor,_knowen_names:HashMap<Ident,Rc<dyn ObjectParser>>) -> syn::Result<(Cursor,Rc<dyn ObjectParser>)>{
	todo!()
}