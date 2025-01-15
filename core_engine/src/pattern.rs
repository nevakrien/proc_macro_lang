use crate::types::Unique;
use std::rc::Rc;

#[derive(Debug,PartialEq)]
pub struct Pattern{

}

#[derive(Debug)]
pub struct Function{
	unique: Unique,
	pub name: Rc<str>,
	pub pattern: Rc<Pattern>,
	pub code: proc_macro2::TokenStream,	
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
       self.unique==other.unique
    }
}