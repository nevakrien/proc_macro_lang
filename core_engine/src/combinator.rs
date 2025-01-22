use std::collections::HashMap;
use crate::name_space::FileNameSpace;
use crate::name_space::GlobalNameSpace;
use crate::name_space::Source;
use proc_macro2::TokenStream;
use std::cell::RefCell;
use syn::buffer::TokenBuffer;
use std::rc::Rc;
use syn::buffer::Cursor;

pub trait Combinator<T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse<'a>(&self, input: Cursor<'a>,state: &mut State<'a>,) -> Result<(Cursor<'a>, T), E>;
}

// pub enum Pakerat<T, E = syn::Error>{
//     Ok(T),
//     Err(E),
//     Pending,
// }

// impl<T, E> From<Pakerat<T,E>> for Result<T,E>{

// fn from(x: Pakerat<T, E>) -> Self { 
//     match x{
//         Pakerat::Err(e) => Err(e),
//         Pakerat::Ok(a) => Ok(a),
//     } 
// }
// }

pub fn initialize_state(text:&str) -> syn::Result<(Rc<TokenBuffer>,State)>{
     let tokens: TokenStream = text.parse()?;
    let buffer = Rc::new(TokenBuffer::new2(tokens));
    let state = State::new(&Rc::downgrade(&buffer));

    Ok((buffer, state))
}





#[derive(Debug)]
pub struct State<'a>{
    pub file:Rc<RefCell<FileNameSpace<'a>>>,
    pub general:GlobalNameSpace<'a>


}

impl State<'_> {
    pub fn new(source:&Source) -> Self{
        let mut general = GlobalNameSpace::default();
        let file = general.get(source);

        State{
            general,file
        }
    }
}
