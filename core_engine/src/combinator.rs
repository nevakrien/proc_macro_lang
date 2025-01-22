use crate::name_space::FileNameSpace;
use crate::name_space::GlobalNameSpace;
use crate::name_space::Source;
use proc_macro2::TokenStream;
use std::cell::RefCell;
use syn::buffer::TokenBuffer;
use std::rc::Rc;
use syn::buffer::Cursor;


///error type for handeling recursive parses.
///
///unlike the usual errors a recursive parse error should terminate the entire parse.
///
///these recursive errors fundementally mean there was an infinite loop in the program.
///
///miss reporting an error as regular can lead to weird caching behivior and wrong/unpredictble behivior.
#[derive(Debug)]
pub enum PakeratError<E> where E: std::error::Error,{
    Regular(E),
    Recursive(E)
}

impl<E: std::error::Error> PakeratError<E>{
    pub fn inner(self) -> E {
        match self {
            PakeratError::Regular(e) => e,
            PakeratError::Recursive(e) => e,
        }
    }

}

pub type Pakerat<T,E = syn::Error> = Result<T,PakeratError<E>>;

///basic parser combinator trait used mainly by object parser
pub trait Combinator<T, E = syn::Error>
where
    E: std::error::Error,
{   

    ///this function respect the caching scheme and thus can work with recursive grammers
    fn parse_pakerat<'a>(&self, input: Cursor<'a>,state: &mut State<'a>,) -> Pakerat<(Cursor<'a>, T), E>
    {
        self.parse(input,state).map_err(|e| PakeratError::Recursive(e))
    }

    ///this function is what you would typically use as an entry point.
    ///
    ///it is mainly intended for use in the aplication level and not for writing parsers directly.
    ///
    ///its generally advisble to implement parse_pakerat for better results with caching.
    fn parse<'a>(&self, input: Cursor<'a>,state: &mut State<'a>,) -> Result<(Cursor<'a>, T), E>{
        self.parse_pakerat(input,state).map_err(|e| e.inner())

    }
}



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
