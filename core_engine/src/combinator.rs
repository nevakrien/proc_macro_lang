use crate::name_space::FileNameSpace;
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
///
///as well as program panics on bad parses (this was chosen over errors to avoid corupted states).
#[derive(Debug)]
pub enum PakeratError<E> where E: std::error::Error,{
    ///these are the errors most user code should generate
    ///
    ///dont construct these from a recursive error
    Regular(E),

    ///when you encounter this avoid calling ANY other parsers on the state. 
    ///
    ///and return a recursive error back
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

///result type used for internal cache managment
pub type Pakerat<T,E = syn::Error> = Result<T,PakeratError<E>>;

///basic parser combinator trait used mainly by object parser
pub trait Combinator<T, E = syn::Error>
where
    E: std::error::Error,
{   

    ///this function respect the caching scheme and thus can work with recursive grammers
    fn parse_pakerat<'a>(&self, input: Cursor<'a>,state: &mut State<'a>,) -> Pakerat<(Cursor<'a>, T), E>
    {   
        //chosing recursive as the baseline to avoid corupted states
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

///basic helper funcion for kick starting the parsing process
///
///for now state does not really depend on the input text
///
///this may change in the future so we keep them paired here
pub fn initialize_state(text:&str) -> syn::Result<(Rc<TokenBuffer>,State)>{
    let tokens: TokenStream = text.parse()?;
    let buffer = Rc::new(TokenBuffer::new2(tokens));
    let state = State::default();

    Ok((buffer, state))
}





#[derive(Debug,Default)]
pub struct State<'a>{
    pub file:Rc<RefCell<FileNameSpace<'a>>>,
    // ///this is not used by the system and is intended for debuging
    // pub log: String 

    // pub source: Rc<TokenBuffer>,//this needs to be rc because we dont wana tie the scope to a paticular refrence
    // pub general:GlobalNameSpace<'a>


}

// impl State<'_> {
//     pub fn new() -> Self{
//         State{
//             file:Rc::new(FileNameSpace::default().into()),
            
//         }
//     }
// }
