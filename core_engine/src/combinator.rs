use crate::basic_parsing::AnyParser;
use crate::basic_parsing::IntParser;
use crate::basic_parsing::EndParser;
use crate::basic_parsing::LiteralParser;
use crate::basic_parsing::WordParser;
use crate::basic_parsing::PuncParser;
use crate::basic_parsing::GroupParser;
use crate::types::BasicType;
use crate::types::Type;
use std::collections::BTreeMap;
use crate::ObjectParser;
use crate::Object;
use std::fmt;
use std::collections::HashMap;
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
        self.parse(input,state).map_err(PakeratError::Recursive)
    }

    ///this function is what you would typically use as an entry point.
    ///
    ///it is mainly intended for use in the aplication level and not for writing parsers directly.
    ///
    ///its generally advisble to implement parse_pakerat for better results with caching.
    fn parse<'a,>(&self, input: Cursor<'a>,state: &mut State<'a>,) -> Result<(Cursor<'a>, T), E>{
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




#[derive(Debug)]
pub struct State<'a>{
    pub file:Rc<RefCell<FileNameSpace>>,
    pub type_info: HashMap<Type,TypeGlobalData<'a>>,

    // ///this is not used by the system and is intended for debuging
    // pub log: String 


}


impl Default for State<'_>{

fn default() -> Self {
    State{
        file:Rc::default(),
        type_info:default_type_info_map(),
    }
}
}

///deafualt here is a place holder!!!
#[derive(Debug,Default)]
pub struct TypeGlobalData<'a>{
    pub parsers:BTreeMap<NonNanFloat,Rc<dyn ObjectParser>>,
    pub (crate) cache: HashMap<usize,CacheState<'a>>,
}

macro_rules! insert_type_parsers {
    ($map:expr, { $($basic_type:expr => $parser:expr),* $(,)? }) => {
        $(
            {
                let mut parsers = BTreeMap::new();
                parsers.insert(NonNanFloat::new(1.0).unwrap(), Rc::new($parser) as Rc<dyn ObjectParser>);
                $map.insert(Type::Basic($basic_type), TypeGlobalData { parsers,cache:HashMap::new() });
            }
        )*
    };
}

pub fn default_type_info_map() -> HashMap<Type, TypeGlobalData<'static>> {
    let mut type_info = HashMap::new();

    insert_type_parsers! {
        type_info, {
            BasicType::Tree => AnyParser,
            BasicType::Int => IntParser,
            BasicType::None => EndParser,
            BasicType::Literal => LiteralParser,
            BasicType::Word => WordParser,
            BasicType::Punc => PuncParser,
            BasicType::Group => GroupParser,
        }
    };

    type_info
}

pub enum CacheState<'a>{
    Pending,
    Err(syn::Error),
    Ok(Cursor<'a>,Object)
}
// Implement Debug for CacheState
impl fmt::Debug for CacheState<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CacheState::Pending => write!(f, "CacheState::Pending"),
            CacheState::Err(error) => write!(f, "CacheState::Err({:?})", error),
            CacheState::Ok ( _,obj) => {
                write!(f, "CacheState::Ok {{ cursor: <opaque>, obj: {:?} }}", obj)
            }
        }
    }
}

use std::cmp::Ordering;

// Wrapper for f32 that ensures no NaN values
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct NonNanFloat(f32);

impl NonNanFloat {
    // Constructor that checks for NaN
    pub fn new(value: f32) -> Option<Self> {
        if value.is_nan() {
            None
        } else {
            Some(Self(value))
        }
    }
}

impl Eq for NonNanFloat {}

#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for NonNanFloat {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).expect("Comparison failed; invalid float values detected")
    }
}


impl TryFrom<f32> for NonNanFloat {
    type Error = &'static str;

    fn try_from(value: f32) -> Result<Self, Self::Error> {
        if value.is_nan() {
            Err("NaN values are not allowed in NonNanFloat")
        } else {
            Ok(Self(value))
        }
    }
}

impl From<NonNanFloat> for f32 {

fn from(x: NonNanFloat) -> f32 { x.0 }
}