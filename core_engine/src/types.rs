use std::fmt::Debug;
use crate::basic_parsing::Combinator;
use std::collections::HashMap;
use std::rc::Rc;
use std::any::Any;
use std::sync::atomic::{AtomicU64, Ordering};

use proc_macro2::{Ident};


//for aliasing we need a unique id
#[derive(Debug,Clone, PartialEq, Eq, Hash)]
pub struct Unique(u64);

static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Unique {
    pub fn new() -> Self {
        let id = UNIQUE_COUNTER.fetch_add(1, Ordering::Relaxed);
        //if some idiot runs a loop that makes lots of these multithreaded this would crash them all
        assert!(id < u64::MAX / 2, "Unique ID counter has overflowed...");
        Unique(id)
    }
    #[inline]
    pub fn id(&self) -> u64 {
        self.0
    }

    //this specifcly takes u32 to avoid wrap around risks
    //if for some reason u need more than a full u32 something went horibly wrong
    #[allow(clippy::unnecessary_fallible_conversions)]
    pub fn ensure_size(size: u32) {
        let size = size.try_into().unwrap();
        let id = UNIQUE_COUNTER.load(Ordering::Relaxed);
        if id >= size {
            return;
        }

        let id = UNIQUE_COUNTER.fetch_add(size - id, Ordering::Relaxed);
        assert!(id < u64::MAX / 2, "Unique ID counter has wrapped around!");
    }
}

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum BasicType {
    Token,
    Number,
    /*might want:
    	Type
    	Token
    	...
    */
}

#[derive(Debug,Clone, PartialEq, Eq)]
pub enum Type{
	Basic(BasicType),
	Array(Rc<Type>),
	Struct(StructTypes),
	Alias(Rc<Type>,Unique),
	Union(Rc<[Type]>),
}


impl From<BasicType> for Type{
	fn from(t: BasicType) -> Self { Type::Basic(t)}
}


#[derive(Debug,Clone)]
pub struct Object{
	pub data: Rc<dyn Any>,
	pub type_info: Type,
}

impl Object {
	pub fn new<T: 'static>(data:T,type_info: Type) -> Self {
		Object{
			data: Rc::new(data),
			type_info
		}
	}
}

pub type StructData = Rc<HashMap<Ident,Object>>;
pub type StructTypes = Rc<HashMap<Ident,Rc<Type>>>;
pub type ArrayData = Rc<Vec<Object>>;

#[test]
fn proc_macro2_equals(){
	use proc_macro2::Span;

	let a = Ident::new("data",Span::call_site());
	let b = Ident::new("data",Span::mixed_site());
	assert_eq!(a,b);
}

pub trait ObjectParser<E = syn::Error>: Combinator<Object, E> + Debug
where
    E: std::error::Error,
{
    fn type_info(&self) -> Type;
}