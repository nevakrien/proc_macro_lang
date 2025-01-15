use std::collections::HashMap;
use crate::pattern::Function;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Unique(u64);

// A global static atomic counter for unique ID generation
static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(11);//5 code types +6 other

impl Unique {
    pub fn new() -> Self {
        let id = UNIQUE_COUNTER.fetch_add(1, Ordering::Relaxed);
        //if some idiot runs a loop that makes lots of these multithreaded this would crash them all
        assert!(id < u64::MAX /2, "Unique ID counter has wrapped around!");
        Unique(id)
    }
    #[inline]
    pub fn id(&self) -> u64{
    	self.0
    }

    //this specifcly takes u32 to avoid wrap around risks
	//if for some reason u need more than a full u32 something went horibly wrong
	pub fn ensure_size(size:u32){
		let size = size.try_into().unwrap();
		let id = UNIQUE_COUNTER.load(Ordering::Relaxed);
		if id>=size{
			return
		}

		let id = UNIQUE_COUNTER.fetch_add(size-id, Ordering::Relaxed);
		assert!(id < u64::MAX /2, "Unique ID counter has wrapped around!");
	}
}



#[derive(Debug,Clone,Copy,PartialEq)]
pub enum CodeType{
	TokenStream,
	ParenExp,
	Word,
	Literal,
	OpChar,
}

#[derive(Debug)]
pub struct Interface{
	unique:Unique,
	pub name: Rc<str>,
	pub methods: Vec<Rc<Function>>
}


impl PartialEq for Interface {
    fn eq(&self, other: &Self) -> bool {
       self.unique==other.unique
    }
}

#[derive(Debug)]
pub struct StructDef{
	unique:Unique,
	pub name: Rc<str>,
	pub feilds:HashMap<Rc<str>,Type>
}

impl PartialEq for StructDef {
    fn eq(&self, other: &Self) -> bool {
       self.unique==other.unique
    }
}

#[derive(Debug)]
pub struct Type{
	pub data:TypeData,
	unique:Unique
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
    	use TypeData::*;
        match (&self.data,&other.data) {
        	//most types have their unique mark them
        	//for composite types we need to do explicit internal checks
        	//note: 
        	//   while interfaces may co implement (iter implemented by array)
        	//   this sort of check is not an equality check and is handled else where

            (Checked(c1), Checked(c2)) => c1 == c2,
            (Iter(i1), Iter(i2)) => i1 == i2,
            (Array(a1), Array(a2)) => a1 == a2,
            (Union(u1), Union(u2)) => u1.len() == u2.len() && u1.iter().zip(u2).all(|(a, b)| a == b),

            _ => self.unique==other.unique,
        }
        
    }
}


#[derive(Debug,Clone)]
pub enum TypeData{
	Unit,
	Int,
	Code(CodeType),
	Pattern,
	AbsInter,
	AbsStruct,
	Type,

	Alias(Rc<Type>),
	Union(Vec<Rc<Type>>),

	Checked(Rc<Type>),
	Iter(Rc<Type>),
	Array(Rc<Type>),

	Interface(Rc<Interface>),
	Struct(Rc<StructDef>),
}


impl From<Rc<Interface>> for Type{
	fn from(x: Rc<Interface>) -> Self { 
		Type{
			unique:Unique(x.unique.0),
			data:TypeData::Interface(x)
		}
	}
}

impl From<Rc<StructDef>> for Type{
	fn from(x: Rc<StructDef>) -> Self { 
		Type{
			unique:Unique(x.unique.0),
			data:TypeData::Struct(x)
		}
	}
}

impl From<()> for Type {
    fn from(_: ()) -> Self {
        Type { data: TypeData::Unit, unique: Unique(5) }
    }
}

impl From<i64> for Type {
    fn from(_: i64) -> Self {
        Type { data: TypeData::Int, unique: Unique(6) }
    }
}

impl From<CodeType> for Type {
    fn from(code: CodeType) -> Self {
    	use CodeType::*;
    	let idx = match code {
    	    TokenStream=>0,
			ParenExp=>1,
			Word=>2,
			Literal=>3,
			OpChar=>4,
    	};
        Type { data: TypeData::Code(code), unique: Unique(idx) }
    }
}