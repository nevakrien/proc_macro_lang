use std::collections::HashMap;
use std::rc::Rc;
use std::any::Any;
use std::sync::atomic::{AtomicU64, Ordering};


#[repr(u64)] // This ensures the enum is represented as a u8 in memory.
pub enum Types {
	Token = 1,
    Tokens,
    Number,
    Type,
    Last
}

#[derive(Debug,Clone, PartialEq, Eq, Hash)]
pub struct Unique(u64);

// A global static atomic counter for unique ID generation
static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(Types::Last as u64); //healthy distant to allow basicly infinite user types

impl Unique {
    pub fn new() -> Self {
        let id = UNIQUE_COUNTER.fetch_add(1, Ordering::Relaxed);
        //if some idiot runs a loop that makes lots of these multithreaded this would crash them all
        assert!(id < u64::MAX / 2, "Unique ID counter has wrapped around!");
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

impl From<Types> for Unique{

	fn from(t: Types) -> Self { Unique(t as u64) }
}

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug,Clone)]
pub struct Object{
	pub data: Rc<dyn Any>,
	pub type_id: Unique,
}

impl Object {
	pub fn new<T: 'static>(data:T,type_id: Unique) -> Self {
		Object{
			data: Rc::new(data),
			type_id
		}
	}
}

pub type StructData = Rc<HashMap<proc_macro2::Ident,Object>>;
pub type ArrayData = Rc<Vec<Object>>;

