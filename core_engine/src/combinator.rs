use proc_macro2::TokenStream;
use std::cell::RefCell;
use std::rc::Weak;
use syn::buffer::TokenBuffer;
use crate::types::Type;
use crate::Object;
use std::rc::Rc;
use std::collections::HashMap;
use crate::name_space::NameSpace;
use syn::buffer::Cursor;

pub trait Combinator<T, E = syn::Error>
where
    E: std::error::Error,
{
    fn parse<'a>(&self, input: Cursor<'a>,state: &mut State,) -> Result<(Cursor<'a>, T), E>;
}


pub fn initialize_state(text:&str) -> syn::Result<(Rc<TokenBuffer>,State)>{
     let tokens: TokenStream = text.parse()?;
    let buffer = Rc::new(TokenBuffer::new2(tokens));
    let state = State::new(&Rc::downgrade(&buffer));

    Ok((buffer, state))
}





#[derive(Debug)]
pub struct State<'name_space>{
    pub name_space : NameSpace<'name_space>,
    pub file_cache:Rc<RefCell<FileCache>>,
    pub general_cache:GeneralCache
}

impl State<'_> {
    pub fn new(source:&Source) -> Self{
        let mut general_cache = GeneralCache::default();
        let file_cache = general_cache.get(source);
        let name_space = NameSpace::new_global();

        State{
            general_cache,file_cache,name_space
        }
    }
}

///for all parses that start at this byte in the source file (using span.byte_range() to retrive it)
#[derive(Debug,Default,Clone)]
pub struct TypeCache(pub HashMap<usize,Option<Rc<Object>>>); 

///used for implementing pakerat parsing
#[derive(Debug,Default,Clone)]
pub struct FileCache(pub HashMap<Type,TypeCache>); 




/// A cache that maps sources to file caches, with automatic cleanup of stale entries.
#[derive(Default,Debug)]
pub struct GeneralCache {
    cache: HashMap<*const TokenBuffer, (Weak<TokenBuffer>, Rc<RefCell<FileCache>>)>,
    counter: usize, // Tracks when to trigger cleanup
}

/// A key used by GeneralCache for the purpose of retriving token buffers
pub type Source = Weak<TokenBuffer>;


impl GeneralCache {
    /// Creates a new empty `GeneralCache`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Retrieves an `Rc<RefCell<FileCache>>`, resetting stale entries if necessary.
    ///
    /// Automatically calls `clean_up` when the internal counter exceeds the cache size.
    /// 
    /// This function is an armotized O(1) time with a potential O(n) spike for cleanups
    pub fn get(&mut self, source: &Source) -> Rc<RefCell<FileCache>> {
        // Perform cleanup when counter reaches cache size (amortized O(1) cleanup)
        if self.counter >= self.cache.len() {
            self.clean_up();
        }

        self.get_no_cleanup(source)
    }

    /// Similar to get just without the automatic call to cleanup
    /// 
    /// As such using this function can cause an effective memory leak unless cleanup is called at some point
    pub fn get_no_cleanup(&mut self, source: &Source) -> Rc<RefCell<FileCache>> {
        // Using the raw pointer to `TokenBuffer` as a key ensures no collisions:
        // Since no 2 Rcs can share memory and not be the same object.
        // Even if a `Weak<TokenBuffer>` points to a freed `Rc`, it is impossible for another `Rc`
        // to reuse the same address while sharing the same `Weak` pointer location.
        let key = source.as_ptr();

        let entry = self.cache.entry(key).or_insert_with(|| {
            self.counter += 1; // Increment counter on new entry
            let cache = Rc::new(RefCell::new(FileCache::default()));
            (source.clone(), cache)
        });

        // Replace stale entries with a fresh `FileCache`
        if entry.0.upgrade().is_none() {
            let cache = Rc::new(RefCell::new(FileCache::default()));
            entry.0 = source.clone();
            entry.1 = cache;
        }

        entry.1.clone()
    }

    /// Removes all stale entries from the cache.
    ///
    /// Resets the counter to zero after cleanup.
    pub fn clean_up(&mut self) {
        self.counter = 0;
        self.cache.retain(|_, (source, _)| source.upgrade().is_some());
    }
}
