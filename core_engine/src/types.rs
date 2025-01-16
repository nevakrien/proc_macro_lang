use crate::pattern::Pattern;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use syn::Ident;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Unique(u64);

// A global static atomic counter for unique ID generation
static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(11); //5 code types +6 other

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

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CodeType {
    TokenStream,
    ParenExp,
    Word,
    Literal,
    OpChar,
}

#[derive(Debug)]
pub struct DeclPat {
    //declared pattern
    unique: Unique,
    pub source_code: proc_macro2::TokenStream,
    pub pattern: Pattern,
}

impl PartialEq for DeclPat {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

#[derive(Debug)]
pub struct Block {
    unique: Unique,
    pub source_code: proc_macro2::TokenStream,
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

#[derive(Debug)]
pub struct Function {
    unique: Unique,
    pub name: Rc<Ident>,
    pub pattern: Rc<DeclPat>,
    pub block: Rc<Block>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

#[derive(Debug)]
pub struct Interface {
    unique: Unique,
    pub name: Rc<Ident>,
    pub methods: Vec<Rc<Function>>,
}

impl PartialEq for Interface {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

#[derive(Debug)]
pub struct StructDef {
    unique: Unique,
    pub name: Rc<Ident>,
    pub fields: HashMap<Rc<Ident>, Type>,
}

impl PartialEq for StructDef {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

impl StructDef {
    pub fn new(name: Rc<Ident>, fields: HashMap<Rc<Ident>, Type>) -> Self {
        StructDef {
            name,
            fields,
            unique: Unique::new(),
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub data: TypeData,
    unique: Unique,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use TypeData::*;
        match (&self.data, &other.data) {
            //most types have their unique mark them
            //for composite types we need to do explicit internal checks
            //note:
            //   while interfaces may co implement (iter implemented by array)
            //   this sort of check is not an equality check and is handled else where
            (Checked(c1), Checked(c2)) => c1 == c2,
            (Iter(i1), Iter(i2)) => i1 == i2,
            (Array(a1), Array(a2)) => a1 == a2,
            (Union(u1), Union(u2)) => {
                u1.len() == u2.len() && u1.iter().zip(u2).all(|(a, b)| a == b)
            }

            _ => self.unique == other.unique,
        }
    }
}

impl Type {
    ///Creates the type of type ie type(type)
    pub fn type_type() -> Self {
        Type {
            data: TypeData::Type,
            unique: Unique(7),
        }
    }

    ///Creates the type(pattern)
    pub fn type_pattern() -> Self {
        Type {
            data: TypeData::Pattern,
            unique: Unique(8),
        }
    }

    ///Creates the type(interface)
    pub fn type_interface() -> Self {
        Type {
            data: TypeData::AbsInter,
            unique: Unique(9),
        }
    }

    ///Creates the type(struct)
    pub fn type_struct() -> Self {
        Type {
            data: TypeData::AbsStruct,
            unique: Unique(10),
        }
    }

    /// Constructs a new `Iter` type with the given inner type.
    pub fn iter(inner: Rc<Type>) -> Self {
        Type {
            data: TypeData::Iter(inner),
            unique: Unique::new(),
        }
    }

    /// Constructs a new `Checked` type with the given inner type.
    pub fn checked(inner: Rc<Type>) -> Self {
        Type {
            data: TypeData::Checked(inner),
            unique: Unique::new(),
        }
    }

    /// Constructs a new `Array` type with the given inner type.
    pub fn array(inner: Rc<Type>) -> Self {
        Type {
            data: TypeData::Array(inner),
            unique: Unique::new(),
        }
    }

    /// Constructs a new `Alias` type with the given alias target.
    pub fn alias(target: Rc<Type>) -> Self {
        Type {
            data: TypeData::Alias(target),
            unique: Unique::new(),
        }
    }

    /// Constructs a new `Union` type with the given variants.
    pub fn union(variants: Box<[Rc<Type>]>) -> Self {
        Type {
            data: TypeData::Union(variants),
            unique: Unique::new(),
        }
    }

    /// Constructs a new `Interface` type from an `Rc<Interface>`.
    pub fn interface(interface: Rc<Interface>) -> Self {
        Type::from(interface)
    }

    /// Constructs a new `Struct` type from an `Rc<StructDef>`.
    pub fn structure(struct_def: Rc<StructDef>) -> Self {
        Type::from(struct_def)
    }
}

///for types like structs and unions that need additional relvent data we hold it here
#[derive(Debug, Clone)]
pub enum TypeData {
    Unit,
    Int,
    Code(CodeType),
    Pattern,
    AbsInter,
    AbsStruct,
    Type,

    Alias(Rc<Type>),
    Union(Box<[Rc<Type>]>),

    Checked(Rc<Type>),
    Iter(Rc<Type>),
    Array(Rc<Type>),

    Interface(Rc<Interface>),
    Struct(Rc<StructDef>),
}

impl From<Rc<Interface>> for Type {
    fn from(x: Rc<Interface>) -> Self {
        Type {
            unique: Unique(x.unique.0),
            data: TypeData::Interface(x),
        }
    }
}

impl From<Rc<StructDef>> for Type {
    fn from(x: Rc<StructDef>) -> Self {
        Type {
            unique: Unique(x.unique.0),
            data: TypeData::Struct(x),
        }
    }
}

impl From<()> for Type {
    fn from(_: ()) -> Self {
        Type {
            data: TypeData::Unit,
            unique: Unique(5),
        }
    }
}

impl From<i64> for Type {
    fn from(_: i64) -> Self {
        Type {
            data: TypeData::Int,
            unique: Unique(6),
        }
    }
}

impl From<CodeType> for Type {
    fn from(code: CodeType) -> Self {
        use CodeType::*;
        let idx = match code {
            TokenStream => 0,
            ParenExp => 1,
            Word => 2,
            Literal => 3,
            OpChar => 4,
        };
        Type {
            data: TypeData::Code(code),
            unique: Unique(idx),
        }
    }
}

#[test]
fn equality() {
    use proc_macro2::Span;

    let a: Type = (0).into();
    let b: Type = (8).into();
    assert_eq!(a, b);

    let ra: Rc<Type> = Rc::new(().into());
    let rb: Rc<Type> = Rc::new(().into());
    assert_eq!(ra, rb);
    assert_eq!(Type::checked(ra.clone()), Type::checked(rb.clone()));

    let s = Rc::new(StructDef::new(
        Ident::new("name", Span::call_site()).into(),
        HashMap::new(),
    ));
    let a: Type = s.clone().into();
    let b: Type = s.into();
    assert_eq!(a, b);

    assert!(a != *rb);
    assert!(*ra != b);

    assert_eq!(Type::type_type(), Type::type_type());
    assert_eq!(Type::type_struct(), Type::type_struct());
    assert!(Type::type_interface() != Type::type_pattern());
}

#[test]
fn weird_toks() {
    let _: syn::Token![;];
    let _: syn::Token![/];
    //missing syn::Token![\];
    let _: syn::Token![*];
    let _: syn::Token![!];
    let _: syn::Token![$];
}
