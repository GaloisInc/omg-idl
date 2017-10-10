use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub struct Id(String);

impl Id {
    pub fn new(s: &str) -> Id {
        Id(s.to_owned())
    }
}

impl Eq for Id {}

// per IDL spec, identifiers collide case-insensitively
impl PartialEq for Id {
    fn eq(&self, other: &Id) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}

// per IDL spec, identifiers collide case-insensitively
impl Hash for Id {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state);
    }
}

impl Id {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct QName(Vec<Id>);

impl QName {
    pub fn new() -> QName {
        QName(vec![])
    }

    pub fn push(&mut self, id: &Id) {
        self.0.push(id.clone());
    }

    pub fn pop(&mut self) -> Option<Id> {
        self.0.pop()
    }

}

pub type GlobalEnv = HashMap<QName, Entry>;

#[derive(Debug)]
pub enum Entry {
    Module(Weak<Module>),
    Const(Weak<Const>),
    Native(Id),
    Struct(Weak<Struct>),
    Union(Weak<Union>),
    Enum(Weak<Enum>),
    EnumVariant(Weak<Enum>),
    TypeDef(Type),
    Except(Weak<Except>),
    Interface(Weak<Interface>),
}

pub struct Specification(pub Vec<Definition>);

#[derive(Debug, PartialEq)]
pub enum Definition {
    Module(Rc<Module>),
    Const(Rc<Const>),
    Struct(Rc<Struct>),
    Union(Rc<Union>),
    Enum(Rc<Enum>),
    Native(Id),
    TypeDef(Id, Type),
    Except(Rc<Except>),
    Interface(Rc<Interface>),
}

#[derive(Debug, PartialEq)]
pub struct Interface {
    pub id: Id,
    pub parents: Vec<QName>,
    pub ops: Vec<Op>,
    pub attrs: Vec<Attr>,
}

#[derive(Debug, PartialEq)]
pub struct Op {
    pub id: Id,
    pub ret: Type,
    pub params: Vec<(Id, ParamDir, Type)>,
    pub raises: Vec<QName>,
}

#[derive(Debug, PartialEq)]
pub enum ParamDir {
    In,
    Out,
    InOut,
}

#[derive(Debug, PartialEq)]
pub struct Attr {
    pub id: Id,
    pub read_only: bool,
    pub ty: Type,
    pub get_raises: Vec<QName>,
    pub set_raises: Vec<QName>,
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub id: Id,
    pub defs: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub struct Member {
    pub id: Id,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub id: Id,
    pub members: Vec<Member>,
}

#[derive(Debug, PartialEq)]
pub struct Except {
    pub id: Id,
    pub members: Vec<Member>,
}

#[derive(Debug, PartialEq)]
pub struct Union {
    pub id: Id,
    pub disc_ty: Type,
    pub variants: Vec<(Id, Type, Vec<UnionLabel>)>,
}

#[derive(Debug, PartialEq)]
pub enum UnionLabel {
    Const(ConstExpr),
    Default,
}

#[derive(Debug, PartialEq)]
pub struct Enum {
    pub id: Id,
    pub enumerators: Vec<QName>,
}

#[derive(Debug, PartialEq)]
pub struct Const {
    pub id: Id,
    pub ty: ConstType,
    pub expr: ConstExpr,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntType {
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FloatType {
    F32,
    F64,
    // unsupported
    F128,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstType {
    Int(IntType),
    Float(FloatType),
    Char,
    WChar,
    Bool,
    String(Bound),
    WString(Bound),
    Enum(QName),
    // unsupported
    Fixed,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int(IntType),
    Float(FloatType),
    Char,
    WChar,
    Bool,
    String(Bound),
    WString(Bound),
    Sequence(Box<Type>, Bound),
    Array(Box<Type>, Vec<PosIntConst>),
    Struct(QName),
    Union(QName),
    Enum(QName),
    Except(QName),
    Interface(QName),
    Void,
    // unsupported
    Fixed,
}

impl Type {
    pub fn is_constr_type(&self) -> bool {
        use self::Type::*;
        match *self {
            Struct(_) => true,
            Union(_) => true,
            Enum(_) => true,
            _ => false,
        }
    }

    pub fn to_const_type(&self) -> Option<ConstType> {
        use self::Type::*;
        match *self {
            Int(ity) => Some(ConstType::Int(ity)),
            Float(fpty) => Some(ConstType::Float(fpty)),
            Char => Some(ConstType::Char),
            WChar => Some(ConstType::WChar),
            Bool => Some(ConstType::Bool),
            String(ref b) => Some(ConstType::String(b.clone())),
            WString(ref b) => Some(ConstType::WString(b.clone())),
            Enum(ref qn) => Some(ConstType::Enum(qn.clone())),
            Fixed => Some(ConstType::Fixed),
            _ => None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PosIntConst(ConstExpr);

impl PosIntConst {
    pub fn new(e: ConstExpr) -> PosIntConst {
        PosIntConst(e)
    }

    pub fn as_const_expr(&self) -> &ConstExpr {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Bound {
    Bounded(PosIntConst),
    Unbounded,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstExpr {
    // literals
    Int(u64),
    Float(f64),
    Char(char),
    WChar(char),
    Bool(bool),
    String(String),
    WString(String),
    Enum(QName),
    // compound
    Or(Box<ConstExpr>, Box<ConstExpr>),
    Xor(Box<ConstExpr>, Box<ConstExpr>),
    And(Box<ConstExpr>, Box<ConstExpr>),
    Shr(Box<ConstExpr>, Box<ConstExpr>),
    Shl(Box<ConstExpr>, Box<ConstExpr>),
    Add(Box<ConstExpr>, Box<ConstExpr>),
    Sub(Box<ConstExpr>, Box<ConstExpr>),
    Mult(Box<ConstExpr>, Box<ConstExpr>),
    Div(Box<ConstExpr>, Box<ConstExpr>),
    Mod(Box<ConstExpr>, Box<ConstExpr>),
    Negate(Box<ConstExpr>),
    Complement(Box<ConstExpr>),
    // unsupported literal
    Fixed,
}
