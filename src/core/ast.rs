use std::collections::HashMap;
use std::rc::{Rc, Weak};

use parser::ast::identifier;

pub type Id = identifier;

pub type QName = Vec<Id>;

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

pub type Specification = Vec<Definition>;

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
    pub raises: Vec<Id>,
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
    pub get_raises: Vec<Id>,
    pub set_raises: Vec<Id>,
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub id: Id,
    pub defs: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub id: Id,
    pub members: Vec<(Id, Type)>,
}

#[derive(Debug, PartialEq)]
pub struct Except {
    pub id: Id,
    pub members: Vec<(Id, Type)>,
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
    pub ty: Type,
    pub expr: ConstExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Char,
    WChar,
    Bool,
    String(Bound),
    WString(Bound),
    Sequence(Box<Type>, Bound),
    Array(Box<Type>, Vec<ConstExpr>),
    Struct(QName),
    Union(QName),
    Enum(QName),
    Except(QName),
    Interface(QName),
    Void,
    // unsupported
    F128,
    Fixed,
}

#[derive(Debug, PartialEq)]
pub enum ConstVal {
    Int(IntVal),
    Float(FloatVal),
    Char(char),
    WChar(char),
    Bool(bool),
    String(String),
    WString(String),
    Enum(QName),
}

#[derive(Debug, PartialEq)]
pub enum IntVal {
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Debug, PartialEq)]
pub enum FloatVal {
    F32(f32),
    F64(f64),
}

pub type PosIntConst = ConstExpr;

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
