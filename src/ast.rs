#[derive(Debug, PartialEq)]
pub struct Id(pub String);

impl Id {
    pub fn from_str(s: &str) -> Id {
        Id(String::from(s))
    }
}

#[derive(Debug, PartialEq)]
pub enum ScopedName {
    Qualified(Vec<Id>),
    FileScope(Vec<Id>),
}

impl ScopedName {
    pub fn push(&mut self, id: Id) {
        match *self {
            ScopedName::Qualified(ref mut ids) => ids.push(id),
            ScopedName::FileScope(ref mut ids) => ids.push(id),
        };
    }
}

#[derive(Debug, PartialEq)]
pub enum ConstExpr {
    ScopedName(ScopedName),
    Literal(Literal),
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
}

impl ConstExpr {
    pub fn binop<F>(ctor: F, l: ConstExpr, r: ConstExpr) -> ConstExpr
        where F: FnOnce(Box<ConstExpr>, Box<ConstExpr>) -> ConstExpr {
        ctor(Box::new(l), Box::new(r))
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(u64),
    FloatPt(f64),
    FixedPt(String),
    Char(char),
    WChar(char),
    Bool(bool),
    String(String),
    WString(String),
}
