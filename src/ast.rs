#[derive(Debug, PartialEq)]
pub struct Id(pub String);

impl Id {
    pub fn from_str(s: &str) -> Id {
        Id(String::from(s))
    }
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(u64),
    FloatPt(f64),
    FixedPt(String),
    Char(char),
    WChar(char),
    Bool(bool),
    String(String),
    WString(String),
}
