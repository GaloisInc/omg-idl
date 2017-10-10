use super::ast::*;
use super::eval::ConstVal;

#[derive(Debug, PartialEq)]
pub enum Error {
    ConstTypeMismatch(&'static str, AnyConst),
    FloatOverflowed(FloatType, AnyConst),
    IntegerOverflowed(&'static str, IntType, AnyConst),
    InvalidSize(ConstVal),
    StringConstantTooLong(ConstType, String),
    UnboundQName(QName),
    Unsupported(&'static str),
}

#[derive(Debug, PartialEq)]
pub enum AnyConst {
    Expr(ConstExpr),
    Val(ConstVal),
}
