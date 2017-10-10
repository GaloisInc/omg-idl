use super::ast::*;
use super::eval::ConstVal;

#[derive(Debug, PartialEq)]
pub enum Error {
    ArraySizeTooLarge(u64),
    ConstTypeMismatch(ConstType, ConstExpr),
    IntegerOverflowed(IntType, ConstExpr),
    InvalidArraySize(ConstVal),
    StringConstantTooLong(ConstType, String),
    UnboundQName(QName),
    Unsupported(&'static str),
}
