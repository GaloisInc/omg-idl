use std;

use super::ast::*;
use super::error::Error;
use super::error::Error::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ConstVal {
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Char(char),
    WChar(char),
    Bool(bool),
    String(String),
    WString(String),
    Enum(QName),
}

// picking up tomorrow!
// generalize to handle checked shifts and binops
macro_rules! const_val_binop {
    ( $name:ident, $op:tt ) => {
        fn $name(&self, rhs: &ConstVal) -> Result<ConstVal, Error> {
            use self::ConstVal::*;
            const_val_binop_body!(self, rhs, $op,
                                  [I16; I32; I64; U8; U16; U32; U64],
                                  [(I16: i16, U8: u8) => I16: i16;
                                   (I16: i16, U16: u16) => U16: u16;
                                   (I16: i16, U32: u32) => U32: u32;
                                   (I16: i16, U64: u64) => U64: u64;
                                   (I32: i32, I16: i16) => I32: i32;
                                   (I32: i32, U8: u8) => I32: i32;
                                   (I32: i32, U16: u16) => I32: i32;
                                   (I32: i32, U32: u32) => U32: u32;
                                   (I32: i32, U64: u64) => U64: u64;
                                   (I64: i64, I16: i16) => I64: i64;
                                   (I64: i64, I32: i32) => I64: i64;
                                   (I64: i64, U8: u8) => I64: i64;
                                   (I64: i64, U16: u16) => I64: i64;
                                   (I64: i64, U32: u32) => I64: i64;
                                   (I64: i64, U64: u64) => U64: u64;
                                   (U16: u16, U8: u8) => U16: u16;
                                   (U32: u32, U8: u8) => U32: u32;
                                   (U32: u32, U16: u16) => U32: u32;
                                   (U64: u64, U8: u8) => U64: u64;
                                   (U64: u64, U16: u16) => U64: u64;
                                   (U64: u64, U32: u32) => U64: u64])
        }
    }
}

macro_rules! const_val_binop_body {
    ( $self:ident, $rhs:ident, $op:tt,
      [ $( $sc:ident );* ],
      [ $( ($lc:ident:$lt:ident, $rc:ident:$rt:ident) => $ctor:ident: $ty:ident );* ] ) => {
        match ($self, $rhs) {
            $(
                (&$sc(l), &$sc(r)) => Ok($sc(l $op r)),
            )*
            $(
                (&$lc(l), &$rc(r)) => {
                    if l < std::$ty::MIN as $lt || l > std::$ty::MAX as $lt {
                        return Err(IntegerOverflowed(IntType::$lc, $self.as_expr()));
                    }
                    if r < std::$ty::MIN as $rt || r > std::$ty::MAX as $rt {
                        return Err(IntegerOverflowed(IntType::$rc, $rhs.as_expr()));
                    }
                    Ok($ctor(l as $ty $op r as $ty))
                },
                (&$rc(r), &$lc(l)) => {
                    if l < std::$ty::MIN as $lt || l > std::$ty::MAX as $lt {
                        return Err(IntegerOverflowed(IntType::$lc, $self.as_expr()));
                    }
                    if r < std::$ty::MIN as $rt || r > std::$ty::MAX as $rt {
                        return Err(IntegerOverflowed(IntType::$rc, $rhs.as_expr()));
                    }
                    Ok($ctor(r as $ty $op l as $ty))
                },
            )*
            _ => Err(ConstTypeMismatch($self.as_type(), $rhs.as_expr()))                
        }
    }
}

impl ConstVal {
    pub fn as_usize(&self) -> Result<usize, Error> {
        use self::ConstVal::*;
        macro_rules! check {
            ($i:ident, $ty:ident) => (
                if $i > std::usize::MAX as $ty {
                    Err(ArraySizeTooLarge($i as u64))
                } else {
                    Ok($i as usize)
                }
            )
        }
        match *self {
            U8(i) => check!(i, u8),
            U16(i) => check!(i, u16),
            U32(i) => check!(i, u32),
            U64(i) => check!(i, u64),
            _ => Err(InvalidArraySize(self.clone()))
        }
    }

    fn as_type(&self) -> ConstType {
        unimplemented!()
    }

    fn as_expr(&self) -> ConstExpr {
        unimplemented!()
    }

    const_val_binop!(or, |);
    const_val_binop!(and, &);
    const_val_binop!(xor, ^);
}

impl ConstExpr {
    /// Evaluate a constant at a particular type. We cannot solely
    /// infer the type from the expression, as literals get
    /// interpreted differently depending on the precision of the
    /// constant type.
    pub fn eval(&self, env: &GlobalEnv, ty: &ConstType) -> Result<ConstVal, Error> {
        use super::ast::ConstType::*;
        macro_rules! charlike {
            ($ctor:ident) => (match *self {
                ConstExpr::$ctor(c) => Ok(ConstVal::$ctor(c)),
                _ => Err(ConstTypeMismatch(ty.clone(), self.clone()))
            })
        }
        macro_rules! stringlike {
            ($ctor:ident, $bound:ident) => (match *self {
                ConstExpr::$ctor(ref s) => {
                    match *$bound {
                        Bound::Unbounded => Ok(ConstVal::$ctor(s.clone())),
                        Bound::Bounded(ref size) => {
                            let size = size.eval()?.as_usize()?;
                            if s.len() > size {
                                Err(StringConstantTooLong(ty.clone(), s.clone()))
                            } else {
                                Ok(ConstVal::$ctor(s.clone()))
                            }
                        },
                    }
                },
                _ => Err(ConstTypeMismatch(ty.clone(), self.clone()))
            })
        }
        match *ty {
            Int(ity) => self.eval_int(ity),
            Float(fpty) => self.eval_float(fpty),
            Char => charlike!(Char),
            WChar => charlike!(WChar),
            Bool => self.eval_bool(),
            String(ref b) => stringlike!(String, b),
            WString(ref b) => stringlike!(WString, b),
            Enum(ref qn) => self.eval_enum(env, ty, qn),
            Fixed => Err(Unsupported("fixed point")),
        }
    }

    // Hooboy. The reason for this macro mess is the combinatorial
    // explosion of case checking. What we're doing in the end is
    // actually pretty straightforward.
    //
    // We check the type that the constant is expected to be, and
    // based on that, choose whether to evaluate it at 32 or 64 bits
    // (the spec doesn't mention 16 or 8 bits, but does mention
    // 32-bits as a fallthrough).
    //
    // Once we evaluate it, we make sure that it's within the range of
    // the type we eventually want, and then cast it.
    fn eval_int(&self, ity: IntType) -> Result<ConstVal, Error> {
        // Top-level macro with the outer match on the type
        macro_rules! body {
            ( [ $( $ctor:ident, $ty:ident, $eval:ident );* ] ) => {
                match ity {
                    $( IntType::$ctor => case!($ctor, $ty, $eval), )*
                }
            }
        };
        // The right-hand side of each case in the outer match, just a
        // wrapper to call `subcases!` with all the different value
        // types we might be casting from
        macro_rules! case {
            ($ctor:ident, $ty:ident, $eval:ident) => ({
                // The innermost macro, does the work of actually
                // checking for overflow, and then doing the cast
                macro_rules! subcase {
                    ($i:ident, $dty:ident) => ({
                        if $i < std::$ty::MIN as $dty || $i > std::$ty::MAX as $dty {
                            Err(IntegerOverflowed(ity, self.clone()))
                        } else {
                            Ok(ConstVal::$ctor($i as $ty))
                        }
                    })
                }
                subcases!($eval, [I16, i16;
                                  I32, i32;
                                  I64, i64;
                                  U8, u8;
                                  U16, u16;
                                  U32, u32;
                                  U64, u64])
            })
        }
        // Do the evaluation, and then match the different cases that
        // might come out the other end
        macro_rules! subcases {
            ( $eval:ident, [ $( $disc:ident, $dty:ident );* ] ) => {
                match self.$eval(ity)? {
                    $( ConstVal::$disc(i) => subcase!(i, $dty), )*
                    _ => Err(ConstTypeMismatch(ConstType::Int(ity), self.clone()))
                }
            }
        };        
        body!([I16, i16, eval_int32;
               I32, i32, eval_int32;
               I64, i64, eval_int64;
               U8, u8, eval_int32;
               U16, u16, eval_int32;
               U32, u32, eval_int32;
               U64, u64, eval_int64])
    }

    fn eval_int32(&self, ity: IntType) -> Result<ConstVal, Error> {
        use super::ast::ConstExpr::*;
        match *self {
            Int(i) if i > std::u32::MAX as u64 => Err(IntegerOverflowed(ity, self.clone())),
            Int(i) => Ok(ConstVal::U32(i as u32)),
            Or(ref l, ref r) => l.eval_int32(ity)?.or(&r.eval_int32(ity)?),
            And(ref l, ref r) => l.eval_int32(ity)?.and(&r.eval_int32(ity)?),
            Xor(ref l, ref r) => l.eval_int32(ity)?.xor(&r.eval_int32(ity)?),
            _ => Err(ConstTypeMismatch(ConstType::Int(ity), self.clone()))
        }
    }

    fn eval_int64(&self, ity: IntType) -> Result<ConstVal, Error> {
        unimplemented!()
    }

    fn eval_float(&self, fpty: FloatType) -> Result<ConstVal, Error> {
        unimplemented!()
    }

    fn eval_bool(&self) -> Result<ConstVal, Error> {
        match *self {
            ConstExpr::Bool(b) => Ok(ConstVal::Bool(b)),
            _ => Err(ConstTypeMismatch(ConstType::Bool, self.clone()))
        }
    }

    fn eval_enum(&self, env: &GlobalEnv, ty: &ConstType, qn: &QName) -> Result<ConstVal, Error> {
        let e = match *env.get(qn).ok_or(UnboundQName(qn.clone()))? {
            Entry::Enum(ref w) => w.upgrade().expect("enum in environment exists"),
            Entry::TypeDef(Type::Enum(ref qn)) =>
                match *env.get(qn).ok_or(UnboundQName(qn.clone()))? {
                    Entry::Enum(ref w) => w.upgrade().expect("enum in environment exists"),
                    _ => return Err(ConstTypeMismatch(ty.clone(), self.clone()))
                },
            _ => return Err(ConstTypeMismatch(ty.clone(), self.clone()))
        };
        if let Some(_) = e.enumerators.iter().find(|&e_qn| e_qn == qn) {
            Ok(ConstVal::Enum(qn.clone()))
        } else {
            Err(ConstTypeMismatch(ty.clone(), self.clone()))
        }
    }
}

impl PosIntConst {
    pub fn eval(&self) -> Result<ConstVal, Error> {
        self.as_const_expr().eval_int(IntType::U64)
    }
}
