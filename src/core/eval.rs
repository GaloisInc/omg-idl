use std;
use std::cmp;

use super::ast::*;
use super::error::{AnyConst, Error};
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

macro_rules! const_val_bitwise_binop {
    ( $name:ident, $op:tt ) => {
        fn $name(&self, rhs: &ConstVal) -> Result<ConstVal, Error> {
            use self::ConstVal::*;
            const_val_bitwise_binop_body!(self, rhs, $op,
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

macro_rules! const_val_bitwise_binop_body {
    ( $self:ident, $rhs:ident, $op:tt,
      [ $( $sc:ident );* ],
      [ $( ($lc:ident:$lt:ident, $rc:ident:$rt:ident) => $ctor:ident: $ty:ident );* ] ) => {
        match ($self, $rhs) {
            $(
                (&$sc(l), &$sc(r)) => Ok($sc(l $op r)),
            )*
            $(
                (&$lc(l), &$rc(r)) => {
                    if l < cmp::min(0, std::$ty::MIN as $lt) || l > cmp::max(std::$ty::MAX as $lt, std::$lt::MAX) {
                        return Err(IntegerOverflowed("bitwise_binop", IntType::$lc, AnyConst::Val($self.clone())));
                    }
                    if r < cmp::min(0, std::$ty::MIN as $rt) || r > cmp::max(std::$ty::MAX as $rt, std::$rt::MAX) {
                        return Err(IntegerOverflowed("bitwise_binop", IntType::$rc, AnyConst::Val($rhs.clone())));
                    }
                    Ok($ctor(l as $ty $op r as $ty))
                },
                (&$rc(r), &$lc(l)) => {
                    if l < cmp::min(0, std::$ty::MIN as $lt) || l > cmp::max(std::$ty::MAX as $lt, std::$lt::MAX) {
                        return Err(IntegerOverflowed("bitwise_binop", IntType::$lc, AnyConst::Val($self.clone())));
                    }
                    if r < cmp::min(0, std::$ty::MIN as $rt) || r > cmp::max(std::$ty::MAX as $rt, std::$rt::MAX) {
                        return Err(IntegerOverflowed("bitwise_binop", IntType::$rc, AnyConst::Val($rhs.clone())));
                    }
                    Ok($ctor(r as $ty $op l as $ty))
                },
            )*
            _ => Err(ConstTypeMismatch("integer", AnyConst::Val($self.clone())))                
        }
    }
}

macro_rules! const_val_checked_binop {
    ( $name:ident, $op:ident ) => {
        fn $name(&self, rhs: &ConstVal) -> Result<ConstVal, Error> {
            use self::ConstVal::*;
            const_val_checked_binop_body!(self, rhs, $op,
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

macro_rules! const_val_checked_binop_body {
    ( $self:ident, $rhs:ident, $op:ident,
      [ $( $sc:ident );* ],
      [ $( ($lc:ident:$lt:ident, $rc:ident:$rt:ident) => $ctor:ident: $ty:ident );* ] ) => {
        // the errors that come out of the checked_ ops should probably blame the whole expression instead
        match ($self, $rhs) {
            $(
                (&$sc(l), &$sc(r)) => Ok($sc(l.$op(r).ok_or(IntegerOverflowed("checked_binop sym op", IntType::$sc, AnyConst::Val($self.clone())))?)),
            )*
            $(
                (&$lc(l), &$rc(r)) => {
                    if l < cmp::min(0, std::$ty::MIN as $lt) || l > cmp::max(std::$ty::MAX as $lt, std::$lt::MAX) {
                        return Err(IntegerOverflowed("checked_binop clamp 1", IntType::$lc, AnyConst::Val($self.clone())));
                    }
                    if r < cmp::min(0, std::$ty::MIN as $rt) || r > cmp::max(std::$ty::MAX as $rt, std::$rt::MAX) {
                        return Err(IntegerOverflowed("checked_binop clamp 2", IntType::$rc, AnyConst::Val($rhs.clone())));
                    }
                    let v = (l as $ty).$op(r as $ty);
                    Ok($ctor(v.ok_or(IntegerOverflowed("checked_binop op 1", IntType::$lc, AnyConst::Val($self.clone())))?))
                },
                (&$rc(r), &$lc(l)) => {
                    if l < cmp::min(0, std::$ty::MIN as $lt) || l > cmp::max(std::$ty::MAX as $lt, std::$lt::MAX) {
                        return Err(IntegerOverflowed("checked_binop clamp 3", IntType::$lc, AnyConst::Val($self.clone())));
                    }
                    if r < cmp::min(0, std::$ty::MIN as $rt) || r > cmp::max(std::$ty::MAX as $rt, std::$rt::MAX) {
                        return Err(IntegerOverflowed("checked_binop clamp 4", IntType::$rc, AnyConst::Val($rhs.clone())));
                    }
                    let v = (l as $ty).$op(r as $ty);
                    Ok($ctor(v.ok_or(IntegerOverflowed("checked_binop op 2", IntType::$lc, AnyConst::Val($self.clone())))?))
                },
            )*
            _ => Err(ConstTypeMismatch("integer", AnyConst::Val($self.clone())))                
        }
    }
}

macro_rules! const_val_shift {
    ( $id:ident, $op:tt ) => {
        fn $id(&self, rhs: &ConstVal) -> Result<ConstVal, Error> {
            use self::ConstVal::*;

            let rhs = rhs.as_usize()?;
            if rhs >= 64 {
                return Err(InvalidSize(self.clone()));
            }

            const_val_shift_body!(self, rhs, [I16; I32; I64; U8; U16; U32; U64])
        }
    }
}

macro_rules! const_val_shift_body {
    ( $self:ident, $rhs:ident, [ $( $ctor:ident );* ] ) => {
        match *$self {
            $(
                $ctor(i) => Ok($ctor(i << $rhs)),
            )*
            _ => Err(ConstTypeMismatch("integer", AnyConst::Val($self.clone())))
        }
    }
}

macro_rules! const_val_float_binop {
    ( $name:ident, $op:tt ) => {
        fn $name(&self, rhs: &ConstVal) -> Result<ConstVal, Error> {
            use self::ConstVal::*;
            match (self, rhs) {
                (&F32(l), &F32(r)) => Ok(F32(l $op r)),
                (&F32(l), &F64(r)) => Ok(F64(l as f64 $op r)),
                (&F64(l), &F32(r)) => Ok(F64(l $op r as f64)),
                (&F64(l), &F64(r)) => Ok(F64(l $op r)),
                // again, arbitrarily report an error on the left...
                _ => Err(ConstTypeMismatch("floating point", AnyConst::Val(self.clone())))
            }
        }
    }
}

impl ConstVal {
    pub fn as_usize(&self) -> Result<usize, Error> {
        use self::ConstVal::*;
        macro_rules! check {
            ($i:ident, $ty:ident) => (
                if $i < 0 as $ty || $i > std::usize::MAX as $ty {
                    Err(InvalidSize(self.clone()))
                } else {
                    Ok($i as usize)
                }
            )
        }
        match *self {
            I16(i) => check!(i, i16),
            I32(i) => check!(i, i32),
            I64(i) => check!(i, i64),
            U8(i) => check!(i, u8),
            U16(i) => check!(i, u16),
            U32(i) => check!(i, u32),
            U64(i) => check!(i, u64),
            _ => Err(InvalidSize(self.clone()))
        }
    }

    const_val_bitwise_binop!(or, |);
    const_val_bitwise_binop!(and, &);
    const_val_bitwise_binop!(xor, ^);

    const_val_shift!(shl, <<);
    const_val_shift!(shr, >>);

    const_val_checked_binop!(iadd, checked_add);
    const_val_checked_binop!(isub, checked_sub);
    const_val_checked_binop!(imul, checked_mul);
    const_val_checked_binop!(idiv, checked_div);
    const_val_checked_binop!(rem, checked_rem);

    fn negate(&self) -> Result<ConstVal, Error> {
        use self::ConstVal::*;
        match *self {
            I16(i) => {
                let (i, overflowed) = i.overflowing_neg();
                if overflowed {
                    Err(IntegerOverflowed("negate", IntType::I16, AnyConst::Val(self.clone())))
                } else {
                    Ok(I16(i))
                }
            },
            I32(i) => {
                let (i, overflowed) = i.overflowing_neg();
                if overflowed {
                    Err(IntegerOverflowed("negate", IntType::I32, AnyConst::Val(self.clone())))
                } else {
                    Ok(I32(i))
                }
            },
            I64(i) => {
                let (i, overflowed) = i.overflowing_neg();
                if overflowed {
                    Err(IntegerOverflowed("negate", IntType::I64, AnyConst::Val(self.clone())))
                } else {
                    Ok(I64(i))
                }
            },
            U8(i) => Ok(I16(-(i as i16))),
            U16(i) => {
                if i > std::i16::MAX as u16 {
                    Ok(I32(-(i as i32)))
                } else {
                    Ok(I16(-(i as i16)))
                }
            },
            U32(i) => {
                if i > std::i32::MAX as u32 {
                    Ok(I64(-(i as i64)))
                } else {
                    Ok(I32(-(i as i32)))
                }
            },
            U64(i) => {
                if i > std::i64::MAX as u64 {
                    Err(IntegerOverflowed("negate", IntType::I64, AnyConst::Val(self.clone())))
                } else {
                    Ok(I64(-(i as i64)))
                }
            },
            F32(f) => Ok(F32(-f)),
            F64(f) => Ok(F64(-f)),
            _ => Err(ConstTypeMismatch("numeric", AnyConst::Val(self.clone())))
        }
    }

    fn complement(&self) -> Result<ConstVal, Error> {
        use self::ConstVal::*;
        match *self {
            I16(i) => Ok(I16(!i)),
            I32(i) => Ok(I32(!i)),
            I64(i) => Ok(I64(!i)),
            U8(i) => Ok(U8(!i)),
            U16(i) => Ok(U16(!i)),
            U32(i) => Ok(U32(!i)),
            U64(i) => Ok(U64(!i)),
            _ => Err(ConstTypeMismatch("integer", AnyConst::Val(self.clone())))
        }
    }

    const_val_float_binop!(fadd, +);
    const_val_float_binop!(fsub, -);
    const_val_float_binop!(fmul, *);
    const_val_float_binop!(fdiv, /);
}

macro_rules! const_expr_eval_int {
    ( $name:ident, $ctor:ident, $ty:ident ) => {
        fn $name(&self, ity: IntType) -> Result<ConstVal, Error> {
            use super::ast::ConstExpr::*;
            match *self {
                Int(i) if i > std::$ty::MAX as u64 => Err(IntegerOverflowed("literal", ity, AnyConst::Expr(self.clone()))),
                Int(i) => Ok(ConstVal::$ctor(i as $ty)),
                Or(ref l, ref r) => l.$name(ity)?.or(&r.$name(ity)?),
                And(ref l, ref r) => l.$name(ity)?.and(&r.$name(ity)?),
                Xor(ref l, ref r) => l.$name(ity)?.xor(&r.$name(ity)?),
                Shr(ref l, ref r) => l.$name(ity)?.shr(&r.$name(ity)?),
                Shl(ref l, ref r) => l.$name(ity)?.shl(&r.$name(ity)?),
                Add(ref l, ref r) => l.$name(ity)?.iadd(&r.$name(ity)?),
                Sub(ref l, ref r) => l.$name(ity)?.isub(&r.$name(ity)?),
                Mult(ref l, ref r) => l.$name(ity)?.imul(&r.$name(ity)?),
                Div(ref l, ref r) => l.$name(ity)?.idiv(&r.$name(ity)?),
                Mod(ref l, ref r) => l.$name(ity)?.rem(&r.$name(ity)?),
                Negate(ref x) => x.$name(ity)?.negate(),
                Complement(ref x) => x.$name(ity)?.complement(),
                _ => Err(ConstTypeMismatch("integer", AnyConst::Expr(self.clone())))
            }
        }
    }
}

macro_rules! const_expr_eval_float {
    ( $name:ident, $ctor:ident, $ty:ident ) => {
        fn $name(&self, fpty: FloatType) -> Result<ConstVal, Error> {
            use super::ast::ConstExpr::*;
            match *self {
                Float(f) => Ok(ConstVal::$ctor(f as $ty)),
                Add(ref l, ref r) => l.$name(fpty)?.fadd(&r.$name(fpty)?),
                Sub(ref l, ref r) => l.$name(fpty)?.fsub(&r.$name(fpty)?),
                Mult(ref l, ref r) => l.$name(fpty)?.fmul(&r.$name(fpty)?),
                Div(ref l, ref r) => l.$name(fpty)?.fdiv(&r.$name(fpty)?),
                Negate(ref x) => x.$name(fpty)?.negate(),
                _ => Err(ConstTypeMismatch("floating point", AnyConst::Expr(self.clone())))
            }
        }
    }
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
                _ => Err(ConstTypeMismatch("$ctor", AnyConst::Expr(self.clone())))
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
                _ => Err(ConstTypeMismatch("$ctor", AnyConst::Expr(self.clone())))
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
            Enum(ref qn) => self.eval_enum(env, qn),
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
                        if $i < cmp::min(0, std::$ty::MIN as $dty) || $i > cmp::max(std::$ty::MAX as $dty, std::$dty::MAX) {
                            Err(IntegerOverflowed("eval_int", ity, AnyConst::Expr(self.clone())))
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
                    _ => Err(ConstTypeMismatch("integer", AnyConst::Expr(self.clone())))
                }
            }
        };
        // this departs a little from the spec, which seems to want to
        // treat everything as unsigned until it gets negated, meaning
        // it's almost impossible to build up negative values
        body!([I16, i16, eval_i16;
               I32, i32, eval_i32;
               I64, i64, eval_i64;
               U8, u8, eval_u8;
               U16, u16, eval_u16;
               U32, u32, eval_u32;
               U64, u64, eval_u64])
    }

    fn eval_float(&self, fpty: FloatType) -> Result<ConstVal, Error> {
        match fpty {
            FloatType::F32 => {
                match self.eval_float32(fpty)? {
                    v@ConstVal::F32(_) => Ok(v),
                    ConstVal::F64(f) => {
                        if f < std::f32::MIN as f64 || f > std::f32::MAX as f64 {
                            Err(FloatOverflowed(fpty, AnyConst::Expr(self.clone())))
                        } else {
                            Ok(ConstVal::F32(f as f32))
                        }
                    },
                    _ => Err(ConstTypeMismatch("floating point", AnyConst::Expr(self.clone())))
                }
            },
            FloatType::F64 => {
                match self.eval_float64(fpty)? {
                    ConstVal::F32(f) => Ok(ConstVal::F64(f as f64)),
                    v@ConstVal::F64(_) => Ok(v),
                    _ => Err(ConstTypeMismatch("floating point", AnyConst::Expr(self.clone())))
                }
            },
            FloatType::F128 => Err(Unsupported("fixed point")),
        }
    }

    const_expr_eval_int!(eval_i16, I16, i16);
    const_expr_eval_int!(eval_i32, I32, i32);
    const_expr_eval_int!(eval_i64, I64, i64);
    const_expr_eval_int!(eval_u8, U8, u8);
    const_expr_eval_int!(eval_u16, U16, u16);
    const_expr_eval_int!(eval_u32, U32, u32);
    const_expr_eval_int!(eval_u64, U64, u64);
    const_expr_eval_float!(eval_float32, F32, f32);
    const_expr_eval_float!(eval_float64, F64, f64);

    fn eval_bool(&self) -> Result<ConstVal, Error> {
        match *self {
            ConstExpr::Bool(b) => Ok(ConstVal::Bool(b)),
            _ => Err(ConstTypeMismatch("boolean", AnyConst::Expr(self.clone())))
        }
    }

    fn eval_enum(&self, env: &GlobalEnv, qn: &QName) -> Result<ConstVal, Error> {
        let e = match *env.get(qn).ok_or(UnboundQName(qn.clone()))? {
            Entry::Enum(ref w) => w.upgrade().expect("enum in environment exists"),
            Entry::TypeDef(Type::Enum(ref qn)) =>
                match *env.get(qn).ok_or(UnboundQName(qn.clone()))? {
                    Entry::Enum(ref w) => w.upgrade().expect("enum in environment exists"),
                    _ => return Err(ConstTypeMismatch("enumeration", AnyConst::Expr(self.clone())))
                },
            _ => return Err(ConstTypeMismatch("enumeration", AnyConst::Expr(self.clone())))
        };
        if let Some(_) = e.enumerators.iter().find(|&e_qn| e_qn == qn) {
            Ok(ConstVal::Enum(qn.clone()))
        } else {
            Err(ConstTypeMismatch("enumeration", AnyConst::Expr(self.clone())))
        }
    }
}

impl PosIntConst {
    pub fn eval(&self) -> Result<ConstVal, Error> {
        self.as_const_expr().eval_int(IntType::U64)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;

    #[test]
    fn literals() {
        let e = &HashMap::new();
        assert_eq!(ConstExpr::Int(0).eval(e, &ConstType::Int(IntType::I16)),
                   Ok(ConstVal::I16(0)));
        assert!(ConstExpr::Int(0).eval(e, &ConstType::Char).is_err());
        assert_eq!(ConstExpr::Float(42.0).eval(e, &ConstType::Float(FloatType::F64)),
                   Ok(ConstVal::F64(42.0)));
        assert!(ConstExpr::Float(42.0).eval(e, &ConstType::Bool).is_err());
        assert_eq!(ConstExpr::String("hello".to_owned()).eval(e, &ConstType::String(Bound::Unbounded)),
                   Ok(ConstVal::String("hello".to_owned())));
        assert!(ConstExpr::String("hello".to_owned()).eval(e, &ConstType::Bool).is_err());
    }

    #[test]
    fn u8s() {
        let e = &HashMap::new();
        assert_eq!(ConstExpr::Add(Box::new(ConstExpr::Int(40)), Box::new(ConstExpr::Int(2)))
                   .eval(e, &ConstType::Int(IntType::U8)),
                   Ok(ConstVal::U8(42)));
        assert!(ConstExpr::Add(Box::new(ConstExpr::Int(400)), Box::new(ConstExpr::Int(2)))
                .eval(e, &ConstType::Int(IntType::U8)).is_err());
    }

    #[test]
    fn i32s() {
        use self::ConstExpr::*;
        let e = &HashMap::new();
        assert_eq!(Add(Box::new(Negate(Box::new(Int(40)))), Box::new(Int(2)))
                   .eval(e, &ConstType::Int(IntType::I32)),
                   Ok(ConstVal::I32(-38)));
        assert!(Add(Box::new(Int(std::i32::MAX as u64)), Box::new(Int(1)))
                .eval(e, &ConstType::Int(IntType::I32)).is_err());
        assert!(Sub(Box::new(Int(std::i32::MAX as u64 + 1)), Box::new(Int(1)))
                .eval(e, &ConstType::Int(IntType::I32)).is_err());
    }

    #[test]
    fn i64s() {
        use self::ConstExpr::*;
        let e = &HashMap::new();
        assert_eq!(Add(Box::new(Negate(Box::new(Int(40)))), Box::new(Int(2)))
                   .eval(e, &ConstType::Int(IntType::I64)),
                   Ok(ConstVal::I64(-38)));
        assert!(Add(Box::new(Int(std::i64::MAX as u64)), Box::new(Int(1)))
                .eval(e, &ConstType::Int(IntType::I64)).is_err());
        assert!(Sub(Box::new(Int(std::i64::MAX as u64 + 1)), Box::new(Int(1)))
                .eval(e, &ConstType::Int(IntType::I64)).is_err());
    }

    #[test]
    fn f32s() {
        use self::ConstExpr::*;
        let e = &HashMap::new();
        assert_eq!(Add(Box::new(Negate(Box::new(Float(40.0)))), Box::new(Float(2.0)))
                   .eval(e, &ConstType::Float(FloatType::F32)),
                   Ok(ConstVal::F32(-38.0)));
    }

}
