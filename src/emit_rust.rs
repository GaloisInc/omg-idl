use quote::{Ident, Tokens};
use std::io::Write;
use rustfmt;

use core::ast::*;
use core::error::Error;
use core::error::Error::*;
use core::eval::ConstVal;

#[derive(Copy, Clone, Debug)]
struct State<'a> {
    env: &'a GlobalEnv,
    module_depth: usize,
}

impl<'a> State<'a> {
    fn new(env: &'a GlobalEnv) -> State<'a> {
        State { env: env, module_depth: 0 }
    }
}

pub fn emit_rust<W: Write>(spec: &Specification, env: &GlobalEnv, w: &mut W) -> Result<(), Error> {
    let st = State::new(env);
    let toks = spec.emit_rust(st)?;
    let rust_src = toks.into_string();
    let mut cfg = rustfmt::config::Config::default();
    cfg.set().write_mode(rustfmt::config::WriteMode::Display);
    rustfmt::format_input(rustfmt::Input::Text(rust_src),
                          &cfg,
                          Some(w)).unwrap();
    // println!("{}", rust_src);
    Ok(())
}

impl Specification {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let defs = self.0.iter().map(|d| d.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        Ok(quote!{#(#defs)*})
    }
}

impl Definition {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        use core::ast::Definition::*;
        match *self {
            Module(ref m) => m.emit_rust(st),
            Const(ref c) => c.emit_rust(st),
            Struct(ref s) => s.emit_rust(st),
            Union(ref u) => u.emit_rust(st),
            Enum(ref e) => e.emit_rust(),
            Native(_) => Ok(quote!{}),
            TypeDef(ref id, ref ty) => {
                let id = id.emit_rust();
                let ty = ty.emit_rust(st)?;
                Ok(quote!{pub type #id = #ty;})
            },
            Except(ref e) => e.emit_rust(st),
            Interface(ref i) => i.emit_rust(st),
        }
    }
}

impl Module {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let st_nested = State { module_depth: st.module_depth + 1, .. st };
        let defs = self.defs.iter().map(|d| d.emit_rust(st_nested)).collect::<Result<Vec<Tokens>, Error>>()?;
        Ok(quote! {
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            pub mod #id {
                #(#defs)*
            }
        })
    }
}

impl Const {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let ty = self.ty.emit_rust(st)?;
        let val = self.expr.eval(st.env, &self.ty)?.emit_rust(st)?;
        Ok(quote!{pub const #id: #ty = #val;})
    }
}

impl ConstVal {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        use core::eval::ConstVal::*;
        match *self {
            I16(i) => Ok(quote!{#i}),
            I32(i) => Ok(quote!{#i}),
            I64(i) => Ok(quote!{#i}),
            U8(i) => Ok(quote!{#i}),
            U16(i) => Ok(quote!{#i}),
            U32(i) => Ok(quote!{#i}),
            U64(i) => Ok(quote!{#i}),
            F32(f) => Ok(quote!{#f}),
            F64(f) => Ok(quote!{#f}),
            Char(c) => Ok(quote!{#c}),
            WChar(c) => Ok(quote!{#c}),
            Bool(b) => Ok(quote!{#b}),
            String(ref s) => Ok(quote!{#s}),
            WString(ref s) => Ok(quote!{#s}),
            Enum(ref qn) => qn.emit_rust(st),
        }
    }
}

impl Struct {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let members = self.members.iter().map(|m| m.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        Ok(quote!{pub struct #id { #(#members),* }})
    }
}

impl Member {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let ty = self.ty.emit_rust(st)?;
        Ok(quote!{pub #id: #ty})
    }
}

impl Union {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        unimplemented!()
    }
}

impl Enum {
    fn emit_rust(&self) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let enumerators: Vec<Tokens> = self.enumerators.iter().map(|e| e.last().expect("enumerators are named").emit_rust()).collect();
        Ok(quote!{pub enum #id { #(#enumerators),* }})
    }
}

impl Except {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        unimplemented!()
    }
}

impl Interface {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let parents = self.parents.iter().map(|p| p.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        let ops = self.ops.iter().map(|op| op.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        let attrs = self.attrs.iter().map(|attr| attr.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        if parents.is_empty() {
            Ok(quote!{pub trait #id { #(#ops)* #(#attrs)* }})
        } else {
            Ok(quote!{pub trait #id: #(#parents)+* { #(#ops)* #(#attrs)* }})
        }
    }
}

impl Op {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        if !self.raises.is_empty() {
            unimplemented!()
        }
        let id = self.id.emit_rust();
        let ret_ty = self.ret.emit_rust(st)?;
        let ret = if self.ret == Type::Void { quote!{} } else { quote!{-> #ret_ty} };
        let params = self.params.iter().map(|p| p.emit_rust(st)).collect::<Result<Vec<Tokens>, Error>>()?;
        if params.is_empty() {
            Ok(quote!{fn #id(&mut self) #ret;})
        } else {
            Ok(quote!{fn #id(&mut self, #(#params),*) #ret;})
        }
    }
}

impl Param {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        let id = self.id.emit_rust();
        let dir = self.dir.emit_rust();
        let ty = self.ty.emit_rust(st)?;
        Ok(quote!{#id: #dir #ty})
    }
}

impl ParamDir {
    fn emit_rust(&self) -> Tokens {
        match *self {
            ParamDir::In => quote!{&},
            ParamDir::Out => quote!{&mut},
            ParamDir::InOut => quote!{&mut},
        }
    }
}

impl Attr {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        // unimplemented!()
        Ok(quote!{})
    }
}

impl IntType {
    fn emit_rust(&self) -> Tokens {
        use core::ast::IntType::*;
        match *self {
            I16 => quote!{i16},
            I32 => quote!{i32},
            I64 => quote!{i64},
            U8 => quote!{u8},
            U16 => quote!{u16},
            U32 => quote!{u32},
            U64 => quote!{u64},
        }
    }
}

impl FloatType {
    fn emit_rust(&self) -> Result<Tokens, Error> {
        use core::ast::FloatType::*;
        match *self {
            F32 => Ok(quote!{f32}),
            F64 => Ok(quote!{f64}),
            F128 => Err(Unsupported("long double")),
        }
    }
}

impl ConstType {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        use core::ast::ConstType::*;
        match *self {
            Int(ref ity) => Ok(ity.emit_rust()),
            Float(ref fpty) => fpty.emit_rust(),
            Char => Ok(quote!{char}),
            WChar => Ok(quote!{char}),
            Bool => Ok(quote!{bool}),
            String(_) => Ok(quote!{&'static str}),
            WString(_) => Ok(quote!{&'static str}),
            Enum(ref qn) => {
                if !st.env.contains_key(qn) {
                    return Err(UnboundQName(qn.clone()));
                }
                qn.emit_rust(st)
            },
            Fixed => Err(Unsupported("fixed point")),
        }
    }
}

impl Type {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        use core::ast::Type::*;
        // when emitting a compound type, make sure it's in the
        // environment and is the correct kind
        macro_rules! compound {
            ($id:ident, $qn:expr) => ({
                use core::ast::Entry;
                match st.env.get($qn) {
                    Some(&Entry::$id(_)) => (),
                    Some(&Entry::TypeDef($id(_))) => (),
                    Some(_) => panic!("type mismatch in global environment"),
                    None => return Err(UnboundQName($qn.clone())),
                }
                $qn.emit_rust(st)
            })
        }
        match *self {
            Int(ref ity) => Ok(ity.emit_rust()),
            Float(ref fpty) => fpty.emit_rust(),
            Char => Ok(quote!{char}),
            WChar => Ok(quote!{char}),
            Bool => Ok(quote!{bool}),
            String(_) => Ok(quote!{String}),
            WString(_) => Ok(quote!{String}),
            Sequence(ref ty, _) => {
                let ty = ty.emit_rust(st)?;
                Ok(quote!{Vec<#ty>})
            },
            Array(ref ty, ref sizes) => {
                let ty = ty.emit_rust(st)?;
                // evaluate and reverse sizes, so the innermost dimension is first
                let sizes: Result<Vec<usize>, Error> = sizes.iter().map(|e| e.eval()?.as_usize()).rev().collect();
                let mut out = ty;
                for size in sizes? {
                    out = quote!{[#out; #size]}
                }
                Ok(out)
            },
            Struct(ref qn) => compound!(Struct, qn),
            Union(ref qn) => compound!(Union, qn),
            Enum(ref qn) => compound!(Enum, qn),
            Except(ref qn) => compound!(Except, qn),
            Interface(ref qn) => {
                use core::ast::Entry;
                match st.env.get(qn) {
                    Some(&Entry::Interface(_)) => (),
                    Some(&Entry::TypeDef(Interface(_))) => (),
                    Some(_) => panic!("type mismatch in global environment"),
                    None => return Err(UnboundQName(qn.clone())),
                }
                let qn = qn.emit_rust(st)?;
                // interfaces are trait objects, so we have to box them
                Ok(quote!{Box<#qn>})
            },
            Void => Ok(quote!{()}),
            Fixed => Err(Unsupported("fixed point")),
        }
    }
}

impl Id {
    fn emit_rust(&self) -> Tokens {
        let id = Ident::from(self.as_str());
        quote!{#id}
    }
}

impl QName {
    fn emit_rust(&self, st: State) -> Result<Tokens, Error> {
        if !st.env.contains_key(self) {
            return Err(UnboundQName(self.clone()));
        }
        let mut out = String::new();
        for _ in 0..st.module_depth {
            out.push_str("super::");
        }
        let mut qn = self.clone();
        qn.reverse();
        out.push_str(qn.pop().expect("emitted QName must have at least one Id").as_str());
        while let Some(id) = qn.pop() {
            out.push_str("::");
            out.push_str(id.as_str());
        }
        let id = Ident::from(out);
        Ok(quote!{#id})
    }
}
