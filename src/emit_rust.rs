use std;
use std::collections::{HashMap};
use std::io::Write;
use quote::{Ident, Tokens};

use ast::*;

#[derive(Debug, PartialEq)]
pub enum Error {
    ConstTypeError(&'static str, const_expr),
    FloatOverflow(floating_pt_type, const_expr),
    IntOverflow(integer_type, const_expr),
    InvalidConstType(scoped_name),
    NameClash(identifier),
    UnboundId(identifier),
    Unimplemented,
    Unknown(String),
    Unsupported(&'static str),
}

pub fn emit_rust<W>(spec: &specification, w: &mut W) -> Result<(), Error> where W: Write {
    let mut st = State::new();
    let toks = spec.emit_rust(&mut st)?;
    write!(w, "{}\n", toks.as_str()).unwrap();
    Ok(())
}

type Path = Vec<identifier>;

#[derive(Debug)]
struct State {
    scopes: HashMap<Path, Scope>,
    current_scope: Path,
    module_depth: usize,
}

impl State {
    fn new() -> State {
        let mut scopes = HashMap::new();
        scopes.insert(vec![], Scope::new());
        State {
            scopes: scopes,
            current_scope: vec![],
            module_depth: 0,
        }
    }
}

#[derive(Debug)]
struct Scope {
    env: HashMap<identifier, ScopeEntry>,
    inherited: Option<Path>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            env: HashMap::new(),
            inherited: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeEntry {
    Child(Path),
    Const,
    Relative(Path),
    EnumType,
    FwdDecl,
    Other,
}

impl State {
    fn get_current_scope(&self) -> &Scope {
        self.scopes.get(&self.current_scope).expect("current scope exists")
    }

    fn get_current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.get_mut(&self.current_scope).expect("current scope exists")
    }

    fn get_root_scope(&self) -> &Scope {
        self.scopes.get(&vec![]).expect("root scope exists")
    }
    
    /// resolve an identifier all the way up the parent scopes and
    /// inherited scopes (if any) of the current scope
    fn resolve(&self, id: &identifier) -> Option<ScopeEntry> {
        self.resolve_in(id, &self.current_scope)
    }
    
    /// resolve an identifier all the way up the parent scopes and
    /// inherited scopes (if any) of the given scope
    fn resolve_in(&self, id: &identifier, path: &Path) -> Option<ScopeEntry> {
        fn resolve_inherited<'a>(st: &'a State, id: &identifier, sc0: &'a Scope) -> Option<&'a ScopeEntry> {
            let mut here = sc0;
            while let Some(ref inh_path) = here.inherited {
                let inh = st.scopes.get(inh_path).expect("inherited scope exists");
                let x = inh.env.get(id);
                if x.is_some() {
                    return x;
                } else {
                    here = inh;
                }
            }
            None
        }

        // A loop that runs on all of the parent scope paths reachable from the target scope:
        // 0. Let here = the target scope path
        // 1. If the identifier is in scope here, return its entry
        // 2. If the identifier is in scope in the current inheritance chain, return its entry
        // 3. Set here = here.pop(); if pop succeeded, goto 1 else return None

        // 0
        let mut path = path.clone();
        loop {
            let here = self.scopes.get(&path).expect("current and parent scopes exist");
            // 1
            if let Some(e) = here.env.get(id) {
                return Some(e.clone());
            }
            // 2
            if let Some(e) = resolve_inherited(self, id, here) {
                return Some(e.clone());
            }
            // 3
            if let None = path.pop() {
                return None;
            }
        }
    }

    /// Perform resolution as in `resolve`, but add the returned entry
    /// to the current scope as required by many use sites in IDL. If
    /// the resolved entry is a child scope, convert it to a relative
    /// scope.
    fn resolve_and_introduce(&mut self, id: &identifier) -> Option<ScopeEntry> {
        let x: Option<ScopeEntry> = self.resolve(id);
        x.map(move |e| {
            let cur = self.get_current_scope_mut();
            let e = match e {
                ScopeEntry::Child(path) => ScopeEntry::Relative(path),
                _ => e
            };
            cur.env.insert(id.clone(), e.clone());
            e
        })
    }

    /// resolves and normalizes a scoped name to file scope
    fn resolve_scoped_name(&mut self, sn: &scoped_name) -> Result<(scoped_name, ScopeEntry), Error> {
        match *sn {
            scoped_name::Qualified(ref path) => {
                let pfx = path.first().expect("qualified name starts with an id");
                // only introduce for the root
                let mut entry: ScopeEntry = self.resolve_and_introduce(pfx).ok_or(Error::UnboundId(pfx.clone()))?;
                let mut out_path = vec![];
                for layer in path.get(1..).expect("qualified name non-empty") {
                    // can't set entry directly within the match
                    let next;
                    match entry {
                        ScopeEntry::Child(ref child) => {
                            let s = self.scopes.get(child).expect("child scope must exist");
                            out_path = child.clone();
                            out_path.push(layer.clone());
                            if let Some(e) = s.env.get(layer) {
                                next = e.clone();
                            } else {
                                return Err(Error::UnboundId(layer.clone()))
                            }
                        },
                        _ => return Err(Error::NameClash(layer.clone()))
                    }
                    entry = next;
                }
                // final entry should be our id
                Ok((scoped_name::FileScope(out_path), entry))
            },
            scoped_name::FileScope(ref path) => {
                let pfx = path.first().expect("file-scoped path has at least one id");
                let mut entry: ScopeEntry = self.get_root_scope().env.get(pfx).cloned().ok_or(Error::UnboundId(pfx.clone()))?;
                if let Some(layers) = path.get(1..) {
                    for layer in layers {
                        // can't set entry directly within the match
                        let next;
                        match entry {
                            ScopeEntry::Child(ref child) => {
                                let s = self.scopes.get(child).expect("child scope must exist");
                                if let Some(e) = s.env.get(layer) {
                                    next = e.clone();
                                } else {
                                    return Err(Error::UnboundId(layer.clone()))
                                }
                            },
                            _ => return Err(Error::NameClash(layer.clone()))
                        }
                        entry = next;
                    }
                    Ok((sn.clone(), entry))
                } else {
                    // path just contained one component; no more work to do
                    Ok((sn.clone(), entry))
                }
            }
        }
    }

    /// bring an identifier into scope as long as it doesn't clash,
    /// and return the Rust representation
    fn bind(&mut self, id: &identifier, entry: ScopeEntry) -> Result<Tokens, Error> {
        let mut cur = self.get_current_scope_mut();
        // catch any clashes
        match cur.env.get(id) {
            None => (),
            Some(&ScopeEntry::FwdDecl) => (),
            _ => return Err(Error::NameClash(id.clone()))
        }
        cur.env.insert(id.clone(), entry);
        let id = Ident::new(id.0.clone());
        Ok(quote!{#id})
    }

    /// emit a file-scoped name. It's an error to emit an unresolved qualified name
    fn emit_scoped_name(&self, sn: &scoped_name) -> Tokens {
        match sn {
            &scoped_name::FileScope(ref path) => {
                let mut id = String::new();
                for _ in 1..self.module_depth {
                    id.push_str("super::");
                }
                for layer in path {
                    id.push_str(layer.0.as_str());
                    id.push_str("::");
                }
                let id = Ident::new(id);
                quote!{#id}
            }
            _ => panic!("tried to emit unresolved qualified name")
        }
    }
}

impl specification {
    fn emit_rust(&self, st: &mut State) -> Result<Tokens, Error> {
        let mut defs = vec![];
        for def in self.0.iter() {
            defs.push(def.emit_rust(st)?);
        }
        Ok(quote! {#(#defs)*})
    }
}

impl definition {
    fn emit_rust(&self, st: &mut State) -> Result<Tokens, Error> {
        match *self {
            definition::module_dcl(ref d) => d.emit_rust(st),
            definition::const_dcl(ref d) => d.emit_rust(st),
            // definition::type_dcl(d) => d.emit_rust(st),
            // definition::except_dcl(d) => d.emit_rust(st),
            // definition::interface_dcl(d) => d.emit_rust(st),
            _ => Err(Error::Unimplemented)
        }
    }
}

impl module_dcl {
    fn emit_rust(&self, st: &mut State) -> Result<Tokens, Error> {
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        let id = st.bind(&self.identifier, ScopeEntry::Child(new_path.clone()))?;
        let mut new_scope = Scope::new();
        new_scope.env.insert(self.identifier.clone(), ScopeEntry::Relative(st.current_scope.clone()));
        st.scopes.insert(new_path.clone(), new_scope);
        st.current_scope = new_path;
        st.module_depth += 1;
        let mut defs = vec![];
        for def in self.defs.iter() {
            defs.push(def.emit_rust(st)?);
        }
        Ok(quote! {mod #id { #(#defs)* }})
    }
}

impl const_dcl {
    fn emit_rust(&self, st: &mut State) -> Result<Tokens, Error> {
        let id = st.bind(&self.identifier, ScopeEntry::Const)?;
        let ty = self.ty.emit_rust(st)?;
        let expr = {
            use ast::const_type::*;
            use ast::integer_type::unsigned_short_int;
            match self.ty {
                integer_type(ref ity) => self.expr.eval_int(ity, false),
                floating_pt_type(ref fpty) => self.expr.eval_fp(fpty),
                char_type => self.expr.eval_char(),
                wide_char_type => self.expr.eval_wide_char(),
                boolean_type => self.expr.eval_boolean(),
                octet_type => self.expr.eval_int(&unsigned_short_int, true),
                string_type(ref b) => self.expr.eval_string(&b),
                wide_string_type(ref b) => self.expr.eval_wide_string(&b),
                scoped_name(_) => self.expr.eval_scoped_name(st),
                fixed_pt_const_type => return Err(Error::Unsupported("fixed")),
            }
        }?;
        Ok(quote! {const #id: #ty = #expr;})
    }
}

impl const_type {
    fn emit_rust(&self, st: &mut State) -> Result<Tokens, Error> {
        use ast::const_type::*;
        use ast::integer_type::*;
        use ast::floating_pt_type::*;

        match *self {
            integer_type(signed_short_int) => Ok(quote! {i16}),
            integer_type(signed_long_int) => Ok(quote! {i32}),
            integer_type(signed_longlong_int) => Ok(quote! {i64}),
            integer_type(unsigned_short_int) => Ok(quote! {u16}),
            integer_type(unsigned_long_int) => Ok(quote! {u32}),
            integer_type(unsigned_longlong_int) => Ok(quote! {u64}),
            floating_pt_type(float) => Ok(quote! {f32}),
            floating_pt_type(double) => Ok(quote! {f64}),
            floating_pt_type(long_double) => Err(Error::Unsupported("long double")),
            char_type => Ok(quote! {char}),
            wide_char_type => Ok(quote! {char}),
            boolean_type => Ok(quote! {bool}),
            octet_type => Ok(quote! {u8}),
            string_type(_) => Ok(quote! {&'static str}),
            wide_string_type(_) => Ok(quote! {&'static str}),
            scoped_name(ref sn) => {
                let (sn, entry) = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::EnumType => Ok(st.emit_scoped_name(&sn)),
                    _ => Err(Error::InvalidConstType(sn.clone()))
                }
            },
            fixed_pt_const_type => Err(Error::Unsupported("fixed")),
        }
    }
}

impl const_expr {
    // TODO: This implementation doesn't exactly match the spec,
    // because it treats all subexpressions as the widest type of the
    // correct sign before casting down at the end. It wouldn't be
    // hard to make it match, so let's come back to it later.
    //
    // TODO: Gross boolean to wedge in octets is gross; for now it
    // only affects the output when integer_type is unsigned_short_int
    fn eval_int(&self, ity: &integer_type, octet: bool) -> Result<Tokens, Error> {
        use ast::const_expr::*;
        use ast::integer_type::*;
        use ast::literal::integer_literal;
        fn eval_signed(e: &const_expr, ity: &integer_type) -> Result<i64, Error> {
            macro_rules! binop {
                ($l:ident $op:tt $r:ident) =>
                    (Ok(eval_signed(&*$l, ity)? $op eval_signed(&*$r, ity)?));
            }
            macro_rules! binop_checked {
                ($l:ident $op:ident $r:ident) =>
                    (Ok(eval_signed(&*$l, ity)?.$op(eval_signed(&*$r, ity)?).ok_or(Error::IntOverflow(*ity, e.clone()))?));
            }
            match *e {
                literal(integer_literal(i)) => Ok(i as i64),
                Or(ref l, ref r) => binop!(l | r),
                Xor(ref l, ref r) => binop!(l ^ r),
                And(ref l, ref r) => binop!(l & r),
                Shr(ref l, ref r) => {
                    let lv = eval_signed(&*l, ity)?;
                    let rv = eval_signed(&*r, ity)?;
                    Ok(lv.checked_shr(rv as u32).ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Shl(ref l, ref r) => {
                    let lv = eval_signed(&*l, ity)?;
                    let rv = eval_signed(&*r, ity)?;
                    Ok(lv.checked_shl(rv as u32).ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Add(ref l, ref r) => binop_checked!(l checked_add r),
                Sub(ref l, ref r) => binop_checked!(l checked_sub r),
                Mult(ref l, ref r) => binop_checked!(l checked_mul r),
                Div(ref l, ref r) => binop_checked!(l checked_div r),
                Mod(ref l, ref r) => binop_checked!(l checked_rem r),
                Negate(ref x) => {
                    let v = eval_signed(&*x, ity)?;
                    Ok(v.checked_neg().ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Complement(ref x) => {
                    let v = eval_signed(&*x, ity)?;
                    Ok(-(v + 1))
                },
                _ => Err(Error::ConstTypeError("signed integer", e.clone()))
            }
        }
        fn eval_unsigned(e: &const_expr, ity: &integer_type) -> Result<u64, Error> {
            macro_rules! binop {
                ($l:ident $op:tt $r:ident) =>
                    (Ok(eval_unsigned(&*$l, ity)? $op eval_unsigned(&*$r, ity)?));
            }
            macro_rules! binop_checked {
                ($l:ident $op:ident $r:ident) =>
                    (Ok(eval_unsigned(&*$l, ity)?.$op(eval_unsigned(&*$r, ity)?).ok_or(Error::IntOverflow(*ity, e.clone()))?));
            }
            match *e {
                literal(integer_literal(i)) => Ok(i),
                Or(ref l, ref r) => binop!(l | r),
                Xor(ref l, ref r) => binop!(l ^ r),
                And(ref l, ref r) => binop!(l & r),
                Shr(ref l, ref r) => {
                    let lv = eval_unsigned(&*l, ity)?;
                    let rv = eval_unsigned(&*r, ity)?;
                    Ok(lv.checked_shr(rv as u32).ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Shl(ref l, ref r) => {
                    let lv = eval_unsigned(&*l, ity)?;
                    let rv = eval_unsigned(&*r, ity)?;
                    Ok(lv.checked_shl(rv as u32).ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Add(ref l, ref r) => binop_checked!(l checked_add r),
                Sub(ref l, ref r) => binop_checked!(l checked_sub r),
                Mult(ref l, ref r) => binop_checked!(l checked_mul r),
                Div(ref l, ref r) => binop_checked!(l checked_div r),
                Mod(ref l, ref r) => binop_checked!(l checked_rem r),
                Negate(ref x) => {
                    let v = eval_unsigned(&*x, ity)?;
                    Ok(v.checked_neg().ok_or(Error::IntOverflow(*ity, e.clone()))?)
                },
                Complement(ref x) => {
                    let v = eval_unsigned(&*x, ity)?;
                    Ok(u64::max_value() - v)
                },
                _ => Err(Error::ConstTypeError("unsigned integer", e.clone()))
            }
        }
        let v = match *ity {
            signed_short_int => {
                let i = eval_signed(self, ity)?;
                if i < i16::min_value() as i64 || i > i16::max_value() as i64 {
                    Err(Error::IntOverflow(*ity, self.clone()))
                } else {
                    let i = i as i16;
                    Ok(quote! {#i})
                }
            },
            signed_long_int => {
                let i = eval_signed(self, ity)?;
                if i < i32::min_value() as i64 || i > i32::max_value() as i64 {
                    Err(Error::IntOverflow(*ity, self.clone()))
                } else {
                    let i = i as i32;
                    Ok(quote! {#i})
                }
            },
            signed_longlong_int => {
                let i = eval_signed(self, ity)?;
                Ok(quote! {#i})
            },
            unsigned_short_int => {
                let i = eval_unsigned(self, ity)?;
                if octet {
                    if i > u8::max_value() as u64 {
                        Err(Error::IntOverflow(*ity, self.clone()))
                    } else {
                        let i = i as u8;
                        Ok(quote! {#i})
                    }
                } else {
                    if i > u16::max_value() as u64 {
                        Err(Error::IntOverflow(*ity, self.clone()))
                    } else {
                        let i = i as u16;
                        Ok(quote! {#i})
                    }
                }
            },
            unsigned_long_int => {
                let i = eval_unsigned(self, ity)?;
                if i > u32::max_value() as u64 {
                    Err(Error::IntOverflow(*ity, self.clone()))
                } else {
                    let i = i as u32;
                    Ok(quote! {#i})
                }
            },
            unsigned_longlong_int => {
                let i = eval_unsigned(self, ity)?;
                Ok(quote! {#i})
            },
        }?;
        Ok(v)
    }

    // TODO: This implementation doesn't exactly match the spec,
    // because it treats all subexpressions as the widest type before
    // casting down at the end. It wouldn't be hard to make it match,
    // so let's come back to it later.
    fn eval_fp(&self, fpty: &floating_pt_type) -> Result<Tokens, Error> {
        use ast::const_expr::*;
        use ast::floating_pt_type::*;
        use ast::literal::floating_pt_literal;
        fn eval(e: &const_expr, fpty: &floating_pt_type) -> Result<f64, Error> {
            macro_rules! binop {
                ($l:ident $op:tt $r:ident) =>
                    (Ok(eval(&*$l, fpty)? $op eval(&*$r, fpty)?));
            }
            match *e {
                literal(floating_pt_literal(x)) => Ok(x),
                Add(ref l, ref r) => binop!(l + r),
                Sub(ref l, ref r) => binop!(l - r),
                Mult(ref l, ref r) => binop!(l * r),
                Div(ref l, ref r) => binop!(l / r),
                Negate(ref x) => Ok(-eval(&*x, fpty)?),
                _ => Err(Error::ConstTypeError("floating point", e.clone()))
            }
        }
        let v = match *fpty {
            float => {
                let x = eval(self, fpty)?;
                if x < std::f32::MIN as f64 || x > std::f32::MAX as f64 {
                    Err(Error::FloatOverflow(*fpty, self.clone()))
                } else {
                    let x = x as f32;
                    Ok(quote! {#x})
                }
            },
            double => {
                let x = eval(self, fpty)?;
                Ok(quote! {#x})
            },
            long_double => Err(Error::Unsupported("long double")),
        }?;
        Ok(v)
    }

    fn eval_char(&self) -> Result<Tokens, Error> {
        use ast::const_expr::literal;
        use ast::literal::character_literal;
        match *self {
            literal(character_literal(c)) => Ok(quote! {#c}),
            _ => Err(Error::ConstTypeError("char", self.clone()))
        }
    }

    fn eval_wide_char(&self) -> Result<Tokens, Error> {
        use ast::const_expr::literal;
        use ast::literal::wide_character_literal;
        match *self {
            literal(wide_character_literal(c)) => Ok(quote! {#c}),
            _ => Err(Error::ConstTypeError("wchar", self.clone()))
        }
    }

    fn eval_boolean(&self) -> Result<Tokens, Error> {
        use ast::const_expr::literal;
        use ast::literal::boolean_literal;
        match *self {
            literal(boolean_literal(b)) => Ok(quote! {#b}),
            _ => Err(Error::ConstTypeError("boolean", self.clone()))
        }
    }

    fn eval_string(&self, b: &Bound) -> Result<Tokens, Error> {
        use ast::const_expr::literal;
        use ast::literal::string_literal;
        match *self {
            literal(string_literal(ref s)) => {
                match *b {
                    Bound::Unbounded => (),
                    Bound::Bounded(_) => return Err(Error::Unsupported("bounded string")),
                }
                Ok(quote! {#s})
            },
            _ => Err(Error::ConstTypeError("string", self.clone()))
        }
    }

    fn eval_wide_string(&self, b: &Bound) -> Result<Tokens, Error> {
        use ast::const_expr::literal;
        use ast::literal::wide_string_literal;
        match *self {
            literal(wide_string_literal(ref s)) => {
                match *b {
                    Bound::Unbounded => (),
                    Bound::Bounded(_) => return Err(Error::Unsupported("bounded wide string")),
                }
                Ok(quote! {#s})
            },
            _ => Err(Error::ConstTypeError("wstring", self.clone()))
        }
    }

    fn eval_scoped_name(&self, st: &mut State) -> Result<Tokens, Error> {
        use ast::const_expr::scoped_name;
        match *self {
            scoped_name(ref sn) => {
                let (sn, entry) = st.resolve_scoped_name(&sn)?;
                match entry {
                    ScopeEntry::EnumType => (),
                    _ => return Err(Error::ConstTypeError("enum", self.clone()))
                }
                let sn = st.emit_scoped_name(&sn);
                Ok(quote! {#sn})
            },
            _ => Err(Error::ConstTypeError("enum", self.clone()))
        }
    }   
}
