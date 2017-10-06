use std::collections::{HashMap};
use std::rc::Rc;

use core::ast as core;
use parser::ast::*;

#[derive(Debug, PartialEq)]
pub enum Error {
    AmbiguousReference(identifier),
    ConstTypeError(&'static str, const_expr),
    FloatOverflow(floating_pt_type, const_expr),
    IntOverflow(integer_type, const_expr),
    InvalidType(&'static str, scoped_name),
    NameClash(identifier),
    UnboundId(identifier),
    Unknown(String),
    Unsupported(&'static str),
}

pub fn to_core(spec: &specification) -> Result<(core::Specification, core::GlobalEnv), Error> {
    let mut st = State::new();
    let spec = spec.to_core(&mut st)?;
    Ok((spec, st.core_env))
}

type Path = Vec<identifier>;

#[derive(Debug)]
struct State {
    scopes: HashMap<Path, Scope>,
    current_scope: Path,
    core_env: core::GlobalEnv,
}

impl State {
    fn new() -> State {
        let mut scopes = HashMap::new();
        scopes.insert(vec![], Scope::new());
        State {
            scopes: scopes,
            current_scope: vec![],
            core_env: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct Scope {
    env: HashMap<identifier, ScopeEntry>,
    inherited: Vec<Path>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            env: HashMap::new(),
            inherited: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeEntry {
    Child(Path, ScopeType),
    Relative(Path, ScopeType),
    Const,
    EnumType(Path),
    EnumVariant(Path),
    FwdDecl(core::Type),
    Native,
    TypeDef(core::Type),
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeType {
    Module,
    Struct,
    Union,
    Except,
    Interface,
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
    fn resolve(&self, id: &identifier) -> Result<Option<ScopeEntry>, Error> {
        self.resolve_in(id, &self.current_scope)
    }

    /// resolve an identifier all the way up the parent scopes and
    /// inherited scopes (if any) of the given scope
    fn resolve_in(&self, id: &identifier, path: &Path) -> Result<Option<ScopeEntry>, Error> {
        fn resolve_inherited(st: &State, id: &identifier, sc0: &Scope) -> Result<Option<ScopeEntry>, Error> {
            if let Some(e) = sc0.env.get(id).cloned() {
                Ok(Some(e))
            } else {
                let mut out = None;
                for inh in sc0.inherited.iter() {
                    let sc = st.scopes.get(inh).expect("inherited scope exists");
                    if let Some(e) = resolve_inherited(st, id, sc)? {
                        if out.is_none() {
                            out = Some(e);
                        } else {
                            return Err(Error::AmbiguousReference(id.clone()));
                        }
                    }
                }
                Ok(out)
            }
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
                return Ok(Some(e.clone()));
            }
            // 2
            if let Some(e) = resolve_inherited(self, id, here)? {
                return Ok(Some(e));
            }
            // 3
            if let None = path.pop() {
                return Ok(None);
            }
        }
    }

    /// Perform resolution as in `resolve`, but add the returned entry
    /// to the current scope as required by many use sites in IDL. If
    /// the resolved entry is a child scope, convert it to a relative
    /// scope.
    fn resolve_and_introduce(&mut self, id: &identifier) -> Result<Option<ScopeEntry>, Error> {
        let x: Option<ScopeEntry> = self.resolve(id)?;
        Ok(x.map(move |e| {
            let cur = self.get_current_scope_mut();
            let e = match e {
                ScopeEntry::Child(path, st) => ScopeEntry::Relative(path, st),
                _ => e
            };
            cur.env.insert(id.clone(), e.clone());
            e
        }))
    }

    /// resolves and normalizes a scoped name
    fn resolve_scoped_name(&mut self, sn: &scoped_name) -> Result<ScopeEntry, Error> {
        match *sn {
            scoped_name::Qualified(ref path) => {
                let pfx = path.first().expect("qualified name starts with an id");
                // only introduce for the root
                let mut entry: ScopeEntry = self.resolve_and_introduce(pfx)?.ok_or(Error::UnboundId(pfx.clone()))?;
                for layer in path.get(1..).expect("qualified name non-empty") {
                    // can't set entry directly within the match
                    let next;
                    match entry {
                        ScopeEntry::Child(ref child, _) => {
                            let s = self.scopes.get(child).expect("child scope must exist");
                            if let Some(e) = s.env.get(layer) {
                                next = e.clone();
                            } else {
                                return Err(Error::UnboundId(layer.clone()))
                            }
                        },
                        ScopeEntry::Relative(ref child, _) => {
                            let s = self.scopes.get(child).expect("relative scope must exist");
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
                Ok(entry)
            },
            scoped_name::FileScope(ref path) => {
                let pfx = path.first().expect("file-scoped path has at least one id");
                let mut entry: ScopeEntry = self.get_root_scope().env.get(pfx).cloned().ok_or(Error::UnboundId(pfx.clone()))?;
                if let Some(layers) = path.get(1..) {
                    for layer in layers {
                        // can't set entry directly within the match
                        let next;
                        match entry {
                            ScopeEntry::Child(ref child, _) => {
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
                    Ok(entry)
                } else {
                    // path just contained one component; no more work to do
                    Ok(entry)
                }
            }
        }
    }

    /// bring an identifier into scope as long as it doesn't clash,
    /// and return the Rust representation
    fn bind(&mut self, id: &identifier, entry: ScopeEntry) -> Result<(), Error> {
        let mut cur = self.get_current_scope_mut();
        // catch any clashes
        match cur.env.get(id) {
            None => (),
            Some(&ScopeEntry::FwdDecl(_)) => (),
            _ => return Err(Error::NameClash(id.clone()))
        }
        cur.env.insert(id.clone(), entry);
        Ok(())
    }
}

impl specification {
    fn to_core(&self, st: &mut State) -> Result<core::Specification, Error> {
        let mut defs = vec![];
        for def in self.0.iter() {
            defs.append(&mut def.to_core(st)?);
        }
        Ok(defs)
    }
}

impl definition {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Definition>, Error> {
        use parser::ast::definition::*;
        match *self {
            module_dcl(ref d) => Ok(vec![core::Definition::Module(d.to_core(st)?)]),
            const_dcl(ref d) => Ok(vec![core::Definition::Const(d.to_core(st)?)]),
            type_dcl(ref d) => d.to_core(st),
            except_dcl(ref d) => Ok(vec![core::Definition::Except(d.to_core(st)?)]),
            interface_dcl(ref d) => Ok(d.to_core(st)?.map(|d| core::Definition::Interface(d)).into_iter().collect()),
        }
    }
}

impl interface_dcl {
    fn to_core(&self, st: &mut State) -> Result<Option<Rc<core::Interface>>, Error> {
        use parser::ast::interface_dcl::*;
        match *self {
            interface_def(ref id) => {
                Ok(Some(id.to_core(st)?))
            },
            interface_forward_dcl(ref id) => {
                let mut path = st.current_scope.clone();
                path.push(id.clone());
                let ty = core::Type::Interface(path);
                st.bind(id, ScopeEntry::FwdDecl(ty.clone()))?;
                Ok(None)
            }
        }
    }
}

impl interface_def {
    fn to_core(&self, st: &mut State) -> Result<Rc<core::Interface>, Error> {
        // save the original scope
        let original_scope = st.current_scope.clone();

        // construct the new scope, though don't change the current
        // scope yet so we bind the child entry in the right place
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::Child(new_path.clone(), ScopeType::Interface))?;

        // create the inner scope
        let new_scope = Scope::new();
        st.scopes.insert(new_path.clone(), new_scope);

        // adjust the current scope before processing members
        st.current_scope = new_path.clone();

        let mut parents = vec![];
        if let Some(ref inh) = self.interface_inheritance_spec {
            for sn in inh.iter() {
                let entry = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::Child(path, ScopeType::Interface) => parents.push(path.clone()),
                    ScopeEntry::Relative(path, ScopeType::Interface) => parents.push(path.clone()),
                    ScopeEntry::TypeDef(core::Type::Interface(path)) => parents.push(path.clone()),
                    _ => ()
                }
            }
        }

        let mut ops = vec![];
        let mut attrs = vec![];
        for export in self.interface_body.iter() {
            use parser::ast::export::*;
            match *export {
                op_dcl(ref od) => ops.push(od.to_core(st)?),
                attr_dcl(ref ad) => attrs.append(&mut ad.to_core(st)?),
            }
        }

        // restore the original scope
        st.current_scope = original_scope;

        let rc = Rc::new(core::Interface {
            id: self.identifier.clone(),
            parents: parents,
            ops: ops,
            attrs: attrs,
        });
        st.core_env.insert(new_path.clone(), core::Entry::Interface(Rc::downgrade(&rc)));
        Ok(rc)
    }
}

impl op_dcl {
    fn to_core(&self, st: &mut State) -> Result<core::Op, Error> {
        let original_scope = st.current_scope.clone();

        // operations introduce a private, unnamed scope
        let private_scope = identifier("<op_dcl>".to_owned());
        let mut new_path = st.current_scope.clone();
        new_path.push(private_scope.clone());

        // create the inner scope
        let new_scope = Scope::new();
        st.scopes.insert(new_path.clone(), new_scope);

        // adjust the current scope before processing parameters
        st.current_scope = new_path.clone();

        let ret = {
            use parser::ast::op_type_spec::*;
            match self.op_type_spec {
                type_spec(ref ts) => ts.to_core(st)?,
                void => core::Type::Void,
            }
        };

        let mut params = vec![];
        for pd in self.parameter_dcls.iter() {
            params.push(pd.to_core(st)?);
        }

        let raises;
        if let Some(ref res) = self.raises_expr {
            raises = res.to_core(st)?;
        } else {
            raises = vec![];
        }

        // restore the original scope
        st.current_scope = original_scope;

        let op = core::Op {
            id: self.identifier.clone(),
            ret: ret,
            params: params,
            raises: raises,
        };
        Ok(op)
    }
}

impl raises_expr {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::QName>, Error> {
        let mut raises = vec![];
        for sn in self.0.iter() {
            let entry = st.resolve_scoped_name(sn)?;
            match entry {
                ScopeEntry::Child(path, ScopeType::Except) => raises.push(path.clone()),
                ScopeEntry::Relative(path, ScopeType::Except) => raises.push(path.clone()),
                ScopeEntry::TypeDef(core::Type::Except(path)) => raises.push(path.clone()),
                _ => return Err(Error::InvalidType("exception", sn.clone()))
            }
        }
        Ok(raises)
    }
}

impl param_dcl {
    fn to_core(&self, st: &mut State) -> Result<(core::Id, core::ParamDir, core::Type), Error> {
        use parser::ast::param_attribute::*;

        let pd = match self.param_attribute {
            in_ => core::ParamDir::In,
            out => core::ParamDir::Out,
            inout => core::ParamDir::InOut,
        };

        let ty = self.type_spec.to_core(st)?;

        Ok((self.simple_declarator.clone(), pd, ty))
    }
}

impl attr_dcl {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Attr>, Error> {
        use parser::ast::attr_dcl::*;
        match *self {
            readonly_attr_spec(ref s) => s.to_core(st),
            attr_spec(ref s) => s.to_core(st),
        }
    }
}

impl readonly_attr_spec {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Attr>, Error> {
        use parser::ast::readonly_attr_declarator::*;

        let ty = self.type_spec.to_core(st)?;

        let mut attrs = vec![];
        match self.readonly_attr_declarator {
            simple_declarator_raises(ref sd, ref raises) => {
                let attr = core::Attr {
                    id: sd.clone(),
                    read_only: true,
                    ty: ty,
                    get_raises: raises.to_core(st)?,
                    set_raises: vec![],
                };
                attrs.push(attr);
            },
            simple_declarators(ref sds) => {
                for sd in sds {
                    let attr = core::Attr {
                        id: sd.clone(),
                        read_only: true,
                        ty: ty.clone(),
                        get_raises: vec![],
                        set_raises: vec![],
                    };
                    attrs.push(attr);
                }
            },
        }
        Ok(attrs)
    }
}

impl attr_spec {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Attr>, Error> {
        use parser::ast::attr_declarator::*;

        let ty = self.type_spec.to_core(st)?;

        let mut attrs = vec![];
        match self.attr_declarator {
            simple_declarator_raises(ref sd, ref are) => {
                let (grs, srs) = are.to_core(st)?;
                let attr = core::Attr {
                    id: sd.clone(),
                    read_only: false,
                    ty: ty,
                    get_raises: grs,
                    set_raises: srs,
                };
                attrs.push(attr);
            },
            simple_declarators(ref sds) => {
                for sd in sds {
                    let attr = core::Attr {
                        id: sd.clone(),
                        read_only: false,
                        ty: ty.clone(),
                        get_raises: vec![],
                        set_raises: vec![],
                    };
                    attrs.push(attr);
                }
            },
        }
        Ok(attrs)
    }
}

impl attr_raises_expr {
    fn to_core(&self, st: &mut State) -> Result<(Vec<core::QName>, Vec<core::QName>), Error> {
        use parser::ast::attr_raises_expr::*;

        match *self {
            get_excep_expr(ref ges, Some(ref ses)) => Ok((ges.to_core(st)?, ses.to_core(st)?)),
            get_excep_expr(ref ges, None) => Ok((ges.to_core(st)?, vec![])),
            set_excep_expr(ref ses) => Ok((vec![], ses.to_core(st)?)),
        }
    }
}

impl except_dcl {
    fn to_core(&self, st: &mut State) -> Result<Rc<core::Except>, Error> {
        // save the original scope
        let original_scope = st.current_scope.clone();

        // construct the new scope, though don't change the current
        // scope yet so we bind the child entry in the right place
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::Child(new_path.clone(), ScopeType::Except))?;

        // create the inner scope
        let new_scope = Scope::new();
        st.scopes.insert(new_path.clone(), new_scope);

        // adjust the current scope before processing members
        st.current_scope = new_path.clone();

        let mut members = vec![];
        for member in self.members.iter() {
            let ty = member.type_spec.to_core(st)?;
            for decl in member.declarators.iter() {
                let (id, arr_sizes) = decl.to_core(st)?;
                match arr_sizes {
                    None => members.push((id, ty.clone())),
                    Some(sizes) => members.push((id, core::Type::Array(Box::new(ty.clone()), sizes))),
                }
            }
        }

        // restore the original scope and module depth
        st.current_scope = original_scope;

        let rc = Rc::new(core::Except { id: self.identifier.clone(), members: members });
        st.core_env.insert(new_path.clone(), core::Entry::Except(Rc::downgrade(&rc)));
        Ok(rc)
    }
}

impl type_dcl {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Definition>, Error> {
        use parser::ast::type_dcl::*;
        match *self {
            constr_type_dcl(ref ctd) => {
                let (def, _ty) = ctd.to_core(st)?;
                Ok(def.into_iter().collect())
            },
            native_dcl(ref id) => {
                st.bind(id, ScopeEntry::Native)?;
                let mut qname = st.current_scope.clone();
                qname.push(id.clone());
                st.core_env.insert(qname, core::Entry::Native(id.clone()));
                Ok(vec![core::Definition::Native(id.clone())])
            },
            typedef_dcl(ref td) => td.to_core(st),
        }
    }
}

impl typedef_dcl {
    fn to_core(&self, st: &mut State) -> Result<Vec<core::Definition>, Error> {
        use parser::ast::typedef_dcl::*;
        // shared code for the cases that have a straightforward type spec
        macro_rules! type_spec {
            ($ts:ident, $decls:ident) => ({
                let ty = $ts.to_core(st)?;
                let mut defs = vec![];
                inner!(ty, $decls, defs);
                Ok(defs)
            })
        }
        // inner loop that creates TypeDef definitions for each of the
        // declarators and adds them to the GlobalEnv
        macro_rules! inner {
            ($ty:ident, $decls:ident, $out:ident) => ({
                for decl in $decls.iter() {
                    let (id, arr_sizes) = decl.to_core(st)?;
                    let ty = match arr_sizes {
                        None => $ty.clone(),
                        Some(sizes) => core::Type::Array(Box::new($ty.clone()), sizes),
                    };
                    // push the definition and add it to the GlobalEnv
                    $out.push(core::Definition::TypeDef(id.clone(), ty.clone()));
                    let mut path = st.current_scope.clone();
                    path.push(id.clone());

                    // bind in the current scope
                    st.bind(&id, ScopeEntry::TypeDef(ty.clone()))?;
                    st.core_env.insert(path, core::Entry::TypeDef(ty));
                }
            })
        }
        match *self {
            simple_type_spec(ref sts, ref decls) => type_spec!(sts, decls),
            template_type_spec(ref tts, ref decls) => type_spec!(tts, decls),
            constr_type_dcl(ref cd, ref decls) => {
                // defining and redefining a type all at once, so we
                // have to get the type from the constr_type_dcl
                let (def, ty) = cd.to_core(st)?;
                // start with the def (if any) from the constructed type
                let mut defs: Vec<core::Definition> = def.into_iter().collect();
                inner!(ty, decls, defs);
                Ok(defs)
            },
        }
    }
}

impl constr_type_dcl {
    fn to_core(&self, st: &mut State) -> Result<(Option<core::Definition>, core::Type), Error> {
        use parser::ast::constr_type_dcl::*;
        match *self {
            struct_dcl(ref sd) => {
                let (s, ty) = sd.to_core(st)?;
                Ok((s.map(|rc| core::Definition::Struct(rc)), ty))
            },
            union_dcl(ref ud) => {
                let (u, ty) = ud.to_core(st)?;
                Ok((u.map(|rc| core::Definition::Union(rc)), ty))
            },
            enum_dcl(ref ed) => {
                let (e, ty) = ed.to_core(st)?;
                Ok((Some(core::Definition::Enum(e)), ty))
            },
        }
    }
}

impl union_dcl {
    fn to_core(&self, st: &mut State) -> Result<(Option<Rc<core::Union>>, core::Type), Error> {
        use parser::ast::union_dcl::*;
        match *self {
            union_def(ref sd) => {
                let (u, ty) = sd.to_core(st)?;
                Ok((Some(u), ty))
            }
            union_forward_dcl(ref id) => {
                let mut path = st.current_scope.clone();
                path.push(id.clone());
                let ty = core::Type::Union(path);
                st.bind(id, ScopeEntry::FwdDecl(ty.clone()))?;
                Ok((None, ty))
            }
        }
    }
}

impl union_def {
    fn to_core(&self, st: &mut State) -> Result<(Rc<core::Union>, core::Type), Error> {
        // save the original scope
        let original_scope = st.current_scope.clone();

        // construct the new scope, though don't change the current
        // scope yet so we bind the child entry in the right place
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::Child(new_path.clone(), ScopeType::Union))?;

        // adjust the current scope before processing members
        st.current_scope = new_path.clone();


        // restore the original scope and module depth
        st.current_scope = original_scope;

        let mut variants = vec![];
        for case in self.switch_body.iter() {
            let (id, arr_sizes) = case.declarator.to_core(st)?;
            let ty = case.type_spec.to_core(st)?;
            let ty = match arr_sizes {
                None => ty,
                Some(sizes) => core::Type::Array(Box::new(ty), sizes),
            };
            let mut labels = vec![];
            for label in case.case_labels.iter() {
                use parser::ast::case_label::*;
                match *label {
                    const_expr(ref expr) => labels.push(core::UnionLabel::Const(expr.to_core(st)?)),
                    default => labels.push(core::UnionLabel::Default),
                }
            }
            variants.push((id, ty, labels));
        }

        let u = core::Union {
            id: self.identifier.clone(),
            disc_ty: self.switch_type_spec.to_core(st)?,
            variants: variants,
        };
        let rc = Rc::new(u);
        st.core_env.insert(new_path.clone(), core::Entry::Union(Rc::downgrade(&rc)));
        Ok((rc, core::Type::Union(new_path)))
    }
}

impl switch_type_spec {
    fn to_core(&self, st: &mut State) -> Result<core::Type, Error> {
        use parser::ast::switch_type_spec::*;
        match *self {
            integer_type(ref ity) => Ok(ity.to_core()),
            char_type => Ok(core::Type::Char),
            boolean_type => Ok(core::Type::Bool),
            scoped_name(ref sn) => {
                let entry = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::EnumType(path) => Ok(core::Type::Enum(path.clone())),
                    ScopeEntry::TypeDef(e@core::Type::Enum(_)) => Ok(e.clone()),
                    _ => Err(Error::InvalidType("switch_type_spec", sn.clone()))
                }
            },
        }
    }
}

impl struct_dcl {
    fn to_core(&self, st: &mut State) -> Result<(Option<Rc<core::Struct>>, core::Type), Error> {
        use parser::ast::struct_dcl::*;
        match *self {
            struct_def(ref sd) => {
                let (s, ty) = sd.to_core(st)?;
                Ok((Some(s), ty))
            },
            struct_forward_dcl(ref id) => {
                let mut path = st.current_scope.clone();
                path.push(id.clone());
                let ty = core::Type::Struct(path);
                st.bind(id, ScopeEntry::FwdDecl(ty.clone()))?;
                Ok((None, ty))
            }
        }
    }
}

impl struct_def {
    fn to_core(&self, st: &mut State) -> Result<(Rc<core::Struct>, core::Type), Error> {
        // save the original scope
        let original_scope = st.current_scope.clone();

        // construct the new scope, though don't change the current
        // scope yet so we bind the child entry in the right place
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::Child(new_path.clone(), ScopeType::Struct))?;

        // create the inner scope
        let new_scope = Scope::new();
        st.scopes.insert(new_path.clone(), new_scope);

        // adjust the current scope before processing members
        st.current_scope = new_path.clone();

        let mut members = vec![];
        for member in self.members.iter() {
            let ty = member.type_spec.to_core(st)?;
            for decl in member.declarators.iter() {
                let (id, arr_sizes) = decl.to_core(st)?;
                match arr_sizes {
                    None => members.push((id, ty.clone())),
                    Some(sizes) => members.push((id, core::Type::Array(Box::new(ty.clone()), sizes))),
                }
            }
        }

        // restore the original scope and module depth
        st.current_scope = original_scope;

        let rc = Rc::new(core::Struct { id: self.identifier.clone(), members: members });
        st.core_env.insert(new_path.clone(), core::Entry::Struct(Rc::downgrade(&rc)));
        Ok((rc, core::Type::Struct(new_path)))
    }
}


impl type_spec {
    fn to_core(&self, st: &mut State) -> Result<core::Type, Error> {
        use parser::ast::type_spec::*;
        match *self {
            simple_type_spec(ref sts) => sts.to_core(st),
            template_type_spec(ref tts) => tts.to_core(st),
        }
    }
}

impl simple_type_spec {
    fn to_core(&self, st: &mut State) -> Result<core::Type, Error> {
        use parser::ast::simple_type_spec::*;
        match *self {
            base_type_spec(ref bts) => {
                use parser::ast::base_type_spec::*;
                match *bts {
                    integer_type(ity) => Ok(ity.to_core()),
                    floating_pt_type(fpty) => Ok(fpty.to_core()),
                    char_type => Ok(core::Type::Char),
                    wide_char_type => Ok(core::Type::WChar),
                    boolean_type => Ok(core::Type::Bool),
                    octet_type => Ok(core::Type::U8),
                }
            },
            scoped_name(ref sn) => {
                let entry = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::Child(path, ScopeType::Except) => Ok(core::Type::Except(path.clone())),
                    ScopeEntry::Child(path, ScopeType::Interface) => Ok(core::Type::Interface(path.clone())),
                    ScopeEntry::Child(path, ScopeType::Struct) => Ok(core::Type::Struct(path.clone())),
                    ScopeEntry::Child(path, ScopeType::Union) => Ok(core::Type::Union(path.clone())),
                    ScopeEntry::Relative(path, ScopeType::Except) => Ok(core::Type::Except(path.clone())),
                    ScopeEntry::Relative(path, ScopeType::Interface) => Ok(core::Type::Interface(path.clone())),
                    ScopeEntry::Relative(path, ScopeType::Struct) => Ok(core::Type::Struct(path.clone())),
                    ScopeEntry::Relative(path, ScopeType::Union) => Ok(core::Type::Union(path.clone())),
                    ScopeEntry::EnumType(path) => Ok(core::Type::Enum(path.clone())),
                    ScopeEntry::TypeDef(ty) => Ok(ty.clone()),
                    ScopeEntry::FwdDecl(ty) => Ok(ty.clone()),
                    _ => panic!("{:?} = {:#?}", sn, entry) //Err(Error::InvalidType("simple_type_spec", sn.clone()))
                }
            }
        }
    }
}

impl template_type_spec {
    fn to_core(&self, st: &mut State) -> Result<core::Type, Error> {
        use parser::ast::template_type_spec::*;
        match *self {
            sequence_type(ref ty, ref b) => Ok(core::Type::Sequence(Box::new(ty.to_core(st)?), b.to_core(st)?)),
            string_type(ref b) => Ok(core::Type::String(b.to_core(st)?)),
            wide_string_type(ref b) => Ok(core::Type::WString(b.to_core(st)?)),
            fixed_pt_type => Ok(core::Type::Fixed),
        }
    }
}

impl declarator {
    // returns the underlying identifier, along with a size if the declarator uses array syntax
    fn to_core(&self, st: &mut State) -> Result<(identifier, Option<Vec<core::ConstExpr>>), Error> {
        use parser::ast::declarator::*;
        match *self {
            simple_declarator(ref id) => Ok((id.clone(), None)),
            array_declarator(ref ad) => {
                let mut sizes = vec![];
                for size in ad.sizes.iter() {
                    sizes.push(size.to_core(st)?);
                }
                Ok((ad.identifier.clone(), Some(sizes)))
            },
        }
    }
}

impl enum_dcl {
    fn to_core(&self, st: &mut State) -> Result<(Rc<core::Enum>, core::Type), Error> {
        let mut path = st.current_scope.clone();
        path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::EnumType(path.clone()))?;
        let mut enumerators = vec![];
        for e in self.enumerators.iter() {
            let mut enum_path = path.clone();
            enum_path.push(e.clone());
            st.bind(e, ScopeEntry::EnumVariant(enum_path.clone()))?;
            enumerators.push(enum_path);
        }
        let rc = Rc::new(core::Enum { id: self.identifier.clone(), enumerators: enumerators.clone() });
        st.core_env.insert(path.clone(), core::Entry::Enum(Rc::downgrade(&rc)));
        for e in enumerators {
            st.core_env.insert(e, core::Entry::EnumVariant(Rc::downgrade(&rc)));
        }
        Ok((rc, core::Type::Enum(path)))
    }
}

impl module_dcl {
    fn to_core(&self, st: &mut State) -> Result<Rc<core::Module>, Error> {
        // save the original scope
        let original_scope = st.current_scope.clone();

        // construct the new scope, though don't change the current
        // scope yet so we bind the child entry in the right place
        let mut new_path = st.current_scope.clone();
        new_path.push(self.identifier.clone());
        st.bind(&self.identifier, ScopeEntry::Child(new_path.clone(), ScopeType::Module))?;

        // create the inner scope with the module name in scope
        let mut new_scope = Scope::new();
        new_scope.env.insert(self.identifier.clone(), ScopeEntry::Relative(st.current_scope.clone(), ScopeType::Module));
        st.scopes.insert(new_path.clone(), new_scope);

        // adjust the current scope and module depth before processing definitions
        st.current_scope = new_path.clone();
        let mut defs = vec![];
        for def in self.defs.iter() {
            defs.append(&mut def.to_core(st)?);
        }

        // restore the original scope and module depth
        st.current_scope = original_scope;

        let rc = Rc::new(core::Module { id: self.identifier.clone(), defs: defs });
        st.core_env.insert(new_path, core::Entry::Module(Rc::downgrade(&rc)));
        Ok(rc)
    }
}

impl const_dcl {
    fn to_core(&self, st: &mut State) -> Result<Rc<core::Const>, Error> {
        st.bind(&self.identifier, ScopeEntry::Const)?;
        let ty = self.ty.to_core(st)?;
        let expr = self.expr.to_core(st)?;

        let rc = Rc::new(core::Const { id: self.identifier.clone(), ty: ty, expr: expr });
        let mut path = st.current_scope.clone();
        path.push(self.identifier.clone());
        st.core_env.insert(path, core::Entry::Const(Rc::downgrade(&rc)));
        Ok(rc)
    }
}

impl integer_type {
    fn to_core(&self) -> core::Type {
        use parser::ast::integer_type::*;
        match *self {
            signed_short_int => core::Type::I16,
            signed_long_int => core::Type::I32,
            signed_longlong_int => core::Type::I64,
            unsigned_short_int => core::Type::U16,
            unsigned_long_int => core::Type::U32,
            unsigned_longlong_int => core::Type::U64,
        }
    }
}

impl floating_pt_type {
    fn to_core(&self) -> core::Type {
        use parser::ast::floating_pt_type::*;
        match *self {
            float => core::Type::F32,
            double => core::Type::F64,
            long_double => core::Type::F128,
        }
    }
}

impl const_type {
    fn to_core(&self, st: &mut State) -> Result<core::Type, Error> {
        use parser::ast::const_type::*;
        match *self {
            integer_type(ity) => Ok(ity.to_core()),
            floating_pt_type(fpty) => Ok(fpty.to_core()),
            char_type => Ok(core::Type::Char),
            wide_char_type => Ok(core::Type::WChar),
            boolean_type => Ok(core::Type::Bool),
            octet_type => Ok(core::Type::U8),
            string_type(ref b) => Ok(core::Type::String(b.to_core(st)?)),
            wide_string_type(ref b) => Ok(core::Type::WString(b.to_core(st)?)),
            scoped_name(ref sn) => {
                let entry = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::EnumType(path) => Ok(core::Type::Enum(path.clone())),
                    ScopeEntry::TypeDef(ty) => Ok(ty.clone()),
                    _ => Err(Error::InvalidType("const_type", sn.clone()))
                }
            },
            fixed_pt_const_type => Ok(core::Type::Fixed),
        }
    }
}

impl Bound {
    fn to_core(&self, st: &mut State) -> Result<core::Bound, Error> {
        use parser::ast::Bound::*;
        match *self {
            Bounded(ref expr) => Ok(core::Bound::Bounded(expr.to_core(st)?)),
            Unbounded => Ok(core::Bound::Unbounded),
        }
    }
}

impl const_expr {
    fn to_core(&self, st: &mut State) -> Result<core::ConstExpr, Error> {
        use parser::ast::const_expr::*;
        use parser::ast::literal::*;
        use core::ast::ConstExpr;
        macro_rules! binop {
            ($ctor:tt, $l:ident, $r:ident) =>
                (Ok(ConstExpr::$ctor(Box::new($l.to_core(st)?), Box::new($r.to_core(st)?))))
        }
        match *self {
            scoped_name(ref sn) => {
                let entry = st.resolve_scoped_name(sn)?;
                match entry {
                    ScopeEntry::EnumVariant(path) => Ok(ConstExpr::Enum(path.clone())),
                    _ => Err(Error::InvalidType("const_expr", sn.clone()))
                }
            },
            literal(integer_literal(i)) => Ok(ConstExpr::Int(i)),
            literal(floating_pt_literal(f)) => Ok(ConstExpr::Float(f)),
            literal(fixed_pt_literal(_)) => Ok(ConstExpr::Fixed),
            literal(character_literal(c)) => Ok(ConstExpr::Char(c)),
            literal(wide_character_literal(c)) => Ok(ConstExpr::WChar(c)),
            literal(boolean_literal(b)) => Ok(ConstExpr::Bool(b)),
            literal(string_literal(ref s)) => Ok(ConstExpr::String(s.clone())),
            literal(wide_string_literal(ref s)) => Ok(ConstExpr::WString(s.clone())),
            Or(ref l, ref r) => binop!(Or, l, r),
            Xor(ref l, ref r) => binop!(Xor, l, r),
            And(ref l, ref r) => binop!(And, l, r),
            Shr(ref l, ref r) => binop!(Shr, l, r),
            Shl(ref l, ref r) => binop!(Shl, l, r),
            Add(ref l, ref r) => binop!(Add, l, r),
            Sub(ref l, ref r) => binop!(Sub, l, r),
            Mult(ref l, ref r) => binop!(Mult, l, r),
            Div(ref l, ref r) => binop!(Div, l, r),
            Mod(ref l, ref r) => binop!(Mod, l, r),
            Negate(ref x) => Ok(ConstExpr::Negate(Box::new(x.to_core(st)?))),
            Complement(ref x) => Ok(ConstExpr::Complement(Box::new(x.to_core(st)?))),
        }
    }
}

/*
impl const_expr {
    // TODO: This implementation doesn't exactly match the spec,
    // because it treats all subexpressions as the widest type of the
    // correct sign before casting down at the end. It wouldn't be
    // hard to make it match, so let's come back to it later.
    //
    // TODO: Gross boolean to wedge in octets is gross; for now it
    // only affects the output when integer_type is unsigned_short_int
    fn eval_int(&self, ity: &integer_type, octet: bool) -> Result<Tokens, Error> {
        use parser::ast::const_expr::*;
        use parser::ast::integer_type::*;
        use parser::ast::literal::integer_literal;
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
        use parser::ast::const_expr::*;
        use parser::ast::floating_pt_type::*;
        use parser::ast::literal::floating_pt_literal;
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
        use parser::ast::const_expr::literal;
        use parser::ast::literal::character_literal;
        match *self {
            literal(character_literal(c)) => Ok(quote! {#c}),
            _ => Err(Error::ConstTypeError("char", self.clone()))
        }
    }

    fn eval_wide_char(&self) -> Result<Tokens, Error> {
        use parser::ast::const_expr::literal;
        use parser::ast::literal::wide_character_literal;
        match *self {
            literal(wide_character_literal(c)) => Ok(quote! {#c}),
            _ => Err(Error::ConstTypeError("wchar", self.clone()))
        }
    }

    fn eval_boolean(&self) -> Result<Tokens, Error> {
        use parser::ast::const_expr::literal;
        use parser::ast::literal::boolean_literal;
        match *self {
            literal(boolean_literal(b)) => Ok(quote! {#b}),
            _ => Err(Error::ConstTypeError("boolean", self.clone()))
        }
    }

    fn eval_string(&self, b: &Bound) -> Result<Tokens, Error> {
        use parser::ast::const_expr::literal;
        use parser::ast::literal::string_literal;
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
        use parser::ast::const_expr::literal;
        use parser::ast::literal::wide_string_literal;
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
        use parser::ast::const_expr::scoped_name;
        match *self {
            scoped_name(ref sn) => {
                let entry = st.resolve_scoped_name(&sn)?;
                match entry {
                    ScopeEntry::EnumVariant(sn) => {
                        let sn = st.emit_scoped_name(&sn);
                        Ok(quote!{#sn})
                    },
                    _ => Err(Error::ConstTypeError("enum", self.clone()))
                }
            },
            _ => Err(Error::ConstTypeError("enum", self.clone()))
        }
    }
}
*/
