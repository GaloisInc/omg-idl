use std::hash::{Hash, Hasher};

#[derive(Clone, Debug)]
pub struct identifier(pub String);

impl Eq for identifier {}

// per IDL spec, identifiers collide case-insensitively
impl PartialEq for identifier {
    fn eq(&self, other: &identifier) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}

// per IDL spec, identifiers collide case-insensitively
impl Hash for identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct specification(pub Vec<definition>);

#[derive(Debug, PartialEq)]
pub enum definition {
    module_dcl(module_dcl),
    const_dcl(const_dcl),
    type_dcl(type_dcl),
    except_dcl(except_dcl),
    interface_dcl(interface_dcl),
}

#[derive(Debug, PartialEq)]
pub struct module_dcl {
    pub identifier: identifier,
    pub defs: Vec<definition>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum scoped_name {
    Qualified(Vec<identifier>),
    FileScope(Vec<identifier>),
}

impl scoped_name {
    pub fn push(&mut self, id: identifier) {
        match *self {
            scoped_name::Qualified(ref mut ids) => ids.push(id),
            scoped_name::FileScope(ref mut ids) => ids.push(id),
        };
    }
}

#[derive(Debug, PartialEq)]
pub struct const_dcl {
    pub ty: const_type,
    pub identifier: identifier,
    pub expr: const_expr,
}

#[derive(Debug, PartialEq)]
pub enum const_type {
    integer_type(integer_type),
    floating_pt_type(floating_pt_type),
    char_type,
    wide_char_type,
    boolean_type,
    octet_type,
    string_type(Bound),
    wide_string_type(Bound),
    scoped_name(scoped_name),
    // unsupported
    fixed_pt_const_type,
}

// Covers all of the const_expr non-terminals in order to better fit
// the `peg` style of parsing:
//
// const_expr, or_expr, xor_expr, and_expr, shift_expr, add_expr,
// mult_expr, unary_expr, unary_operator, primary_expr
#[derive(Clone, Debug, PartialEq)]
pub enum const_expr {
    scoped_name(scoped_name),
    literal(literal),
    Or(Box<const_expr>, Box<const_expr>),
    Xor(Box<const_expr>, Box<const_expr>),
    And(Box<const_expr>, Box<const_expr>),
    Shr(Box<const_expr>, Box<const_expr>),
    Shl(Box<const_expr>, Box<const_expr>),
    Add(Box<const_expr>, Box<const_expr>),
    Sub(Box<const_expr>, Box<const_expr>),
    Mult(Box<const_expr>, Box<const_expr>),
    Div(Box<const_expr>, Box<const_expr>),
    Mod(Box<const_expr>, Box<const_expr>),
    Negate(Box<const_expr>),
    Complement(Box<const_expr>),
}

impl const_expr {
    pub fn binop<F>(ctor: F, l: const_expr, r: const_expr) -> const_expr
        where F: FnOnce(Box<const_expr>, Box<const_expr>) -> const_expr {
        ctor(Box::new(l), Box::new(r))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum literal {
    integer_literal(u64),
    floating_pt_literal(f64),
    fixed_pt_literal(String),
    character_literal(char),
    wide_character_literal(char),
    boolean_literal(bool),
    string_literal(String),
    wide_string_literal(String),
}

#[derive(Debug, PartialEq)]
pub struct positive_int_const(pub const_expr);

#[derive(Debug, PartialEq)]
pub enum type_dcl {
    constr_type_dcl(constr_type_dcl),
    native_dcl(simple_declarator),
    typedef_dcl(typedef_dcl),
}

#[derive(Debug, PartialEq)]
pub enum type_spec {
    simple_type_spec(simple_type_spec),
    template_type_spec(template_type_spec),
}

#[derive(Debug, PartialEq)]
pub enum simple_type_spec {
    base_type_spec(base_type_spec),
    scoped_name(scoped_name),
}

#[derive(Debug, PartialEq)]
pub enum base_type_spec {
    integer_type(integer_type),
    floating_pt_type(floating_pt_type),
    char_type,
    wide_char_type,
    boolean_type,
    octet_type,
}

#[derive(Debug, PartialEq)]
pub enum Bound {
    Bounded(positive_int_const),
    Unbounded,
}

// collapsed
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum integer_type {
    signed_short_int,
    signed_long_int,
    signed_longlong_int,
    unsigned_short_int,
    unsigned_long_int,
    unsigned_longlong_int,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum floating_pt_type {
    float,
    double,
    // unsupported
    long_double,
}

#[derive(Debug, PartialEq)]
pub enum template_type_spec {
    // break recursion with a Box here
    sequence_type(Box<type_spec>, Bound),
    string_type(Bound),
    wide_string_type(Bound),
    // unsupported
    fixed_pt_type,
}

#[derive(Debug, PartialEq)]
pub enum constr_type_dcl {
    struct_dcl(struct_dcl),
    union_dcl(union_dcl),
    enum_dcl(enum_dcl),
}

#[derive(Debug, PartialEq)]
pub enum struct_dcl {
    struct_def(struct_def),
    struct_forward_dcl(identifier),
}

#[derive(Debug, PartialEq)]
pub struct struct_def {
    pub identifier: identifier,
    pub members: Vec<member>,
}

#[derive(Debug, PartialEq)]
pub struct member {
    pub type_spec: type_spec,
    pub declarators: Vec<declarator>,
}

#[derive(Debug, PartialEq)]
pub enum union_dcl {
    union_def(union_def),
    union_forward_dcl(identifier),
}

#[derive(Debug, PartialEq)]
pub struct union_def {
    pub identifier: identifier,
    pub switch_type_spec: switch_type_spec,
    pub switch_body: switch_body,
}

#[derive(Debug, PartialEq)]
pub enum switch_type_spec {
    integer_type(integer_type),
    char_type,
    boolean_type,
    scoped_name(scoped_name),
}

pub type switch_body = Vec<case>;

// case + element_spec collapsed
#[derive(Debug, PartialEq)]
pub struct case {
    pub case_labels: Vec<case_label>,
    pub type_spec: type_spec,
    pub declarator: declarator,
}

#[derive(Debug, PartialEq)]
pub enum case_label {
    const_expr(const_expr),
    default,
}

#[derive(Debug, PartialEq)]
pub struct enum_dcl {
    pub identifier: identifier,
    pub enumerators: Vec<identifier>,
}

// array_declarator + fixed_array_size collapsed
#[derive(Debug, PartialEq)]
pub struct array_declarator {
    pub identifier: identifier,
    pub sizes: Vec<positive_int_const>,
}

pub type simple_declarator = identifier;

// typedef_dcl + type_declarator collapsed
#[derive(Debug, PartialEq)]
pub enum typedef_dcl {
    simple_type_spec(simple_type_spec, Vec<any_declarator>),
    template_type_spec(template_type_spec, Vec<any_declarator>),
    constr_type_dcl(constr_type_dcl, Vec<any_declarator>),
}

#[derive(Debug, PartialEq)]
pub enum any_declarator {
    simple_declarator(simple_declarator),
    array_declarator(array_declarator),
}

#[derive(Debug, PartialEq)]
pub enum declarator {
    simple_declarator(simple_declarator),
    array_declarator(array_declarator),
}

#[derive(Debug, PartialEq)]
pub struct except_dcl {
    pub identifier: identifier,
    pub members: Vec<member>,
}

// interface_dcl + interface_kind collapsed
#[derive(Debug, PartialEq)]
pub enum interface_dcl {
    interface_def(interface_def),
    interface_forward_dcl(identifier),
}

// interface_def + interface_kind + interface_header collapsed
#[derive(Debug, PartialEq)]
pub struct interface_def {
    pub identifier: identifier,
    pub interface_inheritance_spec: Option<Vec<interface_name>>,
    pub interface_body: Vec<export>,
}

pub type interface_name = scoped_name;

#[derive(Debug, PartialEq)]
pub enum export {
    op_dcl(op_dcl),
    attr_dcl(attr_dcl),
}

#[derive(Debug, PartialEq)]
pub struct op_dcl {
    pub op_type_spec: op_type_spec,
    pub identifier: identifier,
    pub parameter_dcls: Vec<param_dcl>,
    pub raises_expr: Option<raises_expr>,
}

#[derive(Debug, PartialEq)]
pub enum op_type_spec {
    type_spec(type_spec),
    void,
}

#[derive(Debug, PartialEq)]
pub struct param_dcl {
    pub param_attribute: param_attribute,
    pub type_spec: type_spec,
    pub simple_declarator: simple_declarator,
}

#[derive(Debug, PartialEq)]
pub enum param_attribute {
    in_,
    out,
    inout,
}

pub type raises_expr = Vec<scoped_name>;

#[derive(Debug, PartialEq)]
pub enum attr_dcl {
    readonly_attr_spec(readonly_attr_spec),
    attr_spec(attr_spec),
}

#[derive(Debug, PartialEq)]
pub struct readonly_attr_spec {
    pub type_spec: type_spec,
    pub readonly_attr_declarator: readonly_attr_declarator,
}

#[derive(Debug, PartialEq)]
pub enum readonly_attr_declarator {
    simple_declarator_raises(simple_declarator, raises_expr),
    simple_declarators(Vec<simple_declarator>),
}

#[derive(Debug, PartialEq)]
pub struct attr_spec {
    pub type_spec: type_spec,
    pub attr_declarator: attr_declarator,
}

#[derive(Debug, PartialEq)]
pub enum attr_declarator {
    simple_declarator_raises(simple_declarator, attr_raises_expr),
    simple_declarators(Vec<simple_declarator>),
}

#[derive(Debug, PartialEq)]
pub enum attr_raises_expr {
    get_excep_expr(get_excep_expr, Option<set_excep_expr>),
    set_excep_expr(set_excep_expr),
}

pub type get_excep_expr = Vec<scoped_name>;

pub type set_excep_expr = Vec<scoped_name>;
