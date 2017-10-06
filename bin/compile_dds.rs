extern crate omg_idl;

use std::io;

use omg_idl::core::from_parsed_ast;
use omg_idl::parser;

const DDS_DCPS: &'static str = include_str!("dds_dcps.idl");
// const DDS_DCPS: &'static str = "const long x = 40 + 2;";
// const DDS_DCPS: &'static str = "const double x = 40. + 2.;";
// const DDS_DCPS: &'static str = "const char c = 'x';";
// const DDS_DCPS: &'static str = "const wchar c = L'\\u03bb';";
// const DDS_DCPS: &'static str = "const Foo::Bar BAZ = Baz;";
// const DDS_DCPS: &'static str = "const wstring s = L\"\\u03bb\";";
// const DDS_DCPS: &'static str = "module Foo { const wstring s = L\"\\u03bb\"; };";
// const DDS_DCPS: &'static str = "enum Foo { Bar, Baz }; const Foo b = Bar;";
// const DDS_DCPS: &'static str = "module M { enum Foo { Bar, Baz }; }; const M::Foo F = M::Bar;";
// const DDS_DCPS: &'static str = "enum Foo { Bar, Baz }; module M { const Foo F = Bar; };";

fn main() {
    let spec = parser::specification(DDS_DCPS).unwrap();
    let spec = from_parsed_ast::to_core(&spec).unwrap();
    println!("{:#?}", spec);
}
