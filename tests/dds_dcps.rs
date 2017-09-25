extern crate omg_idl;

use omg_idl::parser;

const DDS_DCPS: &'static str = include_str!("dds_dcps.idl");

#[test]
pub fn dds_dcps_specification() {
    println!("{:#?}", parser::specification(DDS_DCPS).unwrap());
}
