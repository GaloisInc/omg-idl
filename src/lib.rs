pub mod parser;
#[allow(non_camel_case_types)]
pub mod ast;

#[cfg(test)]
mod tests {
    use ::parser;
    use ::ast;

    #[test]
    fn identifier() {
        assert_eq!(parser::identifier("x").unwrap(), ast::identifier("x".to_owned()));
        assert_eq!(parser::identifier("foo").unwrap(), ast::identifier("foo".to_owned()));
        assert!(parser::identifier("public").is_err());
        assert_eq!(parser::identifier("_public").unwrap(), ast::identifier("public".to_owned()));
    }

    #[test]
    fn scoped_name() {
        assert_eq!(parser::scoped_name("foo").unwrap(), ast::scoped_name::Qualified(vec![ast::identifier("foo".to_owned())]));
        assert_eq!(parser::scoped_name("::foo").unwrap(), ast::scoped_name::FileScope(vec![ast::identifier("foo".to_owned())]));
        assert_eq!(parser::scoped_name("foo::bar").unwrap(), ast::scoped_name::Qualified(
            vec![ast::identifier("foo".to_owned()),
                 ast::identifier("bar".to_owned()),
            ]));
    }

    #[test]
    fn integer_literal() {
        assert_eq!(parser::integer_literal("0").unwrap(), ast::literal::integer_literal(0));
        assert_eq!(parser::integer_literal("42").unwrap(), ast::literal::integer_literal(42));
        assert_eq!(parser::integer_literal("012").unwrap(), ast::literal::integer_literal(0o12));
        assert_eq!(parser::integer_literal("0xdeadbeef").unwrap(), ast::literal::integer_literal(0xdeadbeef));
    }

    #[test]
    fn float_pt_literal() {
        assert_eq!(parser::floating_pt_literal("42.0").unwrap(), ast::literal::floating_pt_literal(42.0));
        assert_eq!(parser::floating_pt_literal("42.").unwrap(), ast::literal::floating_pt_literal(42.0));
        assert_eq!(parser::floating_pt_literal(".42").unwrap(), ast::literal::floating_pt_literal(0.42));
        assert_eq!(parser::floating_pt_literal("42.0e20").unwrap(), ast::literal::floating_pt_literal(42.0e20));
        assert_eq!(parser::floating_pt_literal("42e20").unwrap(), ast::literal::floating_pt_literal(42e20));
        assert!(parser::floating_pt_literal("42").is_err());
    }

    #[test]
    fn fixed_pt_literal() {
        assert_eq!(parser::fixed_pt_literal("42.0d").unwrap(), ast::literal::fixed_pt_literal(String::from("42.0d")));
        assert_eq!(parser::fixed_pt_literal("42.D").unwrap(), ast::literal::fixed_pt_literal(String::from("42.D")));
        assert_eq!(parser::fixed_pt_literal(".42d").unwrap(), ast::literal::fixed_pt_literal(String::from(".42d")));
    }

    #[test]
    fn character_literal() {
        assert_eq!(parser::character_literal("'a'").unwrap(), ast::literal::character_literal('a'));
        assert_eq!(parser::character_literal("'0'").unwrap(), ast::literal::character_literal('0'));
        assert!(parser::character_literal("'\'").is_err());
        assert_eq!(parser::character_literal("'\\\"'").unwrap(), ast::literal::character_literal('"'));
        assert_eq!(parser::character_literal("'\\141'").unwrap(), ast::literal::character_literal('a'));
        assert_eq!(parser::character_literal("'\\x61'").unwrap(), ast::literal::character_literal('a'));
        assert!(parser::character_literal("'\\u61'").is_err())
    }

    #[test]
    fn wide_character_literal() {
        assert_eq!(parser::wide_character_literal("L'a'").unwrap(), ast::literal::wide_character_literal('a'));
        assert_eq!(parser::wide_character_literal("L'0'").unwrap(), ast::literal::wide_character_literal('0'));
        assert!(parser::wide_character_literal("L'\'").is_err());
        assert_eq!(parser::wide_character_literal("L'\\\"'").unwrap(), ast::literal::wide_character_literal('"'));
        assert_eq!(parser::wide_character_literal("L'\\141'").unwrap(), ast::literal::wide_character_literal('a'));
        assert_eq!(parser::wide_character_literal("L'\\x61'").unwrap(), ast::literal::wide_character_literal('a'));
        assert_eq!(parser::wide_character_literal("L'\\u03bb'").unwrap(), ast::literal::wide_character_literal('λ'));
    }

    #[test]
    fn boolean_literal() {
        assert_eq!(parser::boolean_literal("TRUE").unwrap(), ast::literal::boolean_literal(true));
        assert_eq!(parser::boolean_literal("FALSE").unwrap(), ast::literal::boolean_literal(false));
        assert!(parser::boolean_literal("42").is_err());
    }

    #[test]
    fn string_literal() {
        assert_eq!(parser::string_literal("\"hello\"").unwrap(), ast::literal::string_literal(String::from("hello")));
        assert_eq!(parser::string_literal("\"hel\\\"lo\"").unwrap(), ast::literal::string_literal(String::from("hel\"lo")));
        assert_eq!(parser::string_literal("\"hello world\"").unwrap(), ast::literal::string_literal(String::from("hello world")));
        assert_eq!(parser::string_literal("\"hello \" \"world\"").unwrap(), ast::literal::string_literal(String::from("hello world")));
        assert!(parser::string_literal("\"hello \\u03bb\"").is_err());
    }

    #[test]
    fn wide_string_literal() {
        assert_eq!(parser::wide_string_literal("L\"hello\"").unwrap(), ast::literal::wide_string_literal(String::from("hello")));
        assert_eq!(parser::wide_string_literal("L\"hel\\\"lo\"").unwrap(), ast::literal::wide_string_literal(String::from("hel\"lo")));
        assert_eq!(parser::wide_string_literal("L\"hello \\u03bb\"").unwrap(), ast::literal::wide_string_literal(String::from("hello λ")));
        assert_eq!(parser::wide_string_literal("L\"hello \" L\"\\u03bb\"").unwrap(), ast::literal::wide_string_literal(String::from("hello λ")));
    }

    #[test]
    fn const_expr() {
        assert!(parser::const_expr("1").is_ok());
        assert!(parser::const_expr("-1").is_ok());
        assert!(parser::const_expr("2 * 1 % 2 >> \"hello\"").is_ok());
        assert!(parser::const_expr("1 |2 & 3 % -4").is_ok());
    }

    #[test]
    fn const_dcl() {
        parser::const_dcl("const long            TIME_INVALID_SEC        = -1").unwrap();
        parser::const_dcl("const ReturnCode_t RETCODE_ILLEGAL_OPERATION     = 12").unwrap();
        parser::const_dcl("const string DURABILITY_QOS_POLICY_NAME            = \"Durability\"").unwrap();
        parser::const_dcl("const string<42> DURABILITY_QOS_POLICY_NAME            = \"Durability\"").unwrap();
    }

    #[test]
    fn base_type_spec() {
        assert_eq!(parser::base_type_spec("unsigned long").unwrap(), ast::base_type_spec::integer_type(ast::integer_type::unsigned_long_int));
        assert_eq!(parser::base_type_spec("octet").unwrap(), ast::base_type_spec::octet_type);
    }

    #[test]
    fn type_dcl() {
        parser::type_dcl("struct Duration_t { long sec; unsigned long nanosec; }").unwrap();
        parser::type_dcl("typedef long ReturnCode_t").unwrap();
        parser::type_dcl("typedef sequence<InstanceHandle_t> InstanceHandleSeq").unwrap();
        parser::type_dcl("enum OwnershipQosPolicyKind { SHARED_OWNERSHIP_QOS, EXCLUSIVE_OWNERSHIP_QOS }").unwrap();
    }
}
