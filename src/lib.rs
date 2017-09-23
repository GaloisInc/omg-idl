#[macro_use] extern crate lazy_static;
extern crate regex;

pub mod parser;
pub mod ast;

#[cfg(test)]
mod tests {
    use ::parser;
    use ::ast;

    #[test]
    fn identifier() {
        assert_eq!(parser::identifier("foo").unwrap(), ast::Id::from_str("foo"));
        assert!(parser::identifier("public").is_err());
        assert_eq!(parser::identifier("_public").unwrap(), ast::Id::from_str("public"));
    }

    #[test]
    fn scoped_name() {
        assert_eq!(parser::scoped_name("foo").unwrap(), ast::ScopedName::Qualified(vec![ast::Id::from_str("foo")]));
        assert_eq!(parser::scoped_name("::foo").unwrap(), ast::ScopedName::FileScope(vec![ast::Id::from_str("foo")]));
        assert_eq!(parser::scoped_name("foo::bar").unwrap(), ast::ScopedName::Qualified(
            vec![ast::Id::from_str("foo"),
                 ast::Id::from_str("bar"),
            ]));
    }

    #[test]
    fn integer_literal() {
        assert_eq!(parser::integer_literal("0").unwrap(), ast::Literal::Int(0));
        assert_eq!(parser::integer_literal("42").unwrap(), ast::Literal::Int(42));
        assert_eq!(parser::integer_literal("012").unwrap(), ast::Literal::Int(0o12));
        assert_eq!(parser::integer_literal("0xdeadbeef").unwrap(), ast::Literal::Int(0xdeadbeef));
    }

    #[test]
    fn float_pt_literal() {
        assert_eq!(parser::floating_pt_literal("42.0").unwrap(), ast::Literal::FloatPt(42.0));
        assert_eq!(parser::floating_pt_literal("42.").unwrap(), ast::Literal::FloatPt(42.0));
        assert_eq!(parser::floating_pt_literal(".42").unwrap(), ast::Literal::FloatPt(0.42));
        assert_eq!(parser::floating_pt_literal("42.0e20").unwrap(), ast::Literal::FloatPt(42.0e20));
        assert_eq!(parser::floating_pt_literal("42e20").unwrap(), ast::Literal::FloatPt(42e20));
        assert!(parser::floating_pt_literal("42").is_err());
    }

    #[test]
    fn fixed_pt_literal() {
        assert_eq!(parser::fixed_pt_literal("42.0d").unwrap(), ast::Literal::FixedPt(String::from("42.0d")));
        assert_eq!(parser::fixed_pt_literal("42.D").unwrap(), ast::Literal::FixedPt(String::from("42.D")));
        assert_eq!(parser::fixed_pt_literal(".42d").unwrap(), ast::Literal::FixedPt(String::from(".42d")));
    }

    #[test]
    fn character_literal() {
        assert_eq!(parser::character_literal("'a'").unwrap(), ast::Literal::Char('a'));
        assert_eq!(parser::character_literal("'0'").unwrap(), ast::Literal::Char('0'));
        assert!(parser::character_literal("'\'").is_err());
        assert_eq!(parser::character_literal("'\\\"'").unwrap(), ast::Literal::Char('"'));
        assert_eq!(parser::character_literal("'\\141'").unwrap(), ast::Literal::Char('a'));
        assert_eq!(parser::character_literal("'\\x61'").unwrap(), ast::Literal::Char('a'));
        assert!(parser::character_literal("'\\u61'").is_err())
    }

    #[test]
    fn wide_character_literal() {
        assert_eq!(parser::wide_character_literal("L'a'").unwrap(), ast::Literal::WChar('a'));
        assert_eq!(parser::wide_character_literal("L'0'").unwrap(), ast::Literal::WChar('0'));
        assert!(parser::wide_character_literal("L'\'").is_err());
        assert_eq!(parser::wide_character_literal("L'\\\"'").unwrap(), ast::Literal::WChar('"'));
        assert_eq!(parser::wide_character_literal("L'\\141'").unwrap(), ast::Literal::WChar('a'));
        assert_eq!(parser::wide_character_literal("L'\\x61'").unwrap(), ast::Literal::WChar('a'));
        assert_eq!(parser::wide_character_literal("L'\\u03bb'").unwrap(), ast::Literal::WChar('λ'));
    }

    #[test]
    fn boolean_literal() {
        assert_eq!(parser::boolean_literal("TRUE").unwrap(), ast::Literal::Bool(true));
        assert_eq!(parser::boolean_literal("FALSE").unwrap(), ast::Literal::Bool(false));
        assert!(parser::boolean_literal("42").is_err());
    }

    #[test]
    fn string_literal() {
        assert_eq!(parser::string_literal("\"hello\"").unwrap(), ast::Literal::String(String::from("hello")));
        assert_eq!(parser::string_literal("\"hel\\\"lo\"").unwrap(), ast::Literal::String(String::from("hel\"lo")));
        assert_eq!(parser::string_literal("\"hello world\"").unwrap(), ast::Literal::String(String::from("hello world")));
        assert_eq!(parser::string_literal("\"hello \" \"world\"").unwrap(), ast::Literal::String(String::from("hello world")));
        assert!(parser::string_literal("\"hello \\u03bb\"").is_err());
    }

    #[test]
    fn wide_string_literal() {
        assert_eq!(parser::wide_string_literal("L\"hello\"").unwrap(), ast::Literal::WString(String::from("hello")));
        assert_eq!(parser::wide_string_literal("L\"hel\\\"lo\"").unwrap(), ast::Literal::WString(String::from("hel\"lo")));
        assert_eq!(parser::wide_string_literal("L\"hello \\u03bb\"").unwrap(), ast::Literal::WString(String::from("hello λ")));
        assert_eq!(parser::wide_string_literal("L\"hello \" L\"\\u03bb\"").unwrap(), ast::Literal::WString(String::from("hello λ")));
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
    }
}
