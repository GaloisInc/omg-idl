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
        assert_eq!(parser::parse_identifier("foo").unwrap(), ast::Id::from_str("foo"));
        assert!(parser::parse_identifier("public").is_err());
        assert_eq!(parser::parse_identifier("_public").unwrap(), ast::Id::from_str("public"));
    }

    #[test]
    fn integer_literal() {
        assert_eq!(parser::parse_integer_literal("42").unwrap(), ast::Lit::Int(42));
        assert_eq!(parser::parse_integer_literal("012").unwrap(), ast::Lit::Int(0o12));
        assert_eq!(parser::parse_integer_literal("0xdeadbeef").unwrap(), ast::Lit::Int(0xdeadbeef));
    }

    #[test]
    fn float_pt_literal() {
        assert_eq!(parser::parse_floating_pt_literal("42.0").unwrap(), ast::Lit::FloatPt(42.0));
        assert_eq!(parser::parse_floating_pt_literal("42.").unwrap(), ast::Lit::FloatPt(42.0));
        assert_eq!(parser::parse_floating_pt_literal(".42").unwrap(), ast::Lit::FloatPt(0.42));
        assert_eq!(parser::parse_floating_pt_literal("42.0e20").unwrap(), ast::Lit::FloatPt(42.0e20));
        assert_eq!(parser::parse_floating_pt_literal("42e20").unwrap(), ast::Lit::FloatPt(42e20));
        assert!(parser::parse_floating_pt_literal("42").is_err());
    }

    #[test]
    fn fixed_pt_literal() {
        assert_eq!(parser::parse_fixed_pt_literal("42.0d").unwrap(), ast::Lit::FixedPt(String::from("42.0d")));
        assert_eq!(parser::parse_fixed_pt_literal("42.D").unwrap(), ast::Lit::FixedPt(String::from("42.D")));
        assert_eq!(parser::parse_fixed_pt_literal(".42d").unwrap(), ast::Lit::FixedPt(String::from(".42d")));
    }

    #[test]
    fn character_literal() {
        assert_eq!(parser::parse_character_literal("'a'").unwrap(), ast::Lit::Char('a'));
        assert_eq!(parser::parse_character_literal("'0'").unwrap(), ast::Lit::Char('0'));
        assert!(parser::parse_character_literal("'\'").is_err());
        assert_eq!(parser::parse_character_literal("'\\\"'").unwrap(), ast::Lit::Char('"'));
        assert_eq!(parser::parse_character_literal("'\\141'").unwrap(), ast::Lit::Char('a'));
        assert_eq!(parser::parse_character_literal("'\\x61'").unwrap(), ast::Lit::Char('a'));
        assert_eq!(parser::parse_character_literal("'\\u61'").unwrap(), ast::Lit::Char('a'));
    }

    #[test]
    fn wide_character_literal() {
        assert_eq!(parser::parse_wide_character_literal("L'a'").unwrap(), ast::Lit::WChar('a'));
        assert_eq!(parser::parse_wide_character_literal("L'0'").unwrap(), ast::Lit::WChar('0'));
        assert!(parser::parse_wide_character_literal("L'\'").is_err());
        assert_eq!(parser::parse_wide_character_literal("L'\\\"'").unwrap(), ast::Lit::WChar('"'));
        assert_eq!(parser::parse_wide_character_literal("L'\\141'").unwrap(), ast::Lit::WChar('a'));
        assert_eq!(parser::parse_wide_character_literal("L'\\x61'").unwrap(), ast::Lit::WChar('a'));
        assert_eq!(parser::parse_wide_character_literal("L'\\u03bb'").unwrap(), ast::Lit::WChar('λ'));
    }

}
