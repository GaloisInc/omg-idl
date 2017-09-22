use regex::Regex;
use std::char;

use ast::{Literal};

pub fn parse_character_literal(s: &str) -> Literal {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"'([\PC&&[\x{00}-\x{FF}]]+)'").unwrap();
    }
    let processed_chars = process_chars(&RE.captures(s).unwrap()[1]);
    if processed_chars.chars().count() != 1 {
        panic!("character literals must contain exactly one character; saw '{}'", processed_chars);
    } else {
        let c = processed_chars.chars().next().unwrap();
        if c.len_utf8() > 1 {
            panic!("wide character in character literal: '{}'", c);
        } else {
            Literal::Char(c)
        }
    }
}

pub fn parse_wide_character_literal(s: &str) -> Literal {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"L'(\PC+)'").unwrap();
    }
    let processed_chars = process_chars(&RE.captures(s).unwrap()[1]);
    if processed_chars.chars().count() != 1 {
        panic!("character literal with more than one character: '{}'", processed_chars);
    } else {
        let c = processed_chars.chars().next().unwrap();
        Literal::WChar(c)
    }
}

pub fn parse_one_string_literal(s: &str) -> String {
    println!("s={:?}", s);
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r#""((\\"|[\PC&&[\x{00}-\x{FF}]&&[^"]])*)""#).unwrap();
    }
    println!("captures={:?}", &RE.captures(s).unwrap());
    let processed_chars = process_chars(&RE.captures(s).unwrap()[1]);
    println!("processed_chars={:?}", processed_chars);
    processed_chars.chars().map(|c| {
        if c.len_utf8() > 1 {
            panic!("wide character '{}' in string literal: \"{}\"", c, processed_chars);
        }
    }).count();
    processed_chars
}

pub fn parse_one_wide_string_literal(s: &str) -> String {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r#"L"((\\"|[\PC&&[^"]])*)""#).unwrap();
    }
    let processed_chars = process_chars(&RE.captures(s).unwrap()[1]);
    processed_chars
}

pub fn process_chars(s: &str) -> String {
    enum State {
        Normal,
        Escape,
        Octal(String, usize),
        Hex(String, usize),
        Uni(String, usize),
    }

    struct Acc {
        out: String,
        state: State,
    }

    let mut acc_final = s.chars().fold(Acc { out: String::new(), state: State::Normal }, |mut acc, c| {
        match acc.state {
            State::Normal => {
                match c {
                    '\\' => acc.state = State::Escape,
                    _ => acc.out.push(c)
                }
                acc
            },
            State::Escape => {
                match c {
                    'n' => {
                        acc.out.push('\n');
                        acc.state = State::Normal;
                    },
                    't' => {
                        acc.out.push('\t');
                        acc.state = State::Normal;
                    },
                    'v' => {
                        acc.out.push('\u{0B}');
                        acc.state = State::Normal;
                    },
                    'b' => {
                        acc.out.push('\u{08}');
                        acc.state = State::Normal;
                    },
                    'r' => {
                        acc.out.push('\r');
                        acc.state = State::Normal;
                    },
                    'f' => {
                        acc.out.push('\u{0C}');
                        acc.state = State::Normal;
                    },
                    'a' => {
                        acc.out.push('\u{07}');
                        acc.state = State::Normal;
                    },
                    '\\' => {
                        acc.out.push('\\');
                        acc.state = State::Normal;
                    },
                    '?' => {
                        acc.out.push('?');
                        acc.state = State::Normal;
                    },
                    '\'' => {
                        acc.out.push('\'');
                        acc.state = State::Normal;
                    },
                    '"' => {
                        acc.out.push('"');
                        acc.state = State::Normal;
                    },
                    'x' => {
                        acc.state = State::Hex(String::with_capacity(2), 0);
                    },
                    'u' => {
                        acc.state = State::Uni(String::with_capacity(4), 0);
                    },
                    _ => {
                        if c.is_digit(8) {
                            let mut octs = String::with_capacity(3);
                            octs.push(c);
                            acc.state = State::Octal(octs, 1);
                        } else {
                            panic!("unrecognized escape sequence \\{}", c)
                        }
                    }
                }
                acc
            },
            State::Octal(mut octs, run) => {
                if run == 3 || !c.is_digit(8) {
                    acc.out.push(char::from(u8::from_str_radix(&octs, 8).unwrap()));
                    match c {
                        '\\' => acc.state = State::Escape,
                        _ => {
                            acc.out.push(c);
                            acc.state = State::Normal;
                        }
                    }
                } else {
                    octs.push(c);
                    acc.state = State::Octal(octs, run + 1);
                }
                acc
            },
            State::Hex(mut hexes, run) => {
                if run == 0 && !c.is_digit(16) {
                    panic!("invalid hex escape sequence \\x{}", c);
                } else if run == 2 || !c.is_digit(16) {
                    acc.out.push(char::from(u8::from_str_radix(&hexes, 16).unwrap()));
                    match c {
                        '\\' => acc.state = State::Escape,
                        _ => {
                            acc.out.push(c);
                            acc.state = State::Normal;
                        }
                    }
                } else {
                    hexes.push(c);
                    acc.state = State::Hex(hexes, run + 1);
                }
                acc
            },
            State::Uni(mut unis, run) => {
                if run == 0 && !c.is_digit(16) {
                    panic!("invalid unicode escape sequence \\u{}", c);
                } else if run == 4 || !c.is_digit(16) {
                    acc.out.push(char::from_u32(u32::from_str_radix(&unis, 16).unwrap()).unwrap());
                    match c {
                        '\\' => acc.state = State::Escape,
                        _ => {
                            acc.out.push(c);
                            acc.state = State::Normal;
                        }
                    }
                } else {
                    unis.push(c);
                    acc.state = State::Uni(unis, run + 1);
                }
                acc
            },
        }
    });
    // clean up any partial runs of escape sequences that might be left over
    match acc_final.state {
        State::Octal(octs, _) => {
            acc_final.out.push(char::from(u8::from_str_radix(&octs, 8).unwrap()));
        },
        State::Hex(hexes, _) => {
            acc_final.out.push(char::from(u8::from_str_radix(&hexes, 16).unwrap()));
        },
        State::Uni(unis, _) => {
            acc_final.out.push(char::from_u32(u32::from_str_radix(&unis, 16).unwrap()).unwrap());
        },
        _ => ()
    }
    acc_final.out
}
