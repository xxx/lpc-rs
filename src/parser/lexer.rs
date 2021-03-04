use logos::{Logos, Lexer, Filter};
use lazy_static::lazy_static;
use regex::Regex;
use std::str::FromStr;
use regex::internal::Input;

#[derive(Debug)]
struct LexError(String);

#[derive(Debug)]
struct LexState {
    // TODO: remove the allocation for last_slice
    pub last_slice: String,
    pub current_file: String,
    pub current_line: usize
}

impl Default for LexState {
    fn default() -> Self {
        LexState {
            last_slice: String::from("\n"),
            current_file: String::new(),
            current_line: 0
        }
    }
}

/// This exists only because Logos doesn't handle fields with more than one member.
#[derive(Debug, PartialEq, Eq)]
struct LineDirective(String, usize);

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexState)]
enum Token {
    #[token("+", track_slice)]
    Plus,
    #[token("-", track_slice)]
    Minus,
    #[token("*", track_slice)]
    Mul,
    #[token("/", track_slice)]
    Div,
    #[token("%", track_slice)]
    Mod,
    #[token("!", track_slice)]
    Bang,
    #[token("^", track_slice)]
    Caret,
    #[token("~", track_slice)]
    Tilde,
    #[token("&", track_slice)]
    And,
    #[token("&&", track_slice)]
    AndAnd,
    #[token("|", track_slice)]
    Or,
    #[token("||", track_slice)]
    OrOr,
    #[token("<<", track_slice)]
    LeftShift,
    #[token(">>", track_slice)]
    RightShift,
    #[token("==", track_slice)]
    EqEq,
    #[token("!=", track_slice)]
    NotEq,
    #[token("<", track_slice)]
    LessThan,
    #[token("<=", track_slice)]
    LessThanEq,
    #[token(">", track_slice)]
    GreaterThan,
    #[token(">=", track_slice)]
    GreaterThanEq,

    #[token("=", track_slice)]
    Assign,
    #[token("+=", track_slice)]
    PlusEq,
    #[token("-=", track_slice)]
    MinusEq,
    #[token("*=", track_slice)]
    MulEq,
    #[token("/=", track_slice)]
    DivEq,
    #[token("%=", track_slice)]
    ModEq,
    #[token("^=", track_slice)]
    CaretEq,
    #[token("~=", track_slice)]
    TildeEq,
    #[token("&=", track_slice)]
    AndEq,
    #[token("&&=", track_slice)]
    AndAndEq,
    #[token("|=", track_slice)]
    OrEq,
    #[token("||=", track_slice)]
    OrOrEq,
    #[token("<<=", track_slice)]
    LeftShiftEq,
    #[token(">>=", track_slice)]
    RightShiftEq,

    #[token("if", track_slice)]
    If,
    #[token("else", track_slice)]
    Else,
    #[token("while", track_slice)]
    While,
    #[token("for", track_slice)]
    For,
    #[token("inherit", track_slice)]
    Inherit,
    #[token("break", track_slice)]
    Break,
    #[token("continue", track_slice)]
    Continue,
    #[token("case", track_slice)]
    Case,
    #[token("do", track_slice)]
    Do,
    #[token("int", track_slice)]
    Int,
    #[token("float", track_slice)]
    Float,
    #[token("string", track_slice)]
    String,
    #[token("object", track_slice)]
    Object,
    #[token("mapping", track_slice)]
    Mapping,
    #[token("mixed", track_slice)]
    Mixed,
    #[token("void", track_slice)]
    Void,
    #[token("return", track_slice)]
    Return,
    #[token("static", track_slice)]
    Static,
    #[token("nomask", track_slice)]
    Nomask,

    #[token("(", track_slice)]
    LParen,
    #[token(")", track_slice)]
    RParen,
    #[token("[", track_slice)]
    LBracket,
    #[token("]", track_slice)]
    RBracket,
    #[token("{", track_slice)]
    LBrace,
    #[token("}", track_slice)]
    RBrace,
    #[token(",", track_slice)]
    Comma,
    #[token("->", track_slice)]
    CallOther,
    #[token("?", track_slice)]
    Question,
    #[token(":", track_slice)]
    Colon,
    #[token(";", track_slice)]
    Semi,
    
    #[regex(r#""(\\.|[^"])*""#, string_literal)]
    StringLiteral(String),

    #[regex(r"[1-9][0-9_]*", |lex| {
        i64::from_str(&lex.slice().replace("_", "")).ok()
    }, priority = 2)]
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", |lex| {
        i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0x")
                .trim_start_matches("0X"),
                16).ok()
    }, priority = 2)]
    #[regex(r"0[0-7][0-7_]*", |lex| {
        i64::from_str_radix(&lex.slice().replace("_", ""), 8).ok()
    }, priority = 2)]
    #[regex(r"0[bB][01][01_]*", |lex| {
        i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0b")
                .trim_start_matches("0B"),
                2).ok()
    }, priority = 2)]
    IntLiteral(i64),

    #[regex(r#"[0-9][0-9_]*\.[0-9][0-9_]*(?:[eE][-+]?[0-9][0-9_]*)?"#, float_literal)]
    FloatLiteral(f64),

    #[regex(r"[\p{Alphabetic}_]\w*", string_literal, priority = 2)]
    ID(String),

    #[regex(".", track_slice)]
    Token,

    #[error]
    #[regex(r#"#\s*line\s+\d+\s+"[^"]+"\s*"#, |lex| {
        let l = line(lex);
        track_slice(lex);
        if l.is_some() {
            Filter::Skip
        } else {
            Filter::Emit(())
        }
    })]
    #[regex(r"[ \t\f\v]+", |lex| {
        track_slice(lex);
        logos::Skip
    }, priority = 2)]
    #[token("\n", |lex| {
        lex.extras.current_line += 1;
        track_slice(lex);
        logos::Skip
    })]
    Error,
}

fn track_slice(lex: &mut Lexer<Token>) {
    lex.extras.last_slice = String::from(lex.slice());
}

fn string_literal(lex: &mut Lexer<Token>) -> Option<String> {
    track_slice(lex);
    Some(lex.extras.last_slice.clone())
}

fn float_literal(lex: &mut Lexer<Token>) -> Option<f64> {
    track_slice(lex);
    Some(f64::from_str(&lex.slice().replace("_", "")).unwrap())
}

fn line(lex: &mut Lexer<Token>) -> Option<()> {
    lazy_static! {
        static ref LINE: Regex =
            Regex::new(r#"\A#\s*line\s+(\d+)\s+"([^"]+?)"\s*\z"#).unwrap();
        static ref EOL: Regex =
            Regex::new(r#"\s*?\n\s*\z"#).unwrap();
    }

    // test if '#' is exactly at the start of a line
    if lex.extras.last_slice != "\n" && lex.extras.last_slice != "" && lex.span().start != 0 {
        return None;
    }

    let eof = lex.span().end == lex.source().len();

    let slice = lex.slice();
    let extra_junk = !EOL.is_match(slice) && !eof;

    if extra_junk {
        return None;
    }

    if let Some(captures) = LINE.captures(slice) {
        lex.extras.current_file = String::from(&captures[2]);
        lex.extras.current_line = usize::from_str(&captures[1]).unwrap();

        Some(())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn lex_vec(prog: &str) -> Vec<Token> {
        let lexer = Token::lexer(prog);
        lexer.collect::<Vec<_>>()
    }

    fn into_errors(v: Vec<Token>) -> Vec<Token> {
        v.into_iter().filter(|i| *i == Token::Error).collect()
    }

    fn assert_valid(prog: &str) {
        let vec = lex_vec(prog);

        assert!(into_errors(vec).is_empty());
    }

    fn assert_error(prog: &str) {
        let vec = lex_vec(prog);

        assert!(!into_errors(vec).is_empty());
    }

    #[test]
    fn test_line() {
        // valid code. Note that indoc will re-indent this code.
        let prog = indoc! {r#"
            #line 123 "snuh.h"
            a + 3 + as;
            #line 125 "foo.h""#
        };
        assert_valid(prog);

        // "#" isn't the first char on a line - invalid
        let prog = indoc! { r#"
            a + 3 + as;
             #line 125 "foo.h""#
         };
        assert_error(prog);

        // extraneous code after a #line directive - invalid
        let prog = indoc! { r#"
            #line 123 "snuh.h" int a = 3;
        "#
        };
        assert_error(prog);

        // "#" appears on the same line as valid code - invalid
        let prog = indoc! { r#"
            a + 3 + as; #line 123 "foo"
        "#
        };
        assert_error(prog);
    }
}