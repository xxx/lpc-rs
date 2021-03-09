use std::{
    fmt,
    fmt::{Display, Formatter},
    str::FromStr,
};

use lazy_static::lazy_static;
use logos::{Filter, Lexer, Logos};
use regex::Regex;

use crate::{
    convert_escapes,
    errors::{
        compiler_error::lex_error::LexError,
        lazy_files::{add_file_to_cache, FileId},
    },
    parser::lexer::{
        lex_state::LexState,
        logos_token::{FloatToken, IntToken, StringToken},
    },
};

pub mod lex_state;
pub mod logos_token;

pub type Spanned<T> = (usize, T, usize);

/// A wrapper to attach our `Iterator` implementation to.
pub struct LexWrapper<'input> {
    lexer: Lexer<'input, Token>,
}

impl Iterator for LexWrapper<'_> {
    type Item = Result<Spanned<Token>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;
        let span = self.lexer.span();

        match token {
            Token::Error => Some(Err(LexError(format!(
                "Invalid Token `{}`at {:?}",
                self.lexer.slice(),
                span
            )))),
            t => Some(Ok((span.start, t, span.end))),
        }
    }
}

impl<'input> LexWrapper<'input> {
    pub fn new(prog: &'input str) -> LexWrapper<'input> {
        let lexer = Token::lexer(prog);
        Self { lexer }
    }

    #[inline]
    pub fn set_current_file_id(&mut self, file_id: FileId) {
        self.lexer.extras.current_file_id = file_id;
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexState)]
pub enum Token {
    #[token("+", track_slice)]
    Plus(FileId),
    #[token("-", track_slice)]
    Minus(FileId),
    #[token("*", track_slice)]
    Mul(FileId),
    #[token("/", track_slice)]
    Div(FileId),
    #[token("%", track_slice)]
    Mod(FileId),
    #[token("!", track_slice)]
    Bang(FileId),
    #[token("^", track_slice)]
    Caret(FileId),
    #[token("~", track_slice)]
    Tilde(FileId),
    #[token("&", track_slice)]
    And(FileId),
    #[token("&&", track_slice)]
    AndAnd(FileId),
    #[token("|", track_slice)]
    Or(FileId),
    #[token("||", track_slice)]
    OrOr(FileId),
    #[token("<<", track_slice)]
    LeftShift(FileId),
    #[token(">>", track_slice)]
    RightShift(FileId),
    #[token("==", track_slice)]
    EqEq(FileId),
    #[token("!=", track_slice)]
    NotEq(FileId),
    #[token("<", track_slice)]
    LessThan(FileId),
    #[token("<=", track_slice)]
    LessThanEq(FileId),
    #[token(">", track_slice)]
    GreaterThan(FileId),
    #[token(">=", track_slice)]
    GreaterThanEq(FileId),

    #[token("=", track_slice)]
    Assign(FileId),
    #[token("+=", track_slice)]
    PlusEq(FileId),
    #[token("-=", track_slice)]
    MinusEq(FileId),
    #[token("*=", track_slice)]
    MulEq(FileId),
    #[token("/=", track_slice)]
    DivEq(FileId),
    #[token("%=", track_slice)]
    ModEq(FileId),
    #[token("^=", track_slice)]
    CaretEq(FileId),
    #[token("~=", track_slice)]
    TildeEq(FileId),
    #[token("&=", track_slice)]
    AndEq(FileId),
    #[token("&&=", track_slice)]
    AndAndEq(FileId),
    #[token("|=", track_slice)]
    OrEq(FileId),
    #[token("||=", track_slice)]
    OrOrEq(FileId),
    #[token("<<=", track_slice)]
    LeftShiftEq(FileId),
    #[token(">>=", track_slice)]
    RightShiftEq(FileId),

    #[token("if", track_slice)]
    If(FileId),
    #[token("else", track_slice)]
    Else(FileId),
    #[token("while", track_slice)]
    While(FileId),
    #[token("for", track_slice)]
    For(FileId),
    #[token("inherit", track_slice)]
    Inherit(FileId),
    #[token("break", track_slice)]
    Break(FileId),
    #[token("continue", track_slice)]
    Continue(FileId),
    #[token("case", track_slice)]
    Case(FileId),
    #[token("do", track_slice)]
    Do(FileId),
    #[token("int", track_slice)]
    Int(FileId),
    #[token("float", track_slice)]
    Float(FileId),
    #[token("string", track_slice)]
    String(FileId),
    #[token("object", track_slice)]
    Object(FileId),
    #[token("mapping", track_slice)]
    Mapping(FileId),
    #[token("mixed", track_slice)]
    Mixed(FileId),
    #[token("void", track_slice)]
    Void(FileId),
    #[token("return", track_slice)]
    Return(FileId),
    #[token("static", track_slice)]
    Static(FileId),
    #[token("nomask", track_slice)]
    Nomask(FileId),
    #[token("efun", track_slice)]
    Efun(FileId),

    #[token("(", track_slice)]
    LParen(FileId),
    #[token(")", track_slice)]
    RParen(FileId),
    #[token("[", track_slice)]
    LBracket(FileId),
    #[token("]", track_slice)]
    RBracket(FileId),
    #[token("{", track_slice)]
    LBrace(FileId),
    #[token("}", track_slice)]
    RBrace(FileId),
    #[token(",", track_slice)]
    Comma(FileId),
    #[token("->", track_slice)]
    CallOther(FileId),
    #[token("?", track_slice)]
    Question(FileId),
    #[token(":", track_slice)]
    Colon(FileId),
    #[token("::", track_slice)]
    ColonColon(FileId),
    #[token(";", track_slice)]
    Semi(FileId),
    #[token("...", track_slice)]
    Ellipsis(FileId),
    #[token("..", track_slice)]
    Range(FileId),

    #[regex(r#""(\\.|[^"])*""#, string_literal)]
    StringLiteral(StringToken),

    #[regex(r"[1-9][0-9_]*", |lex| {
        match i64::from_str(&lex.slice().replace("_", "")) {
            Ok(i) => Ok(IntToken(lex.extras.current_file_id, i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", |lex| {
        let r = i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0x")
                .trim_start_matches("0X"),
            16);

        match r {
            Ok(i) => Ok(IntToken(lex.extras.current_file_id, i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[oO]?[0-7][0-7_]*", |lex| {
        let r = i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0o")
                .trim_start_matches("0O"),
            8);

        match r {
            Ok(i) => Ok(IntToken(lex.extras.current_file_id, i)),
            Err(e) => return Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[bB][01][01_]*", |lex| {
        let r = i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0b")
                .trim_start_matches("0B"),
            2);

        match r {
            Ok(i) => Ok(IntToken(lex.extras.current_file_id, i)),
            Err(e) => return Err(e)
        }
    }, priority = 2)]
    IntLiteral(IntToken),

    #[regex(
        r#"[0-9][0-9_]*\.[0-9][0-9_]*(?:[eE][-+]?[0-9][0-9_]*)?"#,
        float_literal
    )]
    FloatLiteral(FloatToken),

    #[regex(r"[\p{Alphabetic}_]\w*", id, priority = 2)]
    ID(StringToken),

    #[regex(".", track_slice)]
    Token(FileId),

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
    // Strip whitespace and comments
    #[regex(r"[ \t\f\v]+|//[^\n\r]*[\n\r]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", |lex| {
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

#[inline]
fn track_slice(lex: &mut Lexer<Token>) -> FileId {
    lex.extras.last_slice = String::from(lex.slice());
    lex.extras.current_file_id
}

fn id(lex: &mut Lexer<Token>) -> StringToken {
    track_slice(lex);
    StringToken(lex.extras.current_file_id, lex.slice().to_string())
}

fn string_literal(lex: &mut Lexer<Token>) -> StringToken {
    track_slice(lex);
    let slice = &lex.extras.last_slice;

    let s = if slice.len() < 3 {
        String::from("")
    } else {
        convert_escapes(&slice[1..=(slice.len() - 2)])
    };

    StringToken(lex.extras.current_file_id, s)
}

fn float_literal(lex: &mut Lexer<Token>) -> FloatToken {
    track_slice(lex);
    let f = f64::from_str(&lex.slice().replace("_", "")).unwrap();
    FloatToken(lex.extras.current_file_id, f)
}

fn line(lex: &mut Lexer<Token>) -> Option<()> {
    lazy_static! {
        static ref LINE: Regex = Regex::new(r#"\A#\s*line\s+(\d+)\s+"([^"]+?)"\s*\z"#).unwrap();
        static ref EOL: Regex = Regex::new(r#"\s*?\n\s*\z"#).unwrap();
    }

    // test if '#' is exactly at the start of a line
    if lex.extras.last_slice != "\n" && !lex.extras.last_slice.is_empty() && lex.span().start != 0 {
        return None;
    }

    let eof = lex.span().end == lex.source().len();

    let slice = lex.slice();
    let extra_junk = !EOL.is_match(slice) && !eof;

    if extra_junk {
        return None;
    }

    if let Some(captures) = LINE.captures(slice) {
        let id = add_file_to_cache(&captures[2]);
        lex.extras.current_file_id = id;
        lex.extras.current_line = usize::from_str(&captures[1]).unwrap();

        Some(())
    } else {
        None
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let _s: String;

        let out = match self {
            Token::Plus(_) => "+",
            Token::Minus(_) => "-",
            Token::Mul(_) => "*",
            Token::Div(_) => "/",
            Token::Mod(_) => "%",
            Token::Bang(_) => "!",
            Token::Caret(_) => "^",
            Token::Tilde(_) => "~",
            Token::And(_) => "&",
            Token::AndAnd(_) => "&&",
            Token::Or(_) => "|",
            Token::OrOr(_) => "||",
            Token::LeftShift(_) => "<<",
            Token::RightShift(_) => ">>",
            Token::EqEq(_) => "==",
            Token::NotEq(_) => "!=",
            Token::LessThan(_) => "<",
            Token::LessThanEq(_) => "<=",
            Token::GreaterThan(_) => ">",
            Token::GreaterThanEq(_) => ">=",

            Token::Assign(_) => "=",
            Token::PlusEq(_) => "+=",
            Token::MinusEq(_) => "-=",
            Token::MulEq(_) => "||",
            Token::DivEq(_) => "/=",
            Token::ModEq(_) => "%=",
            Token::CaretEq(_) => "^=",
            Token::TildeEq(_) => "~=",
            Token::AndEq(_) => "&=",
            Token::AndAndEq(_) => "&&=",
            Token::OrEq(_) => "|=",
            Token::OrOrEq(_) => "||=",
            Token::LeftShiftEq(_) => "<<=",
            Token::RightShiftEq(_) => ">>=",

            Token::If(_) => "if",
            Token::Else(_) => "else",
            Token::While(_) => "while",
            Token::For(_) => "for",
            Token::Inherit(_) => "inherit",
            Token::Break(_) => "break",
            Token::Continue(_) => "continue",
            Token::Case(_) => "case",
            Token::Do(_) => "do",
            Token::Int(_) => "int",
            Token::Float(_) => "float",
            Token::String(_) => "string",
            Token::Object(_) => "object",
            Token::Mapping(_) => "mapping",
            Token::Mixed(_) => "mixed",
            Token::Void(_) => "void",
            Token::Return(_) => "return",
            Token::Static(_) => "static",
            Token::Nomask(_) => "nomask",
            Token::Efun(_) => "efun",

            Token::LParen(_) => "(",
            Token::RParen(_) => ")",
            Token::LBracket(_) => "[",
            Token::RBracket(_) => "]",
            Token::LBrace(_) => "{",
            Token::RBrace(_) => "}",
            Token::Comma(_) => ",",
            Token::CallOther(_) => "->",
            Token::Question(_) => "?",
            Token::Colon(_) => ":",
            Token::ColonColon(_) => "::",
            Token::Semi(_) => ";",
            Token::Ellipsis(_) => "...",
            Token::Range(_) => "..",
            Token::StringLiteral(s) => &s.1,
            Token::IntLiteral(i) => return write!(f, "{}", i.1),
            Token::FloatLiteral(fl) => return write!(f, "{}", fl.1),
            Token::ID(id) => &id.1,
            Token::Token(_) => "Unknown token",
            Token::Error => "Error token",
        };

        write!(f, "{}", out)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    fn lex_vec(prog: &str) -> Vec<Result<Spanned<Token>, LexError>> {
        let lexer = LexWrapper::new(prog);
        lexer.collect::<Vec<_>>()
    }

    fn into_errors(
        v: Vec<Result<Spanned<Token>, LexError>>,
    ) -> Vec<Result<Spanned<Token>, LexError>> {
        v.into_iter()
            .filter(|i| matches!(*i, Err(LexError(_))))
            .collect()
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

    #[test]
    fn test_strip_comments() {
        let prog = r#"
            // foo bar baz
            /* foo bar
                int j = 2342323;
                */
        "#;

        let vec = lex_vec(prog);

        assert!(vec.is_empty());
    }
}
