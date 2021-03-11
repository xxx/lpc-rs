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
    parser::{
        lexer::{
            lex_state::LexState,
            logos_token::{FloatToken, IntToken, StringToken},
        },
        span::Span,
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
    Plus(Span),
    #[token("-", track_slice)]
    Minus(Span),
    #[token("*", track_slice)]
    Mul(Span),
    #[token("/", track_slice)]
    Div(Span),
    #[token("%", track_slice)]
    Mod(Span),
    #[token("!", track_slice)]
    Bang(Span),
    #[token("^", track_slice)]
    Caret(Span),
    #[token("~", track_slice)]
    Tilde(Span),
    #[token("&", track_slice)]
    And(Span),
    #[token("&&", track_slice)]
    AndAnd(Span),
    #[token("|", track_slice)]
    Or(Span),
    #[token("||", track_slice)]
    OrOr(Span),
    #[token("<<", track_slice)]
    LeftShift(Span),
    #[token(">>", track_slice)]
    RightShift(Span),
    #[token("==", track_slice)]
    EqEq(Span),
    #[token("!=", track_slice)]
    NotEq(Span),
    #[token("<", track_slice)]
    LessThan(Span),
    #[token("<=", track_slice)]
    LessThanEq(Span),
    #[token(">", track_slice)]
    GreaterThan(Span),
    #[token(">=", track_slice)]
    GreaterThanEq(Span),

    #[token("=", track_slice)]
    Assign(Span),
    #[token("+=", track_slice)]
    PlusEq(Span),
    #[token("-=", track_slice)]
    MinusEq(Span),
    #[token("*=", track_slice)]
    MulEq(Span),
    #[token("/=", track_slice)]
    DivEq(Span),
    #[token("%=", track_slice)]
    ModEq(Span),
    #[token("^=", track_slice)]
    CaretEq(Span),
    #[token("~=", track_slice)]
    TildeEq(Span),
    #[token("&=", track_slice)]
    AndEq(Span),
    #[token("&&=", track_slice)]
    AndAndEq(Span),
    #[token("|=", track_slice)]
    OrEq(Span),
    #[token("||=", track_slice)]
    OrOrEq(Span),
    #[token("<<=", track_slice)]
    LeftShiftEq(Span),
    #[token(">>=", track_slice)]
    RightShiftEq(Span),

    #[token("if", track_slice)]
    If(Span),
    #[token("else", track_slice)]
    Else(Span),
    #[token("while", track_slice)]
    While(Span),
    #[token("for", track_slice)]
    For(Span),
    #[token("inherit", track_slice)]
    Inherit(Span),
    #[token("break", track_slice)]
    Break(Span),
    #[token("continue", track_slice)]
    Continue(Span),
    #[token("case", track_slice)]
    Case(Span),
    #[token("do", track_slice)]
    Do(Span),
    #[token("int", track_slice)]
    Int(Span),
    #[token("float", track_slice)]
    Float(Span),
    #[token("string", track_slice)]
    String(Span),
    #[token("object", track_slice)]
    Object(Span),
    #[token("mapping", track_slice)]
    Mapping(Span),
    #[token("mixed", track_slice)]
    Mixed(Span),
    #[token("void", track_slice)]
    Void(Span),
    #[token("return", track_slice)]
    Return(Span),
    #[token("static", track_slice)]
    Static(Span),
    #[token("nomask", track_slice)]
    Nomask(Span),
    #[token("efun", track_slice)]
    Efun(Span),

    #[token("(", track_slice)]
    LParen(Span),
    #[token(")", track_slice)]
    RParen(Span),
    #[token("[", track_slice)]
    LBracket(Span),
    #[token("]", track_slice)]
    RBracket(Span),
    #[token("{", track_slice)]
    LBrace(Span),
    #[token("}", track_slice)]
    RBrace(Span),
    #[token(",", track_slice)]
    Comma(Span),
    #[token("->", track_slice)]
    CallOther(Span),
    #[token("?", track_slice)]
    Question(Span),
    #[token(":", track_slice)]
    Colon(Span),
    #[token("::", track_slice)]
    ColonColon(Span),
    #[token(";", track_slice)]
    Semi(Span),
    #[token("...", track_slice)]
    Ellipsis(Span),
    #[token("..", track_slice)]
    Range(Span),

    #[regex(r#""(\\.|[^"])*""#, string_literal)]
    StringLiteral(StringToken),

    #[regex(r"[1-9][0-9_]*", |lex| {
        match i64::from_str(&lex.slice().replace("_", "")) {
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
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
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
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
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[bB][01][01_]*", |lex| {
        let r = i64::from_str_radix(
            &lex.slice().replace("_", "")
                .trim_start_matches("0b")
                .trim_start_matches("0B"),
            2);

        match r {
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
            Err(e) => Err(e)
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
    Token(Span),

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
fn track_slice(lex: &mut Lexer<Token>) -> Span {
    lex.extras.last_slice = String::from(lex.slice());
    Span::new(lex.extras.current_file_id, lex.span())
}

fn id(lex: &mut Lexer<Token>) -> StringToken {
    track_slice(lex);
    StringToken(
        Span::new(lex.extras.current_file_id, lex.span()),
        lex.slice().to_string(),
    )
}

fn string_literal(lex: &mut Lexer<Token>) -> StringToken {
    track_slice(lex);
    let slice = &lex.extras.last_slice;

    let s = if slice.len() < 3 {
        String::from("")
    } else {
        convert_escapes(&slice[1..=(slice.len() - 2)])
    };

    StringToken(Span::new(lex.extras.current_file_id, lex.span()), s)
}

fn float_literal(lex: &mut Lexer<Token>) -> FloatToken {
    track_slice(lex);
    let f = f64::from_str(&lex.slice().replace("_", "")).unwrap();
    FloatToken(Span::new(lex.extras.current_file_id, lex.span()), f)
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

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Plus(s) => *s,
            Token::Minus(s) => *s,
            Token::Mul(s) => *s,
            Token::Div(s) => *s,
            Token::Mod(s) => *s,
            Token::Bang(s) => *s,
            Token::Caret(s) => *s,
            Token::Tilde(s) => *s,
            Token::And(s) => *s,
            Token::AndAnd(s) => *s,
            Token::Or(s) => *s,
            Token::OrOr(s) => *s,
            Token::LeftShift(s) => *s,
            Token::RightShift(s) => *s,
            Token::EqEq(s) => *s,
            Token::NotEq(s) => *s,
            Token::LessThan(s) => *s,
            Token::LessThanEq(s) => *s,
            Token::GreaterThan(s) => *s,
            Token::GreaterThanEq(s) => *s,

            Token::Assign(s) => *s,
            Token::PlusEq(s) => *s,
            Token::MinusEq(s) => *s,
            Token::MulEq(s) => *s,
            Token::DivEq(s) => *s,
            Token::ModEq(s) => *s,
            Token::CaretEq(s) => *s,
            Token::TildeEq(s) => *s,
            Token::AndEq(s) => *s,
            Token::AndAndEq(s) => *s,
            Token::OrEq(s) => *s,
            Token::OrOrEq(s) => *s,
            Token::LeftShiftEq(s) => *s,
            Token::RightShiftEq(s) => *s,

            Token::If(s) => *s,
            Token::Else(s) => *s,
            Token::While(s) => *s,
            Token::For(s) => *s,
            Token::Inherit(s) => *s,
            Token::Break(s) => *s,
            Token::Continue(s) => *s,
            Token::Case(s) => *s,
            Token::Do(s) => *s,
            Token::Int(s) => *s,
            Token::Float(s) => *s,
            Token::String(s) => *s,
            Token::Object(s) => *s,
            Token::Mapping(s) => *s,
            Token::Mixed(s) => *s,
            Token::Void(s) => *s,
            Token::Return(s) => *s,
            Token::Static(s) => *s,
            Token::Nomask(s) => *s,
            Token::Efun(s) => *s,

            Token::LParen(s) => *s,
            Token::RParen(s) => *s,
            Token::LBracket(s) => *s,
            Token::RBracket(s) => *s,
            Token::LBrace(s) => *s,
            Token::RBrace(s) => *s,
            Token::Comma(s) => *s,
            Token::CallOther(s) => *s,
            Token::Question(s) => *s,
            Token::Colon(s) => *s,
            Token::ColonColon(s) => *s,
            Token::Semi(s) => *s,
            Token::Ellipsis(s) => *s,
            Token::Range(s) => *s,
            Token::StringLiteral(s) => s.0,
            Token::IntLiteral(i) => i.0,
            Token::FloatLiteral(fl) => fl.0,
            Token::ID(id) => id.0,
            Token::Token(s) => *s,
            Token::Error => Span::new(0, 0..0),
        }
    }

    pub fn file_id(&self) -> FileId {
        self.span().file_id
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
