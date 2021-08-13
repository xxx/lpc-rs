use std::{
    fmt,
    fmt::{Display, Formatter},
    str::FromStr,
};

use logos::{Lexer, Logos};

use crate::{
    convert_escapes,
    errors::{lazy_files::FileId, LpcError},
    parser::{
        lexer::{
            lex_state::LexState,
            logos_token::{FloatToken, IntToken, StringToken},
        },
        span::Span,
    },
    BaseFloat, LpcInt, Result,
};

pub mod lex_state;
pub mod logos_token;

pub type Spanned<T> = (usize, T, usize);

/// A wrapper for the Lexer to attach our `Iterator` implementation to,
/// which allows us to output items that match the shape of the tuples
/// expected by lalrpop.
pub struct LexWrapper<'input> {
    lexer: Lexer<'input, Token>,
}

impl<'input> LexWrapper<'input> {
    pub fn new(prog: &'input str) -> LexWrapper<'input> {
        let lexer = Token::lexer(prog);
        Self { lexer }
    }

    pub fn set_file_id(&mut self, id: FileId) {
        self.lexer.extras.current_file_id = id;
    }
}

impl Iterator for LexWrapper<'_> {
    type Item = Result<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;
        let span = self.lexer.span();

        match token {
            Token::Error => Some(Err(LpcError::new(format!(
                "Lex Error: Invalid Token `{}`  at {:?}",
                self.lexer.slice(),
                span
            )))),
            t => Some(Ok((span.start, t, span.end))),
        }
    }
}

/// A wrapper for vectors of tokens, for lalrpop compatibility
pub struct TokenVecWrapper<'a> {
    vec: &'a Vec<Spanned<Token>>,
    count: usize,
}

impl<'a> TokenVecWrapper<'a> {
    #[allow(clippy::ptr_arg)]
    pub fn new(vec: &'a Vec<Spanned<Token>>) -> Self {
        Self { vec, count: 0 }
    }
}

impl<'a> Iterator for TokenVecWrapper<'a> {
    type Item = Result<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.vec.get(self.count);

        let token = match token {
            Some(t) => t,
            None => return None,
        };

        self.count += 1;

        let span = token.1.span();

        match &token.1 {
            Token::Error => Some(Err(LpcError::new(format!(
                "Lex Error: Invalid Token `{}` at {:?}",
                token.1, span
            )))),
            t => Some(Ok((span.l, t.clone(), span.r))),
        }
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
    AddEq(Span),
    #[token("-=", track_slice)]
    SubEq(Span),
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
    #[token("varargs", track_slice)]
    Varargs(Span),
    #[token("nomask", track_slice)]
    Nomask(Span),
    #[token("efun", track_slice)]
    Efun(Span),
    #[token("catch", track_slice)]
    Catch(Span),
    #[token("switch", track_slice)]
    Switch(Span),
    #[token("default", track_slice)]
    Default(Span),
    #[token("foreach", track_slice)]
    Foreach(Span),
    #[token("function", track_slice)]
    Function(Span),
    #[token("private", track_slice)]
    Private(Span),
    #[token("public", track_slice)]
    Public(Span),

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

    #[token("\n", track_slice, priority = 3)]
    NewLine(Span),

    #[regex(r#""(\\.|[^"])*""#, string_token_without_startend)]
    StringLiteral(StringToken),

    // Allow multiple characters so any Unicode scalar can be matched.
    #[regex(r#"'(\\.|[^'])+'"#, |lex| {
    track_slice(lex);

    let span = Span::new(lex.extras.current_file_id, lex.span());

    match lex.slice().chars().nth(1) {
        Some(c) => Ok(IntToken(span, c as LpcInt)),
        None => {
            Err(LpcError::new(
                format!("Unable to find the character in token `{}`? This is a WTF.", lex.slice())
            ).with_span(Some(span)))
        }
    }
    })]
    #[regex(r"[1-9][0-9_]*|0", |lex| {
        track_slice(lex);

        match LpcInt::from_str(&lex.slice().replace("_", "")) {
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", |lex| {
        track_slice(lex);

        let r = LpcInt::from_str_radix(
            lex.slice().replace("_", "")
                .trim_start_matches("0x")
                .trim_start_matches("0X"),
            16);

        match r {
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[oO]?[0-7][0-7_]*", |lex| {
        track_slice(lex);

        let r = LpcInt::from_str_radix(
            lex.slice().replace("_", "")
                .trim_start_matches("0o")
                .trim_start_matches("0O"),
            8);

        match r {
            Ok(i) => Ok(IntToken(Span::new(lex.extras.current_file_id, lex.span()), i)),
            Err(e) => Err(e)
        }
    }, priority = 2)]
    #[regex(r"0[bB][01][01_]*", |lex| {
        track_slice(lex);

        let r = LpcInt::from_str_radix(
            lex.slice().replace("_", "")
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

    #[regex(r"[\p{Alphabetic}_]\w*", string_token, priority = 2)]
    Id(StringToken),

    // Preprocessor "tokens" just grab the entire line
    #[regex("#[^\n\\S]*include[^\n\\S]+\"[^\"]+\"[^\n]*\n?", string_token)]
    LocalInclude(StringToken),

    #[regex("#[^\n\\S]*include[^\n\\S]+<[^>]+>[^\n]*\n?", string_token)]
    SysInclude(StringToken),

    #[regex("#[^\n\\S]*if[^\n]*\n?", string_token)]
    PreprocessorIf(StringToken),

    #[regex("#[^\n\\S]*ifdef[^\n]*\n?", string_token)]
    IfDef(StringToken),

    #[regex("#[^\n\\S]*ifndef[^\n]*\n?", string_token)]
    IfNDef(StringToken),

    #[regex("#[^\n\\S]*else[^\n]*\n?", string_token)]
    PreprocessorElse(StringToken),

    #[regex("#[^\n\\S]*endif[^\n]*\n?", string_token)]
    Endif(StringToken),

    #[regex("#[^\n\\S]*define[^\n]*\n?", string_token)]
    Define(StringToken),

    #[regex("#[^\n\\S]*undef[^\n]*\n?", string_token)]
    Undef(StringToken),

    #[token("defined", track_slice)]
    Defined(Span),
    #[token("defined(", track_slice)]
    DefinedParen(Span),

    #[token("not", track_slice)]
    Not(Span),

    #[regex("#[^\n\\S]*pragma[^\n]*\n?", string_token)]
    Pragma(StringToken),

    #[error]
    // Strip whitespace and comments
    #[regex(r"[ \t\f\v]+|//[^\n\r]*[\n\r]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", |lex| {
        track_slice(lex);
        logos::Skip
    }, priority = 2)]
    #[token("\n", |lex| {
        track_slice(lex);
        logos::Skip
    })]
    Error,
}

#[inline]
fn track_slice(lex: &mut Lexer<Token>) -> Span {
    let slice = lex.slice();

    // For the span, we do not want to include any trailing newlines,
    // else they show up in error messages.
    let newline_count = slice.to_string().matches('\n').count();
    let span = lex.span();

    lex.extras.last_slice = slice.to_string();
    Span::new(
        lex.extras.current_file_id,
        span.start..(span.end - newline_count),
    )
}

fn string_token(lex: &mut Lexer<Token>) -> StringToken {
    let span = track_slice(lex);

    StringToken(span, lex.extras.last_slice.clone())
}

/// Strip off the start and end characters of a string, then store the result in a [`StringToken`].
/// Used for processing string literals and include paths.
fn string_token_without_startend(lex: &mut Lexer<Token>) -> StringToken {
    track_slice(lex);
    let slice = &lex.extras.last_slice;

    let s = if slice.len() < 3 {
        String::from("")
    } else {
        convert_escapes(&slice[1..=(slice.len() - 2)])
    };

    StringToken(Span::new(lex.extras.current_file_id, lex.span()), s)
}

/// Track and convert float literals to [`FloatToken`]s
fn float_literal(lex: &mut Lexer<Token>) -> FloatToken {
    track_slice(lex);
    let f = BaseFloat::from_str(&lex.slice().replace("_", "")).unwrap();
    FloatToken(Span::new(lex.extras.current_file_id, lex.span()), f)
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Plus(x)
            | Token::Minus(x)
            | Token::Mul(x)
            | Token::Div(x)
            | Token::Mod(x)
            | Token::Bang(x)
            | Token::Caret(x)
            | Token::Tilde(x)
            | Token::And(x)
            | Token::AndAnd(x)
            | Token::Or(x)
            | Token::OrOr(x)
            | Token::LeftShift(x)
            | Token::RightShift(x)
            | Token::EqEq(x)
            | Token::NotEq(x)
            | Token::LessThan(x)
            | Token::LessThanEq(x)
            | Token::GreaterThan(x)
            | Token::GreaterThanEq(x)
            | Token::Assign(x)
            | Token::AddEq(x)
            | Token::SubEq(x)
            | Token::MulEq(x)
            | Token::DivEq(x)
            | Token::ModEq(x)
            | Token::CaretEq(x)
            | Token::TildeEq(x)
            | Token::AndEq(x)
            | Token::AndAndEq(x)
            | Token::OrEq(x)
            | Token::OrOrEq(x)
            | Token::LeftShiftEq(x)
            | Token::RightShiftEq(x)
            | Token::If(x)
            | Token::Else(x)
            | Token::While(x)
            | Token::For(x)
            | Token::Inherit(x)
            | Token::Break(x)
            | Token::Continue(x)
            | Token::Case(x)
            | Token::Do(x)
            | Token::Int(x)
            | Token::Float(x)
            | Token::String(x)
            | Token::Object(x)
            | Token::Mapping(x)
            | Token::Mixed(x)
            | Token::Void(x)
            | Token::Return(x)
            | Token::Static(x)
            | Token::Varargs(x)
            | Token::Nomask(x)
            | Token::Efun(x)
            | Token::LParen(x)
            | Token::RParen(x)
            | Token::LBracket(x)
            | Token::RBracket(x)
            | Token::LBrace(x)
            | Token::RBrace(x)
            | Token::Comma(x)
            | Token::CallOther(x)
            | Token::Question(x)
            | Token::Colon(x)
            | Token::ColonColon(x)
            | Token::Semi(x)
            | Token::Ellipsis(x)
            | Token::Range(x)
            | Token::NewLine(x)
            | Token::StringLiteral(StringToken(x, _))
            | Token::IntLiteral(IntToken(x, _))
            | Token::FloatLiteral(FloatToken(x, _))
            | Token::Id(StringToken(x, _))
            | Token::LocalInclude(StringToken(x, _))
            | Token::SysInclude(StringToken(x, _))
            | Token::PreprocessorIf(StringToken(x, _))
            | Token::IfDef(StringToken(x, _))
            | Token::IfNDef(StringToken(x, _))
            | Token::PreprocessorElse(StringToken(x, _))
            | Token::Endif(StringToken(x, _))
            | Token::Define(StringToken(x, _))
            | Token::Undef(StringToken(x, _))
            | Token::Defined(x)
            | Token::DefinedParen(x)
            | Token::Not(x)
            | Token::Pragma(StringToken(x, _))
            | Token::Catch(x)
            | Token::Switch(x)
            | Token::Default(x)
            | Token::Foreach(x)
            | Token::Function(x)
            | Token::Private(x)
            | Token::Public(x) => *x,
            Token::Error => Span::new(0, 0..0),
        }
    }

    /// A helper to allow us to correct spans, for cases when we lex `#define`d
    /// values for macro expansion.
    fn span_ref(&mut self) -> Option<&mut Span> {
        let span = match self {
            Token::Plus(x)
            | Token::Minus(x)
            | Token::Mul(x)
            | Token::Div(x)
            | Token::Mod(x)
            | Token::Bang(x)
            | Token::Caret(x)
            | Token::Tilde(x)
            | Token::And(x)
            | Token::AndAnd(x)
            | Token::Or(x)
            | Token::OrOr(x)
            | Token::LeftShift(x)
            | Token::RightShift(x)
            | Token::EqEq(x)
            | Token::NotEq(x)
            | Token::LessThan(x)
            | Token::LessThanEq(x)
            | Token::GreaterThan(x)
            | Token::GreaterThanEq(x)
            | Token::Assign(x)
            | Token::AddEq(x)
            | Token::SubEq(x)
            | Token::MulEq(x)
            | Token::DivEq(x)
            | Token::ModEq(x)
            | Token::CaretEq(x)
            | Token::TildeEq(x)
            | Token::AndEq(x)
            | Token::AndAndEq(x)
            | Token::OrEq(x)
            | Token::OrOrEq(x)
            | Token::LeftShiftEq(x)
            | Token::RightShiftEq(x)
            | Token::If(x)
            | Token::Else(x)
            | Token::While(x)
            | Token::For(x)
            | Token::Inherit(x)
            | Token::Break(x)
            | Token::Continue(x)
            | Token::Case(x)
            | Token::Do(x)
            | Token::Int(x)
            | Token::Float(x)
            | Token::String(x)
            | Token::Object(x)
            | Token::Mapping(x)
            | Token::Mixed(x)
            | Token::Void(x)
            | Token::Return(x)
            | Token::Static(x)
            | Token::Varargs(x)
            | Token::Nomask(x)
            | Token::Efun(x)
            | Token::LParen(x)
            | Token::RParen(x)
            | Token::LBracket(x)
            | Token::RBracket(x)
            | Token::LBrace(x)
            | Token::RBrace(x)
            | Token::Comma(x)
            | Token::CallOther(x)
            | Token::Question(x)
            | Token::Colon(x)
            | Token::ColonColon(x)
            | Token::Semi(x)
            | Token::Ellipsis(x)
            | Token::Range(x)
            | Token::NewLine(x)
            | Token::StringLiteral(StringToken(x, _))
            | Token::IntLiteral(IntToken(x, _))
            | Token::FloatLiteral(FloatToken(x, _))
            | Token::Id(StringToken(x, _))
            | Token::LocalInclude(StringToken(x, _))
            | Token::SysInclude(StringToken(x, _))
            | Token::PreprocessorIf(StringToken(x, _))
            | Token::IfDef(StringToken(x, _))
            | Token::IfNDef(StringToken(x, _))
            | Token::PreprocessorElse(StringToken(x, _))
            | Token::Endif(StringToken(x, _))
            | Token::Define(StringToken(x, _))
            | Token::Undef(StringToken(x, _))
            | Token::Defined(x)
            | Token::DefinedParen(x)
            | Token::Not(x)
            | Token::Pragma(StringToken(x, _))
            | Token::Catch(x)
            | Token::Switch(x)
            | Token::Default(x)
            | Token::Foreach(x)
            | Token::Function(x)
            | Token::Private(x)
            | Token::Public(x) => x,
            Token::Error => return None,
        };

        Some(span)
    }

    /// Allow directly setting a new span on a token
    pub fn with_span(mut self, new_span: Span) -> Self {
        if let Some(span) = self.span_ref() {
            span.file_id = new_span.file_id;
            span.l = new_span.l;
            span.r = new_span.r;
        }

        self
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
            Token::AddEq(_) => "+=",
            Token::SubEq(_) => "-=",
            Token::MulEq(_) => "*=",
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
            Token::Varargs(_) => "varargs",
            Token::Nomask(_) => "nomask",
            Token::Efun(_) => "efun",
            Token::Catch(_) => "catch",
            Token::Switch(_) => "switch",
            Token::Default(_) => "default",
            Token::Foreach(_) => "foreach",
            Token::Function(_) => "function",
            Token::Private(_) => "private",
            Token::Public(_) => "public",

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
            Token::NewLine(_) => "\n",
            Token::IntLiteral(i) => return write!(f, "{}", i.1),
            Token::FloatLiteral(fl) => return write!(f, "{}", fl.1),

            Token::StringLiteral(s)
            | Token::Id(s)
            | Token::LocalInclude(s)
            | Token::SysInclude(s)
            | Token::PreprocessorIf(s)
            | Token::IfDef(s)
            | Token::IfNDef(s)
            | Token::PreprocessorElse(s)
            | Token::Endif(s)
            | Token::Define(s)
            | Token::Undef(s)
            | Token::Pragma(s) => &s.1,

            Token::Defined(_) => "defined",
            Token::DefinedParen(_) => "defined(",
            Token::Not(_) => "not",

            Token::Error => "Error token",
        };

        write!(f, "{}", out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_vec(prog: &str) -> Vec<Result<Spanned<Token>>> {
        let lexer = LexWrapper::new(prog);
        lexer
            .filter(|i| !matches!(i, Ok((_, Token::NewLine(..), _))))
            .collect::<Vec<_>>()
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
