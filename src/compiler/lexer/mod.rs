use std::{
    fmt,
    fmt::{Debug, Display, Formatter},
    str::FromStr,
};

use logos::{Lexer, Logos};
use lpc_rs_core::{BaseFloat, LpcIntInner, convert_escapes};
use lpc_rs_errors::{
    Result, lpc_error,
    source_map::FileId,
    span::{HasSpan, Span},
};

use crate::compiler::lexer::{
    lex_state::LexState,
    logos_token::{FloatToken, IntToken, StringToken},
};

pub mod lex_state;
pub mod logos_token;

/// The lalrpop location triple. It exists only at the parser doorstep
/// ([`LexWrapper::triples`], [`TokenTriples`]); everywhere else the
/// token's own [`Span`] is the position channel.
pub type Spanned<T> = (usize, T, usize);

/// The language lexer over one file's text (or a fragment of it, via
/// [`new_at`](LexWrapper::new_at)): tokens are born carrying their true
/// file-coordinate spans.
pub struct LexWrapper<'input> {
    lexer: Lexer<'input, Token>,
}

impl<'input> LexWrapper<'input> {
    /// Lex `prog` as the full text of `file_id`.
    pub fn new(prog: &'input str, file_id: FileId) -> LexWrapper<'input> {
        Self::new_at(prog, file_id, 0)
    }

    /// Lex a fragment of `file_id`'s text that starts at byte `base`:
    /// every span this lexer births — token and lex-error alike — is
    /// offset into file coordinates.
    pub fn new_at(prog: &'input str, file_id: FileId, base: usize) -> LexWrapper<'input> {
        let lexer = Token::lexer_with_extras(
            prog,
            LexState {
                last_slice: String::new(),
                current_file_id: file_id,
                base_offset: base,
            },
        );
        Self { lexer }
    }

    /// The lalrpop doorstep for direct lexing (tests and doctests):
    /// location triples derived from each token's span.
    pub fn triples(self) -> impl Iterator<Item = Result<Spanned<Token>>> {
        self.map(|result| {
            result.map(|t| {
                let span = t.span();
                (span.l(), t, span.r())
            })
        })
    }
}

impl Debug for LexWrapper<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "LexWrapper {{ <lexer> }}")
    }
}

impl Iterator for LexWrapper<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next()?;

        match token {
            Ok(t) => Some(Ok(t)),
            Err(_) => {
                let span = self.lexer.span();
                let base = self.lexer.extras.base_offset;
                Some(Err(lpc_error!(
                    Some(Span::new(
                        self.lexer.extras.current_file_id,
                        base + span.start..base + span.end,
                    )),
                    "Lex Error: Invalid Token `{}`",
                    self.lexer.slice(),
                )))
            }
        }
    }
}

/// The lalrpop doorstep for the scan product: location triples derived
/// from each token's span.
#[derive(Debug)]
pub struct TokenTriples<'a> {
    tokens: &'a [Token],
    count: usize,
}

impl<'a> TokenTriples<'a> {
    /// Wrap a scan product for the parser.
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, count: 0 }
    }
}

impl Iterator for TokenTriples<'_> {
    type Item = Result<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.tokens.get(self.count)?;
        self.count += 1;
        let span = t.span();
        Some(Ok((span.l(), t.clone(), span.r())))
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = LexState)]
// Strip whitespace and comments
#[logos(skip r"[ \t\f\v]+|//[^\n\r]*?[\n\r]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/")]
#[logos(skip r"\n")]
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
    #[token("++", track_slice)]
    Inc(Span),
    #[token("--", track_slice)]
    Dec(Span),

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
    #[token("@", track_slice)]
    Compose(Span),
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
    #[token("ref", track_slice)]
    Ref(Span),
    #[token("efun", track_slice)]
    Efun(Span),
    #[token("switch", track_slice)]
    Switch(Span),
    #[token("default", track_slice)]
    Default(Span),
    #[token("foreach", track_slice)]
    ForEach(Span),
    #[token("function", track_slice)]
    Function(Span),
    #[token("private", track_slice)]
    Private(Span),
    #[token("public", track_slice)]
    Public(Span),
    #[token("protected", track_slice)]
    Protected(Span),

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

    #[regex(r#""(\\.|[^"])*""#, string_token_without_startend)]
    StringLiteral(StringToken),

    // Allow multiple bytes so any Unicode scalar can be matched.
    #[regex(r#"'(\\.|[^']){1,4}'"#, |lex| {
    let span = track_slice(lex);

    match lex.slice().chars().nth(1) {
        Some(c) => Ok(IntToken(span, c as LpcIntInner)),
        None => {
            Err(())
            // Err(LpcError::bug(
            //     format!("Unable to find the character in token `{}`? This is a WTF.", lex.slice())
            // ).with_span(Some(span)))
        }
    }
    })]
    #[regex(r"[1-9][0-9_]*|0", |lex| {
        let span = track_slice(lex);

        match LpcIntInner::from_str(&lex.slice().replace('_', "")) {
            Ok(i) => Ok(IntToken(span, i)),
            Err(_e) => Err(())
        }
    }, priority = 2)]
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", |lex| {
        let span = track_slice(lex);

        let r = LpcIntInner::from_str_radix(
            lex.slice().replace('_', "")
                .trim_start_matches("0x")
                .trim_start_matches("0X"),
            16);

        match r {
            Ok(i) => Ok(IntToken(span, i)),
            Err(_e) => Err(())
        }
    }, priority = 2)]
    #[regex(r"0[oO]?[0-7][0-7_]*", |lex| {
        let span = track_slice(lex);

        let r = LpcIntInner::from_str_radix(
            lex.slice().replace('_', "")
                .trim_start_matches("0o")
                .trim_start_matches("0O"),
            8);

        match r {
            Ok(i) => Ok(IntToken(span, i)),
            Err(_e) => Err(())
        }
    }, priority = 2)]
    #[regex(r"0[bB][01][01_]*", |lex| {
        let span = track_slice(lex);

        let r = LpcIntInner::from_str_radix(
            lex.slice().replace('_', "")
                .trim_start_matches("0b")
                .trim_start_matches("0B"),
            2);

        match r {
            Ok(i) => Ok(IntToken(span, i)),
            Err(_e) => Err(())
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

    #[regex(r"\$[1-9]\d*", string_token, priority = 2)]
    ClosureArgVar(StringToken),

    // A `#` grabs the whole line: one token, and the directive grammar
    // (`preprocessor::directive`) owns everything after the `#`. Whether
    // it is actually a directive is positional — the scan loop judges
    // placement — mid-line and dead it is plain text.
    #[regex("#[^\n]*\n?", string_token, allow_greedy = true)]
    DirectiveLine(StringToken),
}

#[inline]
fn track_slice(lex: &mut Lexer<Token>) -> Span {
    let slice = lex.slice();
    let span = lex.span();
    let base = lex.extras.base_offset;

    // A trailing newline never belongs in a caret; only `DirectiveLine`'s
    // grab can consume one, and its regex admits exactly one.
    let end = span.end - usize::from(slice.ends_with('\n'));

    lex.extras.last_slice = slice.to_string();
    Span::new(lex.extras.current_file_id, base + span.start..base + end)
}

fn string_token(lex: &mut Lexer<Token>) -> StringToken {
    let span = track_slice(lex);

    StringToken(span, lex.extras.last_slice.clone())
}

/// Strip off the start and end characters of a string, then store the result in
/// a [`StringToken`]. Used for processing string literals and include paths.
fn string_token_without_startend(lex: &mut Lexer<Token>) -> StringToken {
    let span = track_slice(lex);
    let slice: &str = &lex.extras.last_slice;

    let s = if slice.len() < 3 {
        String::from("")
    } else {
        convert_escapes(&slice[1..=(slice.len() - 2)])
    };

    StringToken(span, s)
}

/// Track and convert float literals to [`FloatToken`]s
fn float_literal(lex: &mut Lexer<Token>) -> FloatToken {
    let span = track_slice(lex);
    let f = BaseFloat::from_str(&lex.slice().replace('_', "")).unwrap();
    FloatToken(span, f)
}

impl HasSpan for Token {
    fn span(&self) -> Span {
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
            | Token::Inc(x)
            | Token::Dec(x)
            | Token::Assign(x)
            | Token::AddEq(x)
            | Token::SubEq(x)
            | Token::MulEq(x)
            | Token::DivEq(x)
            | Token::ModEq(x)
            | Token::CaretEq(x)
            | Token::AndEq(x)
            | Token::AndAndEq(x)
            | Token::OrEq(x)
            | Token::OrOrEq(x)
            | Token::LeftShiftEq(x)
            | Token::RightShiftEq(x)
            | Token::Compose(x)
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
            | Token::Ref(x)
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
            | Token::StringLiteral(StringToken(x, _))
            | Token::IntLiteral(IntToken(x, _))
            | Token::FloatLiteral(FloatToken(x, _))
            | Token::Id(StringToken(x, _))
            | Token::ClosureArgVar(StringToken(x, _))
            | Token::DirectiveLine(StringToken(x, _))
            | Token::Switch(x)
            | Token::Default(x)
            | Token::ForEach(x)
            | Token::Function(x)
            | Token::Private(x)
            | Token::Public(x)
            | Token::Protected(x) => *x,
        }
    }
}

impl Token {
    /// The use-site collapse: macro expansion
    /// respans body-derived tokens onto the call, once, at replacement
    /// construction. The only span rewrite in the system.
    pub(in crate::compiler) fn span_ref(&mut self) -> &mut Span {
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
            | Token::Inc(x)
            | Token::Dec(x)
            | Token::Assign(x)
            | Token::AddEq(x)
            | Token::SubEq(x)
            | Token::MulEq(x)
            | Token::DivEq(x)
            | Token::ModEq(x)
            | Token::CaretEq(x)
            | Token::AndEq(x)
            | Token::AndAndEq(x)
            | Token::OrEq(x)
            | Token::OrOrEq(x)
            | Token::LeftShiftEq(x)
            | Token::RightShiftEq(x)
            | Token::Compose(x)
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
            | Token::Ref(x)
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
            | Token::StringLiteral(StringToken(x, _))
            | Token::IntLiteral(IntToken(x, _))
            | Token::FloatLiteral(FloatToken(x, _))
            | Token::Id(StringToken(x, _))
            | Token::ClosureArgVar(StringToken(x, _))
            | Token::DirectiveLine(StringToken(x, _))
            | Token::Switch(x)
            | Token::Default(x)
            | Token::ForEach(x)
            | Token::Function(x)
            | Token::Private(x)
            | Token::Public(x)
            | Token::Protected(x) => x,
        }
    }

    /// Allow directly setting a new span on a token
    pub fn with_span(mut self, new_span: Span) -> Self {
        *self.span_ref() = new_span;

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
            Token::Inc(_) => "++",
            Token::Dec(_) => "--",

            Token::Assign(_) => "=",
            Token::AddEq(_) => "+=",
            Token::SubEq(_) => "-=",
            Token::MulEq(_) => "*=",
            Token::DivEq(_) => "/=",
            Token::ModEq(_) => "%=",
            Token::CaretEq(_) => "^=",
            Token::AndEq(_) => "&=",
            Token::AndAndEq(_) => "&&=",
            Token::OrEq(_) => "|=",
            Token::OrOrEq(_) => "||=",
            Token::LeftShiftEq(_) => "<<=",
            Token::RightShiftEq(_) => ">>=",
            Token::Compose(_) => "@",

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
            Token::Ref(_) => "ref",
            Token::Efun(_) => "efun",
            Token::Switch(_) => "switch",
            Token::Default(_) => "default",
            Token::ForEach(_) => "foreach",
            Token::Function(_) => "function",
            Token::Private(_) => "private",
            Token::Public(_) => "public",
            Token::Protected(_) => "protected",

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
            Token::IntLiteral(i) => return write!(f, "{}", i.1),
            Token::FloatLiteral(fl) => return write!(f, "{}", fl.1),

            Token::StringLiteral(s)
            | Token::Id(s)
            | Token::ClosureArgVar(s)
            | Token::DirectiveLine(s) => &s.1,
        };

        write!(f, "{out}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_fragment_is_born_in_file_coordinates() {
        let mut lexer = LexWrapper::new_at("x + 1", 4, 100);
        let tok = lexer.next().unwrap().unwrap();
        assert_eq!(tok.span(), Span::new(4, 100..101));

        let error = LexWrapper::new_at("`", 4, 100)
            .next()
            .unwrap()
            .expect_err("a backtick does not lex");
        assert_eq!(error.span(), Some(Span::new(4, 100..101)));
    }

    #[test]
    fn a_multi_line_string_spans_the_whole_literal() {
        let vec = lex_vec("\"a\nb\"");
        let Ok(Token::StringLiteral(st)) = &vec[0] else {
            panic!("expected a string literal");
        };
        assert_eq!(st.0, Span::new(0, 0..5));
    }

    #[test]
    fn a_directive_line_span_excludes_its_trailing_newline() {
        let vec = lex_vec("#define FOO 1\n");
        let Ok(Token::DirectiveLine(st)) = &vec[0] else {
            panic!("expected a directive line");
        };
        assert_eq!(st.0, Span::new(0, 0..13));
    }

    #[test]
    fn an_invalid_token_carries_its_span() {
        let mut lexer = LexWrapper::new("int x = `;", 7);
        let error = lexer
            .find_map(|item| item.err())
            .expect("a backtick does not lex");
        assert_eq!(error.span(), Some(Span::new(7, 8..9)));
        assert_eq!(error.to_string(), "Lex Error: Invalid Token ```");
    }

    fn lex_vec(prog: &str) -> Vec<Result<Token>> {
        LexWrapper::new(prog, 0).collect::<Vec<_>>()
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

    #[test]
    fn lex_wrapper_triples_are_each_tokens_own_l_and_r() {
        // Include a directive line so its trimmed end (the span excludes
        // the trailing newline) shows up in the triple too.
        let src = "x\n#define FOO 1\n";
        let triples: Vec<_> = LexWrapper::new(src, 0)
            .triples()
            .collect::<Result<Vec<_>>>()
            .unwrap();

        assert_eq!(triples.len(), 2);
        let (l, tok, r) = &triples[0];
        assert!(matches!(tok, Token::Id(_)));
        assert_eq!((*l, *r), (0, 1));

        let (l, tok, r) = &triples[1];
        let Token::DirectiveLine(st) = tok else {
            panic!("expected a directive line");
        };
        // The lexer's raw slice keeps the trailing newline; only the span
        // (used for carets) trims it.
        assert_eq!(st.1, "#define FOO 1\n");
        assert_eq!((*l, *r), (2, 15));
    }

    #[test]
    fn token_triples_wrap_a_token_slice_the_same_shape() {
        let tokens = vec![
            Token::Id(StringToken(Span::new(0, 0..1), "x".into())),
            Token::Semi(Span::new(0, 1..2)),
        ];
        let triples: Vec<_> = TokenTriples::new(&tokens)
            .collect::<Result<Vec<_>>>()
            .unwrap();

        assert_eq!(
            triples,
            vec![(0, tokens[0].clone(), 1), (1, tokens[1].clone(), 2)]
        );
    }
}
