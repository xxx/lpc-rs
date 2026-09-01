//! The directive grammar: the single statement of every preprocessor
//! directive's syntax. The lexer contributes one fact — a `#`-to-newline
//! grab ([`Token::DirectiveLine`](crate::compiler::lexer::Token)) — and
//! everything after the `#` is decided here: [`classify`] names the
//! directive for dead regions (operands never read), [`parse`] is the full
//! grammar for live ones, and [`parse_if_expression`] is the `#if`
//! expression sub-grammar, which object-macro bodies reuse. Comments read
//! as whitespace (C strips them before directive parsing); `defined` and
//! `not` mean something only inside a `#if` operand.

use lpc_rs_errors::{
    LpcError, Result, lpc_error,
    span::{HasSpan, Span},
};

use crate::{
    compile_time_config::MAX_NESTING_DEPTH,
    compiler::{
        ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation},
        lexer::{LexWrapper, Token},
        preprocessor::preprocessor_node::PreprocessorNode,
    },
};

/// A directive's name alone — everything a dead region is allowed to know.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectiveKind {
    /// `#if`
    If,
    /// `#ifdef`
    IfDef,
    /// `#ifndef`
    IfNDef,
    /// `#else`
    Else,
    /// `#endif`
    Endif,
    /// `#elif`
    Elif,
    /// `#include`
    Include,
    /// `#define`
    Define,
    /// `#undef`
    Undef,
    /// `#pragma`
    Pragma,
    /// `#error`
    Error,
    /// A bare `#` line — the C99 6.10.7 null directive.
    Null,
    /// A `#` followed by anything that is not a known directive name.
    Unknown,
}

/// One fully parsed directive line.
#[derive(Debug, PartialEq)]
pub enum Directive {
    /// `#include "path"` (`sys: false`) or `#include <path>` (`sys: true`).
    Include {
        /// The path between the delimiters, verbatim.
        path: String,
        /// `true` for the `<…>` form.
        sys: bool,
    },
    /// `#define name …`. `params: None` is an object macro; `Some(vec![])`
    /// is a `()` function macro (the `(` must sit flush against the name,
    /// C99's rule). `body` is the raw rest of the line, trimmed.
    Define {
        /// The macro's name.
        name: String,
        /// Parameter names; `None` for an object macro.
        params: Option<Vec<String>>,
        /// The raw body text (the preprocessor re-lexes it).
        body: String,
        /// The trimmed body's file coordinates — the base for the body's
        /// in-place lex. For an empty body it is the (widened) point at
        /// end of line — an empty body lexes to nothing.
        body_span: Span,
    },
    /// `#undef name`.
    Undef {
        /// The name to undefine.
        name: String,
    },
    /// `#if expr`, with the operand already parsed.
    If {
        /// The parsed expression.
        expr: PreprocessorNode,
    },
    /// `#ifdef name`.
    IfDef {
        /// The name whose definedness is tested.
        name: String,
    },
    /// `#ifndef name`.
    IfNDef {
        /// The name whose undefinedness is tested.
        name: String,
    },
    /// `#else`.
    Else,
    /// `#elif expr`. The operand is raw — parsed only when the chain is
    /// still undecided (C99 6.10p6; elif-bundle R2).
    Elif {
        /// The raw operand text, trimmed.
        operand: String,
        /// The trimmed operand's file coordinates; for an empty operand,
        /// the (widened) point at end of line.
        operand_span: Span,
    },
    /// `#endif`.
    Endif,
    /// `#pragma name[, name]*` — names validated as identifiers here,
    /// recognized semantically by the preprocessor.
    Pragma {
        /// The pragma names, in order.
        names: Vec<String>,
    },
    /// `#error text` — the rest of the line verbatim, comments included,
    /// macros never expanded (GCC-faithful).
    Error {
        /// The trimmed message text; empty for a bare `#error`.
        text: String,
    },
    /// A bare `#` line: legal, does nothing.
    Null,
}

/// Strip the trailing newline (and a Windows `\r`) the lexer's grab may
/// have consumed.
fn trim_directive_line(line: &str) -> &str {
    line.trim_end_matches('\n').trim_end_matches('\r')
}

/// Skip spaces, tabs, and comments (a comment is whitespace here).
/// `Err((lo, hi))` is an unterminated `/*`, as line-relative offsets.
fn skip_ws_raw(text: &str, pos: &mut usize) -> std::result::Result<(), (usize, usize)> {
    loop {
        let rest = &text[*pos..];
        if rest.starts_with([' ', '\t', '\x0b', '\x0c', '\r']) {
            *pos += 1;
        } else if rest.starts_with("//") {
            *pos = text.len();
        } else if let Some(inner) = rest.strip_prefix("/*") {
            match inner.find("*/") {
                Some(i) => *pos += 2 + i + 2,
                None => return Err((*pos, text.len())),
            }
        } else {
            return Ok(());
        }
    }
}

/// Read a `name` (`[\p{Alphabetic}_]\w*`) at `pos`, advancing past it.
fn read_name_raw<'t>(text: &'t str, pos: &mut usize) -> Option<&'t str> {
    let rest = &text[*pos..];
    let mut chars = rest.char_indices();
    match chars.next() {
        Some((_, c)) if c.is_alphabetic() || c == '_' => {}
        _ => return None,
    }
    let end = chars
        .find(|&(_, c)| !(c.is_alphanumeric() || c == '_'))
        .map(|(i, _)| i)
        .unwrap_or(rest.len());
    *pos += end;
    Some(&rest[..end])
}

/// The name → kind table. Everything else is [`DirectiveKind::Unknown`].
fn kind_of(name: &str) -> DirectiveKind {
    match name {
        "if" => DirectiveKind::If,
        "ifdef" => DirectiveKind::IfDef,
        "ifndef" => DirectiveKind::IfNDef,
        "else" => DirectiveKind::Else,
        "endif" => DirectiveKind::Endif,
        "include" => DirectiveKind::Include,
        "define" => DirectiveKind::Define,
        "undef" => DirectiveKind::Undef,
        "pragma" => DirectiveKind::Pragma,
        "elif" => DirectiveKind::Elif,
        "error" => DirectiveKind::Error,
        _ => DirectiveKind::Unknown,
    }
}

/// Name a directive line's kind without reading its operands. Never
/// errors: garbage classifies as [`DirectiveKind::Unknown`], which a dead
/// region treats as text (C99 6.10.1).
pub fn classify(line: &str) -> DirectiveKind {
    let text = trim_directive_line(line);
    debug_assert!(text.starts_with('#'), "a directive line starts with `#`");
    let mut pos = 1;
    if skip_ws_raw(text, &mut pos).is_err() {
        return DirectiveKind::Unknown;
    }
    if pos >= text.len() {
        return DirectiveKind::Null;
    }
    match read_name_raw(text, &mut pos) {
        Some(name) => kind_of(name),
        None => DirectiveKind::Unknown,
    }
}

/// A cursor over one directive line. Offsets are line-relative and map
/// into the file through the directive's span.
struct Cursor<'a> {
    text: &'a str,
    pos: usize,
    span: Span,
}

impl<'a> Cursor<'a> {
    fn new(line: &'a str, span: Span) -> Self {
        let text = trim_directive_line(line);
        debug_assert!(text.starts_with('#'), "a directive line starts with `#`");
        Self { text, pos: 1, span }
    }

    /// A span for `lo..hi` of this line, in file coordinates. A collapsed
    /// range widens to one byte so the caret is visible.
    fn sub_span(&self, lo: usize, hi: usize) -> Span {
        let l = self.span.l();
        Span::new(self.span.file_id(), l + lo..l + hi.max(lo + 1))
    }

    fn err(&self, lo: usize, hi: usize, msg: String) -> LpcError {
        lpc_error!(Some(self.sub_span(lo, hi)), "{msg}")
    }

    fn at_end(&self) -> bool {
        self.pos >= self.text.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.text[self.pos..].chars().next()
    }

    fn skip_ws(&mut self) -> Result<()> {
        skip_ws_raw(self.text, &mut self.pos).map_err(|(lo, hi)| {
            self.err(
                lo,
                hi,
                "unterminated comment in a preprocessor directive".into(),
            )
        })
    }

    fn read_name(&mut self) -> Option<&'a str> {
        read_name_raw(self.text, &mut self.pos)
    }

    /// Only whitespace and comments may remain (R4's "EOL").
    fn end_of_line(&mut self, directive: &str) -> Result<()> {
        self.skip_ws()?;
        if self.at_end() {
            Ok(())
        } else {
            Err(self.err(
                self.pos,
                self.text.len(),
                format!("unexpected tokens after `#{directive}`"),
            ))
        }
    }

    fn include(mut self) -> Result<Directive> {
        self.skip_ws()?;
        let (close, sys) = match self.peek_char() {
            Some('"') => ('"', false),
            Some('<') => ('>', true),
            _ => {
                return Err(self.err(
                    self.pos,
                    self.text.len(),
                    r#"expected "path" or <path> after `#include`"#.into(),
                ));
            }
        };
        let open = self.pos;
        self.pos += 1;
        let start = self.pos;
        let Some(len) = self.text[start..].find(close) else {
            return Err(self.err(
                open,
                self.text.len(),
                "unterminated path in `#include`".into(),
            ));
        };
        let path = &self.text[start..start + len];
        if path.is_empty() {
            return Err(self.err(
                open,
                start + len + 1,
                r#"expected "path" or <path> after `#include`"#.into(),
            ));
        }
        self.pos = start + len + 1;
        self.end_of_line("include")?;
        Ok(Directive::Include {
            path: path.to_owned(),
            sys,
        })
    }

    fn define(mut self) -> Result<Directive> {
        self.skip_ws()?;
        let Some(name) = self.read_name() else {
            return Err(self.err(
                self.pos,
                self.text.len(),
                "expected an identifier after `#define`".into(),
            ));
        };
        // C99's rule, and today's: a function macro only when `(` sits
        // flush against the name.
        let params = if self.peek_char() == Some('(') {
            self.pos += 1;
            Some(self.params()?)
        } else {
            None
        };
        let rest = &self.text[self.pos..];
        let body = rest.trim();
        let start = self.pos + (rest.len() - rest.trim_start().len());
        let body_span = self.sub_span(start, start + body.len());
        Ok(Directive::Define {
            name: name.to_owned(),
            params,
            body: body.to_owned(),
            body_span,
        })
    }

    fn params(&mut self) -> Result<Vec<String>> {
        let mut params: Vec<String> = vec![];
        self.skip_ws()?;
        if self.peek_char() == Some(')') {
            self.pos += 1;
            return Ok(params);
        }
        loop {
            self.skip_ws()?;
            let start = self.pos;
            let Some(p) = self.read_name() else {
                return Err(self.err(
                    start,
                    start + 1,
                    "macro parameters must be identifiers".into(),
                ));
            };
            if params.iter().any(|q| q == p) {
                return Err(self.err(start, self.pos, format!("duplicate macro parameter `{p}`")));
            }
            params.push(p.to_owned());
            self.skip_ws()?;
            match self.peek_char() {
                Some(',') => self.pos += 1,
                Some(')') => {
                    self.pos += 1;
                    return Ok(params);
                }
                _ => {
                    return Err(self.err(
                        self.pos,
                        self.text.len(),
                        "unterminated parameter list in `#define`".into(),
                    ));
                }
            }
        }
    }

    /// `#undef` / `#ifdef` / `#ifndef`: one identifier, then EOL.
    fn named(mut self, ctor: fn(String) -> Directive, directive: &str) -> Result<Directive> {
        self.skip_ws()?;
        let Some(name) = self.read_name() else {
            return Err(self.err(
                self.pos,
                self.text.len(),
                format!("expected an identifier after `#{directive}`"),
            ));
        };
        let parsed = ctor(name.to_owned());
        self.end_of_line(directive)?;
        Ok(parsed)
    }

    /// `#else` / `#endif`: nothing but EOL.
    fn bare(mut self, directive: Directive, name: &str) -> Result<Directive> {
        self.end_of_line(name)?;
        Ok(directive)
    }

    /// `#elif expr` — the operand is stored raw, `#define`-body style.
    fn elif(self) -> Result<Directive> {
        let rest = &self.text[self.pos..];
        let operand = rest.trim();
        let start = self.pos + (rest.len() - rest.trim_start().len());
        let operand_span = self.sub_span(start, start + operand.len());
        Ok(Directive::Elif {
            operand: operand.to_owned(),
            operand_span,
        })
    }

    /// `#error text` — nothing to validate.
    fn error_directive(self) -> Result<Directive> {
        Ok(Directive::Error {
            text: self.text[self.pos..].trim().to_owned(),
        })
    }

    fn if_expr(mut self) -> Result<Directive> {
        self.skip_ws()?;
        if self.at_end() {
            return Err(self.err(
                self.pos,
                self.text.len(),
                "expected an expression after `#if`".into(),
            ));
        }
        let operand = &self.text[self.pos..];
        let base = self.sub_span(self.pos, self.text.len());
        let expr = parse_if_expression(operand, base)?;
        Ok(Directive::If { expr })
    }

    fn pragma(mut self) -> Result<Directive> {
        self.skip_ws()?;
        let mut names = vec![];
        loop {
            let start = self.pos;
            let Some(name) = self.read_name() else {
                return Err(self.err(
                    start,
                    self.text.len(),
                    "expected a pragma name after `#pragma`".into(),
                ));
            };
            names.push(name.to_owned());
            self.skip_ws()?;
            if self.peek_char() == Some(',') {
                self.pos += 1;
                self.skip_ws()?;
            } else {
                break;
            }
        }
        self.end_of_line("pragma")?;
        Ok(Directive::Pragma { names })
    }
}

/// Parse one directive line — the single grammar. `span` is the
/// [`Token::DirectiveLine`]'s span (its `l()` is the `#`'s file offset),
/// so every diagnostic points at the offending slice of the line.
pub fn parse(line: &str, span: Span) -> Result<Directive> {
    let mut c = Cursor::new(line, span);
    c.skip_ws()?;
    if c.at_end() {
        return Ok(Directive::Null);
    }
    let start = c.pos;
    let Some(name) = c.read_name() else {
        return Err(c.err(
            start,
            c.text.len(),
            "expected a directive name after `#`".into(),
        ));
    };
    match kind_of(name) {
        DirectiveKind::Include => c.include(),
        DirectiveKind::Define => c.define(),
        DirectiveKind::Undef => c.named(|name| Directive::Undef { name }, "undef"),
        DirectiveKind::If => c.if_expr(),
        DirectiveKind::IfDef => c.named(|name| Directive::IfDef { name }, "ifdef"),
        DirectiveKind::IfNDef => c.named(|name| Directive::IfNDef { name }, "ifndef"),
        DirectiveKind::Else => c.bare(Directive::Else, "else"),
        DirectiveKind::Endif => c.bare(Directive::Endif, "endif"),
        DirectiveKind::Elif => c.elif(),
        DirectiveKind::Pragma => c.pragma(),
        DirectiveKind::Error => c.error_directive(),
        DirectiveKind::Null => unreachable!("a name was read"),
        DirectiveKind::Unknown => Err(c.err(
            start,
            c.pos,
            format!("unknown preprocessor directive `#{name}`"),
        )),
    }
}

/// Parse a `#if` operand (also an object-macro body, for `#if FOO`).
/// Tokens come from the language lexer over `text`; `defined` and `not`
/// are recognized contextually. `base` locates `text` in its file, so
/// diagnostics carry real source spans.
pub fn parse_if_expression(text: &str, base: Span) -> Result<PreprocessorNode> {
    let tokens = LexWrapper::new_at(text, base.file_id(), base.l()).collect::<Result<Vec<_>>>()?;
    if tokens.is_empty() {
        return Err(lpc_error!(Some(base), "expected an expression after `#if`"));
    }
    let mut parser = ExprParser {
        tokens,
        pos: 0,
        depth: 0,
    };
    let (expr, _height) = parser.expr()?;
    if let Some(t) = parser.tokens.get(parser.pos) {
        return Err(ExprParser::err_at(
            t.span(),
            "unexpected tokens after `#if` expression",
        ));
    }
    Ok(expr)
}

/// One precedence level per entry, loosest first — C's ladder below
/// `||`/`&&` down to `* / %` (elif-bundle R6).
const LADDER: &[fn(&Token) -> Option<BinaryOperation>] = &[
    |t| matches!(t, Token::OrOr(_)).then_some(BinaryOperation::OrOr),
    |t| matches!(t, Token::AndAnd(_)).then_some(BinaryOperation::AndAnd),
    |t| matches!(t, Token::Or(_)).then_some(BinaryOperation::Or),
    |t| matches!(t, Token::Caret(_)).then_some(BinaryOperation::Xor),
    |t| matches!(t, Token::And(_)).then_some(BinaryOperation::And),
    |t| match t {
        Token::EqEq(_) => Some(BinaryOperation::EqEq),
        Token::NotEq(_) => Some(BinaryOperation::NotEq),
        _ => None,
    },
    |t| match t {
        Token::LessThan(_) => Some(BinaryOperation::Lt),
        Token::GreaterThan(_) => Some(BinaryOperation::Gt),
        Token::LessThanEq(_) => Some(BinaryOperation::Lte),
        Token::GreaterThanEq(_) => Some(BinaryOperation::Gte),
        _ => None,
    },
    |t| match t {
        Token::LeftShift(_) => Some(BinaryOperation::Shl),
        Token::RightShift(_) => Some(BinaryOperation::Shr),
        _ => None,
    },
    |t| match t {
        Token::Plus(_) => Some(BinaryOperation::Add),
        Token::Minus(_) => Some(BinaryOperation::Sub),
        _ => None,
    },
    |t| match t {
        Token::Mul(_) => Some(BinaryOperation::Mul),
        Token::Div(_) => Some(BinaryOperation::Div),
        Token::Mod(_) => Some(BinaryOperation::Mod),
        _ => None,
    },
];

/// Recursive descent over the operand's tokens: C's integer operator
/// ladder (elif-bundle R6) — [`LADDER`] levels, then unary, then primary.
/// Two checks keep every tree under `MAX_NESTING_DEPTH` (nesting-cap R5):
/// `depth` refuses to descend into a `(` or prefix operator the tree could
/// not afford, and every built node reports its height upward.
struct ExprParser {
    tokens: Vec<Token>,
    pos: usize,
    /// `(` and prefix operators enclosing the current point.
    depth: usize,
}

/// A node and its height.
type Built = (PreprocessorNode, usize);

impl ExprParser {
    fn err_at(span: Span, msg: &str) -> LpcError {
        lpc_error!(Some(span), "{msg}")
    }

    fn too_deep(span: Span) -> LpcError {
        lpc_error!(
            Some(span),
            "`#if` expression nests too deeply (limit {})",
            MAX_NESTING_DEPTH
        )
    }

    fn end_err(&self) -> LpcError {
        let span = self
            .tokens
            .last()
            .map(|t| t.span())
            .expect("parse_if_expression rejects an empty operand");
        Self::err_at(span, "unexpected end of `#if` expression")
    }

    fn next(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.pos).cloned();
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn peek(&self, ahead: usize) -> Option<&Token> {
        self.tokens.get(self.pos + ahead)
    }

    fn eat(&mut self, pred: fn(&Token) -> bool) -> bool {
        if self.peek(0).is_some_and(pred) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Before descending into a `(` or prefix operator at `span`: the node
    /// built there is at least `depth + 2` high (itself, its enclosing
    /// levels, the atom under it).
    fn descend(&mut self, span: Span) -> Result<()> {
        if self.depth + 2 > MAX_NESTING_DEPTH {
            return Err(Self::too_deep(span));
        }
        self.depth += 1;
        Ok(())
    }

    fn ascend(&mut self) {
        self.depth -= 1;
    }

    /// A node of height `height`, built at `span`.
    fn built(node: PreprocessorNode, height: usize, span: Span) -> Result<Built> {
        if height > MAX_NESTING_DEPTH {
            return Err(Self::too_deep(span));
        }
        Ok((node, height))
    }

    fn expr(&mut self) -> Result<Built> {
        self.level(0)
    }

    fn level(&mut self, prec: usize) -> Result<Built> {
        let Some(op_of) = LADDER.get(prec) else {
            return self.unary();
        };
        let (mut node, mut height) = self.level(prec + 1)?;
        while let Some(tok) = self.peek(0) {
            let Some(op) = op_of(tok) else { break };
            let op_span = tok.span();
            self.pos += 1;
            let (rhs, rhs_height) = self.level(prec + 1)?;
            (node, height) = Self::built(
                PreprocessorNode::BinaryOp(op, Box::new(node), Box::new(rhs)),
                1 + height.max(rhs_height),
                op_span,
            )?;
        }
        Ok((node, height))
    }

    /// Prefix `!` `-` `~`, stacking (`!!X`, `-~X`).
    fn unary(&mut self) -> Result<Built> {
        let (op, span) = match self.peek(0) {
            Some(t @ Token::Bang(_)) => (UnaryOperation::Bang, t.span()),
            Some(t @ Token::Minus(_)) => (UnaryOperation::Negate, t.span()),
            Some(t @ Token::Tilde(_)) => (UnaryOperation::BitwiseNot, t.span()),
            _ => return self.primary(),
        };
        self.pos += 1;
        self.descend(span)?;
        let inner = self.unary();
        self.ascend();
        let (inner, height) = inner?;
        Self::built(
            PreprocessorNode::UnaryOp(op, Box::new(inner)),
            height + 1,
            span,
        )
    }

    fn primary(&mut self) -> Result<Built> {
        let Some(token) = self.next() else {
            return Err(self.end_err());
        };
        match token {
            Token::IntLiteral(t) => Ok((PreprocessorNode::Int(t.1), 1)),
            Token::StringLiteral(t) => Ok((PreprocessorNode::String(t.1), 1)),
            Token::LParen(sp) => {
                // A paren counts as a level here: each one costs this
                // parser 13 frames.
                self.descend(sp)?;
                let inner = self.expr();
                self.ascend();
                let (inner, height) = inner?;
                if self.eat(|t| matches!(t, Token::RParen(_))) {
                    Self::built(inner, height + 1, sp)
                } else {
                    Err(Self::err_at(sp, "unmatched `(` in `#if` expression"))
                }
            }
            Token::Id(t) if t.1 == "not" && self.is_defined_call_ahead() => {
                self.pos += 1; // the `defined`
                Ok((self.defined_call(true)?, 1))
            }
            Token::Id(t) if t.1 == "defined" && matches!(self.peek(0), Some(Token::LParen(_))) => {
                Ok((self.defined_call(false)?, 1))
            }
            Token::Id(t) => Ok((PreprocessorNode::Var(t.1), 1)),
            other => Err(Self::err_at(
                other.span(),
                "unexpected token in `#if` expression",
            )),
        }
    }

    /// Is the upcoming pair `defined` `(` — i.e. `not` really negates a
    /// defined-test? Otherwise `not` is a plain Var.
    fn is_defined_call_ahead(&self) -> bool {
        matches!(self.peek(0), Some(Token::Id(t)) if t.1 == "defined")
            && matches!(self.peek(1), Some(Token::LParen(_)))
    }

    /// `( name )` after a `defined`. The caller peeked the `(`.
    fn defined_call(&mut self, negated: bool) -> Result<PreprocessorNode> {
        let lparen = self.next().expect("caller peeked the `(`");
        match self.next() {
            Some(Token::Id(t)) => match self.next() {
                Some(Token::RParen(_)) => Ok(PreprocessorNode::Defined(t.1, negated)),
                _ => Err(Self::err_at(
                    lparen.span(),
                    "unmatched `(` in `#if` expression",
                )),
            },
            Some(other) => Err(Self::err_at(
                other.span(),
                "unexpected token in `#if` expression",
            )),
            None => Err(self.end_err()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(line: &str) -> Result<Directive> {
        parse(line, Span::new(0, 0..line.len()))
    }

    fn perr(line: &str) -> String {
        p(line).expect_err("should not parse").to_string()
    }

    /// Parse `line` as a `#define`, panicking on anything else.
    fn define_of(line: &str) -> (String, Option<Vec<String>>, String, Span) {
        let Directive::Define {
            name,
            params,
            body,
            body_span,
        } = p(line).unwrap()
        else {
            panic!("expected a define");
        };
        (name, params, body, body_span)
    }

    fn x(text: &str) -> Result<PreprocessorNode> {
        parse_if_expression(text, Span::new(0, 0..text.len()))
    }

    fn xerr(text: &str) -> String {
        x(text).expect_err("should not parse").to_string()
    }

    #[test]
    fn classify_names_every_directive_and_nothing_more() {
        let cases = [
            ("#if FOO", DirectiveKind::If),
            ("#ifdef FOO", DirectiveKind::IfDef),
            ("#ifndef FOO", DirectiveKind::IfNDef),
            ("#else", DirectiveKind::Else),
            ("#endif junk", DirectiveKind::Endif),
            ("# include \"a\"", DirectiveKind::Include),
            ("#define A B", DirectiveKind::Define),
            ("#undef A", DirectiveKind::Undef),
            ("#pragma x", DirectiveKind::Pragma),
            ("#", DirectiveKind::Null),
            ("#  // c", DirectiveKind::Null),
            ("#elif FOO", DirectiveKind::Elif),
            ("#error out", DirectiveKind::Error),
            ("#line 5", DirectiveKind::Unknown),
            ("#iffy", DirectiveKind::Unknown),
            ("#123", DirectiveKind::Unknown),
            ("#/* junk", DirectiveKind::Unknown),
            ("#/* c */ifdef X", DirectiveKind::IfDef),
        ];
        for (line, kind) in cases {
            assert_eq!(classify(line), kind, "classify({line:?})");
        }
    }

    #[test]
    fn null_directive_parses_to_null() {
        assert_eq!(p("#").unwrap(), Directive::Null);
        assert_eq!(p("#   /* c */").unwrap(), Directive::Null);
        assert_eq!(p("# // c\n").unwrap(), Directive::Null);
    }

    #[test]
    fn include_forms() {
        assert_eq!(
            p("# include \"a/b.h\"").unwrap(),
            Directive::Include {
                path: "a/b.h".into(),
                sys: false
            }
        );
        assert_eq!(
            p("#include <sys.h> /* why */").unwrap(),
            Directive::Include {
                path: "sys.h".into(),
                sys: true
            }
        );
        assert_eq!(
            perr("#include foo"),
            r#"expected "path" or <path> after `#include`"#
        );
        assert_eq!(
            perr("#include \"\""),
            r#"expected "path" or <path> after `#include`"#
        );
        assert_eq!(perr("#include \"a.h"), "unterminated path in `#include`");
        assert_eq!(
            perr("#include <a.h> junk"),
            "unexpected tokens after `#include`"
        );
    }

    #[test]
    fn a_define_body_span_locates_the_body() {
        let line = "#define ADD(a, b)   a + b";
        let (_, _, body, body_span) = define_of(line);
        assert_eq!(body, "a + b");
        assert_eq!(&line[body_span.l()..body_span.r()], "a + b");
    }

    #[test]
    fn define_object_macros() {
        let line = "#define FOO 1 + 2";
        let (name, params, body, body_span) = define_of(line);
        assert_eq!(name, "FOO");
        assert_eq!(params, None);
        assert_eq!(body, "1 + 2");
        assert_eq!(&line[body_span.l()..body_span.r()], "1 + 2");

        let (_, _, body, _) = define_of("#define FOO");
        assert!(body.is_empty());

        // C99's space rule: `(` not flush against the name = object macro.
        let line = "#define F (x)";
        let (name, params, body, body_span) = define_of(line);
        assert_eq!(name, "F");
        assert_eq!(params, None);
        assert_eq!(body, "(x)");
        assert_eq!(&line[body_span.l()..body_span.r()], "(x)");

        assert_eq!(perr("#define"), "expected an identifier after `#define`");
        assert_eq!(
            perr("#define 123"),
            "expected an identifier after `#define`"
        );
    }

    #[test]
    fn define_function_macros() {
        let line = "#define F(a, b) a + b";
        let (name, params, body, body_span) = define_of(line);
        assert_eq!(name, "F");
        assert_eq!(params, Some(vec!["a".into(), "b".into()]));
        assert_eq!(body, "a + b");
        assert_eq!(&line[body_span.l()..body_span.r()], "a + b");

        let line = "#define F() body";
        let (name, params, body, body_span) = define_of(line);
        assert_eq!(name, "F");
        assert_eq!(params, Some(vec![]));
        assert_eq!(body, "body");
        assert_eq!(&line[body_span.l()..body_span.r()], "body");

        assert_eq!(
            perr("#define F(1, 2) x"),
            "macro parameters must be identifiers"
        );
        assert_eq!(perr("#define F(a, a) x"), "duplicate macro parameter `a`");
        assert_eq!(
            perr("#define F(a, b"),
            "unterminated parameter list in `#define`"
        );
        assert_eq!(
            perr("#define F(a b) x"),
            "unterminated parameter list in `#define`"
        );
    }

    #[test]
    fn named_directives_take_one_identifier() {
        assert_eq!(
            p("#undef FOO").unwrap(),
            Directive::Undef { name: "FOO".into() }
        );
        assert_eq!(
            p("#ifdef FOO").unwrap(),
            Directive::IfDef { name: "FOO".into() }
        );
        assert_eq!(
            p("# ifndef _F0").unwrap(),
            Directive::IfNDef { name: "_F0".into() }
        );
        assert_eq!(perr("#undef"), "expected an identifier after `#undef`");
        assert_eq!(perr("#ifdef"), "expected an identifier after `#ifdef`");
        assert_eq!(perr("#ifndef 1"), "expected an identifier after `#ifndef`");
        assert_eq!(perr("#undef FOO bar"), "unexpected tokens after `#undef`");
        assert_eq!(perr("#ifdef FOO BAR"), "unexpected tokens after `#ifdef`");
    }

    #[test]
    fn bare_directives_take_nothing() {
        assert_eq!(p("#else").unwrap(), Directive::Else);
        assert_eq!(p("#endif").unwrap(), Directive::Endif);
        assert_eq!(p("#else /* the why */").unwrap(), Directive::Else);
        assert_eq!(p("#endif // FOO").unwrap(), Directive::Endif);
        assert_eq!(perr("#else 1 + 4"), "unexpected tokens after `#else`");
        assert_eq!(perr("#endif FOO"), "unexpected tokens after `#endif`");
    }

    #[test]
    fn an_elif_operand_is_raw_and_located() {
        let line = "#elif  FOO + )junk( /* open";
        let Directive::Elif {
            operand,
            operand_span,
        } = p(line).unwrap()
        else {
            panic!("expected an elif");
        };
        // Raw: junk and an unterminated comment survive parse (elif-bundle R2).
        assert_eq!(operand, "FOO + )junk( /* open");
        assert_eq!(&line[operand_span.l()..operand_span.r()], operand);

        let Directive::Elif { operand, .. } = p("#elif").unwrap() else {
            panic!("expected an elif");
        };
        assert!(operand.is_empty());
    }

    #[test]
    fn error_text_is_verbatim() {
        let text_of = |line: &str| {
            let Directive::Error { text } = p(line).unwrap() else {
                panic!("expected an error directive");
            };
            text
        };
        assert_eq!(
            text_of("#error bad config /* kept */"),
            "bad config /* kept */"
        );
        assert_eq!(text_of("#error"), "");
        assert_eq!(text_of("#error   UNDEFINED_MACRO  "), "UNDEFINED_MACRO");
    }

    #[test]
    fn pragma_takes_comma_separated_names() {
        assert_eq!(
            p("#pragma no_clone").unwrap(),
            Directive::Pragma {
                names: vec!["no_clone".into()]
            }
        );
        assert_eq!(
            p("#pragma strict_types, no_clone , no_inherit").unwrap(),
            Directive::Pragma {
                names: vec![
                    "strict_types".into(),
                    "no_clone".into(),
                    "no_inherit".into()
                ]
            }
        );
        assert_eq!(perr("#pragma"), "expected a pragma name after `#pragma`");
        assert_eq!(
            perr("#pragma 123"),
            "expected a pragma name after `#pragma`"
        );
        assert_eq!(perr("#pragma x,"), "expected a pragma name after `#pragma`");
        assert_eq!(perr("#pragma x y"), "unexpected tokens after `#pragma`");
    }

    #[test]
    fn unknown_directives_are_named_in_the_error() {
        assert_eq!(perr("#iffy"), "unknown preprocessor directive `#iffy`");
        assert_eq!(perr("#line 5"), "unknown preprocessor directive `#line`");
        assert_eq!(
            perr("#warning w"),
            "unknown preprocessor directive `#warning`"
        );
        assert_eq!(perr("#123"), "expected a directive name after `#`");
    }

    #[test]
    fn unterminated_comment_is_an_error_where_the_grammar_scans() {
        assert_eq!(
            perr("#endif /* open"),
            "unterminated comment in a preprocessor directive"
        );
        assert_eq!(
            perr("# /* open"),
            "unterminated comment in a preprocessor directive"
        );
        // A define BODY is the re-lex's domain; the grammar accepts it raw.
        let (_, _, body, _) = define_of("#define X /* open");
        assert_eq!(body, "/* open");
    }

    #[test]
    fn expression_atoms_and_precedence() {
        assert_eq!(x("42").unwrap(), PreprocessorNode::Int(42));
        assert_eq!(x("FOO").unwrap(), PreprocessorNode::Var("FOO".into()));
        assert_eq!(x(r#""s""#).unwrap(), PreprocessorNode::String("s".into()));
        assert_eq!(x("(FOO)").unwrap(), PreprocessorNode::Var("FOO".into()));
        // `+` binds tighter than `&&`, which binds tighter than `||`.
        assert_eq!(
            x("1 + 2 && 3 || 4").unwrap(),
            PreprocessorNode::BinaryOp(
                BinaryOperation::OrOr,
                Box::new(PreprocessorNode::BinaryOp(
                    BinaryOperation::AndAnd,
                    Box::new(PreprocessorNode::BinaryOp(
                        BinaryOperation::Add,
                        Box::new(PreprocessorNode::Int(1)),
                        Box::new(PreprocessorNode::Int(2)),
                    )),
                    Box::new(PreprocessorNode::Int(3)),
                )),
                Box::new(PreprocessorNode::Int(4)),
            )
        );
    }

    #[test]
    fn nesting_is_capped_at_the_constant() {
        use crate::compile_time_config::MAX_NESTING_DEPTH as MAX;
        let too_deep = format!("`#if` expression nests too deeply (limit {MAX})");

        // 255 parens around an atom is height 256; the 256th `(` is refused
        // before the parser descends into it.
        let parens = |n: usize| format!("{}1{}", "(".repeat(n), ")".repeat(n));
        assert!(x(&parens(MAX - 1)).is_ok());
        let e = x(&parens(MAX)).expect_err("256 parens");
        assert_eq!(e.to_string(), too_deep);
        assert_eq!(e.span().map(|s| s.l()), Some(MAX - 1)); // the 256th `(`

        // 255 prefix operators likewise.
        let bangs = |n: usize| format!("{}1", "!".repeat(n));
        assert!(x(&bangs(MAX - 1)).is_ok());
        let e = x(&bangs(MAX)).expect_err("256 bangs");
        assert_eq!(e.to_string(), too_deep);
        assert_eq!(e.span().map(|s| s.l()), Some(MAX - 1));

        // A chain never recurses; its height is caught bottom-up at the
        // operator that would make it 257.
        let chain = |terms: usize| vec!["1"; terms].join(" + ");
        assert!(x(&chain(MAX)).is_ok());
        let e = x(&chain(MAX + 1)).expect_err("257 terms");
        assert_eq!(e.to_string(), too_deep);
        assert_eq!(e.span().map(|s| s.l()), Some(4 * (MAX - 1) + 2)); // the 256th `+`

        // Nesting and chains add: 100 bangs, then a chain of 156 is 256.
        let mixed = |terms: usize| format!("{}({})", "!".repeat(100), chain(terms));
        assert!(x(&mixed(MAX - 101)).is_ok()); // 100 bangs + paren + 155 chain
        assert!(x(&mixed(MAX - 100)).is_err());
    }

    #[test]
    fn unary_operators_stack_and_bind_tightest() {
        assert_eq!(
            x("!!FOO").unwrap(),
            PreprocessorNode::UnaryOp(
                UnaryOperation::Bang,
                Box::new(PreprocessorNode::UnaryOp(
                    UnaryOperation::Bang,
                    Box::new(PreprocessorNode::Var("FOO".into())),
                )),
            )
        );
        assert_eq!(
            x("!defined(FOO)").unwrap(),
            PreprocessorNode::UnaryOp(
                UnaryOperation::Bang,
                Box::new(PreprocessorNode::Defined("FOO".into(), false)),
            )
        );
        // `-1 * 2` is `(-1) * 2`: unary binds above `*`.
        assert_eq!(
            x("-1 * 2").unwrap(),
            PreprocessorNode::BinaryOp(
                BinaryOperation::Mul,
                Box::new(PreprocessorNode::UnaryOp(
                    UnaryOperation::Negate,
                    Box::new(PreprocessorNode::Int(1)),
                )),
                Box::new(PreprocessorNode::Int(2)),
            )
        );
    }

    #[test]
    fn the_ladder_orders_comparisons_below_shifts() {
        // `1 < 2 << 3` is `1 < (2 << 3)` — shift binds tighter.
        assert_eq!(
            x("1 < 2 << 3").unwrap(),
            PreprocessorNode::BinaryOp(
                BinaryOperation::Lt,
                Box::new(PreprocessorNode::Int(1)),
                Box::new(PreprocessorNode::BinaryOp(
                    BinaryOperation::Shl,
                    Box::new(PreprocessorNode::Int(2)),
                    Box::new(PreprocessorNode::Int(3)),
                )),
            )
        );
    }

    #[test]
    fn the_ladder_orders_the_mid_ladder_levels() {
        // `|` is loosest of the three: `1 | 2 ^ 3` is `1 | (2 ^ 3)`.
        assert_eq!(
            x("1 | 2 ^ 3").unwrap(),
            PreprocessorNode::BinaryOp(
                BinaryOperation::Or,
                Box::new(PreprocessorNode::Int(1)),
                Box::new(PreprocessorNode::BinaryOp(
                    BinaryOperation::Xor,
                    Box::new(PreprocessorNode::Int(2)),
                    Box::new(PreprocessorNode::Int(3)),
                )),
            )
        );
        // `==` binds tighter than `&`: `1 & 2 == 3` is `1 & (2 == 3)`.
        assert_eq!(
            x("1 & 2 == 3").unwrap(),
            PreprocessorNode::BinaryOp(
                BinaryOperation::And,
                Box::new(PreprocessorNode::Int(1)),
                Box::new(PreprocessorNode::BinaryOp(
                    BinaryOperation::EqEq,
                    Box::new(PreprocessorNode::Int(2)),
                    Box::new(PreprocessorNode::Int(3)),
                )),
            )
        );
    }

    #[test]
    fn every_ladder_level_parses_in_one_expression() {
        let tree = x("1 || 2 && 3 | 4 ^ 5 & 6 == 7 < 8 << 9 + 10 * !11").unwrap();
        // Display parenthesizes nothing, so shape is pinned by the trees
        // above; here: it parses, and the loosest split is `||`.
        let PreprocessorNode::BinaryOp(BinaryOperation::OrOr, l, _) = &tree else {
            panic!("`||` must be the loosest binding");
        };
        assert_eq!(**l, PreprocessorNode::Int(1));
    }

    #[test]
    fn defined_is_contextual() {
        assert_eq!(
            x("defined(FOO)").unwrap(),
            PreprocessorNode::Defined("FOO".into(), false)
        );
        assert_eq!(
            x("defined (FOO)").unwrap(),
            PreprocessorNode::Defined("FOO".into(), false)
        );
        assert_eq!(
            x("not defined(FOO)").unwrap(),
            PreprocessorNode::Defined("FOO".into(), true)
        );
        // Without a call shape, they are ordinary names (R7).
        assert_eq!(
            x("defined").unwrap(),
            PreprocessorNode::Var("defined".into())
        );
        assert_eq!(x("not").unwrap(), PreprocessorNode::Var("not".into()));
        assert_eq!(
            x("not defined").unwrap_err().to_string(),
            "unexpected tokens after `#if` expression"
        );
        assert_eq!(xerr("defined(1)"), "unexpected token in `#if` expression");
        assert_eq!(xerr("defined(FOO"), "unmatched `(` in `#if` expression");
    }

    #[test]
    fn expression_errors() {
        assert_eq!(xerr("(1 + 2"), "unmatched `(` in `#if` expression");
        assert_eq!(xerr("1 2"), "unexpected tokens after `#if` expression");
        assert_eq!(xerr("1 +"), "unexpected end of `#if` expression");
        assert_eq!(xerr(";"), "unexpected token in `#if` expression");
        assert_eq!(xerr("/* only */"), "expected an expression after `#if`");
        assert!(xerr("`").starts_with("Lex Error"));
    }

    #[test]
    fn expression_diagnostics_carry_file_spans() {
        // base pretends the operand starts at file offset 100.
        let base = Span::new(3, 100..110);
        let e = parse_if_expression("1 + `", base).expect_err("backtick");
        let span = e.span().expect("has a span");
        assert_eq!(span.file_id(), 3);
        assert_eq!(span.l(), 104); // 100 + the backtick's offset
    }
}
