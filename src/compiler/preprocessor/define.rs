use std::borrow::Cow;

use lpc_rs_errors::{source_map::SourceMap, span::HasSpan};

use crate::compiler::{lexer::Token, preprocessor::preprocessor_node::PreprocessorNode};

/// LPC keywords can name macros and parameters during preprocessing.
pub(super) fn identifier_name(token: &Token) -> Option<&str> {
    match token {
        Token::Id(name) => Some(&name.1),
        _ => token.keyword_text(),
    }
}

/// An object macro, e.g. `#define FOO 123`
#[derive(Debug, Clone)]
pub struct ObjectMacro {
    /// The vector of Tokens being defined
    pub tokens: Vec<Token>,
    /// The body as a preprocessor expression, for `#if`; `None` when the
    /// body is substitution-only text that no `#if` can evaluate.
    pub expr: Option<PreprocessorNode>,
}

/// A function macro, e.g. `#define FOO(a, b) (a * b)`
#[derive(Debug, Clone)]
pub struct FunctionMacro {
    /// Replacement tokens, including single-`#` directive tokens for stringification.
    pub tokens: Vec<Token>,
    /// The vector of defined arguments, which are matched against during
    /// expansion for replacement
    pub args: Vec<String>,
}

/// Enumeration of `#define` types
#[derive(Debug, Clone)]
pub enum Define {
    Object(ObjectMacro),
    Function(FunctionMacro),
}

impl Define {
    /// Create a new object macro
    pub fn new_object(tokens: Vec<Token>, expr: Option<PreprocessorNode>) -> Self {
        Define::Object(ObjectMacro { tokens, expr })
    }

    /// Create a new function macro
    pub fn new_function(tokens: Vec<Token>, args: Vec<String>) -> Self {
        Define::Function(FunctionMacro { tokens, args })
    }

    /// A benign redefinition has the same parameters, token spelling and whitespace positions.
    pub fn same_as(&self, other: &Define, sources: &SourceMap) -> bool {
        fn spelling<'a>(token: &Token, sources: &'a SourceMap) -> Cow<'a, str> {
            let span = token.span();
            sources
                .get(span.file_id())
                .ok()
                .and_then(|file| file.source().get(span.l()..span.r()))
                .map(Cow::Borrowed)
                .unwrap_or_else(|| Cow::Owned(token.to_string()))
        }
        let same_tokens = |a: &[Token], b: &[Token]| {
            a.len() == b.len()
                && a.iter().zip(b).enumerate().all(|(i, (left, right))| {
                    let same_spacing = i == 0
                        || (a[i - 1].span().r() < left.span().l())
                            == (b[i - 1].span().r() < right.span().l());
                    same_spacing
                        && std::mem::discriminant(left) == std::mem::discriminant(right)
                        && spelling(left, sources) == spelling(right, sources)
                })
        };
        match (self, other) {
            (Define::Object(a), Define::Object(b)) => same_tokens(&a.tokens, &b.tokens),
            (Define::Function(a), Define::Function(b)) => {
                a.args == b.args && same_tokens(&a.tokens, &b.tokens)
            }
            _ => false,
        }
    }
}
