use crate::{
    errors::LpcError,
    parser::{lexer::Token, span::Span},
};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError as LalrpopParseError;
use std::{
    fmt,
    fmt::{Debug, Display, Formatter},
};

#[derive(Debug, Clone)]
enum ParseErrorType {
    InvalidToken,
    UnrecognizedEof,
    UnrecognizedToken,
    ExtraToken,
    User,
}

#[derive(Debug, Clone)]
struct ParseErrorToken(usize, String);

/// A wrapper around the lalrpop ParseError, to avoid the need to
/// drag explicit lifetimes and type params all across the AST.
/// Maybe there's a better way to do it.
#[derive(Debug, Clone)]
pub struct ParseError {
    type_: ParseErrorType,
    location: Option<Span>,
    token: Option<Token>,
    expected: Option<Vec<String>>,
}

fn format_expected(expected: &[String]) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}

impl LpcError for ParseError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let diagnostic: Diagnostic<usize>;

        match &self.type_ {
            ParseErrorType::InvalidToken => {
                let token = if let Some(t) = &self.token {
                    t
                } else {
                    unreachable!()
                };

                let span = token.span();
                diagnostic = Diagnostic::error()
                    .with_message("Invalid Token")
                    .with_labels(vec![Label::primary(span.file_id, span.l..span.r)])
            }
            ParseErrorType::UnrecognizedEof => {
                let token = if let Some(t) = &self.token {
                    t
                } else {
                    unreachable!()
                };

                let span = token.span();
                let expected = if let Some(e) = &self.expected {
                    e.clone()
                } else {
                    unreachable!();
                };

                diagnostic = Diagnostic::error()
                    .with_message("Unexpected EOF")
                    .with_labels(vec![Label::primary(span.file_id, span.l..span.r)])
                    .with_notes(vec![format_expected(&expected)]);
            }
            ParseErrorType::UnrecognizedToken => {
                let token = if let Some(t) = &self.token {
                    t
                } else {
                    unreachable!()
                };

                let expected = if let Some(e) = &self.expected {
                    e
                } else {
                    unreachable!()
                };

                let span = token.span();

                diagnostic = Diagnostic::error()
                    .with_message(format!("Unrecognized Token: {}", token))
                    .with_labels(vec![Label::primary(span.file_id, span.l..span.r)])
                    .with_notes(vec![format_expected(&expected)]);
            }
            ParseErrorType::ExtraToken => {
                let token = if let Some(t) = &self.token {
                    t
                } else {
                    unreachable!()
                };

                diagnostic = Diagnostic::error().with_message(format!("Extra Token {:?}", token))
            }
            ParseErrorType::User => diagnostic = Diagnostic::error().with_message("User error"),
        }

        vec![diagnostic]
    }
}

impl<'a, E> From<LalrpopParseError<usize, Token, E>> for ParseError {
    fn from(err: LalrpopParseError<usize, Token, E>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => ParseError {
                type_: ParseErrorType::InvalidToken,
                location: None,
                token: None,
                expected: None,
            },
            LalrpopParseError::UnrecognizedEOF { expected, .. } => ParseError {
                type_: ParseErrorType::UnrecognizedEof,
                location: None,
                token: None,
                expected: Some(expected),
            },
            LalrpopParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                ref expected,
            } => ParseError {
                type_: ParseErrorType::UnrecognizedToken,
                location: Some(token.span()),
                token: Some(token.clone()),
                expected: Some(expected.to_vec()),
            },
            LalrpopParseError::ExtraToken { ref token } => ParseError {
                type_: ParseErrorType::ExtraToken,
                location: None,
                token: Some(token.1.clone()),
                expected: None,
            },

            // We don't track this error, to avoid having the type param leak *all over* the place.
            LalrpopParseError::User { error: _ } => ParseError {
                type_: ParseErrorType::User,
                location: None,
                token: None,
                expected: None,
            },
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Parse error: {:?} {:?} {:?} {:?}",
            self.type_, self.location, self.token, self.expected
        )
    }
}
