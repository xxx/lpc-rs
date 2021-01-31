use lalrpop_util::ParseError as LalrpopParseError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::{Display, Formatter, Debug};
use std::fmt;
use lalrpop_util::lexer::Token;
use crate::parser::span::Span;

#[derive(Debug, Clone)]
enum ParseErrorType {
    InvalidToken,
    UnrecognizedEOF,
    UnrecognizedToken,
    ExtraToken,
    User
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
    token: Option<ParseErrorToken>,
    expected: Option<Vec<String>>
}

fn format_expected(expected: &Vec<String>) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}

impl ParseError {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let diagnostic: Diagnostic<usize>;

        match &self.type_ {
            ParseErrorType::InvalidToken => {
                let loc = self.location.unwrap();
                diagnostic = Diagnostic::error()
                    .with_message("Invalid Token")
                    .with_labels(
                        vec!(
                            Label::primary(file_id, loc.l..loc.r)
                        )
                    )
            }
            ParseErrorType::UnrecognizedEOF => {
                let loc = self.location.unwrap();
                let expected = if let Some(e) = &self.expected {
                    e.clone()
                } else {
                    unreachable!();
                };

                diagnostic = Diagnostic::error()
                    .with_message("Unexpected EOF")
                    .with_labels(
                        vec!(
                            Label::primary(file_id, loc.l..loc.r)
                        )
                    )
                    .with_notes(vec![format_expected(&expected)]);
            }
            ParseErrorType::UnrecognizedToken => {
                let loc = self.location.unwrap();
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

                diagnostic = Diagnostic::error()
                    .with_message(format!("Unrecognized Token: {}", token.1))
                    .with_labels(
                        vec!(
                            Label::primary(file_id, loc.l..loc.r)
                        )
                    )
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
            ParseErrorType::User => {
                diagnostic = Diagnostic::error().with_message("User error")
            }
        }

        vec![diagnostic]
    }
}

impl<'a, E> From<LalrpopParseError<usize, Token<'a>, E>> for ParseError {
    fn from(err: LalrpopParseError<usize, Token<'a>, E>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { location } => {
                ParseError {
                    type_: ParseErrorType::InvalidToken,
                    location: Some(Span { l: location, r: location }),
                    token: None,
                    expected: None
                }
            }
            LalrpopParseError::UnrecognizedEOF { location, expected } => {
                ParseError {
                    type_: ParseErrorType::UnrecognizedEOF,
                    location: Some(Span { l: location, r: location }),
                    token: None,
                    expected: Some(expected)
                }
            }
            LalrpopParseError::UnrecognizedToken { token: (start, ref token, end), ref expected } => {
                ParseError {
                    type_: ParseErrorType::UnrecognizedToken,
                    location: Some(Span { l: start, r: end }),
                    token: Some(ParseErrorToken(token.0, token.1.to_string())),
                    expected: Some(expected.to_vec())
                }
            }
            LalrpopParseError::ExtraToken { ref token } => {
                ParseError {
                    type_: ParseErrorType::ExtraToken,
                    location: None,
                    token: Some(ParseErrorToken(token.0, token.1.to_string())),
                    expected: None
                }
            }

            // We don't track this error, to avoid having the type param leak *all over* the place.
            LalrpopParseError::User { error: _ } => {
                ParseError {
                    type_: ParseErrorType::User,
                    location: None,
                    token: None,
                    expected: None
                }
            }
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error: {:?} {:?} {:?} {:?}", self.type_, self.location, self.token, self.expected)
    }
}