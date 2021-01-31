use lalrpop_util::ParseError as LalrpopParseError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::{Display, Formatter, Debug};
use std::fmt;
use lalrpop_util::lexer::Token;

pub struct ParseError<'a, E> {
    pub error: LalrpopParseError<usize, Token<'a>, E>
}

fn format_expected(expected: &Vec<String>) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}

impl<'a, E> ParseError<'a, E> {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>>
        where E: Debug
    {
        let diagnostic: Diagnostic<usize>;

        match &self.error {
            LalrpopParseError::InvalidToken { location } => {
                diagnostic = Diagnostic::error()
                    .with_message("Invalid Token")
                    .with_labels(
                        vec!(
                            Label::primary(file_id, *location..*location)
                        )
                    )
            }
            LalrpopParseError::UnrecognizedEOF { location, expected } => {
                diagnostic = Diagnostic::error()
                    .with_message("Unexpected EOF")
                    .with_labels(
                        vec!(
                            Label::primary(file_id, *location..*location)
                        )
                    )
                    .with_notes(vec![format_expected(expected)]);
            }
            LalrpopParseError::UnrecognizedToken { token: (start, ref tok, end), expected } => {
                diagnostic = Diagnostic::error()
                    .with_message(format!("Unrecognized Token: {}", tok.1))
                    .with_labels(
                        vec!(
                            Label::primary(file_id, *start..*end)
                        )
                    )
                    .with_notes(vec![format_expected(expected)]);
            }
            LalrpopParseError::ExtraToken { ref token } => {
                diagnostic = Diagnostic::error().with_message(format!("Extra Token {:?}", token))
            }
            LalrpopParseError::User { error } => {
                diagnostic = Diagnostic::error().with_message(format!("{:?}", error))
            }
        }

        vec![diagnostic]
    }
}

impl<'a, E> Display for ParseError<'a, E>
    where E: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error: {:?}", self.error)
    }
}