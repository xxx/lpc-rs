use crate::{errors::NewError, parser::lexer::Token};
use lalrpop_util::ParseError as LalrpopParseError;
use std::fmt::Display;

fn format_expected(expected: &[String]) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}

/// Map LALRpop's parse errors into our local error type
impl<'a, E> From<LalrpopParseError<usize, Token, E>> for NewError
where
    E: Display,
{
    fn from(err: LalrpopParseError<usize, Token, E>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => NewError::new("Invalid token"),
            LalrpopParseError::UnrecognizedEOF { ref expected, .. } => {
                NewError::new("Unexpected EOF").with_note(format_expected(expected))
            }
            LalrpopParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                ref expected,
            } => NewError::new(format!("Unrecognized Token: {}", token))
                .with_span(Some(token.span()))
                .with_note(format_expected(expected)),
            LalrpopParseError::ExtraToken {
                token: (_start, ref token, _end),
            } => NewError::new(format!("Extra Token: `{}`", token)).with_span(Some(token.span())),
            LalrpopParseError::User { error } => NewError::new(format!("User error: {}", error)),
        }
    }
}
