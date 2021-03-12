use crate::errors::{default_diagnostic, LPCError};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct LexError(pub String);

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl LPCError for LexError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("Lex Error: {}", self), None)
    }
}

impl Error for LexError {}
