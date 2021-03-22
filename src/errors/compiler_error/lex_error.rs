use crate::errors::{default_diagnostic, LPCError};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct LexError(pub String);

impl LPCError for LexError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), None)
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Lex Error: {}", self.0)
    }
}

impl Error for LexError {}
