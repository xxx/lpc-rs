use std::fmt::{Display, Formatter};
use std::fmt;
use crate::errors::{LPCError, default_diagnostic};
use codespan_reporting::diagnostic::Diagnostic;
use std::error::Error;

#[derive(Debug, Clone)]
pub struct LexError(pub String);

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl LPCError for LexError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("Lex Error: {}", self), file_id, &None)
    }
}

impl Error for LexError {}
