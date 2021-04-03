use crate::{
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

/// Errors for indexing into an array when it isn't long enough.
#[derive(Debug)]
pub struct IndexError {
    /// index used
    pub index: i64,

    /// array length
    pub length: usize,

    /// The code span related to the operation
    pub span: Option<Span>,
}

impl LpcError for IndexError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for IndexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Runtime Error: Attempting to access index {} in an array of length {}",
            self.index, self.length
        )
    }
}

impl Error for IndexError {}
