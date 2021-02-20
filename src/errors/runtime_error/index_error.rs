use crate::{errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use crate::errors::default_diagnostic;

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

impl LPCError for IndexError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), file_id, self.span)
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
