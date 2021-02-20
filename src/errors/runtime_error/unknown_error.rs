use crate::{errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic};
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use crate::errors::default_diagnostic;

/// Errors for unknown issues at runtime. Anything that pops one of these should be
/// fixed to use another error type.
#[derive(Debug)]
pub struct UnknownError {
    /// The code span
    pub span: Option<Span>,
}

impl LPCError for UnknownError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), file_id, self.span)
    }
}

impl Display for UnknownError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime Error: Unknown error. This is a fallback for when another error handler fails.")
    }
}
