use crate::{
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Errors for unknown issues at runtime. Anything that pops one of these should be
/// fixed to use another error type.
#[derive(Debug)]
pub struct UnknownError {
    /// The code span
    pub span: Option<Span>,
}

impl LpcError for UnknownError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for UnknownError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime Error: Unknown error. This is a fallback for when another error handler fails.")
    }
}
