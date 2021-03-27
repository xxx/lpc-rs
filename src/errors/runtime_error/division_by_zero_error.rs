use crate::{
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Errors for division by zero at runtime.
#[derive(Debug)]
pub struct DivisionByZeroError {
    /// The code span related to the operation
    pub span: Option<Span>,
}

impl LpcError for DivisionByZeroError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for DivisionByZeroError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime Error: Division by zero")
    }
}
