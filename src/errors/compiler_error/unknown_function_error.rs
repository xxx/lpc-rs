use crate::{
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use std::error::Error;

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct UnknownFunctionError {
    /// The function's name
    pub name: String,

    /// The span of the full call (including arguments)
    pub span: Option<Span>,
}

impl LpcError for UnknownFunctionError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for UnknownFunctionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Call to unknown function `{}`", self.name)
    }
}

impl Error for UnknownFunctionError {}
