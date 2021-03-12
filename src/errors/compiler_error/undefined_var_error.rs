use crate::{
    errors::{default_diagnostic, LPCError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct UndefinedVarError {
    /// The name
    pub name: String,

    /// The span
    pub span: Option<Span>,
}

impl LPCError for UndefinedVarError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for UndefinedVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Undefined variable `{}`", self.name)
    }
}
