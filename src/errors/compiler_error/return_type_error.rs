use crate::{
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
    semantic::lpc_type::LpcType,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use std::error::Error;

#[derive(Debug, Clone)]
pub struct ReturnTypeError {
    /// The return type
    pub type_: LpcType,

    /// The expected return type
    pub expected: LpcType,

    /// The span of the call
    pub span: Option<Span>,
}

impl LpcError for ReturnTypeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for ReturnTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Invalid return type {}. Expected {}.",
            self.type_, self.expected
        )
    }
}

impl Error for ReturnTypeError {}
