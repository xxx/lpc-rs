use crate::{
    errors::{default_diagnostic, LPCError},
    parser::span::Span,
    semantic::lpc_type::LPCType,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct ReturnTypeError {
    /// The return type
    pub type_: LPCType,

    /// The expected return type
    pub expected: LPCType,

    /// The span of the call
    pub span: Option<Span>,
}

impl LPCError for ReturnTypeError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), file_id, &self.span)
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
