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

/// Error for mismatched types in binary operations
#[derive(Debug, Clone)]
pub struct AssignmentError {
    /// Name of left-hand term
    pub left_name: String,

    /// Type of left-side term
    pub left_type: LPCType,

    /// Name of left-hand term
    pub right_name: String,

    /// Type of right-side term
    pub right_type: LPCType,

    /// The span of the operation
    pub span: Option<Span>,
}

impl LPCError for AssignmentError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for AssignmentError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Mismatched types: `{}` ({}) = `{}` ({})",
            self.left_name, self.left_type, self.right_name, self.right_type
        )
    }
}
