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

/// Error for mismatched types in Ranges
#[derive(Debug, Clone)]
pub struct RangeError {
    /// Name of left-side
    pub left_name: String,

    /// Type of left-side
    pub left_type: LPCType,

    /// Name of left-side
    pub right_name: String,

    /// Type of right-side
    pub right_type: LPCType,

    /// The span of the range
    pub span: Option<Span>,
}

impl LPCError for RangeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for RangeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Invalid range types: `{}` ({}) .. `{}` ({})",
            self.left_name, self.left_type, self.right_name, self.right_type
        )
    }
}
