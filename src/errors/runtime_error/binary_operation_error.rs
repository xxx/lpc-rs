use crate::{
    ast::binary_op_node::BinaryOperation,
    errors::{default_diagnostic, LpcError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

/// Errors for mismatched binary operations at runtime.
#[derive(Debug)]
pub struct BinaryOperationError {
    /// The operation
    pub op: BinaryOperation,

    /// Type of left-side term
    pub left_type: String,

    /// Type of right-side term
    pub right_type: String,

    /// The code span related to the operation
    pub span: Option<Span>,
}

impl LpcError for BinaryOperationError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.span)
    }
}

impl Display for BinaryOperationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Runtime Error: Mismatched types: ({}) {} ({})",
            self.left_type, self.op, self.right_type
        )
    }
}

impl Error for BinaryOperationError {}
