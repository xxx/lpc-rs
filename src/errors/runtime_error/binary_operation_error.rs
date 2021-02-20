use crate::{ast::binary_op_node::BinaryOperation, errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};
use crate::errors::default_diagnostic;

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

impl LPCError for BinaryOperationError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), file_id, self.span)
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
