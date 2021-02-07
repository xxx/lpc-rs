use crate::ast::binary_op_node::BinaryOperation;
use crate::parser::span::Span;
use std::fmt::{Formatter, Display};
use std::fmt;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::errors::LPCError;

/// Errors for mismatched binary operations at runtime.
pub struct BinaryOperationError {
    /// The operation
    pub op: BinaryOperation,

    /// Type of left-side term
    pub left_type: String,

    /// Type of right-side term
    pub right_type: String,

    /// The code span related to the operation
    pub span: Option<Span>
}

impl LPCError for BinaryOperationError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error()
            .with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(file_id, span.l..span.r));
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for BinaryOperationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,
               "Runtime Error: Mismatched types: ({}) {} ({})",
               self.left_type,
               self.op,
               self.right_type
        )
    }
}
