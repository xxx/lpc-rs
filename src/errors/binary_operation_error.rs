use std::fmt;
use std::fmt::{Display, Formatter};
use crate::ast::binary_op_node::BinaryOperation;
use crate::parser::span::Span;
use crate::semantic::lpc_type::LPCType;
use codespan_reporting::diagnostic::{Diagnostic, Label};

/// Error for mismatched types in binary operations
#[derive(Debug, Clone)]
pub struct BinaryOperationError {
    /// The operation
    pub op: BinaryOperation,

    /// Name of left-hand term
    pub left_name: String,

    /// Type of left-side term
    pub left_type: LPCType,

    /// Name of left-hand term
    pub right_name: String,

    /// Type of right-side term
    pub right_type: LPCType,

    /// The span of the operation
    pub span: Option<Span>
}

impl BinaryOperationError {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
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
               "Mismatched types: `{}` ({}) {} `{}` ({})",
               self.left_name,
               self.left_type,
               self.op,
               self.right_name,
               self.right_type
        )
    }
}
