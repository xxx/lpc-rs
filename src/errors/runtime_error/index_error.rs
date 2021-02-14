use crate::{ast::binary_op_node::BinaryOperation, errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Errors for indexing into an array when it isn't long enough.
#[derive(Debug)]
pub struct IndexError {
    /// index used
    pub index: i64,

    /// array length
    pub length: usize,

    /// The code span related to the operation
    pub span: Option<Span>,
}

impl LPCError for IndexError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(file_id, span.l..span.r));
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for IndexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Runtime Error: Attempting to access index {} in an array of length {}",
            self.index, self.length
        )
    }
}
