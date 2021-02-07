use crate::parser::span::Span;
use std::fmt::{Formatter, Display};
use std::fmt;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::errors::LPCError;

/// Errors for division by zero at runtime.
#[derive(Debug)]
pub struct DivisionByZeroError {
    /// The code span related to the operation
    pub span: Option<Span>
}

impl LPCError for DivisionByZeroError {
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

impl Display for DivisionByZeroError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime Error: Division by zero")
    }
}
