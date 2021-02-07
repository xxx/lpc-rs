use std::fmt;
use std::fmt::{Display, Formatter};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::parser::span::Span;

/// General error used by the interpreter at runtime
#[derive(Debug, Clone)]
pub struct RuntimeError {
    /// The message for the error
    pub message: String,

    /// The span corresponding to the instruction that errored.
    pub span: Option<Span>
}

impl RuntimeError {
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

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime error: {}", self.message)
    }
}
