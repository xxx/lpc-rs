use crate::parser::span::Span;
use std::fmt::{Formatter, Display};
use std::fmt;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::errors::LPCError;

/// Errors for unknown issues at runtime. Anything that pops one of these should be
/// fixed to use another error type.
#[derive(Debug)]
pub struct UnknownError {
    /// The code span
    pub span: Option<Span>
}

impl LPCError for UnknownError {
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

impl Display for UnknownError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime Error: Unknown error. This is a fallback for when another error handler fails.")
    }
}
