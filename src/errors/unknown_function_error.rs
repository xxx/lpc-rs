use std::fmt;
use std::fmt::{Display, Formatter};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::parser::span::Span;

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct UnknownFunctionError {
    /// The function's name
    pub name: String,

    /// The span of the full call (including arguments)
    pub span: Option<Span>
}

impl UnknownFunctionError {
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

impl Display for UnknownFunctionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Call to unknown function `{}`", self.name)
    }
}
