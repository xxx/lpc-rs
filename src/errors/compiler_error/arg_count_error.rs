use crate::{errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct ArgCountError {
    /// The function name
    pub name: String,

    /// How many args were expected
    pub expected: usize,

    /// How many were actually passed
    pub actual: usize,

    /// The span of the call
    pub span: Option<Span>,

    /// The span of the function prototype
    pub prototype_span: Option<Span>,
}

impl LPCError for ArgCountError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(span.file_id, span.l..span.r));
        }

        if let Some(span) = self.prototype_span {
            labels.push(Label::secondary(span.file_id, span.l..span.r).with_message("Defined here"));
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for ArgCountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Incorrect argument count in call to `{}`: expected: {}, received: {}",
            self.name, self.expected, self.actual
        )
    }
}
