use crate::{errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct UndefinedVarError {
    /// The name
    pub name: String,

    /// The span
    pub span: Option<Span>,
}

impl LPCError for UndefinedVarError {
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

impl Display for UndefinedVarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Undefined variable `{}`", self.name)
    }
}
