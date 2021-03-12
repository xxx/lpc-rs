use crate::{errors::LPCError, parser::span::Span, semantic::symbol::Symbol};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct VarRedefinitionError {
    /// Reference to the original symbol
    pub symbol: Symbol,

    /// The span of the *re*definition
    pub span: Option<Span>,
}

impl LPCError for VarRedefinitionError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(span.file_id, span.l..span.r));
        }

        if let Some(span) = self.symbol.span {
            labels.push(
                Label::secondary(span.file_id, span.l..span.r).with_message("Originally defined here."),
            );
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for VarRedefinitionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Redefinition of `{}`", self.symbol.name)
    }
}
