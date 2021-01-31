use std::fmt;
use std::fmt::{Display, Formatter};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::parser::span::Span;
use crate::semantic::symbol::Symbol;

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct VarRedefinitionError {
    /// Reference to the original symbol
    pub symbol: Symbol,

    /// The span of the *re*definition
    pub span: Option<Span>
}

impl VarRedefinitionError {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error()
            .with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(file_id, span.l..span.r));
        }

        if let Some(span) = self.symbol.span {
            labels.push(
                Label::secondary(file_id, span.l..span.r)
                    .with_message("Originally defined here.")
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
