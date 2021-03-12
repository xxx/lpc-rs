use crate::{errors::LPCError, parser::span::Span, semantic::lpc_type::LPCType};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct ArgTypeError {
    /// The argument name
    pub name: String,

    /// The argument type
    pub type_: LPCType,

    /// The expected arg type
    pub expected: LPCType,

    /// The span of the call
    pub span: Option<Span>,

    /// The span of the var declaration (within the function decl)
    pub declaration_span: Option<Span>,
}

impl LPCError for ArgTypeError {
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(span.file_id, span.l..span.r));
        }

        if let Some(span) = self.declaration_span {
            labels.push(Label::secondary(span.file_id, span.l..span.r).with_message("Declared here"));
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for ArgTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Unexpected argument type to `{}`: {}. Expected {}.",
            self.name, self.type_, self.expected
        )
    }
}
