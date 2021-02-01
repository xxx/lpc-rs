use crate::parser::span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::semantic::lpc_type::LPCReturnType;

#[derive(Debug, Clone)]
pub struct ReturnTypeError {
    /// The return type
    pub type_: LPCReturnType,

    /// The expected return type
    pub expected: LPCReturnType,
    
    /// The span of the call
    pub span: Option<Span>,
}

impl ReturnTypeError {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error()
            .with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(file_id, span.l..span.r));
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for ReturnTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,
               "Invalid return type {}. Expected {}.",
               self.type_,
               self.expected
        )
    }
}
