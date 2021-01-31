use crate::parser::span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct ArgCountError {
    /// The function name
    pub name: String,
    
    /// How many args were expected
    pub expected: usize,
    
    /// How many were actually passed
    pub actual: usize,

    /// The span of the operation
    pub span: Option<Span>
}

impl ArgCountError {
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

impl Display for ArgCountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,
               "Incorrect argument count in call to `{}`: expected: {}, received: {}",
               self.name,
               self.expected,
               self.actual
        )
    }
}
