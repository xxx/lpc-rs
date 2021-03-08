use crate::{errors::LPCError, parser::span::Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
    ops::Range,
};

/// Handle preprocessing

#[derive(Debug, Clone)]
pub struct PreprocessorError {
    pub message: String,
    pub file_id: usize,
    pub span: Option<Span>,
    pub labels: Vec<Label<usize>>,
}

impl Error for PreprocessorError {}

impl PreprocessorError {
    pub fn new<T>(message: T, file_id: usize, span: Span) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            file_id,
            span: Some(span),
            labels: Vec::new(),
        }
    }
}

impl PreprocessorError {
    pub fn add_label(&mut self, message: &str, file_id: usize, range: Range<usize>) {
        self.labels
            .push(Label::secondary(file_id, range).with_message(message));
    }
}

impl LPCError for PreprocessorError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(&self.message);
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(file_id, span.l..span.r));
        }

        for label in &self.labels {
            labels.push(label.clone());
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        vec![diagnostic]
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.message)
    }
}
