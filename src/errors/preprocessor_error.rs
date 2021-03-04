use crate::{
    errors::{default_diagnostic, LPCError},
    parser::span::Span,
};
use codespan_reporting::diagnostic::Diagnostic;
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

/// Handle preprocessing

#[derive(Debug, Clone)]
pub struct PreprocessorError {
    pub message: String,
    pub file_id: usize,
    pub span: Option<Span>,
}

impl Error for PreprocessorError {}

impl PreprocessorError {
    pub fn new(message: &str, file_id: usize, span: Span) -> Self {
        Self {
            message: String::from(message),
            file_id,
            span: Some(span),
        }
    }
}

impl LPCError for PreprocessorError {
    fn to_diagnostics(&self, _file_id: usize) -> Vec<Diagnostic<usize>> {
        default_diagnostic(format!("{}", self), self.file_id, self.span)
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PreprocessorError: {}", self.message)
    }
}
