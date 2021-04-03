use crate::parser::span::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{ColorChoice, StandardStream},
};
use std::fmt::{Debug, Display};

pub mod compiler_error;
pub mod lazy_files;
pub mod preprocessor_error;
pub mod runtime_error;
use crate::errors::lazy_files::FILE_CACHE;
use std::error::Error;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct NewError {
    message: String,
    span: Option<Span>,
    labels: Vec<Label<usize>>
}

impl NewError {
    /// Create a new LpcError, with a message
    pub fn new<T>(message: T) -> Self
    where
        T: Into<String>
    {
        Self {
            message: message.into(),
            span: None,
            labels: Vec::new()
        }
    }

    /// Set the primary span for this error
    pub fn with_span(mut self, span: Option<Span>) -> Self {
        self.span = span;

        self
    }

    /// Add a secondary label for this error
    pub fn with_label<T>(mut self, message: T, span: Option<Span>) -> Self
    where
        T: AsRef<str>
    {
        if let Some(s) = span {
            self.labels.push(
                Label::secondary(s.file_id, s.l..s.r).with_message(message.as_ref())
            );
        }

        self
    }

    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", self));
        let mut labels = vec![];

        if let Some(span) = self.span {
            labels.push(Label::primary(span.file_id, span.l..span.r));
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

impl Display for NewError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for NewError {}

pub trait LpcError: Debug + Error {
    /// Return a vector of [`Diagnostic`]s, to be emitted to the user.
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>>;
}

/// Emit nice error messages to the console.
///
/// # Arguments
/// * `errors` - A slice of [`LpcError`]s to display diagnostics for.
pub fn emit_diagnostics<T>(errors: &[T])
where
    T: LpcError,
{
    let files = FILE_CACHE.read();

    let diagnostics: Vec<Diagnostic<usize>> =
        errors.iter().flat_map(|e| e.to_diagnostics()).collect();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    for diagnostic in &diagnostics {
        if let Err(e) =
            codespan_reporting::term::emit(&mut writer.lock(), &config, &*files, diagnostic)
        {
            eprintln!("error attempting to emit error: {:?}", e);
        };
    }
}

/// An extracted function that covers the most common use case for generating diagnostics.
///
/// # Arguments
/// `message` - The main message for the error
/// `span` - The [`Span`] of the code that created this error
pub fn default_diagnostic(message: String, span: Option<Span>) -> Vec<Diagnostic<usize>> {
    let mut diagnostic = Diagnostic::error().with_message(message);

    if let Some(span) = span {
        let labels = vec![Label::primary(span.file_id, span.l..span.r)];
        diagnostic = diagnostic.with_labels(labels);
    }

    vec![diagnostic]
}
