use std::{
    error::Error,
    fmt::{Debug, Display},
    fs::OpenOptions,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{ColorChoice, StandardStream, WriteColor},
};
use itertools::Itertools;
use lalrpop_util::ParseError as LalrpopParseError;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;

use crate::{
    errors::{file_stream::FileStream, lazy_files::FILE_CACHE},
    parser::{lexer::Token, span::Span},
};

pub mod file_stream;
pub mod lazy_files;

#[derive(Debug, Clone)]
pub struct LpcError {
    /// The main message to be printed out
    message: String,
    /// The primary span causing this error
    pub span: Option<Span>,
    /// Any secondary labels that are additionally printed with the error
    labels: Vec<Label<usize>>,
    /// Additional text notes, suggestions, etc. to be printed to the user.
    notes: Vec<String>,
    /// Additional errors that were collected before this one. This is only
    /// used during compilation, when non-fatal errors can occur.
    additional_errors: Option<Vec<LpcError>>,
    /// Optional stack trace for printing
    stack_trace: Option<Vec<String>>,
}

impl LpcError {
    /// Create a new `LpcError`, with a message
    pub fn new<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            span: None,
            labels: Vec::new(),
            notes: Vec::new(),
            additional_errors: None,
            stack_trace: None,
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
        T: AsRef<str>,
    {
        if let Some(s) = span {
            self.labels
                .push(Label::secondary(s.file_id, s.l..s.r).with_message(message.as_ref()));
        }

        self
    }

    /// Add some notes the diagnostic
    pub fn with_note<T>(mut self, note: T) -> Self
    where
        T: Into<String>,
    {
        self.notes.push(note.into());

        self
    }

    pub fn with_additional_errors(mut self, additional_errors: Vec<LpcError>) -> Self {
        self.additional_errors = Some(additional_errors);

        self
    }

    pub fn with_stack_trace(mut self, stack_trace: Vec<String>) -> Self {
        self.stack_trace = Some(stack_trace);

        self
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        let mut v = vec![Diagnostic::from(self)];

        if let Some(ref additional_errors) = self.additional_errors {
            v.extend(additional_errors.iter().flat_map(|e| e.to_diagnostics()));
        }

        v
    }

    /// Emit this error's collected diagnostics
    pub fn emit_diagnostics(&self) {
        {
            output_diagnostics(
                &self.to_diagnostics(),
                &mut StandardStream::stderr(ColorChoice::Auto).lock(),
            );
        }
    }

    /// Emit this error's collected diagnostics
    pub fn _emit_diagnostics_to_file(&self, path: &str) {
        let file = OpenOptions::new().write(true).create(true).open(path);

        match file {
            Ok(file) => {
                let mut file_stream = FileStream::new(file);

                output_diagnostics(&self.to_diagnostics(), &mut file_stream);
            }
            Err(e) => {
                eprintln!("Error opening file `{}`: {}", path, e);
            }
        }
    }
}

impl Display for LpcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LpcError {}

impl AsRef<str> for LpcError {
    fn as_ref(&self) -> &str {
        &self.message
    }
}

/// Map LALRpop's parse errors into our local error type
impl<'a> From<LalrpopParseError<usize, Token, LpcError>> for LpcError {
    fn from(err: LalrpopParseError<usize, Token, LpcError>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => LpcError::new("Invalid token"),
            LalrpopParseError::UnrecognizedEOF { ref expected, .. } => {
                LpcError::new("Unexpected EOF").with_note(format_expected(expected))
            }
            LalrpopParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                ref expected,
            } => LpcError::new(format!("Unrecognized Token: {}", token))
                .with_span(Some(token.span()))
                .with_note(format_expected(expected)),
            LalrpopParseError::ExtraToken {
                token: (_start, ref token, _end),
            } => LpcError::new(format!("Extra Token: `{}`", token)).with_span(Some(token.span())),
            LalrpopParseError::User { error } => error,
        }
    }
}

impl From<std::io::Error> for LpcError {
    fn from(e: std::io::Error) -> Self {
        Self::new(e.to_string())
    }
}

impl From<&LpcError> for Diagnostic<usize> {
    fn from(error: &LpcError) -> Self {
        let mut diagnostic = Diagnostic::error().with_message(format!("{}", error));
        let mut labels = vec![];

        if let Some(span) = error.span {
            labels.push(Label::primary(span.file_id, span.l..span.r));
        }

        for label in &error.labels {
            labels.push(label.clone());
        }

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        if !error.notes.is_empty() {
            diagnostic = diagnostic.with_notes(error.notes.clone())
        }

        if let Some(stack_trace) = &error.stack_trace {
            diagnostic.notes.push(format!(
                "Stack trace:\n\n{}",
                stack_trace.iter().rev().join("\n")
            ));
        }

        diagnostic
    }
}

fn output_diagnostics(diagnostics: &[Diagnostic<usize>], writer: &mut dyn WriteColor) {
    let files = FILE_CACHE.read();

    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        if let Err(e) = codespan_reporting::term::emit(writer, &config, &*files, diagnostic) {
            eprintln!(
                "error attempting to emit diagnostic: {:?} ::: {:?} ::: {:?}",
                e, diagnostic, files
            );
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

/// Just a shared helper.
pub fn format_expected(expected: &[String]) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}
