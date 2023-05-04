#![forbid(unsafe_code)]

use std::{
    convert::Infallible,
    error::Error,
    fmt::{Debug, Display, Formatter},
    fs::OpenOptions,
    hash::{Hash, Hasher},
    num::TryFromIntError,
    result,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::termcolor::{ColorChoice, StandardStream, WriteColor},
};
use codespan_reporting::term::termcolor::Buffer;
use derive_builder::UninitializedFieldError;
use itertools::Itertools;
use lalrpop_util::ParseError as LalrpopParseError;
use span::HasSpan;

use crate::{file_stream::FileStream, lazy_files::FILE_CACHE, span::Span};

pub mod file_stream;
pub mod lazy_files;
pub mod span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LpcErrorSeverity {
    Warning,
    Error,
    Bug,
}

/// A convenience helper for creating a new `LpcError`. (`Error` severity)
#[macro_export]
macro_rules! lpc_error {
    ($fmt:literal, $($arg:tt)*) => {
        $crate::LpcError::new(format!($fmt, $($arg)*)).into()
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::new($msg).with_span($span).into()
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::new(format!($fmt, $($arg)*)).with_span($span).into()
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::new($msg).into()
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::new($err).into()
    };
}

/// A convenience helper for creating a new `LpcError`. (`Warning` severity)
#[macro_export]
macro_rules! lpc_warning {
    ($fmt:literal, $($arg:tt)*) => {
        $crate::LpcError::new_warning(format!($fmt, $($arg)*)).into()
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::new_warning($msg).with_span($span).into()
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::new_warning(format!($fmt, $($arg)*)).with_span($span).into()
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::new_warning($msg).into()
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::new_warning($err).into()
    };
}

/// A convenience helper for creating a new `LpcError`. (`Bug` severity)
#[macro_export]
macro_rules! lpc_bug {
    ($fmt:literal, $($arg:tt)*) => {
        $crate::LpcError::new_bug(format!($fmt, $($arg)*)).into()
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::new_bug($msg).with_span($span).into()
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::new_bug(format!($fmt, $($arg)*)).with_span($span).into()
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::new_bug($msg).into()
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::new_bug($err).into()
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// The severity of this error. Warnings are printed, but do not stop
    /// compilation or execution.
    pub severity: LpcErrorSeverity,
}

impl LpcError {
    /// Create a new `LpcError` with severity [`LpcErrorSeverity::Error`], and a
    /// message
    pub fn new<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            span: None,
            labels: vec![],
            notes: vec![],
            additional_errors: None,
            stack_trace: None,
            severity: LpcErrorSeverity::Error,
        }
    }

    /// Create a new `LpcError` with severity [`LpcErrorSeverity::Warning`], and
    /// a message
    pub fn new_warning<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            span: None,
            labels: vec![],
            notes: vec![],
            additional_errors: None,
            stack_trace: None,
            severity: LpcErrorSeverity::Warning,
        }
    }

    /// Create a new `LpcError` with severity [`LpcErrorSeverity::Bug`], and
    /// a message
    pub fn new_bug<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            span: None,
            labels: vec![],
            notes: vec![],
            additional_errors: None,
            stack_trace: None,
            severity: LpcErrorSeverity::Bug,
        }
    }

    pub fn is_warning(&self) -> bool {
        self.severity == LpcErrorSeverity::Warning
    }

    pub fn is_error(&self) -> bool {
        self.severity == LpcErrorSeverity::Error
    }

    pub fn is_bug(&self) -> bool {
        self.severity == LpcErrorSeverity::Bug
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

    pub fn with_additional_errors(mut self, additional_errors: Vec<Box<LpcError>>) -> Self {
        self.additional_errors = Some(additional_errors.into_iter().map(|e| *e).collect());

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

    /// Emit this error's collected diagnostics to stderr
    pub fn emit_diagnostics(&self) {
        output_diagnostics(
            &self.to_diagnostics(),
            &mut StandardStream::stderr(ColorChoice::Auto).lock(),
        );
    }

    /// Emit this error's collected diagnostics
    pub fn _emit_diagnostics_to_file(&self, path: &str) {
        let file = OpenOptions::new().append(true).create(true).open(path);

        match file {
            Ok(file) => {
                let mut file_stream = FileStream::new(file);

                output_diagnostics(&self.to_diagnostics(), &mut file_stream);
            }
            Err(e) => {
                eprintln!("Error opening file `{path}`: {e}");
            }
        }
    }

    /// Emit the diagnostics as a String
    pub fn diagnostic_string(&self) -> String {
        let mut err = self.to_string();
        err.push('\n');

        let mut buffer = Buffer::ansi();
        let diagnostics = self.to_diagnostics();

        output_diagnostics(&diagnostics, &mut buffer);
        err.push_str(std::str::from_utf8(buffer.as_slice()).unwrap_or("<diagnostic with invalid utf8?>"));
        err.push('\n');

        err
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
impl<T> From<LalrpopParseError<usize, T, LpcError>> for LpcError
where
    T: Display + HasSpan,
{
    fn from(err: LalrpopParseError<usize, T, LpcError>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => lpc_error!("Invalid token"),
            LalrpopParseError::UnrecognizedEOF { ref expected, .. } => {
                LpcError::new("Unexpected EOF").with_note(format_expected(expected))
            }
            LalrpopParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                ref expected,
            } => LpcError::new(format!("Unrecognized Token: {token}"))
                .with_span(Some(token.span()))
                .with_note(format_expected(expected)),
            LalrpopParseError::ExtraToken {
                token: (_start, ref token, _end),
            } => LpcError::new(format!("Extra Token: `{token}`")).with_span(Some(token.span())),
            LalrpopParseError::User { error } => error,
        }
    }
}

/// Map LALRpop's parse errors into our local error type
impl<T> From<LalrpopParseError<usize, T, Box<LpcError>>> for LpcError
where
    T: Display + HasSpan,
{
    fn from(err: LalrpopParseError<usize, T, Box<LpcError>>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => lpc_error!("Invalid token"),
            LalrpopParseError::UnrecognizedEOF { ref expected, .. } => {
                LpcError::new("Unexpected EOF").with_note(format_expected(expected))
            }
            LalrpopParseError::UnrecognizedToken {
                token: (_start, ref token, _end),
                ref expected,
            } => LpcError::new(format!("Unrecognized Token: {token}"))
                .with_span(Some(token.span()))
                .with_note(format_expected(expected)),
            LalrpopParseError::ExtraToken {
                token: (_start, ref token, _end),
            } => LpcError::new(format!("Extra Token: `{token}`")).with_span(Some(token.span())),
            LalrpopParseError::User { error } => *error,
        }
    }
}

impl From<std::io::Error> for LpcError {
    fn from(e: std::io::Error) -> Self {
        Self::new(e.to_string())
    }
}

impl From<UninitializedFieldError> for LpcError {
    fn from(e: UninitializedFieldError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<&LpcError> for Diagnostic<usize> {
    fn from(error: &LpcError) -> Self {
        let mut diagnostic = match error.severity {
            LpcErrorSeverity::Warning => Diagnostic::warning(),
            LpcErrorSeverity::Error => Diagnostic::error(),
            LpcErrorSeverity::Bug => Diagnostic::bug(),
        };

        diagnostic = diagnostic.with_message(format!("{error}"));

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

impl<T> From<tokio::sync::mpsc::error::SendError<T>> for LpcError {
    fn from(e: tokio::sync::mpsc::error::SendError<T>) -> Self {
        Self::new(e.to_string())
    }
}

impl From<TryFromIntError> for LpcError {
    fn from(e: TryFromIntError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<TryFromIntError> for Box<LpcError> {
    fn from(e: TryFromIntError) -> Self {
        Self::new(lpc_error!(e.to_string()))
    }
}

impl From<Infallible> for Box<LpcError> {
    fn from(_e: Infallible) -> Self {
        unreachable!("this better be unreachable.")
    }
}

impl Hash for LpcError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.message.hash(state);
        self.severity.hash(state);
        self.span.hash(state);
        self.labels.iter().for_each(|l| {
            match l.style {
                LabelStyle::Primary => 0,
                LabelStyle::Secondary => 1,
            }
            .hash(state);
            l.file_id.hash(state);
            l.range.hash(state);
            l.message.hash(state);
        });
        self.notes.hash(state);
        self.additional_errors.hash(state);
        self.stack_trace.hash(state);
    }
}

/// Write a list of diagnostics to a writer.
pub fn output_diagnostics(diagnostics: &[Diagnostic<usize>], writer: &mut dyn WriteColor) {
    let files = FILE_CACHE.read();

    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        if let Err(e) = codespan_reporting::term::emit(writer, &config, &*files, diagnostic) {
            eprintln!(
                "error attempting to emit diagnostic: {e:?} ::: {diagnostic:?} ::: {files:?}"
            );
        };
    }
}

/// An extracted function that covers the most common use case for generating
/// diagnostics.
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

/// Common `Result` type
pub type Result<T> = result::Result<T, Box<LpcError>>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builder() {
        let error = LpcError::new("test error")
            .with_span(Some(Span::new(0, 0..1)))
            .with_note("test note")
            .with_label("my label", Some(Span::new(0, 0..1)))
            .with_additional_errors(vec![lpc_error!("test error 2")])
            .with_stack_trace(vec!["test".to_string(), "test2".to_string()]);

        assert_eq!(error.message, "test error");
        assert_eq!(error.span.unwrap().l, 0);
        assert_eq!(error.span.unwrap().r, 1);
        assert_eq!(error.notes[0], "test note");
        assert_eq!(error.labels[0].message, "my label");
        assert_eq!(error.additional_errors.unwrap()[0].message, "test error 2");
        assert_eq!(error.stack_trace.unwrap(), vec!["test", "test2"]);
    }

    #[test]
    fn test_severity() {
        let error = LpcError::new("test error");
        assert_eq!(error.severity, LpcErrorSeverity::Error);
        assert!(error.is_error());

        let error = LpcError::new_warning("test warning");
        assert_eq!(error.severity, LpcErrorSeverity::Warning);
        assert!(error.is_warning());

        let error = LpcError::new_bug("test bug");
        assert_eq!(error.severity, LpcErrorSeverity::Bug);
        assert!(error.is_bug());
    }

    #[test]
    fn test_into_diagnostic() {
        let error = LpcError::new("test error")
            .with_span(Some(Span::new(0, 0..1)))
            .with_note("test note")
            .with_label("my label", Some(Span::new(0, 0..7)))
            .with_additional_errors(vec![lpc_error!("test error 2")])
            .with_stack_trace(vec!["test".to_string(), "test2".to_string()]);

        let diagnostic = Diagnostic::from(&error);

        assert_eq!(diagnostic.message, "test error");
        assert_eq!(
            diagnostic.labels[0],
            Label::primary(0, 0..1).with_message("")
        );
        assert_eq!(
            diagnostic.labels[1],
            Label::secondary(0, 0..7).with_message("my label")
        );
        assert_eq!(diagnostic.notes[0], "test note");
        assert_eq!(diagnostic.notes[1], "Stack trace:\n\ntest2\ntest");
    }
}
