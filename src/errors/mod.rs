use std::{
    error::Error,
    fmt::{Debug, Display},
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{ColorChoice, StandardStream},
};
use lalrpop_util::ParseError as LalrpopParseError;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;

use crate::{
    errors::lazy_files::FILE_CACHE,
    parser::{lexer::Token, span::Span},
};

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
}

impl LpcError {
    /// Create a new LpcError, with a message
    pub fn new<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self {
            message: message.into(),
            span: None,
            labels: Vec::new(),
            notes: Vec::new(),
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

    pub fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
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

        if !self.notes.is_empty() {
            diagnostic = diagnostic.with_notes(self.notes.to_vec())
        }

        vec![diagnostic]
    }
}

impl Display for LpcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LpcError {}

/// Map LALRpop's parse errors into our local error type
impl<'a, E> From<LalrpopParseError<usize, Token, E>> for LpcError
where
    E: Display,
{
    fn from(err: LalrpopParseError<usize, Token, E>) -> Self {
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
            LalrpopParseError::User { error } => LpcError::new(format!("User error: {}", error)),
        }
    }
}

/// Emit nice error messages to the console.
///
/// # Arguments
/// * `errors` - A slice of [`LpcError`]s to display diagnostics for.
pub fn emit_diagnostics(errors: &[LpcError]) {
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

/// Just a shared helper.
pub fn format_expected(expected: &[String]) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}
