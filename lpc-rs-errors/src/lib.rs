#![forbid(unsafe_code)]

use std::{
    error::Error,
    fmt::{Debug, Display, Formatter},
    num::TryFromIntError,
    result,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{Buffer, ColorChoice, StandardStream, WriteColor},
};
use derive_builder::UninitializedFieldError;
use lalrpop_util::ParseError as LalrpopParseError;
use span::HasSpan;

use crate::{
    source_map::{FileId, SOURCE_MAP},
    span::Span,
};

pub mod source_map;
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
        $crate::LpcError::new(format!($fmt, $($arg)*))
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::new(format!($msg)).with_span($span)
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::new(format!($fmt, $($arg)*)).with_span($span)
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::new(format!($msg))
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::new($err)
    };
}

/// A convenience helper for creating a new `LpcError`. (`Warning` severity)
#[macro_export]
macro_rules! lpc_warning {
    ($fmt:literal, $($arg:tt)*) => {
        $crate::LpcError::warning(format!($fmt, $($arg)*))
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::warning(format!($msg)).with_span($span)
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::warning(format!($fmt, $($arg)*)).with_span($span)
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::warning(format!($msg))
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::warning($err)
    };
}

/// A convenience helper for creating a new `LpcError`. (`Bug` severity)
#[macro_export]
macro_rules! lpc_bug {
    ($fmt:literal, $($arg:tt)*) => {
        $crate::LpcError::bug(format!($fmt, $($arg)*))
    };
    ($span:expr, $msg:literal $(,)?) => {
        $crate::LpcError::bug(format!($msg)).with_span($span)
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::LpcError::bug(format!($fmt, $($arg)*)).with_span($span)
    };
    ($msg:literal $(,)?) => {
        $crate::LpcError::bug(format!($msg))
    };
    ($err:expr $(,)?) => {
        $crate::LpcError::bug($err)
    };
}

/// A compile-time or runtime diagnostic: a message, the code it points at,
/// and anything else worth printing alongside. One pointer wide, so it
/// travels in `Result` without a `Box` at the call site.
#[derive(Clone)]
pub struct LpcError(Box<Inner>);

#[derive(Debug, Clone)]
struct Inner {
    message: String,
    span: Option<Span>,
    labels: Vec<Label<FileId>>,
    notes: Vec<String>,
    /// Further diagnostics collected before this one, rendered after it.
    additional_errors: Vec<LpcError>,
    stack_trace: Option<Vec<String>>,
    severity: LpcErrorSeverity,
}

impl LpcError {
    fn with_severity<T>(message: T, severity: LpcErrorSeverity) -> Self
    where
        T: Into<String>,
    {
        Self(Box::new(Inner {
            message: message.into(),
            span: None,
            labels: vec![],
            notes: vec![],
            additional_errors: vec![],
            stack_trace: None,
            severity,
        }))
    }

    /// An error: stops compilation, and is catchable at runtime.
    pub fn new<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self::with_severity(message, LpcErrorSeverity::Error)
    }

    /// A warning: printed, but stops nothing.
    pub fn warning<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self::with_severity(message, LpcErrorSeverity::Warning)
    }

    /// A bug: a broken driver invariant, never catchable at runtime.
    pub fn bug<T>(message: T) -> Self
    where
        T: Into<String>,
    {
        Self::with_severity(message, LpcErrorSeverity::Bug)
    }

    /// An error raised while running LPC; the prefix is part of the message
    /// `catch()` hands back.
    pub fn runtime<T>(message: T) -> Self
    where
        T: AsRef<str>,
    {
        Self::new(format!("runtime error: {}", message.as_ref()))
    }

    /// A bug raised while running LPC.
    pub fn runtime_bug<T>(message: T) -> Self
    where
        T: AsRef<str>,
    {
        Self::bug(format!("runtime bug: {}", message.as_ref()))
    }

    pub fn message(&self) -> &str {
        &self.0.message
    }

    pub fn span(&self) -> Option<Span> {
        self.0.span
    }

    pub fn severity(&self) -> LpcErrorSeverity {
        self.0.severity
    }

    pub fn is_warning(&self) -> bool {
        self.0.severity == LpcErrorSeverity::Warning
    }

    pub fn is_bug(&self) -> bool {
        self.0.severity == LpcErrorSeverity::Bug
    }

    /// The other diagnostics of the same compile, in recording order.
    pub fn additional_errors(&self) -> &[LpcError] {
        &self.0.additional_errors
    }

    /// Set the primary span for this error
    pub fn with_span(mut self, span: Option<Span>) -> Self {
        self.0.span = span;

        self
    }

    /// Set the primary span only when none was recorded at the source.
    pub fn or_span(self, span: Option<Span>) -> Self {
        if self.0.span.is_some() {
            return self;
        }
        self.with_span(span)
    }

    /// Add a secondary label for this error
    pub fn with_label<T>(mut self, message: T, span: Option<Span>) -> Self
    where
        T: AsRef<str>,
    {
        if let Some(s) = span {
            self.0
                .labels
                .push(Label::secondary(s.file_id(), s.l()..s.r()).with_message(message.as_ref()));
        }

        self
    }

    /// Add some notes the diagnostic
    pub fn with_note<T>(mut self, note: T) -> Self
    where
        T: Into<String>,
    {
        self.0.notes.push(note.into());

        self
    }

    pub fn with_additional_errors(mut self, additional_errors: Vec<LpcError>) -> Self {
        self.0.additional_errors = additional_errors;

        self
    }

    /// Frames, innermost task first: a later attachment comes from the
    /// calling task, so its frames go in front.
    pub fn with_stack_trace(mut self, mut stack_trace: Vec<String>) -> Self {
        if let Some(inner) = self.0.stack_trace.take() {
            stack_trace.extend(inner);
        }
        self.0.stack_trace = Some(stack_trace);

        self
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic<FileId>> {
        let mut v = vec![Diagnostic::from(self)];
        v.extend(
            self.0
                .additional_errors
                .iter()
                .flat_map(|e| e.to_diagnostics()),
        );

        v
    }

    /// Emit this error's collected diagnostics to stderr
    pub fn emit_diagnostics(&self) {
        output_diagnostics(
            &self.to_diagnostics(),
            &mut StandardStream::stderr(ColorChoice::Auto).lock(),
        );
    }

    /// The diagnostics rendered as plain text; `emit_diagnostics` is the
    /// colored terminal output.
    pub fn diagnostic_string(&self) -> String {
        let mut buffer = Buffer::no_color();
        output_diagnostics(&self.to_diagnostics(), &mut buffer);

        String::from_utf8(buffer.into_inner()).unwrap_or_else(|_| self.to_string())
    }
}

impl Debug for LpcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.0, f)
    }
}

impl Display for LpcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.message)
    }
}

impl Error for LpcError {}

/// Map LALRpop's parse errors into our local error type
impl<T> From<LalrpopParseError<usize, T, LpcError>> for LpcError
where
    T: Display + HasSpan,
{
    fn from(err: LalrpopParseError<usize, T, LpcError>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { .. } => lpc_error!("Invalid token"),
            LalrpopParseError::UnrecognizedEof { ref expected, .. } => {
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

impl From<UninitializedFieldError> for LpcError {
    fn from(e: UninitializedFieldError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<&LpcError> for Diagnostic<FileId> {
    fn from(error: &LpcError) -> Self {
        let inner = &*error.0;
        let mut diagnostic = match inner.severity {
            LpcErrorSeverity::Warning => Diagnostic::warning(),
            LpcErrorSeverity::Error => Diagnostic::error(),
            LpcErrorSeverity::Bug => Diagnostic::bug(),
        };

        diagnostic = diagnostic.with_message(&inner.message);

        let mut labels = vec![];

        if let Some(span) = inner.span {
            labels.push(Label::primary(span.file_id(), span.l()..span.r()));
        }

        labels.extend(inner.labels.iter().cloned());

        if !labels.is_empty() {
            diagnostic = diagnostic.with_labels(labels);
        }

        if !inner.notes.is_empty() {
            diagnostic = diagnostic.with_notes(inner.notes.clone())
        }

        if let Some(stack_trace) = &inner.stack_trace {
            diagnostic.notes.push(format!(
                "Stack trace:\n\n{}",
                stack_trace
                    .iter()
                    .rev()
                    .map(String::as_str)
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }

        diagnostic
    }
}

impl From<TryFromIntError> for LpcError {
    fn from(e: TryFromIntError) -> Self {
        Self::new(e.to_string())
    }
}

/// Write a list of diagnostics to a writer.
pub fn output_diagnostics(diagnostics: &[Diagnostic<FileId>], writer: &mut dyn WriteColor) {
    let files = SOURCE_MAP.read();

    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        if let Err(e) =
            codespan_reporting::term::emit_to_write_style(writer, &config, &*files, diagnostic)
        {
            eprintln!("error attempting to emit diagnostic: {e:?} ::: {diagnostic:?}");
        };
    }
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
pub type Result<T> = result::Result<T, LpcError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_error_is_one_pointer_wide() {
        assert_eq!(std::mem::size_of::<LpcError>(), 8);
        assert_eq!(std::mem::size_of::<Result<()>>(), 8);
    }

    #[test]
    fn constructors_name_their_severity() {
        assert_eq!(LpcError::new("e").severity(), LpcErrorSeverity::Error);
        assert_eq!(LpcError::warning("w").severity(), LpcErrorSeverity::Warning);
        assert_eq!(LpcError::bug("b").severity(), LpcErrorSeverity::Bug);
    }

    #[test]
    fn the_message_and_span_are_readable() {
        let e = LpcError::new("boom").with_span(Some(Span::new(3, 1..4)));
        assert_eq!(e.message(), "boom");
        assert_eq!(e.span(), Some(Span::new(3, 1..4)));
    }

    #[test]
    fn macros_build_errors_by_value() {
        let plain: LpcError = lpc_error!("a {}", 1);
        let warned: LpcError = lpc_warning!(Some(Span::new(0, 0..1)), "b");
        let bugged: LpcError = lpc_bug!("c");
        assert_eq!(plain.message(), "a 1");
        assert!(warned.is_warning());
        assert!(bugged.is_bug());
        assert!(warned.span().is_some());
    }

    #[test]
    fn additional_errors_are_taken_by_value_and_rendered_after() {
        let e = LpcError::new("head").with_additional_errors(vec![lpc_error!("tail")]);
        let diagnostics = e.to_diagnostics();
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[1].message, "tail");
    }

    #[test]
    fn a_literal_message_formats_inline_captures() {
        let name = "x";
        let plain: LpcError = lpc_error!("undefined symbol {name}");
        let spanned: LpcError = lpc_error!(Some(Span::new(0, 0..1)), "undefined symbol {name}");
        assert_eq!(plain.message(), "undefined symbol x");
        assert_eq!(spanned.message(), "undefined symbol x");
    }

    #[test]
    fn runtime_constructors_own_the_prefix() {
        let e = LpcError::runtime("Division by zero");
        assert_eq!(e.to_string(), "runtime error: Division by zero");
        assert_eq!(e.severity(), LpcErrorSeverity::Error);

        let b = LpcError::runtime_bug("stack is empty");
        assert_eq!(b.to_string(), "runtime bug: stack is empty");
        assert!(b.is_bug());
    }

    #[test]
    fn or_span_fills_only_an_empty_span() {
        let first = Some(Span::new(1, 0..1));
        let second = Some(Span::new(2, 5..9));
        assert_eq!(LpcError::new("x").or_span(first).span(), first);
        assert_eq!(
            LpcError::new("x").with_span(first).or_span(second).span(),
            first
        );
    }

    #[test]
    fn diagnostic_string_states_the_message_once() {
        let rendered = LpcError::new("one of a kind").diagnostic_string();
        assert_eq!(rendered.matches("one of a kind").count(), 1);
    }

    #[test]
    fn diagnostic_string_is_plain_text() {
        let file_id = crate::source_map::SOURCE_MAP
            .write()
            .add("/plain.c".to_owned(), "int x = ;\n".to_owned());
        let rendered = LpcError::new("no escapes")
            .with_span(Some(Span::new(file_id, 8..9)))
            .diagnostic_string();
        assert!(!rendered.contains('\u{1b}'), "{rendered:?}");
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
