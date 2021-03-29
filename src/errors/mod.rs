use crate::parser::span::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{ColorChoice, StandardStream},
};
use std::fmt::Debug;

pub mod compiler_error;
pub mod lazy_files;
pub mod preprocessor_error;
pub mod runtime_error;
use crate::errors::lazy_files::FILE_CACHE;
use std::error::Error;

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
