use crate::parser::span::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::termcolor::{ColorChoice, StandardStream},
};

pub mod compiler_error;
pub mod runtime_error;

pub trait LPCError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>>;
}

/// Emit nice error messages to the console.
///
/// # Arguments
/// * `filename` - The name of the file, for the messaging. In practice, this is the full filepath.
/// * `file_content` - The actual content of the file, used for messaging.
/// * `errors` - A slice of errors to display diagnostics for.
pub fn emit_diagnostics<T>(filename: &str, file_content: &str, errors: &[T])
where
    T: LPCError,
{
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, file_content);

    let diagnostics: Vec<Diagnostic<usize>> = errors
        .iter()
        .flat_map(|e| e.to_diagnostics(file_id))
        .collect();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    for diagnostic in &diagnostics {
        if let Err(e) =
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, diagnostic)
        {
            eprintln!("error attempting to emit error: {:?}", e);
        };
    }
}

/// An extracted function that covers the most common use case for generating diagnostics.
pub fn default_diagnostic(
    message: String,
    file_id: usize,
    span: Option<Span>,
) -> Vec<Diagnostic<usize>> {
    let mut diagnostic = Diagnostic::error().with_message(message);

    if let Some(span) = span {
        let mut labels = vec![];
        labels.push(Label::primary(file_id, span.l..span.r));
        diagnostic = diagnostic.with_labels(labels);
    }

    vec![diagnostic]
}
