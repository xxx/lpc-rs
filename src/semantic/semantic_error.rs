use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;
use crate::parser::span::Span;

/// Handle a semantic error by pretty printing it to the console.
///
/// # Arguments
///
/// * `filepath` - The path to the file the error originated in.
/// * `msg` - The error message
/// * `span` - An `Option` containing the character `Span` for the error
pub fn handle_semantic_error(filepath: &str, msg: &str, span: Option<Span>) {
    let input = fs::read_to_string(filepath)
        .unwrap_or_else(|_| panic!("cannot read file: {}", filepath));

    let mut files = SimpleFiles::new();
    let file_id = files.add(filepath, input);
    let mut diagnostic = Diagnostic::error()
        .with_message(msg);

    if let Some(span) = span {
        diagnostic = diagnostic.with_labels(
            vec!(
                Label::primary(file_id, span.l..span.r)
            )
        );
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    if let Err(e) = term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
        eprintln!("error attempting to emit semantic error: {:?}", e);
    };
}