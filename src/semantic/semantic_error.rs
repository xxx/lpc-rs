use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::fs;
use crate::errors::VarRedefinitionError;

/// Handle a var redefinition error by pretty printing it to the console.
///
/// # Arguments
///
/// * `filepath` - The path to the file the error originated in.
/// * `err` - The error object
pub fn var_redefinition_error(filepath: &str, err: &VarRedefinitionError) {
    let input = fs::read_to_string(filepath)
        .unwrap_or_else(|_| panic!("cannot read file: {}", filepath));

    let mut files = SimpleFiles::new();
    let file_id = files.add(filepath, input);
    let mut diagnostic = Diagnostic::error()
        .with_message(format!("{}", err));
    let mut labels = vec![];

    if let Some(span) = err.span {
        labels.push(Label::primary(file_id, span.l..span.r));
    }

    if let Some(span) = err.symbol.span {
        labels.push(
            Label::secondary(file_id, span.l..span.r)
                .with_message("Originally defined here.")
        );
    }

    if labels.len() > 0 {
        diagnostic = diagnostic.with_labels(labels);
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    if let Err(e) = term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
        eprintln!("error attempting to emit semantic error: {:?}", e);
    };
}