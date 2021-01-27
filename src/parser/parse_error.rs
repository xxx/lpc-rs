use std::fmt::Debug;
use lalrpop_util::ParseError;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lalrpop_util::lexer::Token;

fn format_expected(expected: &Vec<String>) -> String {
    if expected.len() == 1 {
        format!("expected: {}", expected[0])
    } else {
        format!("expected one of: {}", expected.join(", "))
    }
}

/// Handle a parse error by pretty printing it to the console.
pub fn handle_parse_error<E>(filename: &str, input: &str, err: &ParseError<usize, Token, E>)
    where E: Debug {

    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, input);
    let diagnostic: Diagnostic<usize>;

    match err {
        ParseError::InvalidToken { location } => {
            diagnostic = Diagnostic::error()
                .with_message("Invalid Token")
                .with_labels(
                    vec!(
                        Label::primary(file_id, *location..*location)
                    )
                )
        }
        ParseError::UnrecognizedEOF { location, expected } => {
            diagnostic = Diagnostic::error()
                .with_message("Unexpected EOF")
                .with_labels(
                    vec!(
                        Label::primary(file_id, *location..*location)
                    )
                )
                .with_notes(vec![format_expected(expected)]);
        }
        ParseError::UnrecognizedToken { token: (start, ref tok, end), expected } => {
            diagnostic = Diagnostic::error()
                .with_message(format!("Unrecognized Token: {}", tok.1))
                .with_labels(
                    vec!(
                        Label::primary(file_id, *start..*end)
                    )
                )
                .with_notes(vec![format_expected(expected)]);
        }
        ParseError::ExtraToken { ref token } => {
            diagnostic = Diagnostic::error().with_message(format!("Extra Token {:?}", token))
        }
        ParseError::User { error } => {
            diagnostic = Diagnostic::error().with_message(format!("{:?}", error))
        }
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    if let Err(e) = term::emit(&mut writer.lock(), &config, &files, &diagnostic) {
        eprintln!("error attempting to emit parse error: {:?}", e);
    };
}