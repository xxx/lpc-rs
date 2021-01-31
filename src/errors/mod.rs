use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use binary_operation_error::BinaryOperationError;
use var_redefinition_error::VarRedefinitionError;

pub mod binary_operation_error;
pub mod var_redefinition_error;

/// General error wrapper type
#[derive(Debug, Clone)]
pub enum CompilerError {
    ParseError,
    VarRedefinitionError(VarRedefinitionError),
    BinaryOperationError(BinaryOperationError),
    MultiError(Vec<CompilerError>),
}

impl CompilerError {
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            CompilerError::VarRedefinitionError(err) => err.to_diagnostics(file_id),
            CompilerError::BinaryOperationError(err) => err.to_diagnostics(file_id),
            CompilerError::MultiError(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics(file_id) ).collect()
            },
            _ => unimplemented!()
        }
    }
}

/// Emit nice error messages to the console.
///
/// # Arguments
/// * `filename` - The name of the file, for the messaging. In practice, this is the full filepath.
/// * `file_content` - The actual content of the file, used for messaging.
/// * `errors` - A vector of errors to display diagnostics for.
pub fn emit_diagnostics(filename: &str, file_content: &str, errors: &Vec<CompilerError>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, file_content);

    let diagnostics: Vec<Diagnostic<usize>> = errors
        .iter()
        .flat_map(|e| e.to_diagnostics(file_id))
        .collect();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    for diagnostic in &diagnostics {
        if let Err(e) = codespan_reporting::term::emit(&mut writer.lock(), &config, &files, diagnostic) {
            eprintln!("error attempting to emit semantic error: {:?}", e);
        };
    }
}
