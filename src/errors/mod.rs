pub mod arg_count_error;
pub mod arg_type_error;
pub mod assignment_error;
pub mod binary_operation_error;
pub mod var_redefinition_error;
pub mod parse_error;
pub mod return_type_error;
pub mod undefined_var_error;
pub mod unknown_function_error;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use parse_error::ParseError;
use binary_operation_error::BinaryOperationError;
use var_redefinition_error::VarRedefinitionError;
use assignment_error::AssignmentError;
use unknown_function_error::UnknownFunctionError;
use arg_count_error::ArgCountError;
use arg_type_error::ArgTypeError;
use return_type_error::ReturnTypeError;
use undefined_var_error::UndefinedVarError;

/// General error wrapper type
#[derive(Debug, Clone)]
pub enum LPCError {
    ArgCountError(ArgCountError),
    ArgTypeError(ArgTypeError),
    AssignmentError(AssignmentError),
    BinaryOperationError(BinaryOperationError),
    ParseError(ParseError),
    UnknownFunctionError(UnknownFunctionError),
    VarRedefinitionError(VarRedefinitionError),
    ReturnTypeError(ReturnTypeError),
    UndefinedVarError(UndefinedVarError),
    MultiError(Vec<LPCError>),
}

impl LPCError {
    /// Get the error diagnostics for printing to the user.
    pub fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            LPCError::ArgCountError(err) => err.to_diagnostics(file_id),
            LPCError::ArgTypeError(err) => err.to_diagnostics(file_id),
            LPCError::AssignmentError(err) => err.to_diagnostics(file_id),
            LPCError::BinaryOperationError(err) => err.to_diagnostics(file_id),
            LPCError::ParseError(err) => err.to_diagnostics(file_id),
            LPCError::UnknownFunctionError(err) => err.to_diagnostics(file_id),
            LPCError::VarRedefinitionError(err) => err.to_diagnostics(file_id),
            LPCError::ReturnTypeError(err) => err.to_diagnostics(file_id),
            LPCError::UndefinedVarError(err) => err.to_diagnostics(file_id),
            LPCError::MultiError(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics(file_id) ).collect()
            }
        }
    }
}

/// Emit nice error messages to the console.
///
/// # Arguments
/// * `filename` - The name of the file, for the messaging. In practice, this is the full filepath.
/// * `file_content` - The actual content of the file, used for messaging.
/// * `errors` - A slice of errors to display diagnostics for.
pub fn emit_diagnostics(filename: &str, file_content: &str, errors: &[LPCError]) {
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
            eprintln!("error attempting to emit error: {:?}", e);
        };
    }
}
