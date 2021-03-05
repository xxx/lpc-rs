pub mod arg_count_error;
pub mod arg_type_error;
pub mod assignment_error;
pub mod binary_operation_error;
pub mod parse_error;
pub mod range_error;
pub mod return_type_error;
pub mod undefined_var_error;
pub mod unknown_function_error;
pub mod var_redefinition_error;

use codespan_reporting::diagnostic::Diagnostic;

use crate::errors::{
    compiler_error::range_error::RangeError, preprocessor_error::PreprocessorError, LPCError,
};
use arg_count_error::ArgCountError;
use arg_type_error::ArgTypeError;
use assignment_error::AssignmentError;
use binary_operation_error::BinaryOperationError;
use parse_error::ParseError;
use return_type_error::ReturnTypeError;
use undefined_var_error::UndefinedVarError;
use unknown_function_error::UnknownFunctionError;
use var_redefinition_error::VarRedefinitionError;

/// General error wrapper type for the compiler
#[derive(Debug, Clone)]
pub enum CompilerError {
    ArgCountError(ArgCountError),
    ArgTypeError(ArgTypeError),
    AssignmentError(AssignmentError),
    BinaryOperationError(BinaryOperationError),
    ParseError(ParseError),
    PreprocessorError(PreprocessorError),
    UnknownFunctionError(UnknownFunctionError),
    VarRedefinitionError(VarRedefinitionError),
    RangeError(RangeError),
    ReturnTypeError(ReturnTypeError),
    UndefinedVarError(UndefinedVarError),
    MultiError(Vec<CompilerError>),
}

impl LPCError for CompilerError {
    /// Get the error diagnostics for printing to the user.
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            CompilerError::ArgCountError(err) => err.to_diagnostics(file_id),
            CompilerError::ArgTypeError(err) => err.to_diagnostics(file_id),
            CompilerError::AssignmentError(err) => err.to_diagnostics(file_id),
            CompilerError::BinaryOperationError(err) => err.to_diagnostics(file_id),
            CompilerError::ParseError(err) => err.to_diagnostics(file_id),
            CompilerError::PreprocessorError(err) => err.to_diagnostics(file_id),
            CompilerError::UnknownFunctionError(err) => err.to_diagnostics(file_id),
            CompilerError::VarRedefinitionError(err) => err.to_diagnostics(file_id),
            CompilerError::RangeError(err) => err.to_diagnostics(file_id),
            CompilerError::ReturnTypeError(err) => err.to_diagnostics(file_id),
            CompilerError::UndefinedVarError(err) => err.to_diagnostics(file_id),
            CompilerError::MultiError(errs) => errs
                .iter()
                .flat_map(|e| e.to_diagnostics(file_id))
                .collect(),
        }
    }
}
