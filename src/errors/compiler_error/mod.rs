pub mod arg_count_error;
pub mod arg_type_error;
pub mod assignment_error;
pub mod binary_operation_error;
pub mod lex_error;
pub mod parse_error;
pub mod range_error;
pub mod return_type_error;
pub mod undefined_var_error;
pub mod unknown_function_error;
pub mod var_redefinition_error;

use codespan_reporting::diagnostic::Diagnostic;

use crate::errors::{
    compiler_error::range_error::RangeError, preprocessor_error::PreprocessorError, LpcError,
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
use std::fmt::Display;
use modular_bitfield::private::static_assertions::_core::fmt::Formatter;
use std::error::Error;

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

impl LpcError for CompilerError {
    /// Get the error diagnostics for printing to the user.
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            CompilerError::ArgCountError(err) => err.to_diagnostics(),
            CompilerError::ArgTypeError(err) => err.to_diagnostics(),
            CompilerError::AssignmentError(err) => err.to_diagnostics(),
            CompilerError::BinaryOperationError(err) => err.to_diagnostics(),
            CompilerError::ParseError(err) => err.to_diagnostics(),
            CompilerError::PreprocessorError(err) => err.to_diagnostics(),
            CompilerError::UnknownFunctionError(err) => err.to_diagnostics(),
            CompilerError::VarRedefinitionError(err) => err.to_diagnostics(),
            CompilerError::RangeError(err) => err.to_diagnostics(),
            CompilerError::ReturnTypeError(err) => err.to_diagnostics(),
            CompilerError::UndefinedVarError(err) => err.to_diagnostics(),
            CompilerError::MultiError(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics()).collect()
            }
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::ArgCountError(err) => write!(f, "{}", err),
            CompilerError::ArgTypeError(err) => write!(f, "{}", err),
            CompilerError::AssignmentError(err) => write!(f, "{}", err),
            CompilerError::BinaryOperationError(err) => write!(f, "{}", err),
            CompilerError::ParseError(err) => write!(f, "{}", err),
            CompilerError::PreprocessorError(err) => write!(f, "{}", err),
            CompilerError::UnknownFunctionError(err) => write!(f, "{}", err),
            CompilerError::VarRedefinitionError(err) => write!(f, "{}", err),
            CompilerError::RangeError(err) => write!(f, "{}", err),
            CompilerError::ReturnTypeError(err) => write!(f, "{}", err),
            CompilerError::UndefinedVarError(err) => write!(f, "{}", err),
            CompilerError::MultiError(errs) => {
                let s = errs.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(" ");
                write!(f, "{}", s)
            }
        }
    }
}

impl Error for CompilerError {}