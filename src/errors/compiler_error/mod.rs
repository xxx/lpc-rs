pub mod lex_error;
pub mod parse_error;
pub mod return_type_error;
pub mod undefined_var_error;
pub mod unknown_function_error;
pub mod var_redefinition_error;

use codespan_reporting::diagnostic::Diagnostic;

use crate::errors::{preprocessor_error::PreprocessorError, LpcError, NewError};
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
    PreprocessorError(PreprocessorError),
    UnknownFunctionError(UnknownFunctionError),
    VarRedefinitionError(VarRedefinitionError),
    ReturnTypeError(ReturnTypeError),
    UndefinedVarError(UndefinedVarError),
    MultiError(Vec<CompilerError>),

    NewError(NewError)
}

impl LpcError for CompilerError {
    /// Get the error diagnostics for printing to the user.
    fn to_diagnostics(&self) -> Vec<Diagnostic<usize>> {
        match self {
            CompilerError::PreprocessorError(err) => err.to_diagnostics(),
            CompilerError::UnknownFunctionError(err) => err.to_diagnostics(),
            CompilerError::VarRedefinitionError(err) => err.to_diagnostics(),
            CompilerError::ReturnTypeError(err) => err.to_diagnostics(),
            CompilerError::UndefinedVarError(err) => err.to_diagnostics(),
            CompilerError::MultiError(errs) => {
                errs.iter().flat_map(|e| e.to_diagnostics()).collect()
            },
            CompilerError::NewError(err) => err.to_diagnostics(),
        }
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::PreprocessorError(err) => write!(f, "{}", err),
            CompilerError::UnknownFunctionError(err) => write!(f, "{}", err),
            CompilerError::VarRedefinitionError(err) => write!(f, "{}", err),
            CompilerError::ReturnTypeError(err) => write!(f, "{}", err),
            CompilerError::UndefinedVarError(err) => write!(f, "{}", err),
            CompilerError::MultiError(errs) => {
                let s = errs.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(" ");
                write!(f, "{}", s)
            }
            CompilerError::NewError(err) => write!(f, "{}", err),
        }
    }
}

impl Error for CompilerError {}