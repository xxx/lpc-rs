pub mod binary_operation_error;
pub mod var_redefinition_error;

use codespan_reporting::diagnostic::Diagnostic;
use var_redefinition_error::VarRedefinitionError;
use binary_operation_error::BinaryOperationError;

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