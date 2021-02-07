use crate::errors::runtime_error::binary_operation_error::BinaryOperationError;
use codespan_reporting::diagnostic::Diagnostic;
use crate::errors::LPCError;

pub mod binary_operation_error;

#[derive(Debug)]
pub enum RuntimeError {
    BinaryOperationError(BinaryOperationError)
}

impl LPCError for RuntimeError {
    fn to_diagnostics(&self, file_id: usize) -> Vec<Diagnostic<usize>> {
        match self {
            RuntimeError::BinaryOperationError(err) => err.to_diagnostics(file_id)
        }
    }
}
