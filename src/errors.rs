use std::fmt::{Display, Formatter};
use crate::semantic::symbol::Symbol;
use std::fmt;
use crate::parser::span::Span;
use crate::semantic::lpc_type::LPCVarType;
use crate::ast::binary_op_node::BinaryOperation;

/// General error wrapper type
#[derive(Debug, Clone)]
pub enum CompilerError<'a> {
    ParseError,
    VarRedefinitionError(VarRedefinitionError<'a>),
    BinaryOperationError(BinaryOperationError)
}

/// Error for duplicate var definitions in a single local scope.
#[derive(Debug, Clone)]
pub struct VarRedefinitionError<'a> {
    /// Reference to the original symbol
    pub symbol: &'a Symbol,

    /// The span of the *re*definition
    pub span: Option<Span>
}

impl Display for VarRedefinitionError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Redefinition of `{}`", self.symbol.name)
    }
}

/// Error for mismatched types in binary operations
#[derive(Debug, Clone)]
pub struct BinaryOperationError {
    /// The operation
    pub op: BinaryOperation,

    /// Name of left-hand term
    pub left_name: String,

    /// Type of left-side term
    pub left_type: LPCVarType,

    /// Name of left-hand term
    pub right_name: String,

    /// Type of right-side term
    pub right_type: LPCVarType,

    /// The span of the operation
    pub span: Option<Span>
}

impl Display for BinaryOperationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,
               "Mismatched types: {} ({}) {} {} ({})",
               self.left_name,
               self.left_type,
               self.op,
               self.right_name,
               self.right_type
        )
    }
}
