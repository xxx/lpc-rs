use crate::ast::binary_op_node::BinaryOperation;

/// A node type for evaluating preprocessor `#if` expressions
#[derive(Debug)]
pub enum PreprocessorNode {
    Var(String),
    // Char(char),
    // Macro(String),
    Int(i64),
    Defined(String),
    BinaryOp(
        BinaryOperation,
        Box<PreprocessorNode>,
        Box<PreprocessorNode>,
    ),
}
