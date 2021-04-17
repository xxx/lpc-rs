use crate::ast::binary_op_node::BinaryOperation;
use std::fmt::{Display, Formatter};

/// A node type for evaluating preprocessor `#if` expressions
#[derive(Debug, PartialEq)]
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

impl Display for PreprocessorNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PreprocessorNode::Var(v) => write!(f, "{}", v),
            PreprocessorNode::Int(i) => write!(f, "{}", i),
            PreprocessorNode::Defined(d) => write!(f, "defined({})", d),
            PreprocessorNode::BinaryOp(op, l, r) => write!(f, "{} {} {}", l, op, r)
        }
    }
}
