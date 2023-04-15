use std::fmt::{Display, Formatter};

use lpc_rs_core::LpcIntInner;

use crate::compiler::ast::binary_op_node::BinaryOperation;

/// A node type for evaluating preprocessor `#if` expressions
#[derive(Debug, PartialEq)]
pub enum PreprocessorNode {
    Var(String),
    Int(LpcIntInner),
    String(String),
    /// `bool` denotes whether this is negated or not
    Defined(String, bool),
    BinaryOp(
        BinaryOperation,
        Box<PreprocessorNode>,
        Box<PreprocessorNode>,
    ),
}

impl Display for PreprocessorNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PreprocessorNode::Var(x) => write!(f, "{x}"),
            PreprocessorNode::Int(x) => write!(f, "{x}"),
            PreprocessorNode::String(x) => write!(f, "{x}"),
            PreprocessorNode::Defined(x, negated) => {
                let not = if *negated { "not " } else { "" };

                write!(f, "{not}defined({x})")
            }
            PreprocessorNode::BinaryOp(op, l, r) => write!(f, "{l} {op} {r}"),
        }
    }
}
