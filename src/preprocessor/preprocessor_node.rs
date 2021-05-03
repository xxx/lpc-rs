use crate::{ast::binary_op_node::BinaryOperation, LpcInt};
use std::fmt::{Display, Formatter};

/// A node type for evaluating preprocessor `#if` expressions
#[derive(Debug, PartialEq)]
pub enum PreprocessorNode {
    Var(String),
    // Char(char),
    Int(LpcInt),
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
            PreprocessorNode::Var(v) => write!(f, "{}", v),
            PreprocessorNode::Int(i) => write!(f, "{}", i),
            PreprocessorNode::Defined(d, negated) => {
                let not = if *negated { "not " } else { "" };

                write!(f, "{}defined({})", not, d)
            }
            PreprocessorNode::BinaryOp(op, l, r) => write!(f, "{} {} {}", l, op, r),
        }
    }
}
