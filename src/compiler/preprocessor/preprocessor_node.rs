use std::fmt::{Display, Formatter};

use lpc_rs_core::LpcIntInner;

use crate::compiler::ast::{binary_op_node::BinaryOperation, unary_op_node::UnaryOperation};

/// A node type for evaluating preprocessor `#if` expressions
#[derive(Debug, PartialEq, Clone)]
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
    UnaryOp(UnaryOperation, Box<PreprocessorNode>),
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
            PreprocessorNode::UnaryOp(op, inner) => write!(f, "{op}{inner}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_expressions_preserve_values_names_and_operators() {
        use PreprocessorNode::{BinaryOp, Defined, Int, String as Text, UnaryOp, Var};

        let cases = [
            (Var("FEATURE".into()), "FEATURE"),
            (Int(-7), "-7"),
            (Text("literal".into()), "literal"),
            (Defined("FEATURE".into(), false), "defined(FEATURE)"),
            (Defined("FEATURE".into(), true), "not defined(FEATURE)"),
            (
                BinaryOp(BinaryOperation::Add, Box::new(Int(2)), Box::new(Int(3))),
                "2 + 3",
            ),
            (
                UnaryOp(
                    UnaryOperation::Bang,
                    Box::new(Defined("FEATURE".into(), false)),
                ),
                "!defined(FEATURE)",
            ),
        ];

        for (expression, expected) in cases {
            assert_eq!(expression.to_string(), expected);
        }
    }
}
