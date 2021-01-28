use crate::ast::expression_node::ExpressionNode;
use crate::ast::binary_op_node::{BinaryOperation, BinaryOpNode};
use crate::ast::int_node::IntNode;
use std::iter::repeat;
use crate::ast::string_node::StringNode;

/// Combine literals in cases where we have enough information to do so.
pub fn collapse_expression(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode
) -> ExpressionNode {
    match op {
        BinaryOperation::Add => {
            match &l {
                ExpressionNode::Int(node) => {
                    match r {
                        ExpressionNode::Int(node2) => {
                            ExpressionNode::Int(IntNode { value: node.value + node2.value })
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                }
                ExpressionNode::String(node) => {
                    match r {
                        // "string" + 123 == "string123"
                        ExpressionNode::Int(node2) => {
                            ExpressionNode::String(StringNode {
                                value: node.value.clone() + &node2.value.to_string()
                            })
                        },
                        // concat string literals
                        ExpressionNode::String(node2) => {
                            ExpressionNode::String(
                                StringNode { value: node.value.clone() + &node2.value }
                            )
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                },
                _ => ExpressionNode::BinaryOp(
                    BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                )
            }
        },
        BinaryOperation::Sub => {
            match &l {
                ExpressionNode::Int(node) => {
                    match r {
                        ExpressionNode::Int(node2) => {
                            ExpressionNode::Int(IntNode { value: node.value - node2.value })
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                },
                _ => ExpressionNode::BinaryOp(
                    BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                )
            }
        },
        BinaryOperation::Mul => {
            match &l {
                ExpressionNode::Int(node) => {
                    match r {
                        ExpressionNode::Int(node2) => {
                            ExpressionNode::Int(IntNode { value: node.value * node2.value })
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                },
                ExpressionNode::String(node) => {
                    match r {
                        // "string" * 3 == "stringstringstring"
                        ExpressionNode::Int(node2) => {
                            if node2.value >= 0 {
                                let value = repeat(node.value.clone())
                                    .take(node2.value as usize).collect::<String>();
                                ExpressionNode::String(StringNode { value })
                            } else {
                                ExpressionNode::String(StringNode { value: String::from("") })
                            }
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                },
                _ => ExpressionNode::BinaryOp(
                    BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                )
            }
        },
        BinaryOperation::Div => {
            match &l {
                ExpressionNode::Int(node) => {
                    match r {
                        ExpressionNode::Int(node2) => {
                            ExpressionNode::Int(IntNode { value: node.value / node2.value })
                        },
                        _ => ExpressionNode::BinaryOp(
                            BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                        )
                    }
                },
                _ => ExpressionNode::BinaryOp(
                    BinaryOpNode { l: Box::new(l), r: Box::new(r), op }
                )
            }
        }
    }
}