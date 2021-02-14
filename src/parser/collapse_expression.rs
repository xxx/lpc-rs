use crate::{
    ast::{
        binary_op_node::{BinaryOpNode, BinaryOperation},
        expression_node::ExpressionNode,
        int_node::IntNode,
        string_node::StringNode,
    },
    parser::span::Span,
};
use std::iter::repeat;

/// Combine literals in cases where we have enough information to do so.
///
/// # Arguments
/// * `op` - The operation being performed
/// * `l` - The left operand
/// * `r` - The right operand
/// * `span` - The span encompassing the entire expression
pub fn collapse_expression(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match op {
        BinaryOperation::Add => {
            match &l {
                ExpressionNode::Int(node) => match r {
                    ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                        value: node.value + node2.value,
                        span: Some(span),
                    }),
                    _ => ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(l),
                        r: Box::new(r),
                        op,
                        span: Some(span),
                    }),
                },
                ExpressionNode::String(node) => {
                    match r {
                        // "string" + 123 == "string123"
                        ExpressionNode::Int(node2) => ExpressionNode::String(StringNode {
                            value: node.value.clone() + &node2.value.to_string(),
                            span: Some(span),
                        }),
                        // concat string literals
                        ExpressionNode::String(node2) => ExpressionNode::String(StringNode {
                            value: node.value.clone() + &node2.value,
                            span: Some(span),
                        }),
                        _ => ExpressionNode::BinaryOp(BinaryOpNode {
                            l: Box::new(l),
                            r: Box::new(r),
                            op,
                            span: Some(span),
                        }),
                    }
                }
                _ => ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op,
                    span: Some(span),
                }),
            }
        }
        BinaryOperation::Sub => match &l {
            ExpressionNode::Int(node) => match r {
                ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                    value: node.value - node2.value,
                    span: Some(span),
                }),
                _ => ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op,
                    span: Some(span),
                }),
            },
            _ => ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(l),
                r: Box::new(r),
                op,
                span: Some(span),
            }),
        },
        BinaryOperation::Mul => {
            match &l {
                ExpressionNode::Int(node) => match r {
                    ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                        value: node.value * node2.value,
                        span: Some(span),
                    }),
                    _ => ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(l),
                        r: Box::new(r),
                        op,
                        span: Some(span),
                    }),
                },
                ExpressionNode::String(node) => {
                    match r {
                        // "string" * 3 == "stringstringstring"
                        ExpressionNode::Int(node2) => {
                            if node2.value >= 0 {
                                let value = repeat(node.value.clone())
                                    .take(node2.value as usize)
                                    .collect::<String>();
                                ExpressionNode::String(StringNode {
                                    value,
                                    span: Some(span),
                                })
                            } else {
                                ExpressionNode::String(StringNode {
                                    value: String::from(""),
                                    span: Some(span),
                                })
                            }
                        }
                        _ => ExpressionNode::BinaryOp(BinaryOpNode {
                            l: Box::new(l),
                            r: Box::new(r),
                            op,
                            span: Some(span),
                        }),
                    }
                }
                _ => ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op,
                    span: Some(span),
                }),
            }
        }
        BinaryOperation::Div => match &l {
            ExpressionNode::Int(node) => match r {
                ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                    value: node.value / node2.value,
                    span: Some(span),
                }),
                _ => ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op,
                    span: Some(span),
                }),
            },
            _ => ExpressionNode::BinaryOp(BinaryOpNode {
                l: Box::new(l),
                r: Box::new(r),
                op,
                span: Some(span),
            }),
        },
    }
}
