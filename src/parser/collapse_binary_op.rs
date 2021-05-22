use crate::{
    ast::{
        binary_op_node::{BinaryOpNode, BinaryOperation},
        expression_node::ExpressionNode,
        int_node::IntNode,
        string_node::StringNode,
    },
    parser::span::Span,
    LpcInt,
};
use std::iter::repeat;

/// Combine literals in cases where we have enough information to do so.
///
/// # Arguments
/// * `op` - The operation being performed
/// * `l` - The left operand
/// * `r` - The right operand
/// * `span` - The span encompassing the entire expression
pub fn collapse_binary_op(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match op {
        BinaryOperation::Add => collapse_add(op, l, r, span),
        BinaryOperation::Sub => collapse_sub(op, l, r, span),
        BinaryOperation::Mul => collapse_mul(op, l, r, span),
        BinaryOperation::Div => collapse_div(op, l, r, span),
        BinaryOperation::AndAnd => todo!(),
        BinaryOperation::OrOr => todo!(),
        BinaryOperation::Index | BinaryOperation::EqEq => ExpressionNode::BinaryOp(BinaryOpNode {
            l: Box::new(l),
            r: Box::new(r),
            op,
            span: Some(span),
        }),
    }
}

fn collapse_add(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match &l {
        ExpressionNode::Int(node) => match r {
            ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                value: node.value + node2.value,
                span: Some(span),
            }),
            ExpressionNode::String(node2) => ExpressionNode::String(StringNode {
                value: node.value.to_string() + &node2.value,
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

fn collapse_sub(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match &l {
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
    }
}

fn collapse_mul(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match &l {
        ExpressionNode::Int(node) => match r {
            ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                value: node.value * node2.value,
                span: Some(span),
            }),
            // 3 * "string" = "stringstringstring"
            ExpressionNode::String(node2) => collapse_repeat_string(node2.value, node.value, span),
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
                    collapse_repeat_string(node.value.clone(), node2.value, span)
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

fn collapse_div(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => {
            if node2.value != 0 {
                ExpressionNode::Int(IntNode {
                    value: node.value / node2.value,
                    span: Some(span),
                })
            } else {
                // Push it off until runtime so errors are nicer
                // This branch is only hit if you're dividing by a 0 int literal.
                ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(l),
                    r: Box::new(r),
                    op,
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

/// handle string * int and int * string
fn collapse_repeat_string(string: String, amount: LpcInt, span: Span) -> ExpressionNode {
    if amount >= 0 {
        let value = repeat(string).take(amount as usize).collect::<String>();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collapses_add_int_and_int() {
        let node1 = ExpressionNode::from(123);
        let node2 = ExpressionNode::from(456);
        let op = BinaryOperation::Add;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::Int(IntNode {
                value: 579,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_sub_int_and_int() {
        let node1 = ExpressionNode::from(123);
        let node2 = ExpressionNode::from(456);
        let op = BinaryOperation::Sub;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::Int(IntNode {
                value: -333,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_mul_int_and_int() {
        let node1 = ExpressionNode::from(123);
        let node2 = ExpressionNode::from(3);
        let op = BinaryOperation::Mul;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::Int(IntNode {
                value: 369,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_div_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(3);
        let op = BinaryOperation::Div;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::Int(IntNode {
                value: 40,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_add_int_and_string() {
        let node1 = ExpressionNode::from(123);
        let node2 = ExpressionNode::from("hello");
        let op = BinaryOperation::Add;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::String(StringNode {
                value: "123hello".to_string(),
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_add_string_and_int() {
        let node1 = ExpressionNode::from("hello");
        let node2 = ExpressionNode::from(123);
        let op = BinaryOperation::Add;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::String(StringNode {
                value: "hello123".to_string(),
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_mul_int_and_string() {
        let node1 = ExpressionNode::from(4);
        let node2 = ExpressionNode::from("hello");
        let op = BinaryOperation::Mul;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::String(StringNode {
                value: "hellohellohellohello".to_string(),
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_mul_string_and_int() {
        let node1 = ExpressionNode::from("hello");
        let node2 = ExpressionNode::from(4);
        let op = BinaryOperation::Mul;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_eq!(
            result,
            ExpressionNode::String(StringNode {
                value: "hellohellohellohello".to_string(),
                span: Some(span)
            })
        );
    }
}
