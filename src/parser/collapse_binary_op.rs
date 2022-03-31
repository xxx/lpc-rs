use crate::{
    ast::{
        binary_op_node::{BinaryOpNode, BinaryOperation},
        expression_node::ExpressionNode,
        int_node::IntNode,
        string_node::StringNode,
    },
    parser::{lexer, span::Span},
    util::repeat_string,
    LpcError, LpcInt,
};
use lalrpop_util::ParseError;

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
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    match op {
        BinaryOperation::Add => collapse_add(op, l, r, span),
        BinaryOperation::Sub => collapse_sub(op, l, r, span),
        BinaryOperation::Mul => collapse_mul(op, l, r, span),
        BinaryOperation::Div => collapse_div(op, l, r, span),
        BinaryOperation::Mod => collapse_mod(op, l, r, span),
        BinaryOperation::And => collapse_and(op, l, r, span),
        BinaryOperation::Or => collapse_or(op, l, r, span),
        BinaryOperation::Xor => collapse_xor(op, l, r, span),
        BinaryOperation::Shl => collapse_shl(op, l, r, span),
        BinaryOperation::Shr => collapse_shr(op, l, r, span),
        BinaryOperation::Index
        | BinaryOperation::EqEq
        | BinaryOperation::Lt
        | BinaryOperation::Lte
        | BinaryOperation::Gt
        | BinaryOperation::Gte
        | BinaryOperation::AndAnd
        | BinaryOperation::OrOr => Ok(default_node(op, l, r, span)),
    }
}

fn collapse_add(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match &l {
        ExpressionNode::Int(node) => match r {
            ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                value: node.value + node2.value,
                span: Some(span),
            }),
            ExpressionNode::String(node2) => ExpressionNode::String(StringNode {
                value: node.value.to_string() + &node2.value,
                span: Some(span),
            }),
            _ => default_node(op, l, r, span),
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
                _ => default_node(op, l, r, span),
            }
        }
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_sub(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match &l {
        ExpressionNode::Int(node) => match r {
            ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                value: node.value - node2.value,
                span: Some(span),
            }),
            _ => default_node(op, l, r, span),
        },
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_mul(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match &l {
        ExpressionNode::Int(node) => match r {
            ExpressionNode::Int(node2) => ExpressionNode::Int(IntNode {
                value: node.value * node2.value,
                span: Some(span),
            }),
            // 3 * "string" = "stringstringstring"
            ExpressionNode::String(node2) => collapse_repeat_string(node2.value, node.value, span)?,
            _ => default_node(op, l, r, span),
        },
        ExpressionNode::String(node) => {
            match r {
                // "string" * 3 == "stringstringstring"
                ExpressionNode::Int(node2) => {
                    collapse_repeat_string(node.value.clone(), node2.value, span)?
                }
                _ => default_node(op, l, r, span),
            }
        }
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_div(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => {
            if node2.value != 0 {
                ExpressionNode::Int(IntNode {
                    value: node.value / node2.value,
                    span: Some(span),
                })
            } else {
                // Push it off until runtime so errors are nicer
                // This branch is only hit if you're dividing by a 0 int literal.
                default_node(op, l, r, span)
            }
        }
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_mod(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => {
            if node2.value != 0 {
                ExpressionNode::Int(IntNode {
                    value: node.value % node2.value,
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
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_and(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => ExpressionNode::Int(IntNode {
            value: node.value & node2.value,
            span: Some(span),
        }),
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_or(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => ExpressionNode::Int(IntNode {
            value: node.value | node2.value,
            span: Some(span),
        }),
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_xor(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => ExpressionNode::Int(IntNode {
            value: node.value ^ node2.value,
            span: Some(span),
        }),
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_shl(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => ExpressionNode::Int(IntNode {
            value: node.value << node2.value,
            span: Some(span),
        }),
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

fn collapse_shr(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let result = match (&l, &r) {
        (ExpressionNode::Int(node), ExpressionNode::Int(node2)) => ExpressionNode::Int(IntNode {
            value: node.value >> node2.value,
            span: Some(span),
        }),
        _ => default_node(op, l, r, span),
    };

    Ok(result)
}

/// handle string * int and int * string
fn collapse_repeat_string(
    string: String,
    amount: LpcInt,
    span: Span,
) -> Result<ExpressionNode, ParseError<usize, lexer::Token, LpcError>> {
    let value = repeat_string::repeat_string(string.as_str(), amount)?;
    let node = ExpressionNode::String(StringNode {
        value,
        span: Some(span),
    });

    Ok(node)
}

#[inline]
fn default_node(
    op: BinaryOperation,
    l: ExpressionNode,
    r: ExpressionNode,
    span: Span,
) -> ExpressionNode {
    ExpressionNode::BinaryOp(BinaryOpNode {
        l: Box::new(l),
        r: Box::new(r),
        op,
        span: Some(span),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use claim::*;

    #[test]
    fn test_collapses_add_int_and_int() {
        let node1 = ExpressionNode::from(123);
        let node2 = ExpressionNode::from(456);
        let op = BinaryOperation::Add;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 40,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_mod_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(36);
        let op = BinaryOperation::Mod;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 12,
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
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
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::String(StringNode {
                value: "hellohellohellohello".to_string(),
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_handles_mul_string_int_overflow() {
        let node1 = ExpressionNode::from("hello");
        let node2 = ExpressionNode::from(LpcInt::MAX);
        let op = BinaryOperation::Mul;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_err!(result.clone());
        assert_eq!(
            result.unwrap_err().to_string().as_str(),
            "capacity overflow in string repetition"
        );
    }

    #[test]
    fn test_collapses_and_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(36);
        let op = BinaryOperation::And;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 32,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_or_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(36);
        let op = BinaryOperation::Or;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 124,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_xor_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(36);
        let op = BinaryOperation::Xor;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 92,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_shl_int_and_int() {
        let node1 = ExpressionNode::from(120);
        let node2 = ExpressionNode::from(36);
        let op = BinaryOperation::Shl;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 8246337208320,
                span: Some(span)
            })
        );
    }

    #[test]
    fn test_collapses_shr_int_and_int() {
        let node1 = ExpressionNode::from(120_000);
        let node2 = ExpressionNode::from(8);
        let op = BinaryOperation::Shr;
        let span = Span::new(0, 0..1);

        let result = collapse_binary_op(op, node1, node2, span);
        assert_ok!(result.clone());
        assert_eq!(
            result.unwrap(),
            ExpressionNode::Int(IntNode {
                value: 468,
                span: Some(span)
            })
        );
    }
}
