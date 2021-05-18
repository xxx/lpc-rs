use crate::ast::{
    expression_node::ExpressionNode,
    float_node::FloatNode,
    int_node::IntNode,
    unary_op_node::{UnaryOpNode, UnaryOperation},
};

/// # Arguments
/// * `op` - The operation being performed
/// * `l` - The left operand
/// * `r` - The right operand
/// * `span` - The span encompassing the entire expression
pub fn collapse_unary_op(node: UnaryOpNode) -> ExpressionNode {
    let UnaryOpNode { op, expr, span, .. } = node;

    match op {
        UnaryOperation::Negate => match *expr {
            ExpressionNode::Int(x) => ExpressionNode::Int(IntNode {
                value: -x.value,
                span,
            }),
            ExpressionNode::Float(x) => ExpressionNode::Float(FloatNode {
                value: -x.value,
                span,
            }),
            x => x,
        },
        UnaryOperation::Inc => todo!(),
        UnaryOperation::Dec => todo!(),
        UnaryOperation::Bang => todo!(),
        UnaryOperation::Tilde => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span::Span;
    use decorum::Total;

    #[test]
    fn test_collapses_negate_int() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(123)),
            is_post: false,
            op: UnaryOperation::Negate,
            span,
        };

        let result = collapse_unary_op(node);
        assert_eq!(result, ExpressionNode::Int(IntNode { value: -123, span }));
    }

    #[test]
    fn test_collapses_negate_float() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(3.14)),
            is_post: false,
            op: UnaryOperation::Negate,
            span,
        };

        let result = collapse_unary_op(node);
        assert_eq!(
            result,
            ExpressionNode::Float(FloatNode {
                value: Total::from(-3.14),
                span
            })
        );
    }
}
