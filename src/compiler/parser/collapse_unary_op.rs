use crate::compiler::ast::{
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
    match node.op {
        UnaryOperation::Negate => match &*node.expr {
            ExpressionNode::Int(x) => ExpressionNode::Int(IntNode {
                value: -x.value,
                span: node.span,
            }),
            ExpressionNode::Float(x) => ExpressionNode::Float(FloatNode {
                value: -x.value,
                span: node.span,
            }),
            _ => ExpressionNode::UnaryOp(node),
        },
        UnaryOperation::Inc | UnaryOperation::Dec | UnaryOperation::Bang => {
            ExpressionNode::UnaryOp(node)
        }
        UnaryOperation::BitwiseNot => match &*node.expr {
            ExpressionNode::Int(x) => ExpressionNode::Int(IntNode {
                value: !x.value,
                span: node.span,
            }),
            _ => ExpressionNode::UnaryOp(node),
        },
    }
}

#[cfg(test)]
mod tests {
    use decorum::Total;
    use lpc_rs_errors::span::Span;

    use super::*;

    #[test]
    fn collapses_negate_int() {
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
    fn collapses_negate_float() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(4.13)),
            is_post: false,
            op: UnaryOperation::Negate,
            span,
        };

        let result = collapse_unary_op(node);
        assert_eq!(
            result,
            ExpressionNode::Float(FloatNode {
                value: Total::from(-4.13),
                span
            })
        );
    }

    #[test]
    fn collapses_bang() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(4.13)),
            is_post: false,
            op: UnaryOperation::Bang,
            span,
        };

        let result = collapse_unary_op(node.clone());
        assert_eq!(result, ExpressionNode::UnaryOp(node));
    }

    #[test]
    fn collapses_inc() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(123)),
            is_post: false,
            op: UnaryOperation::Inc,
            span,
        };

        let result = collapse_unary_op(node.clone());
        assert_eq!(result, ExpressionNode::UnaryOp(node));
    }

    #[test]
    fn collapses_dec() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(123)),
            is_post: false,
            op: UnaryOperation::Dec,
            span,
        };

        let result = collapse_unary_op(node.clone());
        assert_eq!(result, ExpressionNode::UnaryOp(node));
    }

    #[test]
    fn collapses_bitwise_not_int() {
        let span = Some(Span::new(0, 0..1));
        let node = UnaryOpNode {
            expr: Box::new(ExpressionNode::from(123)),
            is_post: false,
            op: UnaryOperation::BitwiseNot,
            span,
        };

        let result = collapse_unary_op(node);
        assert_eq!(result, ExpressionNode::Int(IntNode { value: !123, span }));
    }
}
