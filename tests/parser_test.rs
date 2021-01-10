use mathstack::mathstack_parser;
use mathstack::ast::int_node::IntNode;
use mathstack::ast::binary_op_node::{BinaryOpNode, BinaryOperation};
use mathstack::ast::expression_node::ExpressionNode;

#[test]
fn test_operator_precedence_add_first() {
    let expr = "1 + 2 * 3";
    let node = mathstack_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::BinaryOp(BinaryOpNode {
        l: Box::new(ExpressionNode::Int(IntNode::new(1))),
        r: Box::new(ExpressionNode::BinaryOp(
            BinaryOpNode {
                l: Box::new(ExpressionNode::Int(IntNode::new(2))),
                r: Box::new(ExpressionNode::Int(IntNode::new(3))),
                op: BinaryOperation::Mul
            }
        )),
        op: BinaryOperation::Add
    });

    assert_eq!(node, expected);
}

#[test]
fn test_operator_precedence_mul_first() {
    let expr = "3 * 2 + 1";
    let node = mathstack_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::BinaryOp(BinaryOpNode {
        l: Box::new(ExpressionNode::BinaryOp(
            BinaryOpNode {
                l: Box::new(ExpressionNode::Int(IntNode::new(3))),
                r: Box::new(ExpressionNode::Int(IntNode::new(2))),
                op: BinaryOperation::Mul
            }
        )),
        r: Box::new(ExpressionNode::Int(IntNode::new(1))),
        op: BinaryOperation::Add
    });

    assert_eq!(node, expected);
}