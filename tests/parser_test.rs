use lpc_rs::lpc_parser;
use lpc_rs::ast::int_node::IntNode;
use lpc_rs::ast::expression_node::ExpressionNode;
use lpc_rs::ast::string_node::StringNode;

#[test]
fn test_operator_precedence_add_first() {
    let expr = "1 + 2 * 3";
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::Int(IntNode { value: 7 });

    assert_eq!(node, expected);
}

#[test]
fn test_operator_precedence_mul_first() {
    let expr = "3 * 2 + 1";
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::Int(IntNode { value: 7 });

    assert_eq!(node, expected);
}

#[test]
fn test_int_literal_collapse() {
    let expr = "10 - 3 * 2 + 4 / 2";
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::Int(IntNode::new(6));

    assert_eq!(node, expected);
}

#[test]
fn test_string_literal_concat() {
    let expr = r##""foo" + "bar" + "baz" + "quux""##;
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::String(StringNode::new("foobarbazquux"));

    assert_eq!(node, expected);
}

#[test]
fn test_string_literal_repeat() {
    let expr = r##""foo" * 3"##;
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::String(StringNode::new("foofoofoo"));

    assert_eq!(node, expected);

    // test negative multiplier
    let expr = r##""foo" * -3"##;
    let node = lpc_parser::ExpressionParser::new()
        .parse(expr)
        .unwrap();

    let expected = ExpressionNode::String(StringNode::new(""));

    assert_eq!(node, expected);
}
