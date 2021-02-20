use lpc_rs::{
    ast::{
        ast_node::ASTNode,
        binary_op_node::{BinaryOpNode, BinaryOperation},
        decl_node::DeclNode,
        expression_node::ExpressionNode,
        float_node::FloatNode,
        int_node::IntNode,
        program_node::ProgramNode,
        string_node::StringNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    lpc_parser,
    parser::span::Span,
    semantic::lpc_type::LPCType,
};

// just a helper for a very common pattern
fn assert_int(value: i64, expr: &str) {
    let node = lpc_parser::ExpressionParser::new().parse(expr).unwrap();

    let expected = ExpressionNode::Int(IntNode {
        value,
        span: Some(Span {
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn test_program_global_vars() {
    let prog = "int i = 123; int j = i - 8; string *k;";
    let node = lpc_parser::ProgramParser::new().parse(prog).unwrap();

    let expected = ProgramNode {
        body: vec![
            ASTNode::from(DeclNode {
                type_: LPCType::Int(false),
                initializations: vec![VarInitNode {
                    type_: LPCType::Int(false),
                    name: "i".to_string(),
                    value: Some(ExpressionNode::Int(IntNode {
                        value: 123,
                        span: Some(Span { l: 8, r: 11 }),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span { l: 4, r: 11 }),
                }],
            }),
            ASTNode::from(DeclNode {
                type_: LPCType::Int(false),
                initializations: vec![VarInitNode {
                    type_: LPCType::Int(false),
                    name: "j".to_string(),
                    value: Some(ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(ExpressionNode::Var(VarNode {
                            name: "i".to_string(),
                            span: Some(Span { l: 21, r: 22 }),
                            global: false,
                        })),
                        r: Box::new(ExpressionNode::Int(IntNode {
                            value: 8,
                            span: Some(Span { l: 25, r: 26 }),
                        })),
                        op: BinaryOperation::Sub,
                        span: Some(Span { l: 21, r: 26 }),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span { l: 17, r: 26 }),
                }],
            }),
            ASTNode::from(DeclNode {
                type_: LPCType::String(true),
                initializations: vec![VarInitNode {
                    type_: LPCType::String(true),
                    name: "k".to_string(),
                    value: None,
                    array: true,
                    global: true,
                    span: Some(Span { l: 35, r: 37 }),
                }],
            }),
        ],
    };

    assert_eq!(node, expected);
}

#[test]
fn test_operator_precedence_add_first() {
    assert_int(7, "1 + 2 * 3");
}

#[test]
fn test_operator_precedence_mul_first() {
    assert_int(7, "3 * 2 + 1");
}

#[test]
fn test_int_literal_collapse() {
    assert_int(6, "10 - 3 * 2 + 4 / 2");
}

#[test]
fn test_int_literal_underscores() {
    assert_int(1234567890, "1_234_56___7890");
}

#[test]
fn test_int_literal_hexadecimal() {
    assert_int(1638, "0x66_6");
    assert_int(0, "0X0");
}

#[test]
fn test_int_literal_octal() {
    assert_int(272, "0420");
    assert_int(0, "00");
}

#[test]
fn test_int_literal_binary() {
    assert_int(109, "0b1101101");
    assert_int(0, "0B0");
}

#[test]
fn test_float_literal_underscores() {
    let expr = "1_1.234_332e2_2";
    let node = lpc_parser::ExpressionParser::new().parse(expr).unwrap();

    let expected = ExpressionNode::Float(FloatNode {
        value: 112343320000000000000000.0,
        span: Some(Span {
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn test_string_literal_concat() {
    let expr = r##""foo" + "bar" + "baz" + "quux""##;
    let node = lpc_parser::ExpressionParser::new().parse(expr).unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from("foobarbazquux"),
        span: Some(Span {
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn test_string_literal_repeat() {
    let expr = r##""foo" * 3"##;
    let node = lpc_parser::ExpressionParser::new().parse(expr).unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from("foofoofoo"),
        span: Some(Span {
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);

    // test negative multiplier
    // TODO: Once unary negatives are in, reenable this.
    // let expr = r##""foo" * -3"##;
    // let node = lpc_parser::ExpressionParser::new().parse(expr).unwrap();
    //
    // let expected = ExpressionNode::String(StringNode {
    //     value: String::from(""),
    //     span: Some(Span {
    //         l: 0,
    //         r: expr.len(),
    //     }),
    // });
    //
    // assert_eq!(node, expected);
}
