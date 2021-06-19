use lpc_rs::{
    ast::{
        assignment_node::AssignmentNode,
        ast_node::AstNode,
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
    parser::{lexer::LexWrapper, span::Span},
    semantic::lpc_type::LpcType,
    LpcFloat, LpcInt,
};
use lpc_rs::ast::function_def_node::FunctionDefNode;

// just a helper for a very common pattern
fn assert_int(value: LpcInt, expr: &str) {
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::Int(IntNode {
        value,
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn program_global_vars() {
    let prog = "int i = 123; int j = i - 8; string *k;";
    let lexer = LexWrapper::new(prog);
    let node = lpc_parser::ProgramParser::new().parse(lexer).unwrap();

    let expected = ProgramNode {
        body: vec![
            AstNode::from(DeclNode {
                type_: LpcType::Int(false),
                initializations: vec![VarInitNode {
                    type_: LpcType::Int(false),
                    name: "i".to_string(),
                    value: Some(ExpressionNode::Int(IntNode {
                        value: 123,
                        span: Some(Span {
                            file_id: 0,
                            l: 8,
                            r: 11,
                        }),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span {
                        file_id: 0,
                        l: 4,
                        r: 11,
                    }),
                }],
            }),
            AstNode::from(DeclNode {
                type_: LpcType::Int(false),
                initializations: vec![VarInitNode {
                    type_: LpcType::Int(false),
                    name: "j".to_string(),
                    value: Some(ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(ExpressionNode::Var(VarNode {
                            name: "i".to_string(),
                            span: Some(Span {
                                file_id: 0,
                                l: 21,
                                r: 22,
                            }),
                            global: false,
                        })),
                        r: Box::new(ExpressionNode::Int(IntNode {
                            value: 8,
                            span: Some(Span {
                                file_id: 0,
                                l: 25,
                                r: 26,
                            }),
                        })),
                        op: BinaryOperation::Sub,
                        span: Some(Span {
                            file_id: 0,
                            l: 21,
                            r: 26,
                        }),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span {
                        file_id: 0,
                        l: 17,
                        r: 26,
                    }),
                }],
            }),
            AstNode::from(DeclNode {
                type_: LpcType::String(true),
                initializations: vec![VarInitNode {
                    type_: LpcType::String(true),
                    name: "k".to_string(),
                    value: None,
                    array: true,
                    global: true,
                    span: Some(Span {
                        file_id: 0,
                        l: 35,
                        r: 37,
                    }),
                }],
            }),
        ],
    };

    assert_eq!(node, expected);
}

#[test]
fn operator_precedence_add_first() {
    assert_int(7, "1 + 2 * 3");
}

#[test]
fn operator_precedence_mul_first() {
    assert_int(7, "3 * 2 + 1");
}

#[test]
fn int_literal_collapse() {
    assert_int(6, "10 - 3 * 2 + 4 / 2");
}

#[test]
fn int_literal_underscores() {
    assert_int(1234567890, "1_234_56___7890");
}

#[test]
fn int_literal_hexadecimal() {
    assert_int(1638, "0x66_6");
    assert_int(0, "0X0");
}

#[test]
fn int_literal_octal() {
    assert_int(272, "0420");
    assert_int(0, "00");
}

#[test]
fn int_literal_binary() {
    assert_int(109, "0b1101101");
    assert_int(0, "0B0");
}

#[test]
fn float_literal_underscores() {
    let expr = "1_1.234_332e2_2";
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::Float(FloatNode {
        value: LpcFloat::from(112343320000000000000000.0),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn string_literal_concat() {
    let expr = r##""foo" + "bar" + "baz" + "quux""##;
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from("foobarbazquux"),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn string_literal_repeat() {
    let expr = r##""foo" * 3"##;
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from("foofoofoo"),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);

    // test negative multiplier
    let expr = r##""foo" * -3"##;
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from(""),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn compound_assignment_decompose() {
    let expr = "a += 2";
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new().parse(lexer).unwrap();

    let expected = ExpressionNode::Assignment(AssignmentNode {
        lhs: Box::new(ExpressionNode::Var(VarNode {
            name: "a".to_string(),
            span: Some(Span {
                l: 0,
                r: 1,
                file_id: 0,
            }),
            global: false,
        })),
        rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
            l: Box::new(ExpressionNode::Var(VarNode {
                name: "a".to_string(),
                span: Some(Span {
                    l: 0,
                    r: 1,
                    file_id: 0,
                }),
                global: false,
            })),
            r: Box::new(ExpressionNode::Int(IntNode {
                value: 2,
                span: Some(Span {
                    l: 5,
                    r: 6,
                    file_id: 0,
                }),
            })),
            op: BinaryOperation::Add,
            span: Some(Span {
                l: 5,
                r: 6,
                file_id: 0,
            }),
        })),
        span: Some(Span {
            l: 0,
            r: 6,
            file_id: 0,
        }),
    });

    assert_eq!(node, expected);
}

#[test]
fn typeless_functions_are_mixed() {
    let prog = r#"marfin() {
            return "hello, we're marfin'!";
        }"#.replace("\n", "");
    let lexer = LexWrapper::new(&prog);
    let node = lpc_parser::FunctionDefParser::new().parse(lexer).unwrap();

    assert!(matches!(node, FunctionDefNode { return_type: LpcType::Mixed(false), .. }));
}
