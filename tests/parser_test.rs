use claim::assert_ok;
use if_chain::if_chain;
use indoc::indoc;
use lpc_rs::{
    compiler::{
        ast::{
            assignment_node::AssignmentNode,
            ast_node::AstNode,
            binary_op_node::{BinaryOpNode, BinaryOperation},
            decl_node::DeclNode,
            expression_node::ExpressionNode,
            float_node::FloatNode,
            function_def_node::FunctionDefNode,
            function_ptr_node::FunctionPtrNode,
            int_node::IntNode,
            program_node::ProgramNode,
            string_node::StringNode,
            var_init_node::VarInitNode,
            var_node::VarNode,
        },
        compilation_context::CompilationContext,
        lexer::{LexWrapper, TokenVecWrapper},
        Compiler,
    },
    lpc_parser,
};
use lpc_rs_core::{
    global_var_flags::GlobalVarFlags, lpc_type::LpcType, visibility::Visibility, LpcFloat, LpcInt,
};
use lpc_rs_errors::{span::Span, Result};

// just a helper for a very common pattern
fn assert_int(value: LpcInt, expr: &str) {
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

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
    let prog = "int i = 123; private int j = i - 8; private static string *k;";
    let lexer = LexWrapper::new(prog);
    let node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let expected = ProgramNode {
        inherits: vec![],
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
                    flags: Some(GlobalVarFlags::new().with_visibility(Visibility::Public)),
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
                                l: 29,
                                r: 30,
                            }),
                            global: false,
                            function_name: false,
                            external_capture: false,
                        })),
                        r: Box::new(ExpressionNode::Int(IntNode {
                            value: 8,
                            span: Some(Span {
                                file_id: 0,
                                l: 33,
                                r: 34,
                            }),
                        })),
                        op: BinaryOperation::Sub,
                        span: Some(Span {
                            file_id: 0,
                            l: 29,
                            r: 34,
                        }),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span {
                        file_id: 0,
                        l: 25,
                        r: 34,
                    }),
                    flags: Some(GlobalVarFlags::new().with_visibility(Visibility::Private)),
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
                        l: 58,
                        r: 60,
                    }),
                    flags: Some(
                        GlobalVarFlags::new()
                            .with_visibility(Visibility::Private)
                            .with_is_static(true),
                    ),
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
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

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
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from("foobarbazquux"),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);

    // test overflow
    let expr = r##"("f" * 1_000_000_000) + ("b" * 1_000_000_000)"##;
    let lexer = LexWrapper::new(expr);
    let error = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap_err();

    assert_eq!(error.to_string(), "overflow in string concatenation");
}

#[test]
fn string_literal_repeat() {
    let expr = r##""foo" * 3"##;
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

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
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let expected = ExpressionNode::String(StringNode {
        value: String::from(""),
        span: Some(Span {
            file_id: 0,
            l: 0,
            r: expr.len(),
        }),
    });

    assert_eq!(node, expected);

    // test overflow
    let expr = r##""foo" * 9223372036854775807"##;
    let lexer = LexWrapper::new(expr);
    let error = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap_err();

    assert_eq!(error.to_string(), "overflow in string repetition");
}

#[test]
fn compound_assignment_decompose() {
    let expr = "a += 2";
    let lexer = LexWrapper::new(expr);
    let node = lpc_parser::ExpressionParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let expected = ExpressionNode::Assignment(AssignmentNode {
        lhs: Box::new(ExpressionNode::Var(VarNode {
            name: "a".to_string(),
            span: Some(Span {
                l: 0,
                r: 1,
                file_id: 0,
            }),
            global: false,
            function_name: false,
            external_capture: false,
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
                function_name: false,
                external_capture: false,
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
        }"#
    .replace('\n', "");
    let lexer = LexWrapper::new(&prog);
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let node = prog_node.body.first().unwrap();

    assert!(matches!(
        node,
        AstNode::FunctionDef(FunctionDefNode {
            return_type: LpcType::Mixed(false),
            ..
        })
    ));
}

#[test]
fn error_when_pragma_strict_types_without_return_type() {
    let prog = indoc! { r#"
        #pragma strict_types

        create() {
            dump("sup?");
        }
    "# };

    let program = parse_prog(prog);

    assert_eq!(&program.unwrap_err().to_string(), "Missing return type");
}

#[test]
fn allows_extra_commas_for_array() {
    let prog = indoc! { r#"
        mixed foo = ({ 1, 2, 3, });
    "# };

    let program = parse_prog(prog);

    assert!(program.is_ok());
}

#[test]
fn allows_extra_commas_for_mapping() {
    let prog = indoc! { r#"
        mapping thing = ([
            "foo": "bar",
            "baz": "quux",
            1: 2,
        ]);
    "# };

    let program = parse_prog(prog);

    assert!(program.is_ok());
}

#[test]
fn ellipsis_sets_the_flag_when_only_arg() {
    let prog = indoc! { r#"
        int tacos(...) {
            return 666;
        }
    "#
    }
    .replace('\n', "");

    let lexer = LexWrapper::new(&prog);
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let node = prog_node.body.first().unwrap();

    if let AstNode::FunctionDef(fd_node) = node {
        assert!(fd_node.flags.ellipsis())
    } else {
        panic!("Expected function def node");
    }
}

#[test]
fn ellipsis_sets_the_flag_when_not_only_arg() {
    let prog = indoc! { r#"
        int tacos(int i, ...) {
            return 666;
        }
    "#
    }
    .replace('\n', "");

    let lexer = LexWrapper::new(&prog);
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let node = prog_node.body.first().unwrap();

    if let AstNode::FunctionDef(fd_node) = node {
        assert!(fd_node.flags.ellipsis())
    } else {
        panic!("Expected function def node");
    }
}

#[test]
fn partial_application_argument_lists<'a>() {
    let prog = indoc! { r#"
        function a = &foo();
        function b = &foo(1);
        function c = &foo(1, 2, 3);
        function d = &foo(1, 2, 3, );
        function e = &foo(, 2, 3, );
        function f = &foo(,, 3, );
        function g = &foo(,, 3);
        function h = &foo(,,,);
        function i = &foo(,);
    "# };

    let program = parse_prog(prog).expect("Failed to parse");

    let get_args = |node: &AstNode| -> Option<Vec<Option<LpcInt>>> {
        // println!("node {:?}", node);
        // panic!()
        if_chain! {
            if let AstNode::Decl(
                DeclNode {
                    type_: LpcType::Function(false),
                    initializations
                }
            ) = node;
            if let VarInitNode { value, .. } = &initializations[0];
            if let Some(ExpressionNode::FunctionPtr(FunctionPtrNode { arguments, .. } )) = value;
            then {
                arguments.as_ref().map(|array| {
                    array.iter().map(|arg| {
                        arg.as_ref().map(|expr| {
                            if let ExpressionNode::Int(IntNode { value, .. }) = expr {
                                *value
                            } else {
                                panic!("bad map?")
                            }
                        })
                    }).collect()
                })
            } else {
                panic!("panic? {node:?}")
            }
        }
    };

    assert_eq!(get_args(&program.body[0]), None);
    assert_eq!(get_args(&program.body[1]), Some(vec![Some(1)]));
    assert_eq!(
        get_args(&program.body[2]),
        Some(vec![Some(1), Some(2), Some(3)])
    );
    assert_eq!(
        get_args(&program.body[3]),
        Some(vec![Some(1), Some(2), Some(3), None])
    );
    assert_eq!(
        get_args(&program.body[4]),
        Some(vec![None, Some(2), Some(3), None])
    );
    assert_eq!(
        get_args(&program.body[5]),
        Some(vec![None, None, Some(3), None])
    );
    assert_eq!(get_args(&program.body[6]), Some(vec![None, None, Some(3)]));
    assert_eq!(
        get_args(&program.body[7]),
        Some(vec![None, None, None, None])
    );
    assert_eq!(get_args(&program.body[8]), Some(vec![None, None]));
}

#[test]
fn error_when_multiple_visibilities_given() {
    let prog = indoc! { r#"
        public private void foo() {
            dump("sup?");
        }
    "# };

    let program = parse_prog(prog);

    assert_eq!(
        &program.unwrap_err().to_string(),
        "multiple visibilities specified"
    );
}

#[test]
fn error_on_varargs_var() {
    let prog = indoc! { r#"
        varargs string a;
    "# };

    let program = parse_prog(prog);

    assert_eq!(
        &program.unwrap_err().to_string(),
        "`varargs` is intended for functions only"
    );
}

#[test]
fn error_on_nomask_var() {
    let prog = indoc! { r#"
        nomask string a;
    "# };

    let program = parse_prog(prog);

    assert_eq!(
        &program.unwrap_err().to_string(),
        "`nomask` is intended for functions only"
    );
}

#[test]
fn warning_on_prototype() {
    let prog = indoc! { r#"
        private int tacos(string a);
    "# };

    let compiler = Compiler::default();
    let (code, preprocessor) = compiler.preprocess_string("foo/bar.c", prog).unwrap();
    let code = TokenVecWrapper::new(&code);
    let mut context = preprocessor.into_context();

    let program = lpc_parser::ProgramParser::new().parse(&mut context, code);

    assert_ok!(program);

    assert_eq!(
        &context.errors.first().unwrap().to_string(),
        "prototypes are ignored in this flavor of LPC"
    );
}

#[test]
fn test_closure_bodies() {
    let prog = indoc! { r#"
        function f = (: :);
        function g = (: 1 :);
        function h = (: 1; :);
        function i = (: this_object() :);
        function j = (: this_object(); :);
        function k = (: "foo" + "bar" :);
        function l = (: k() + "baz"; :);
        function m = (: return l(); :);
        function n = (: return m :);
        function o = (:
            if (!f()) {
                int i = 666;
                return i;
            } else {
                return 1;
            }
        :);
    "#};

    assert_ok!(parse_prog(prog));
}

fn parse_prog(prog: &str) -> Result<ProgramNode> {
    let compiler = Compiler::default();
    let (code, preprocessor) = compiler.preprocess_string("foo/bar.c", prog).unwrap();
    let code = TokenVecWrapper::new(&code);
    let mut context = preprocessor.into_context();

    lpc_parser::ProgramParser::new()
        .parse(&mut context, code)
        .map_err(|e| e.into())
}
