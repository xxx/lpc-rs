use claims::assert_ok;
use indoc::indoc;
use lpc_rs::{
    compiler::{
        Compiler,
        ast::{
            assignment_node::AssignmentNode,
            ast_node::AstNode,
            binary_op_node::{BinaryOpNode, BinaryOperation},
            cast_node::CastNode,
            decl_node::DeclNode,
            expression_node::ExpressionNode,
            float_node::FloatNode,
            for_each_node::ForEachInit,
            function_def_node::FunctionDefNode,
            function_ptr_node::FunctionPtrNode,
            if_node::IfNode,
            int_node::IntNode,
            operator_node::{Operator, OperatorNode},
            program_node::ProgramNode,
            ref_node::RefNode,
            return_node::ReturnNode,
            string_node::StringNode,
            unary_op_node::UnaryOperation,
            var_init_node::VarInitNode,
            var_node::VarNode,
        },
        compilation_context::CompilationContext,
        lexer::{LexWrapper, TokenTriples},
    },
    lpc_parser,
};
use lpc_rs_core::{
    LpcFloatInner, LpcIntInner, global_var_flags::GlobalVarFlags, lpc_type::LpcType,
    visibility::Visibility,
};
use lpc_rs_errors::{Result, span::Span};
use ustr::ustr;

// just a helper for a very common pattern
fn assert_int(value: LpcIntInner, expr: &str) {
    let code = format!("void create() {{ {}; }}", expr);
    // File id 0 = coordinate-only scratch: these tests never render or call code().
    let lexer = LexWrapper::new(&code, 0).triples();
    let prog_node: ProgramNode = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let AstNode::FunctionDef(function_def_node) = prog_node.body[0].clone() else {
        panic!("Expected a function");
    };
    let AstNode::Expression(expr_node) = function_def_node.body[0].clone() else {
        panic!("Expected an expression");
    };

    let expected = ExpressionNode::Int(IntNode {
        value,
        span: Some(Span::new(0, 16..16 + expr.len())),
    });

    assert_eq!(expr_node, expected);
}

#[test]
fn program_global_vars() {
    let prog = "int i = 123; private int j = i - 8; private static string *k;";
    let lexer = LexWrapper::new(prog, 0).triples();
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
                    name: ustr("i"),
                    value: Some(ExpressionNode::Int(IntNode {
                        value: 123,
                        span: Some(Span::new(0, 8..11)),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span::new(0, 4..11)),
                    flags: Some(GlobalVarFlags::new().with_visibility(Visibility::Public)),
                    by_ref: false,
                }],
            }),
            AstNode::from(DeclNode {
                type_: LpcType::Int(false),
                initializations: vec![VarInitNode {
                    type_: LpcType::Int(false),
                    name: ustr("j"),
                    value: Some(ExpressionNode::BinaryOp(BinaryOpNode {
                        l: Box::new(ExpressionNode::Var(VarNode {
                            name: ustr("i"),
                            span: Some(Span::new(0, 29..30)),
                            global: false,
                            function_name: false,
                        })),
                        r: Box::new(ExpressionNode::Int(IntNode {
                            value: 8,
                            span: Some(Span::new(0, 33..34)),
                        })),
                        op: BinaryOperation::Sub,
                        span: Some(Span::new(0, 29..34)),
                    })),
                    array: false,
                    global: true,
                    span: Some(Span::new(0, 25..34)),
                    flags: Some(GlobalVarFlags::new().with_visibility(Visibility::Private)),
                    by_ref: false,
                }],
            }),
            AstNode::from(DeclNode {
                type_: LpcType::String(true),
                initializations: vec![VarInitNode {
                    type_: LpcType::String(true),
                    name: ustr("k"),
                    value: None,
                    array: true,
                    global: true,
                    span: Some(Span::new(0, 58..60)),
                    flags: Some(
                        GlobalVarFlags::new()
                            .with_visibility(Visibility::Private)
                            .with_is_static(true),
                    ),
                    by_ref: false,
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
    let expr = "void create() { 1_1.234_332e2_2; }";
    let lexer = LexWrapper::new(expr, 0).triples();
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let AstNode::FunctionDef(function_def_node) = prog_node.body[0].clone() else {
        panic!("Expected a function");
    };
    let AstNode::Expression(node) = function_def_node.body[0].clone() else {
        panic!("Expected an expression");
    };

    let expected = ExpressionNode::Float(FloatNode {
        value: LpcFloatInner::from(112343320000000000000000.0),
        span: Some(Span::new(0, 16..31)),
    });

    assert_eq!(node, expected);
}

#[test]
fn string_literal_concat() {
    let expr = r##"void create() { "foo" + "bar" + "baz" + "quux"; }"##;
    let lexer = LexWrapper::new(expr, 0).triples();
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let AstNode::FunctionDef(function_def_node) = prog_node.body[0].clone() else {
        panic!("Expected a function");
    };

    let AstNode::Expression(node) = function_def_node.body[0].clone() else {
        panic!("Expected an expression");
    };

    let expected = ExpressionNode::String(StringNode {
        value: "foobarbazquux".into(),
        span: Some(Span::new(0, 16..46)),
    });

    assert_eq!(node, expected);

    // test overflow
    let expr = r##"void create() { ("f" * 5000) + ("b" * 5000); }"##;
    let lexer = LexWrapper::new(expr, 0).triples();
    let error = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap_err();

    assert_eq!(error.to_string(), "overflow in string concatenation");
}

#[test]
fn string_literal_repeat() {
    let expr = r##"void create() { "foo" * 3; }"##;
    let lexer = LexWrapper::new(expr, 0).triples();
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let AstNode::FunctionDef(function_def_node) = prog_node.body[0].clone() else {
        panic!("Expected a function");
    };

    let AstNode::Expression(node) = function_def_node.body[0].clone() else {
        panic!("Expected an expression");
    };
    let expected = ExpressionNode::String(StringNode {
        value: "foofoofoo".into(),
        span: Some(Span::new(0, 16..25)),
    });

    assert_eq!(node, expected);

    // test negative multiplier
    let expr = r##"void create() { "foo" * -3; }"##;
    let lexer = LexWrapper::new(expr, 0).triples();
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();

    let AstNode::FunctionDef(function_def_node) = prog_node.body[0].clone() else {
        panic!("Expected a function");
    };
    let AstNode::Expression(node) = function_def_node.body[0].clone() else {
        panic!("Expected an expression");
    };

    let expected = ExpressionNode::String(StringNode {
        value: "".into(),
        span: Some(Span::new(0, 16..26)),
    });

    assert_eq!(node, expected);

    // test overflow
    let expr = r##"void create() { "foo" * 9223372036854775807; }"##;
    let lexer = LexWrapper::new(expr, 0).triples();
    let error = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap_err();

    assert_eq!(error.to_string(), "overflow in string repetition");
}

#[test]
fn compound_assignment_decompose() {
    let expr = "void create() { a += 2; }";
    let lexer = LexWrapper::new(expr, 0).triples();
    let prog_node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let AstNode::FunctionDef(FunctionDefNode { body, .. }) = prog_node.body[0].clone() else {
        panic!("Expected a function def");
    };
    let AstNode::Expression(node) = body[0].clone() else {
        panic!("Expected a declaration");
    };

    let expected = ExpressionNode::Assignment(AssignmentNode {
        lhs: Box::new(ExpressionNode::Var(VarNode {
            name: ustr("a"),
            span: Some(Span::new(0, 16..17)),
            global: false,
            function_name: false,
        })),
        rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
            l: Box::new(ExpressionNode::Var(VarNode {
                name: ustr("a"),
                span: Some(Span::new(0, 16..17)),
                global: false,
                function_name: false,
            })),
            r: Box::new(ExpressionNode::Int(IntNode {
                value: 2,
                span: Some(Span::new(0, 21..22)),
            })),
            op: BinaryOperation::Add,
            span: Some(Span::new(0, 21..22)),
        })),
        span: Some(Span::new(0, 16..22)),
    });

    assert_eq!(node, expected);
}

#[test]
fn mod_and_xor_compound_assignments_desugar() {
    for (source, op) in [
        ("a %= 2;", BinaryOperation::Mod),
        ("a ^= 2;", BinaryOperation::Xor),
    ] {
        let code = format!("void create() {{ {source} }}");
        let lexer = LexWrapper::new(&code, 0).triples();
        let prog_node = lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), lexer)
            .unwrap();
        let AstNode::FunctionDef(FunctionDefNode { body, .. }) = prog_node.body[0].clone() else {
            panic!("Expected a function def");
        };
        let AstNode::Expression(ExpressionNode::Assignment(AssignmentNode { rhs, .. })) =
            body[0].clone()
        else {
            panic!("Expected an assignment");
        };
        let ExpressionNode::BinaryOp(BinaryOpNode { op: found, .. }) = *rhs else {
            panic!("Expected a desugared binary op");
        };
        assert_eq!(found, op, "{source}");
    }
}

#[test]
fn typeless_functions_are_mixed() {
    let prog = r#"marfin() {
            return "hello, we're marfin'!";
        }"#
    .replace('\n', "");
    let lexer = LexWrapper::new(&prog, 0).triples();
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

#[tokio::test]
async fn error_when_pragma_strict_types_without_return_type() {
    let prog = indoc! { r#"
        #pragma strict_types

        create() {
            dump("sup?");
        }
    "# };

    let program = parse_prog(prog).await;

    assert_eq!(&program.unwrap_err().to_string(), "Missing return type");
}

#[tokio::test]
async fn allows_extra_commas_for_array() {
    let prog = indoc! { r#"
        mixed foo = ({ 1, 2, 3, });
    "# };

    let program = parse_prog(prog).await;

    assert!(program.is_ok());
}

#[tokio::test]
async fn allows_extra_commas_for_mapping() {
    let prog = indoc! { r#"
        mapping thing = ([
            "foo": "bar",
            "baz": "quux",
            1: 2,
        ]);
    "# };

    let program = parse_prog(prog).await;

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

    let lexer = LexWrapper::new(&prog, 0).triples();
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

    let lexer = LexWrapper::new(&prog, 0).triples();
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

#[tokio::test]
async fn partial_application_argument_lists() {
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

    let program = parse_prog(prog).await.expect("Failed to parse");

    let get_args = |node: &AstNode| -> Option<Vec<Option<LpcIntInner>>> {
        if let AstNode::Decl(DeclNode {
            type_: LpcType::Function(false),
            initializations,
        }) = node
            && let Some(ExpressionNode::FunctionPtr(FunctionPtrNode { arguments, .. })) =
                &initializations[0].value
        {
            arguments.as_ref().map(|array| {
                array
                    .iter()
                    .map(|arg| {
                        arg.as_ref().map(|expr| {
                            if let ExpressionNode::Int(IntNode { value, .. }) = expr {
                                *value
                            } else {
                                panic!("bad map?")
                            }
                        })
                    })
                    .collect()
            })
        } else {
            panic!("panic? {node:?}")
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

/// The operator node a one-declaration program initializes `f` with.
fn operator_of(prog: &ProgramNode) -> &OperatorNode {
    let AstNode::Decl(DeclNode {
        initializations, ..
    }) = &prog.body[0]
    else {
        panic!("expected a declaration");
    };
    let Some(ExpressionNode::Operator(node)) = &initializations[0].value else {
        panic!("expected an operator function");
    };
    node
}

#[tokio::test]
async fn an_operator_is_the_closure_over_its_positional_arguments() {
    let prog = parse_prog("function f = operator(+);").await.unwrap();
    let node = operator_of(&prog);

    assert_eq!(node.op, Operator::Binary(BinaryOperation::Add));
    assert!(node.arguments.is_none());
    assert_eq!(node.closure.name.as_str(), "closure-0");
    assert!(node.closure.parameters.is_none());
    let [AstNode::Expression(ExpressionNode::BinaryOp(body))] = node.closure.body.as_slice() else {
        panic!("expected one binary operation");
    };
    assert_eq!(body.op, BinaryOperation::Add);
    assert_eq!(body.span, node.span);
    let positional = |name: &str| {
        let mut var = VarNode::new(name);
        var.span = node.span;
        ExpressionNode::Var(var)
    };
    assert_eq!(*body.l, positional("$1"));
    assert_eq!(*body.r, positional("$2"));
    assert_eq!(node.to_string(), "operator(+)");
    assert_eq!(node.closure.to_string(), "(: $1 + $2 :)");
}

#[tokio::test]
async fn a_unary_operator_takes_one_positional() {
    let prog = parse_prog("function f = operator(!);").await.unwrap();
    let node = operator_of(&prog);

    assert_eq!(node.op, Operator::Unary(UnaryOperation::Bang));
    let [AstNode::Expression(ExpressionNode::UnaryOp(body))] = node.closure.body.as_slice() else {
        panic!("expected one unary operation");
    };
    assert_eq!(body.op, UnaryOperation::Bang);
    assert!(!body.is_post);
    assert_eq!(node.closure.to_string(), "(: !$1 :)");
}

#[tokio::test]
async fn an_index_operator_indexes_its_first_positional_by_its_second() {
    let prog = parse_prog("function f = operator([]);").await.unwrap();
    let node = operator_of(&prog);

    assert_eq!(node.op, Operator::Binary(BinaryOperation::Index));
    assert_eq!(node.to_string(), "operator([])");
    assert_eq!(node.closure.to_string(), "(: $1 [] $2 :)");
}

#[tokio::test]
async fn a_partially_applied_operator_keeps_its_holes() {
    let prog = parse_prog("function f = &operator(-)(, 1);").await.unwrap();
    let node = operator_of(&prog);

    assert_eq!(node.op, Operator::Binary(BinaryOperation::Sub));
    let Some(args) = &node.arguments else {
        panic!("expected bound arguments");
    };
    assert_eq!(args.len(), 2);
    assert!(args[0].is_none());
    assert!(matches!(
        args[1],
        Some(ExpressionNode::Int(IntNode { value: 1, .. }))
    ));
    assert_eq!(node.to_string(), "&operator(-)(, 1)");
    assert_eq!(node.closure.name.as_str(), "closure-0");
}

#[tokio::test]
async fn every_operator_symbol_parses_in_both_forms() {
    let symbols = [
        "+", "-", "*", "/", "%", ">", "<", ">=", "<=", "==", "!=", "&", "^", "|", "<<", ">>", "[]",
        "!", "~",
    ];
    for symbol in symbols {
        let bare = format!("function f = operator({symbol});");
        assert_ok!(parse_prog(&bare).await, "{symbol}");
        let partial = format!("function g = &operator({symbol})(1);");
        assert_ok!(parse_prog(&partial).await, "{symbol}");
    }
}

#[tokio::test]
async fn an_operator_the_form_lacks_is_rejected() {
    for symbol in ["&&", "||", "=", "+=", "++", "--", "@", "?", ",", "", "-1"] {
        let code = format!("function f = operator({symbol});");
        assert!(parse_prog(&code).await.is_err(), "{symbol}");
    }
}

#[tokio::test]
async fn operator_is_a_keyword() {
    assert!(parse_prog("int operator;").await.is_err());
    assert!(parse_prog("void operator() {}").await.is_err());
    assert!(parse_prog("void f() { operator = 1; }").await.is_err());
}

#[tokio::test]
async fn the_partial_form_needs_its_argument_list() {
    assert!(parse_prog("function f = &operator(+);").await.is_err());
    assert_ok!(parse_prog("function f = &operator(+)();").await);
}

#[tokio::test]
async fn operator_functions_and_closures_share_one_numbering() {
    let code = "function f = operator(+);\nfunction g = (: 1 :);\nfunction h = &operator(<)(1);";
    let prog = parse_prog(code).await.unwrap();

    let names = prog
        .body
        .iter()
        .map(|node| {
            let AstNode::Decl(DeclNode {
                initializations, ..
            }) = node
            else {
                panic!("expected a declaration");
            };
            match &initializations[0].value {
                Some(ExpressionNode::Operator(node)) => node.closure.name.to_string(),
                Some(ExpressionNode::Closure(node)) => node.name.to_string(),
                other => panic!("unexpected {other:?}"),
            }
        })
        .collect::<Vec<_>>();
    assert_eq!(names, ["closure-0", "closure-1", "closure-2"]);
}

#[tokio::test]
async fn an_operator_function_is_an_ordinary_argument_and_operand() {
    assert_ok!(parse_prog("void f(mixed a) { map(a, operator(+)); }").await);
    assert_ok!(parse_prog("void f(mixed a, mixed b) { b = a & operator(+); }").await);
    assert_ok!(parse_prog("void f(mixed a, mixed b) { b = a & &operator(+)(1); }").await);
}

#[tokio::test]
async fn error_when_multiple_visibilities_given() {
    let prog = indoc! { r#"
        public private void foo() {
            dump("sup?");
        }
    "# };

    let program = parse_prog(prog).await;

    assert_eq!(
        &program.unwrap_err().to_string(),
        "multiple visibilities specified"
    );
}

#[tokio::test]
async fn error_on_varargs_var() {
    let prog = indoc! { r#"
        varargs string a;
    "# };

    let program = parse_prog(prog).await;

    assert_eq!(
        &program.unwrap_err().to_string(),
        "`varargs` is intended for functions only"
    );
}

#[tokio::test]
async fn error_on_nomask_var() {
    let prog = indoc! { r#"
        nomask string a;
    "# };

    let program = parse_prog(prog).await;

    assert_eq!(
        &program.unwrap_err().to_string(),
        "`nomask` is intended for functions only"
    );
}

#[tokio::test]
async fn warning_on_prototype() {
    let prog = indoc! { r#"
        private int tacos(string a);
    "# };

    let compiler = Compiler::default();
    let (code, preprocessor) = compiler.preprocess_string("foo/bar.c", prog).await.unwrap();
    let code = TokenTriples::new(&code);
    let mut context = preprocessor.into_context();

    let program = lpc_parser::ProgramParser::new().parse(&mut context, code);

    assert_ok!(program);

    assert_eq!(
        &context.diagnostics.errors().first().unwrap().to_string(),
        "function prototypes are ignored; the definition declares the function"
    );
}

#[tokio::test]
async fn test_closure_bodies() {
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

    assert_ok!(parse_prog(prog).await);
}

#[tokio::test]
async fn a_foreach_variable_may_be_typed() {
    let code = "void f(mixed a) { foreach (string s : a) {} foreach (int *k, mixed v : a) {} }";
    let prog = parse_prog(code).await.unwrap();
    let AstNode::FunctionDef(def) = &prog.body[0] else {
        panic!("expected a function");
    };
    let AstNode::ForEach(first) = &def.body[0] else {
        panic!("expected a foreach");
    };
    let ForEachInit::Array(var) = &first.initializer else {
        panic!("expected one variable");
    };
    assert_eq!(var.type_, LpcType::String(false));
    assert_eq!(var.name.as_str(), "s");
    let AstNode::ForEach(second) = &def.body[1] else {
        panic!("expected a foreach");
    };
    let ForEachInit::Mapping { key, value } = &second.initializer else {
        panic!("expected a key and a value");
    };
    assert_eq!(key.type_, LpcType::Int(true));
    assert!(key.array);
    assert_eq!(value.type_, LpcType::Mixed(false));
}

#[tokio::test]
async fn an_untyped_foreach_variable_is_mixed() {
    let prog = parse_prog("void f(mixed a) { foreach (x : a) {} }")
        .await
        .unwrap();
    let AstNode::FunctionDef(def) = &prog.body[0] else {
        panic!("expected a function");
    };
    let AstNode::ForEach(node) = &def.body[0] else {
        panic!("expected a foreach");
    };
    let ForEachInit::Array(var) = &node.initializer else {
        panic!("expected one variable");
    };
    assert_eq!(var.type_, LpcType::Mixed(false));
}

#[tokio::test]
async fn a_void_foreach_variable_is_rejected() {
    let err = parse_prog("void f(mixed a) { foreach (void x : a) {} }")
        .await
        .unwrap_err();
    assert_eq!(err.to_string(), "a `foreach` variable cannot be `void`");
}

async fn parse_prog(prog: &str) -> Result<ProgramNode> {
    let compiler = Compiler::default();
    let (code, preprocessor) = compiler.preprocess_string("foo/bar.c", prog).await.unwrap();
    let code = TokenTriples::new(&code);
    let mut context = preprocessor.into_context();

    lpc_parser::ProgramParser::new()
        .parse(&mut context, code)
        .map_err(lpc_rs_errors::LpcError::from)
}

#[test]
fn a_ref_parameter_is_marked() {
    let prog = "void inc(int ref x) { }";
    let lexer = LexWrapper::new(prog, 0).triples();
    let node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let AstNode::FunctionDef(def) = &node.body[0] else {
        panic!("expected a function");
    };
    assert_eq!(def.parameters.len(), 1);
    assert_eq!(def.parameters[0].name, ustr("x"));
    assert_eq!(def.parameters[0].type_, LpcType::Int(false));
    assert!(def.parameters[0].by_ref);
}

#[test]
fn a_ref_argument_parses_to_a_ref_node() {
    let prog = "void f() { inc(ref y); }";
    let lexer = LexWrapper::new(prog, 0).triples();
    let node = lpc_parser::ProgramParser::new()
        .parse(&mut CompilationContext::default(), lexer)
        .unwrap();
    let AstNode::FunctionDef(def) = &node.body[0] else {
        panic!("expected a function");
    };
    let AstNode::Expression(ExpressionNode::Call(call)) = &def.body[0] else {
        panic!("expected a call");
    };
    assert_eq!(
        call.arguments,
        vec![ExpressionNode::Ref(RefNode {
            name: ustr("y"),
            span: Some(Span::new(0, 15..20)),
            global: false,
        })]
    );
}

#[test]
fn ref_needs_a_bare_variable() {
    for prog in ["void f() { inc(ref a[1]); }", "void f() { inc(ref g()); }"] {
        let lexer = LexWrapper::new(prog, 0).triples();
        let error = lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), lexer)
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "only a variable can be passed by reference",
            "{prog}"
        );
    }

    let lexer = LexWrapper::new("int ref x;", 0).triples();
    assert!(
        lpc_parser::ProgramParser::new()
            .parse(&mut CompilationContext::default(), lexer)
            .is_err(),
        "int ref x; must not parse"
    );
}

#[tokio::test]
async fn a_void_parameter_list_is_empty() {
    let prog = parse_prog("int f(void) { return 1; }").await.unwrap();
    let AstNode::FunctionDef(def) = &prog.body[0] else {
        panic!("expected a function");
    };
    assert!(def.parameters.is_empty());
    assert!(!def.flags.ellipsis());
}

#[tokio::test]
async fn a_bare_semicolon_is_an_empty_statement() {
    let prog = parse_prog("void create() { ; if (1) ; else ; }")
        .await
        .unwrap();
    let AstNode::FunctionDef(def) = &prog.body[0] else {
        panic!("expected a function");
    };
    assert!(matches!(def.body[0], AstNode::NoOp));
    let AstNode::If(IfNode {
        body, else_clause, ..
    }) = &def.body[1]
    else {
        panic!("expected an if");
    };
    assert!(matches!(**body, AstNode::NoOp));
    assert!(matches!(**else_clause, Some(AstNode::NoOp)));
}

fn first_return_value(prog: &ProgramNode) -> ExpressionNode {
    let AstNode::FunctionDef(def) = &prog.body[0] else {
        panic!("expected a function");
    };
    let AstNode::Return(ReturnNode {
        value: Some(value), ..
    }) = &def.body[0]
    else {
        panic!("expected a return with a value");
    };
    value.clone()
}

#[tokio::test]
async fn a_cast_wraps_its_operand() {
    let prog = parse_prog("mixed f(mixed a) { return (int) a; }")
        .await
        .unwrap();
    let ExpressionNode::Cast(CastNode { expr, type_, .. }) = first_return_value(&prog) else {
        panic!("expected a cast");
    };
    assert_eq!(type_, LpcType::Int(false));
    assert!(matches!(*expr, ExpressionNode::Var(_)));
}

#[tokio::test]
async fn an_array_cast_carries_the_star() {
    let prog = parse_prog("mixed f(mixed a) { return (string *) a; }")
        .await
        .unwrap();
    let ExpressionNode::Cast(CastNode { type_, .. }) = first_return_value(&prog) else {
        panic!("expected a cast");
    };
    assert_eq!(type_, LpcType::String(true));
}

#[tokio::test]
async fn a_cast_binds_tighter_than_a_binary_operator() {
    let prog = parse_prog("mixed f(mixed a) { return (int) a + 1; }")
        .await
        .unwrap();
    let ExpressionNode::BinaryOp(BinaryOpNode { l, op, .. }) = first_return_value(&prog) else {
        panic!("expected a binary op");
    };
    assert_eq!(op, BinaryOperation::Add);
    assert!(matches!(*l, ExpressionNode::Cast(_)));
}

#[tokio::test]
async fn a_cast_applies_to_a_unary_operand() {
    let prog = parse_prog("mixed f(mixed a) { return (int) -a; }")
        .await
        .unwrap();
    let ExpressionNode::Cast(CastNode { expr, .. }) = first_return_value(&prog) else {
        panic!("expected a cast");
    };
    assert!(matches!(*expr, ExpressionNode::UnaryOp(_)));
}

#[tokio::test]
async fn a_cast_to_void_is_rejected() {
    let err = parse_prog("mixed f(mixed a) { return (void) a; }")
        .await
        .unwrap_err();
    assert_eq!(err.to_string(), "cannot cast to `void`");
}
