pub mod array_node;
pub mod assignment_node;
pub mod ast_node;
pub mod binary_op_node;
pub mod block_node;
pub mod break_node;
pub mod call_node;
pub mod capture_node;
pub mod cast_node;
pub mod catch_node;
pub mod closure_node;
pub mod comma_expression_node;
pub mod continue_node;
pub mod decl_node;
pub mod do_while_node;
pub mod expression_node;
pub mod float_node;
pub mod for_each_node;
pub mod for_node;
pub mod function_def_node;
pub mod function_ptr_node;
pub mod if_node;
pub mod inherit_node;
pub mod int_node;
pub mod label_node;
pub mod mapping_node;
pub mod nil_node;
pub mod operator_node;
pub mod program_node;
pub mod range_node;
pub mod ref_node;
pub mod return_node;
pub mod spread_node;
pub mod string_node;
pub mod switch_node;
pub mod ternary_node;
pub mod unary_op_node;
pub mod var_init_node;
pub mod var_node;
pub mod while_node;

pub mod class_def_node;
pub mod labeled_statement_node;
pub mod parsed_type;

pub mod class_expression;

#[cfg(test)]
mod tests {
    use super::{
        ast_node::AstNode, do_while_node::DoWhileNode, expression_node::ExpressionNode,
        if_node::IfNode, int_node::IntNode, switch_node::SwitchNode, while_node::WhileNode,
    };

    #[test]
    fn control_flow_formatting_balances_delimiters() {
        let condition = || ExpressionNode::Int(IntNode::new(1));
        let body = || AstNode::NoOp;
        let nodes = [
            DoWhileNode::new(body(), condition(), None).to_string(),
            IfNode::new(condition(), body(), None, None).to_string(),
            IfNode::new(condition(), body(), Some(body()), None).to_string(),
            SwitchNode::new(condition(), body(), None).to_string(),
            WhileNode::new(condition(), body(), None).to_string(),
        ];

        assert_eq!(
            nodes,
            [
                "do { NoOp } while (1)",
                "if(1) { NoOp }",
                "if(1) { NoOp } else { NoOp }",
                "switch(1) { NoOp }",
                "while(1) { NoOp }",
            ]
        );
    }

    #[test]
    fn statement_formatters_preserve_labels_types_and_body_text() {
        use super::{
            block_node::BlockNode, catch_node::CatchNode, decl_node::DeclNode, for_node::ForNode,
            label_node::LabelNode, labeled_statement_node::LabeledStatementNode,
            program_node::ProgramNode, var_init_node::VarInitNode,
        };
        use lpc_rs_core::lpc_type::LpcType;

        let block = || BlockNode::new(vec![AstNode::NoOp, AstNode::NoOp]);
        let number = |value| ExpressionNode::Int(IntNode::new(value));
        let mut variable = VarInitNode::new("count", LpcType::Int(false));
        variable.value = Some(number(7));
        let declaration = DeclNode {
            type_: LpcType::Int(false).into(),
            initializations: vec![variable],
        };
        let text = [
            block().to_string(),
            CatchNode {
                body: block(),
                span: None,
            }
            .to_string(),
            declaration.to_string(),
            ForNode::new(None, Some(number(1)), Some(number(2)), AstNode::NoOp, None).to_string(),
            LabeledStatementNode::new(AstNode::NoOp, LabelNode::new(number(1), None)).to_string(),
            LabeledStatementNode::new(AstNode::NoOp, LabelNode::new_default(None)).to_string(),
            ProgramNode {
                inherits: vec![],
                body: vec![AstNode::NoOp, AstNode::NoOp],
            }
            .to_string(),
        ];

        assert_eq!(
            text,
            [
                "{ NoOp, NoOp }",
                "catch { NoOp, NoOp }",
                "int count = 7",
                "for (; 1; 2) { NoOp }",
                "1: NoOp",
                "default: NoOp",
                "NoOp, NoOp",
            ]
        );
    }

    #[tokio::test]
    async fn parsed_literals_keep_their_source_text_and_display() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, literal) in ["7", "1.25", "\"hello\"", "nil"].into_iter().enumerate() {
            let source = format!("/* source prefix */ mixed value = {literal};");
            let path = format!("/literal-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::Decl(mut declaration) = program.body.pop().unwrap() else {
                panic!("expected a global declaration");
            };
            let expression = declaration.initializations.pop().unwrap().value.unwrap();

            let source = match &expression {
                ExpressionNode::Int(node) => node.to_code(),
                ExpressionNode::Float(node) => node.to_code(),
                ExpressionNode::String(node) => node.to_code(),
                ExpressionNode::Nil(node) => node.to_code(),
                _ => panic!("expected a literal"),
            };
            assert_eq!(source, literal);
            assert_eq!(expression.to_string(), literal);
        }
    }

    #[tokio::test]
    async fn parsed_jumps_keep_their_source_text_and_display() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let code = r#"
            void exits() { while (1) break; }
            void repeats() { while (1) continue; }
            int answer() { return 7; }
            void empty() { return; }
        "#;
        let compiler = Compiler::new(test_config());
        let (program, _) = compiler
            .parse_string(&"/jumps.c".into(), code)
            .await
            .unwrap();
        let mut sources = Vec::new();
        let mut displays = Vec::new();
        for declaration in program.body {
            let AstNode::FunctionDef(function) = declaration else {
                panic!("expected a function definition");
            };
            let (source, display) = match function.body.into_iter().next().unwrap() {
                AstNode::While(loop_) => match *loop_.body {
                    AstNode::Break(node) => (node.to_code(), node.to_string()),
                    AstNode::Continue(node) => (node.to_code(), node.to_string()),
                    _ => panic!("expected a loop jump"),
                },
                AstNode::Return(node) => (node.to_code(), node.to_string()),
                _ => panic!("expected a jump"),
            };
            sources.push(source);
            displays.push(display.trim_end().to_owned());
        }
        assert_eq!(sources, ["break", "continue", "return 7", "return"]);
        assert_eq!(displays, ["break", "continue", "return 7", "return"]);
    }

    #[tokio::test]
    async fn parsed_expressions_keep_their_source_text_and_display() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, (expression, expected)) in [
            ("(: $(7) :)", "$(7)"),
            ("(int) 7", "(int) 7"),
            ("(1, 2)", "1, 2"),
            ("1 ? 7 : 9", "1 ? 7 : 9"),
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!("/* source prefix */ mixed value = {expression};");
            let path = format!("/expression-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::Decl(mut declaration) = program.body.pop().unwrap() else {
                panic!("expected a global declaration");
            };
            let expression = declaration.initializations.pop().unwrap().value.unwrap();
            let (source, display) = match expression {
                ExpressionNode::Closure(closure) => {
                    match closure.body.into_iter().next().unwrap() {
                        AstNode::Expression(ExpressionNode::Capture(node)) => {
                            (node.to_code(), node.to_string())
                        }
                        _ => panic!("expected an explicit capture"),
                    }
                }
                ExpressionNode::Cast(node) => (node.to_code(), node.to_string()),
                ExpressionNode::CommaExpression(node) => (node.to_code(), node.to_string()),
                ExpressionNode::Ternary(node) => (node.to_code(), node.to_string()),
                _ => panic!("expected an expression"),
            };
            assert_eq!(source, expected);
            assert_eq!(display, expected);
        }
    }
    #[tokio::test]
    async fn parsed_values_keep_their_source_text_and_display() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, (expression, expected_source, expected_display)) in [
            ("input[0..1]", "input[0..1]", "0..1"),
            ("input[..1]", "input[..1]", "..1"),
            ("input[0..]", "input[0..]", "0.."),
            (r#"([ "k": 7 ])"#, r#"([ "k": 7 ])"#, r#"([ "k": 7 ])"#),
            ("-input", "-input", "-input"),
            ("input++", "input++", "input++"),
            ("input--", "input--", "input--"),
            ("set(ref input)", "ref input", "ref input"),
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!(
                "mixed set(mixed ref target) {{ return target; }} mixed value(mixed input) {{ return {expression}; }}"
            );
            let path = format!("/value-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::FunctionDef(mut function) = program.body.pop().unwrap() else {
                panic!("expected a function definition");
            };
            let AstNode::Return(result) = function.body.pop().unwrap() else {
                panic!("expected a return expression");
            };
            let (source, display) = match result.value.unwrap() {
                ExpressionNode::BinaryOp(binary) => match *binary.r {
                    ExpressionNode::Range(node) => (node.to_code(), node.to_string()),
                    _ => panic!("expected a range"),
                },
                ExpressionNode::Mapping(node) => (node.to_code(), node.to_string()),
                ExpressionNode::UnaryOp(node) => (node.to_code(), node.to_string()),
                ExpressionNode::Call(call) => match call.arguments.into_iter().next().unwrap() {
                    ExpressionNode::Ref(node) => (node.to_code(), node.to_string()),
                    _ => panic!("expected a reference argument"),
                },
                _ => panic!("expected a value expression"),
            };
            assert_eq!(source, expected_source);
            assert_eq!(display, expected_display);
        }
    }

    #[tokio::test]
    async fn analyzed_reference_arguments_distinguish_globals_from_locals() {
        use crate::{compiler::Compiler, test_support::test_config};

        let source = r#"
            int shared;
            void increment(int ref target) { target++; }
            void exercise() { int local; increment(ref shared); increment(ref local); }
        "#;
        let (mut program, _) = Compiler::new(test_config())
            .analyze_string("/references.c", source)
            .await
            .unwrap();
        let AstNode::FunctionDef(function) = program.body.pop().unwrap() else {
            panic!("expected a function definition");
        };
        let references: Vec<_> = function
            .body
            .into_iter()
            .filter_map(|statement| match statement {
                AstNode::Call(call) | AstNode::Expression(ExpressionNode::Call(call)) => Some(call),
                _ => None,
            })
            .flat_map(|call| call.arguments)
            .filter_map(|argument| match argument {
                ExpressionNode::Ref(reference) => Some(reference),
                _ => None,
            })
            .collect();

        assert_eq!(references.len(), 2);
        for (reference, (name, global)) in
            references.iter().zip([("shared", true), ("local", false)])
        {
            assert_eq!(reference.name.as_str(), name);
            assert_eq!(reference.global, global);
        }
    }

    #[tokio::test]
    async fn parsed_definitions_and_labels_keep_their_source_text_and_display() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        let (mut program, _) = compiler
            .parse_string(&"/function.c".into(), "void work() {}")
            .await
            .unwrap();
        let AstNode::FunctionDef(function) = program.body.pop().unwrap() else {
            panic!("expected a function definition");
        };
        assert_eq!(function.to_code(), "void work");
        assert_eq!(function.to_string(), "void work() {  }");

        for (index, (source, display)) in [
            (r#"inherit "/base";"#, "inherit /base"),
            (
                r#"private inherit "/base" parent;"#,
                "private inherit /base parent",
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let path = format!("/inherit-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let inherit = program.inherits.pop().unwrap();
            assert_eq!(inherit.to_code(), source);
            assert_eq!(inherit.to_string(), display);
        }

        let (mut program, _) = compiler
            .parse_string(
                &"/labels.c".into(),
                "void labels() { switch (1) { case 1:; default:; } }",
            )
            .await
            .unwrap();
        let AstNode::FunctionDef(function) = program.body.pop().unwrap() else {
            panic!("expected a function definition");
        };
        let AstNode::Switch(switch) = function.body.into_iter().next().unwrap() else {
            panic!("expected a switch");
        };
        let AstNode::Block(block) = *switch.body else {
            panic!("expected a switch body");
        };
        assert_eq!(block.body.len(), 2);
        for (statement, (source, display, default)) in block
            .body
            .into_iter()
            .zip([("case 1:", "1", false), ("default", "default", true)])
        {
            let AstNode::LabeledStatement(statement) = statement else {
                panic!("expected a labeled statement");
            };
            assert_eq!(statement.label.to_code(), source);
            assert_eq!(statement.label.to_string(), display);
            assert_eq!(statement.label.is_default(), default);
        }
    }

    #[tokio::test]
    async fn parsed_call_diagnostics_preserve_namespaces_and_receivers() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, call) in [
            "work(7)",
            "::work(7)",
            "base::work(7)",
            "efun::strlen(\"x\")",
            "obj->work(7)",
            "obj->base::work(7)",
            "get()(7)",
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!("mixed caller(mixed obj) {{ return {call}; }}");
            let path = format!("/call-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::FunctionDef(mut function) = program.body.pop().unwrap() else {
                panic!("expected a function definition");
            };
            let AstNode::Return(result) = function.body.pop().unwrap() else {
                panic!("expected a return expression");
            };
            let ExpressionNode::Call(node) = result.value.unwrap() else {
                panic!("expected a call");
            };
            assert_eq!(node.to_code(), call);
            assert_eq!(node.to_string(), call);
        }
    }

    #[tokio::test]
    async fn parsed_pointer_diagnostics_preserve_receivers_and_bound_arguments() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, (pointer, display)) in [
            ("&work()", "&work"),
            ("&->work()", "&->work"),
            ("&obj->work()", "&(obj)->work"),
            ("&work(7)", "&work(7)"),
            ("&->work(7)", "&->work(7)"),
            ("&obj->work(7)", "&(obj)->work(7)"),
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!("mixed caller(mixed obj) {{ return {pointer}; }}");
            let path = format!("/pointer-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::FunctionDef(mut function) = program.body.pop().unwrap() else {
                panic!("expected a function definition");
            };
            let AstNode::Return(result) = function.body.pop().unwrap() else {
                panic!("expected a return expression");
            };
            let ExpressionNode::FunctionPtr(node) = result.value.unwrap() else {
                panic!("expected a function pointer");
            };
            assert_eq!(node.to_code(), pointer);
            assert_eq!(node.to_string(), display);
        }
    }

    #[tokio::test]
    async fn parsed_foreach_diagnostics_keep_initializer_separators() {
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, (source, expected)) in [
            (
                "void each(int *items) { foreach (int value: items); }",
                "foreach (int value: items) { NoOp }",
            ),
            (
                "void each(mapping items) { foreach (int key, int value: items); }",
                "foreach (int key, int value: items) { NoOp }",
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let path = format!("/foreach-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::FunctionDef(function) = program.body.pop().unwrap() else {
                panic!("expected a function definition");
            };
            let AstNode::ForEach(node) = function.body.into_iter().next().unwrap() else {
                panic!("expected a foreach loop");
            };
            assert_eq!(node.to_string(), expected);
        }
    }

    #[tokio::test]
    async fn parsed_class_and_binary_diagnostics_preserve_their_operations() {
        use super::ast_node::SpannedNode;
        use crate::{compiler::Compiler, test_support::test_config};

        let compiler = Compiler::new(test_config());
        for (index, expression) in [
            "new(class Point, x: 7)",
            "point.x",
            "point.x += 7",
            "left + right",
        ]
        .into_iter()
        .enumerate()
        {
            let source = format!(
                "class Point {{ int x; }} mixed value(class Point point, int left, int right) {{ return {expression}; }}"
            );
            let path = format!("/class-expression-{index}.c").into();
            let (mut program, _) = compiler.parse_string(&path, source).await.unwrap();
            let AstNode::FunctionDef(mut function) = program.body.pop().unwrap() else {
                panic!("expected a function definition");
            };
            let AstNode::Return(result) = function.body.pop().unwrap() else {
                panic!("expected a return expression");
            };
            let (source, display) = match result.value.unwrap() {
                ExpressionNode::ClassInit(node) => (node.to_code(), node.to_string()),
                ExpressionNode::Member(node) => (node.to_code(), node.to_string()),
                ExpressionNode::CompoundAssignment(node) => (node.to_code(), node.to_string()),
                ExpressionNode::BinaryOp(node) => (node.to_code(), node.to_string()),
                _ => panic!("expected a class or binary expression"),
            };
            assert_eq!(source, expression);
            assert_eq!(display, expression);
        }
    }
}
