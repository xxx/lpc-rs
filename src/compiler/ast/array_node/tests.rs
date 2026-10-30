use std::sync::Arc;

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::span::Span;
use lpc_rs_utils::config::{Config, ConfigBuilder};

use crate::{
    compiler::{
        Compiler,
        ast::{
            ast_node::{AstNode, SpannedNode},
            expression_node::ExpressionNode,
        },
        compilation_context::CompilationContext,
        lexer::LexWrapper,
    },
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::{initialize_program, lib_holding, run_prog, strings_of, temp_lib_config},
};

#[test]
fn parsed_array_retains_its_element_range_and_source_form() {
    let source = "int *items = ({ 12, 34 });";
    let mut context = CompilationContext::default();
    let mut program = crate::lpc_parser::ProgramParser::new()
        .parse(&mut context, LexWrapper::new(source, 7).triples())
        .unwrap();
    let Some(AstNode::Decl(mut declaration)) = program.body.pop() else {
        panic!("array declaration");
    };
    let Some(ExpressionNode::Array(array)) = declaration
        .initializations
        .pop()
        .and_then(|init| init.value)
    else {
        panic!("array initializer");
    };
    assert_eq!(
        array.span(),
        Some(Span::new(
            7,
            source.find("12").unwrap()..source.find("34").unwrap() + 2
        ))
    );
    assert_eq!(array.to_string(), "({ 12, 34 })");
}

fn config(mode: NullMode) -> Arc<Config> {
    ConfigBuilder::default()
        .lib_dir("tests/fixtures/code")
        .null_mode(mode)
        .build()
        .unwrap()
        .into()
}

async fn run_with_config(code: &str, config: Arc<Config>) -> LpcRef {
    let program = Compiler::new(config.clone())
        .compile_string("/array_spreading.c", code)
        .await
        .unwrap()
        .program;
    let vm = Vm::new(config);
    initialize_program::<256>(program, vm.global_state.clone())
        .await
        .unwrap()
        .result()
        .unwrap()
}

#[tokio::test]
async fn array_spreads_accept_every_position_empty_operands_and_trailing_commas() {
    let values = strings_of(
        r#"
        mixed *create() {
            int *xs = ({ 1, 2 });
            int *empty = ({});
            return ({ empty..., xs..., 3, empty..., ({ 4, 5 })..., xs..., });
        }
    "#,
    )
    .await;
    assert_eq!(values, ["1", "2", "3", "4", "5", "1", "2"]);
    assert!(
        strings_of("mixed *create() { return ({ ({})... }); }")
            .await
            .is_empty()
    );
}

#[tokio::test]
async fn array_spreads_accept_slices_and_nested_literals() {
    let values = strings_of(
        r#"
        int *create() {
            int *elems = ({ 1, 3, 4 });
            int pos = 0;
            return ({ elems[0..pos]..., 2, ({ elems[pos+1..]..., 5 })... });
        }
    "#,
    )
    .await;
    assert_eq!(values, ["1", "2", "3", "4", "5"]);
}

#[tokio::test]
async fn array_spreads_make_a_fresh_shallow_copy() {
    let task = run_prog(
        r#"
        int create() {
            int *inner = ({ 1 });
            mixed *source = ({ inner, 2 });
            mixed *copy = ({ source... });
            mixed *wrapped = ({ source..., source });
            copy[0][0] = 9;
            copy[1] = 8;
            return copy != source && copy[0] == inner && inner[0] == 9
                && source[1] == 2 && sizeof(wrapped) == 3 && wrapped[2] == source;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_infer_element_types_including_nominal_classes() {
    let task = run_prog(
        r#"
        class Record { int value; }
        int create() {
            int *ints = ({ 1 });
            string *strings = ({ "a" });
            class Record *records = ({ new(class Record, value: 3) });
            int *more_ints = ({ ints..., 2 });
            string *more_strings = ({ strings..., "b" });
            class Record *more_records = ({ records..., new(class Record, value: 4) });
            return more_ints[1] == 2 && more_strings[1] == "b" && more_records[1].value == 4;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_do_not_erase_known_element_types() {
    for code in [
        "void create() { int *xs = ({ 1 }); string *ys = ({ xs... }); }",
        "void take(string *xs) {} void create() { int *xs = ({ 1 }); take(({ xs... })); }",
        "class A { int n; } class B { int n; }
         void create() { class A *xs = ({ new(class A) }); class B *ys = ({ xs... }); }",
    ] {
        let error = Compiler::new(config(NullMode::Zero))
            .compile_string("/test.c", code)
            .await
            .unwrap_err();
        assert!(error.to_string().contains("type"), "{error}");
    }
}

#[tokio::test]
async fn array_spreads_reject_static_non_arrays_even_in_external_call_arguments() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        for expression in [
            "0",
            "nil",
            "1",
            "1.5",
            "\"text\"",
            "([])",
            "this_object()",
            "(1 ? ({ 1 }) : 2)",
        ] {
            for body in [
                format!("({{ {expression}... }});"),
                format!("this_object()->receive(({{ {expression}... }}));"),
            ] {
                let code = format!("void create() {{ {body} }}");
                let error = Compiler::new(config(mode))
                    .compile_string("/test.c", code.as_str())
                    .await
                    .unwrap_err();
                assert!(
                    error.to_string().contains("`...` spreads an array"),
                    "{mode:?}: {code}: {error}"
                );
            }
        }
    }
}

#[tokio::test]
async fn array_spreads_accept_a_choice_between_different_array_types() {
    let task = run_prog(
        r#"
        mixed *choose(int condition) {
            return ({ (condition ? ({ 1 }) : ({ "text" }))... });
        }
        int create() { return choose(1)[0] == 1 && choose(0)[0] == "text"; }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_check_mixed_values_at_runtime_in_both_null_modes() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let result = run_with_config(
            r#"
            int create() {
                mixed xs = ({ 4, 5 });
                int *values = ({ xs... });
                mixed *bad = ({ 0, nil, 1, 1.5, "text", ([]), this_object() });
                foreach (mixed value : bad) {
                    mixed error = catch(({ value... }));
                    if (!stringp(error) || strindex(error, "cannot spread") < 0) return 0;
                }
                return sizeof(values) == 2 && values[1] == 5;
            }
        "#,
            config(mode),
        )
        .await;
        assert_eq!(result, LpcRef::from(1), "{mode:?}");
    }
}

#[tokio::test]
async fn array_spreads_evaluate_each_expression_once_left_to_right() {
    let task = run_prog(
        r#"
        int order;
        int mark(int n) { order = order * 10 + n; return n; }
        int *pair(int n) { return ({ mark(n), n + 10 }); }
        int create() {
            int *values = ({ mark(1), pair(2)..., mark(3), pair(4)... });
            return order == 1234 && sizeof(values) == 6
                && values[0] == 1 && values[1] == 2 && values[2] == 12
                && values[3] == 3 && values[4] == 4 && values[5] == 14;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_keep_evaluated_values_when_later_expressions_reassign_variables() {
    let task = run_prog(
        r#"
        int create() {
            int n = 1;
            int *xs = ({ 2 });
            mixed *values = ({ n, xs..., n = 3, (xs = ({ 4 }))... });
            return sizeof(values) == 4 && values[0] == 1 && values[1] == 2
                && values[2] == 3 && values[3] == 4 && xs[0] == 4;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_expand_after_all_element_expressions_are_evaluated() {
    let task = run_prog(
        r#"
        int create() {
            int *xs = ({ 1 });
            int *values = ({ xs..., xs[0] = 2 });
            return values[0] == 2 && values[1] == 2;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spread_errors_leave_no_partial_result_or_staged_elements() {
    let task = run_prog(
        r#"
        int create() {
            mixed bad = 7;
            int *result = ({ 99 });
            mixed error = catch(result = ({ 1, ({ 2 })..., bad... }));
            mixed *next = ({ ({ 3, 4 })..., ([ "value": ({ 5 }) ]) });
            return stringp(error) && sizeof(result) == 1 && result[0] == 99
                && sizeof(next) == 3 && next[0] == 3 && next[1] == 4
                && next[2]["value"][0] == 5;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_work_with_closure_captures_and_call_spreads() {
    let task = run_prog(
        r#"
        int sum(int a, int b, int c) { return a + b + c; }
        int create() {
            int *xs = ({ 1, 2 });
            function shared = (: ({ xs..., 3 }) :);
            function snapshot = (: ({ $(xs)..., 4 }) :);
            xs = ({ 5, 6 });
            mixed *a = shared();
            mixed *b = snapshot();
            return sum(({ a... })...) == 14 && sum(b...) == 7;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_preserve_registers_when_inherited_functions_are_relocated() {
    let root = lib_holding(
        "array-spreading",
        &[
            ("first.c", "int padding = 99;"),
            (
                "base.c",
                "int *xs = ({ 1, 2 }); int *expanded() { return ({ xs..., 3 }); }",
            ),
        ],
    );
    let result = run_with_config(
        r#"
        inherit "/first";
        inherit "/base";
        int create() { int *a = expanded(); return a[0] == 1 && a[1] == 2 && a[2] == 3; }
    "#,
        Arc::new(temp_lib_config(&root)),
    )
    .await;
    assert_eq!(result, LpcRef::from(1));
}

#[tokio::test]
async fn array_spreads_are_not_limited_to_the_call_argument_count() {
    let task = run_prog(
        r#"
        int create() {
            mixed *xs = allocate(70000, 5);
            mixed *ys = ({ 1, xs..., 2 });
            return sizeof(ys) == 70002 && ys[0] == 1 && ys[70000] == 5 && ys[70001] == 2;
        }
    "#,
    )
    .await;
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn array_spreads_do_not_admit_refs_or_spreads_outside_lists() {
    for expression in ["({ ref xs })", "xs...", "([ xs...: 1 ])", "({ xs...... })"] {
        let code = format!("void create() {{ int *xs = ({{ 1 }}); {expression}; }}");
        assert!(
            Compiler::new(config(NullMode::Zero))
                .compile_string("/test.c", code)
                .await
                .is_err(),
            "{expression}"
        );
    }
}
