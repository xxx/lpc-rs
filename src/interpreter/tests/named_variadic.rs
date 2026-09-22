use super::{fails, run, run_with, s};
use crate::{
    compiler::Compiler,
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::{PERMISSIVE_MASTER, lib_holding, temp_lib_config, test_config},
};
use lpc_rs_utils::config::ConfigBuilder;

async fn values(code: &str) -> Vec<LpcRef> {
    run("", &[], code).await
}

async fn compile_error(code: &str) -> String {
    Compiler::new(test_config())
        .compile_string("/named_variadic.c", code)
        .await
        .unwrap_err()
        .to_string()
}

#[tokio::test]
async fn collects_zero_one_and_many_arguments_in_order() {
    assert_eq!(
        values(
            r#"
            mixed *collect(mixed *args...) { return args; }
            mixed *create() {
                return ({ sizeof(collect()), sizeof(collect(0)) }) + collect(1, "two", 3);
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(0),
            LpcRef::from(1),
            LpcRef::from(1),
            s("two"),
            LpcRef::from(3)
        ]
    );
}

#[tokio::test]
async fn separates_the_fixed_prefix_from_the_collector() {
    assert_eq!(
        values(
            r#"
            mixed *collect(int n, string label, mixed *args...) {
                return ({ n, label, sizeof(args) }) + args;
            }
            mixed *create() { return collect(7, "label", 8, 9); }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            s("label"),
            LpcRef::from(2),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn defaults_do_not_consume_or_require_a_collector_argument() {
    assert_eq!(
        values(
            r#"
            int defaults;
            mixed *collect(int n = ++defaults, mixed *args...) {
                return ({ n, sizeof(args) }) + args;
            }
            mixed *create() {
                return collect() + collect(0) + collect(8, 9) + ({ defaults });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(8),
            LpcRef::from(1),
            LpcRef::from(9),
            LpcRef::from(1)
        ]
    );
}

#[tokio::test]
async fn varargs_still_allows_omitting_fixed_parameters() {
    assert_eq!(
        values(
            r#"
            varargs mixed *collect(int n, int m, mixed *args...) {
                return ({ n, m, sizeof(args) }) + args;
            }
            mixed *create() { return collect() + collect(7) + collect(1, 2, 3); }
        "#
        )
        .await,
        vec![
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(7),
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(1),
            LpcRef::from(3)
        ]
    );
}

#[tokio::test]
async fn typed_collectors_supply_the_declared_element_type_to_the_body() {
    assert_eq!(
        values(
            r#"
            string join(string *names...) { return implode(names, "/"); }
            int sum(int *numbers...) { return numbers[0] + numbers[1]; }
            mixed *create() { return ({ join("east", "west"), sum(2, 3) }); }
        "#
        )
        .await,
        vec![s("east/west"), LpcRef::from(5)]
    );
    let error = compile_error("int first(string *names...) { return names[0]; }").await;
    assert!(
        error.contains("return") && error.contains("string"),
        "{error}"
    );
}

#[tokio::test]
async fn typed_collectors_keep_existing_shallow_array_typing() {
    assert_eq!(
        values(
            r#"
            int count(string *names...) { return sizeof(names); }
            mixed *create() { return ({ count(1, "two"), count(({ 1, "two" })...) }); }
        "#
        )
        .await,
        vec![LpcRef::from(2), LpcRef::from(2)]
    );
}

#[tokio::test]
async fn creates_fresh_collectors_without_copying_nested_values() {
    assert_eq!(
        values(
            r#"
            mixed *collect(mixed *args...) { return args; }
            mixed *create() {
                int *item = ({ 1 });
                mixed *input = ({ item, 2 });
                mixed *a = collect(input...);
                mixed *b = collect(input...);
                a[1] = 9;
                a[0][0] = 7;
                return ({ a == b, collect() == collect(), input[1], b[1], item[0], b[0][0] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(2),
            LpcRef::from(2),
            LpcRef::from(7),
            LpcRef::from(7)
        ]
    );
}

#[tokio::test]
async fn forwards_named_arguments_into_bare_ellipsis_and_back() {
    assert_eq!(
        values(
            r#"
            mixed *collect(int n, mixed *args...) { return ({ n }) + args; }
            mixed *legacy(...) { return collect(argv...); }
            mixed *forward(mixed *args...) { return legacy(args...); }
            mixed *create() { return forward(1, 2, 3); }
        "#
        )
        .await,
        vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn local_pointers_keep_bound_and_spread_tail_arguments() {
    assert_eq!(
        values(
            r#"
            mixed *collect(int n, mixed *args...) { return ({ n }) + args; }
            mixed *create() {
                function f = &collect(1, 2);
                function g = &collect(, 8);
                return f(3, ({ 4, 5 })...) + g(7, 9);
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(4),
            LpcRef::from(5),
            LpcRef::from(7),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn external_calls_and_pointers_populate_the_named_collector() {
    assert_eq!(
        run(
            "",
            &[(
                "/target.c",
                r#"
            mixed *collect(int n, mixed *args...) { return ({ n, sizeof(args) }) + args; }
        "#
            )],
            r#"
            mixed *create() {
                function f = &("/target")->collect(7);
                return "/target"->collect(1, 2) + call_other("/target", "collect", 3, 4)
                    + f(({ 8, 9 })...) + "/target"->collect();
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(1),
            LpcRef::from(4),
            LpcRef::from(7),
            LpcRef::from(2),
            LpcRef::from(8),
            LpcRef::from(9),
            LpcRef::from(0),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn closures_combine_defaults_fixed_positions_and_named_collectors() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                function f = (: [int n = 7, mixed *args...] ({ n, $1, sizeof(args) }) + args :);
                return f() + f(2, 3, 4);
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            LpcRef::from(7),
            LpcRef::from(0),
            LpcRef::from(2),
            LpcRef::from(2),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(4)
        ]
    );
}

#[tokio::test]
async fn named_collectors_survive_capture_and_nested_closure_calls() {
    assert_eq!(
        values(
            r#"
            function hold(int n, mixed *args...) {
                return (: [mixed *more...] ({ n }) + args + more :);
            }
            mixed *create() {
                function f = hold(1, 2, 3);
                function factory = (: [mixed *args...] (: args :) :);
                function saved = factory(8, 9);
                return f(4, 5) + f(6) + saved();
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(4),
            LpcRef::from(5),
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(6),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn collectors_can_be_reassigned_through_references_and_captures() {
    assert_eq!(
        values(
            r#"
            void replace(mixed ref *items) { items = ({ 7, 8 }); }
            mixed *collect(mixed *args...) {
                function read = (: args :);
                replace(ref args);
                return read();
            }
            mixed *create() { return collect(1, 2, 3); }
        "#
        )
        .await,
        vec![LpcRef::from(7), LpcRef::from(8)]
    );
}

#[tokio::test]
async fn a_required_reference_prefix_can_precede_the_collector() {
    assert_eq!(
        values(r#"
            int count(int ref n, mixed *args...) { n += sizeof(args); return n; }
            mixed *create() { int n = 4; int result = count(ref n, "a", "b"); return ({ n, result }); }
        "#).await,
        vec![LpcRef::from(6), LpcRef::from(6)]
    );
}

#[tokio::test]
async fn inherited_constructors_forward_named_tails_to_setup() {
    let root = lib_holding(
        "named-variadic-constructor",
        &[
            (
                "base.c",
                r#"
        int *received;
        void setup(int *args...) { received = args; }
        void create(mixed *args...) { setup(args...); }
        int *query_received() { return received; }
    "#,
            ),
            (
                "target.c",
                r#"
        inherit "/base";
        void setup(int *args...) { base::setup(9, args...); }
    "#,
            ),
        ],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
            mixed *create() {
                object a = clone_object("/target", 1, 2);
                object b = clone_object("/target");
                return a->query_received() + b->query_received() + "/target"->query_received();
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(9),
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(9),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn argv_is_an_ordinary_name_with_a_named_collector() {
    assert_eq!(
        values(
            r#"
            mixed *collect(mixed *argv...) { return argv; }
            mixed *prefix(mixed *args...) { int argv = 7; return ({ argv }) + args; }
            mixed *create() { return collect(1, 2) + prefix(3); }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(7),
            LpcRef::from(3)
        ]
    );
}

#[tokio::test]
async fn nested_callables_have_independent_argv_reservations() {
    assert_eq!(
        values(
            r#"
            mixed *legacy(...) {
                function named = (: [mixed *argv...] argv :);
                function ordinary = (: int argv = 8; argv :);
                return named(7) + ({ ordinary() }) + argv;
            }
            mixed *create() {
                function named = (: [mixed *args...]
                    function bare = (: [...] argv :);
                    bare(args...)
                :);
                return legacy(9) + named(10);
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            LpcRef::from(8),
            LpcRef::from(9),
            LpcRef::from(10)
        ]
    );
}

#[tokio::test]
async fn named_collectors_do_not_define_implicit_argv() {
    for code in [
        "mixed *collect(mixed *args...) { return argv; }",
        "function make() { return (: [mixed *args...] argv :); }",
    ] {
        let error = compile_error(code).await;
        assert!(error.contains("undefined variable `argv`"), "{error}");
    }
}

#[tokio::test]
async fn named_tails_do_not_relax_required_prefix_count_or_types() {
    for (call, expected) in [
        ("collect()", "argument count"),
        ("collect(\"bad\")", "argument type"),
    ] {
        let error = compile_error(&format!(
            "int collect(int n, mixed *args...) {{ return n; }} int create() {{ return {call}; }}"
        ))
        .await;
        assert!(error.contains(expected), "{error}");
    }
    let error = fails(
        "",
        &[],
        r#"
        int collect(int n, mixed *args...) { return n; }
        int create() { return collect(({})...); }
    "#,
    )
    .await;
    assert!(error.contains("argument count"), "{error}");
}

#[tokio::test]
async fn rejects_non_array_reference_and_defaulted_collectors() {
    for (parameter, expected) in [
        ("mixed args...", "array type"),
        ("string args...", "array type"),
        ("mixed ref *args...", "cannot be `ref`"),
        ("mixed *args = ({})...", "cannot have a default"),
    ] {
        for code in [
            format!("void collect({parameter}) {{}}"),
            format!("function make() {{ return (: [{parameter}] 0 :); }}"),
        ] {
            let error = compile_error(&code).await;
            assert!(error.contains(expected), "{code}: {error}");
        }
    }
}

#[tokio::test]
async fn the_collector_must_be_the_last_parameter() {
    for parameters in [
        "mixed *args..., int n",
        "mixed *args..., ...",
        "..., mixed *args...",
        "mixed *a..., mixed *b...",
    ] {
        let error = compile_error(&format!("void collect({parameters}) {{}}")).await;
        assert!(
            error.contains("Unrecognized Token"),
            "{parameters}: {error}"
        );
    }
}

#[tokio::test]
async fn rejects_duplicate_collector_bindings() {
    for code in [
        "void collect(int args, mixed *args...) {}",
        "void collect(mixed *args...) { int args; }",
        "function make() { return (: [int args, mixed *args...] 0 :); }",
        "function make() { return (: [mixed *args...] int args; 0 :); }",
    ] {
        let error = compile_error(code).await;
        assert!(
            error.contains("Redefinition") || error.contains("redeclaration"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn positional_references_cannot_extend_a_named_closures_fixed_prefix() {
    for parameters in ["mixed *args...", "int n, mixed *args..."] {
        let error = compile_error(&format!(
            "function make() {{ return (: [{parameters}] $2 :); }}"
        ))
        .await;
        assert!(error.contains("beyond the fixed parameters"), "{error}");
    }
}

#[tokio::test]
async fn a_nested_bare_closure_keeps_argv_reserved() {
    for body in [
        "function f = (: [...] int argv; 0 :);",
        "function f = (: [int argv, ...] 0 :);",
    ] {
        let error = compile_error(&format!("void collect(mixed *args...) {{ {body} }}")).await;
        assert!(
            error.contains("redeclaration of `argv`") || error.contains("Redefinition of `argv`"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn implicit_argv_reservation_ends_at_the_function_boundary() {
    assert_eq!(
        values(
            r#"
            int count(...) { return sizeof(argv); }
            int argv = 7;
            mixed *create() { return ({ argv, count(1, 2) }); }
        "#
        )
        .await,
        vec![LpcRef::from(7), LpcRef::from(2)]
    );
}

#[tokio::test]
async fn omitted_collector_types_follow_strict_types() {
    assert_eq!(
        values(
            r#"
            mixed *collect(*args...) { return args; }
            mixed *create() { return collect(1, 2); }
        "#
        )
        .await,
        vec![LpcRef::from(1), LpcRef::from(2)]
    );
    let error = compile_error("#pragma strict_types\nvoid collect(*args...) {}").await;
    assert!(error.contains("Missing type for argument"), "{error}");
}

#[tokio::test]
async fn simul_efun_calls_and_pointers_keep_the_named_tail() {
    let root = lib_holding(
        "named-variadic-simul",
        &[(
            "simul.c",
            r#"
        int count(string *args...) { return sizeof(args); }
    "#,
        )],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/simul")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.global_state
        .initialize_simul_efuns()
        .await
        .unwrap()
        .unwrap();
    let process = vm
        .initialize_process_from_code(
            "/main.c",
            r#"
        int create() {
            function f = &count("bound");
            return count("one") * 100 + f(({ "two", "three" })...);
        }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(process.result(), Some(LpcRef::from(103)));
}
