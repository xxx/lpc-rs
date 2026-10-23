use crate::{
    compiler::Compiler,
    interpreter::{
        lpc_ref::LpcRef,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::{PERMISSIVE_MASTER, lib_holding, run_prog, temp_lib_config, test_config},
};

async fn returns(code: &str, expected: i64) {
    assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(expected)));
}

async fn rejects(code: &str, expected: &str) {
    let error = Compiler::new(test_config())
        .compile_string("/captures.c", code)
        .await
        .unwrap_err();
    assert!(error.diagnostic_string().contains(expected), "{error:?}");
}

#[tokio::test]
async fn saves_a_value_while_implicit_captures_still_share_the_variable() {
    returns(
        r#"
        int create() {
            int n = 10;
            function live = (: n + $1 :);
            function saved = (: $(n) + $1 :);
            n = 20;
            return live(1) * 100 + saved(1);
        }
    "#,
        2111,
    )
    .await;
}

#[tokio::test]
async fn locals_initialized_from_a_capture_do_not_alias_its_storage() {
    returns(
        r#"
        int create() {
            int n = 7;
            function f = (: int local = $(n); ++local :);
            return f() == 8 && f() == 8 && n == 7;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn evaluates_expressions_once_at_creation_in_source_order() {
    returns(
        r#"
        int calls;
        int next() { return ++calls; }
        int create() {
            function f = (: $(next()) * 10 + $(next()) :);
            int at_creation = calls;
            int first = f();
            int second = f();
            return at_creation == 2 && calls == 2 && first == 12 && second == 12;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn captures_in_unselected_body_branches_are_still_eager() {
    returns(
        r#"
        int calls;
        int next() { return ++calls; }
        int create() {
            function f = (: $1 ? $(next()) : $(next()) :);
            function g = (: 0 && $(next()) :);
            return calls == 3 && f(1) == 1 && f(0) == 2 && g() == 0 && calls == 3;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn skips_captures_when_the_closure_itself_is_not_created() {
    returns(
        r#"
        int create() {
            int n;
            function f = 0 ? (: $(++n) :) : (: $(n += 2) :);
            return n == 2 && f() == 2;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn each_loop_iteration_gets_a_fresh_saved_value() {
    returns(
        r#"
        int create() {
            function *saved = ({});
            function *live = ({});
            for (int i = 0; i < 3; i++) {
                saved += ({ (: $(i) :) });
                live += ({ (: i :) });
            }
            return saved[0]() == 0 && saved[1]() == 1 && saved[2]() == 2 &&
                live[0]() == 3 && live[1]() == 3 && live[2]() == 3;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn resolves_names_before_closure_parameter_and_local_shadowing() {
    returns(
        r#"
        int create() {
            int n = 7;
            function f = (: [int n] $(n) * 10 + n :);
            function g = (: int n = 4; $(n) * 10 + n :);
            n = 9;
            return f(2) == 72 && g() == 74;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn nested_captures_resolve_the_creators_positional_arguments() {
    returns(
        r#"
        int create() {
            function factory = (: (: $($2) * 10 + $1 :) :);
            function first = factory(99, 3);
            function second = factory(99, 4);
            return first(5) == 35 && second(6) == 46;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn nested_capture_expressions_escape_one_closure_each() {
    returns(
        r#"
        int create() {
            int n = 1;
            function factory = (: (: $($(n)) * 10 + $(n) :) :);
            n = 2;
            function f = factory();
            n = 3;
            function g = factory();
            return f() == 12 && g() == 13;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn a_closure_inside_a_capture_keeps_its_own_capture_rules() {
    returns(
        r#"
        int create() {
            int n = 1;
            function factory = (: $((: n + $(n) :)) :);
            n = 10;
            function f = factory();
            return f();
        }
    "#,
        11,
    )
    .await;
}

#[tokio::test]
async fn global_initializers_do_not_shift_ordinary_function_capture_slots() {
    returns(
        r#"
        int n = 5;
        function saved = (: $(n) :);
        function factory(int x) { return (: x + $(x) :); }
        int create() {
            n = 9;
            function f = factory(3);
            return saved() == 5 && f() == 6;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn arrays_mappings_and_classes_keep_shared_identity_and_contents() {
    returns(
        r#"
        class record { int n; }
        int create() {
            int *a = ({ 1 });
            mapping m = ([ "n": 2 ]);
            class record c = new(class record, n: 3);
            function fa = (: $(a) :);
            function fm = (: $(m) :);
            function fc = (: $(c) :);
            function fields = (: $(a)[0] + $(m)["n"] + $(c).n :);
            a[0] = 10;
            m["n"] = 20;
            c.n = 30;
            int same = fa() == a && fm() == m && fc() == c;
            a = ({ 100 }); m = ([]); c = new(class record);
            return same && fields() == 60 && fa() != a && fm() != m && fc() != c;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn members_and_elements_of_captured_values_remain_writable() {
    returns(
        r#"
        class record { int n; }
        int create() {
            class record c = new(class record, n: 1);
            int *a = ({ 2 });
            mapping m = ([ "n": 3 ]);
            function f = (: $(c).n++; $(a)[0] += 10; $(m)["n"] = 30; :);
            f(); f();
            return c.n == 3 && a[0] == 22 && m["n"] == 30;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn compound_assignment_desugaring_does_not_duplicate_capture_evaluation() {
    returns(
        r#"
        int calls;
        int *a = ({ 0 });
        int *get_array() { calls++; return a; }
        int create() {
            function f = (: $(get_array())[0] += 1 :);
            f(); f();
            return calls == 1 && a[0] == 2;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn separate_macro_expansions_capture_separate_values() {
    returns(
        r#"
        #define SAVE(x) $(x)
        int create() {
            int n;
            function f = (: SAVE(++n) * 10 + SAVE(++n) :);
            return n == 2 && f() == 12;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn mapping_key_deduplication_does_not_drop_explicit_captures() {
    returns(
        r#"
        int create() {
            int n;
            function f = (: ([ "same": $(++n), "same": $(++n) ]) :);
            function g = (: ([ $(++n): 30, $(++n): 40 ]) :);
            mapping first = f();
            mapping second = g();
            return n == 4 && sizeof(first) == 1 && first["same"] == 2 &&
                sizeof(second) == 2 && second[3] == 30 && second[4] == 40;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn reference_parameters_are_captured_by_value() {
    returns(
        r#"
        function snapshot(int ref n) { return (: $(n) :); }
        int create() {
            int n = 7;
            function f = snapshot(ref n);
            n = 9;
            return f();
        }
    "#,
        7,
    )
    .await;
}

#[tokio::test]
async fn captured_functions_are_callable_with_the_saved_value() {
    returns(
        r#"
        int create() {
            function operation = (: $1 + 1 :);
            function saved = (: $(operation)($1) :);
            operation = (: $1 + 10 :);
            return saved(5);
        }
    "#,
        6,
    )
    .await;
}

#[tokio::test]
async fn efun_outputs_can_write_through_a_captured_array() {
    returns(
        r#"
        int create() {
            int *output = ({ 0 });
            function parse = (: sscanf($1, "%d", $(output)[0]) :);
            return parse("37") == 1 && output[0] == 37;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn captures_preserve_default_and_variadic_closure_arguments() {
    returns(
        r#"
        int create() {
            int n = 7;
            function f = (: [int x = $(n), mixed *rest...] x + sizeof(rest) :);
            n = 9;
            return f() == 7 && f(10, 20, 30) == 12;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn capture_failures_are_raised_at_creation_before_later_captures() {
    returns(
        r#"
        int fail() { throw("capture failed"); return 0; }
        int create() {
            int n;
            function f;
            mixed error = catch(f = (: $(fail()) + $(++n) :));
            return stringp(error) && n == 0 && !f;
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn lima_style_filter_captures_match_the_value_at_creation() {
    returns(
        r#"
        int create() {
            string file = "keep";
            function predicate = (: $1 == $(file) :);
            file = "other";
            string *matches = filter(({ "keep", "other", "keep" }), predicate);
            return sizeof(matches) == 2 && matches[0] == "keep" && matches[1] == "keep";
        }
    "#,
        1,
    )
    .await;
}

#[tokio::test]
async fn inherited_global_and_local_captures_keep_their_own_storage() {
    let root = lib_holding(
        "explicit-captures",
        &[(
            "parent.c",
            r#"
        int n = 4;
        function saved = (: $(n) :);
        function factory(int x) { return (: $(x) + n :); }
        int parent_value() { return saved(); }
    "#,
        )],
    );
    let vm = Vm::new(temp_lib_config(&root));
    vm.initialize_process_from_code("/secure/master.c", PERMISSIVE_MASTER)
        .await
        .unwrap();
    let task = vm
        .initialize_process_from_code(
            "/child.c",
            r#"
        inherit "/parent";
        int padding = 100;
        function own = (: $(padding) :);
        int create() {
            function f = factory(3);
            n = 10;
            return parent_value() == 4 && f() == 13 && own() == 100;
        }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(task.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn captured_nested_values_survive_gc_after_the_creator_returns() {
    let vm = Vm::new(test_config());
    let task = vm
        .initialize_process_from_code(
            "/captures.c",
            r#"
        class record { int *items; }
        function saved;
        void create() {
            class record r = new(class record, items: ({ 7 }));
            saved = (: $(r).items[0] :);
        }
        int read() { return saved(); }
    "#,
        )
        .await
        .unwrap();
    let process = task.context.process().clone();
    drop(task);
    vm.global_state.gc().await.unwrap().unwrap();
    let result = apply_function_by_name(
        "read",
        &[],
        process,
        TaskTemplate::from(vm.global_state.clone()),
        None,
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(result, LpcRef::from(7));
}

#[tokio::test]
async fn an_aborted_attempt_discards_capture_side_effects_and_the_new_pointer() {
    let vm = Vm::new(test_config());
    let task = vm
        .initialize_process_from_code(
            "/captures.c",
            r#"
        int n = 1;
        function saved = (: $(n) :);
        void fail() { saved = (: $(++n) :); throw("abort"); }
        int read() { return n == 1 && saved() == 1; }
    "#,
        )
        .await
        .unwrap();
    let process = task.context.process().clone();
    drop(task);
    let template = TaskTemplate::from(vm.global_state.clone());
    let error = apply_function_by_name("fail", &[], process.clone(), template.clone(), None)
        .await
        .unwrap()
        .unwrap_err();
    assert!(error.to_string().contains("abort"));
    vm.global_state.gc().await.unwrap().unwrap();
    let result = apply_function_by_name("read", &[], process, template, None)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(result, LpcRef::from(1));
}

#[tokio::test]
async fn rejects_capture_outside_a_closure() {
    rejects(
        "int create() { return $(1); }",
        "`$()` can only be used within a closure",
    )
    .await;
}

#[tokio::test]
async fn rejects_a_closures_own_parameter_in_its_capture_expression() {
    rejects(
        "function create() { return (: [int n] $(n) :); }",
        "undefined variable `n`",
    )
    .await;
}

#[tokio::test]
async fn rejects_a_closures_own_positional_in_its_capture_expression() {
    rejects(
        "function create() { return (: $($1) :); }",
        "positional argument variables can only be used within a closure",
    )
    .await;
}

#[tokio::test]
async fn rejects_direct_assignment_to_a_captured_value() {
    for expression in ["$(n) = 2", "$(n) += 2", "$(n)++", "--$(n)"] {
        rejects(
            &format!("function create() {{ int n; return (: {expression} :); }}"),
            "`$()` is not assignable",
        )
        .await;
    }
}

#[tokio::test]
async fn preserves_static_type_checks_on_captured_expressions() {
    rejects(
        r#"function create() { return (: $("hello") - 1 :); }"#,
        "Mismatched types",
    )
    .await;
}

#[tokio::test]
async fn rejects_void_capture_expressions() {
    rejects(
        "void nothing() {} function create() { return (: $(nothing()) :); }",
        "cannot capture a void expression",
    )
    .await;
}
