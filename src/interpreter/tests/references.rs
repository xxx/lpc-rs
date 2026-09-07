//! By-reference arguments, end to end through the compiler and the VM.

use indoc::indoc;

use crate::{
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
    test_support::test_config,
};

/// The int `create()` returns for `code` compiled as `/master.c`.
async fn create_returns(code: &str) -> i64 {
    let vm = Vm::new(test_config());
    let proc = vm
        .initialize_process_from_code("/master.c", code)
        .await
        .unwrap();
    let LpcRef::Int(LpcInt(n)) = proc.result().expect("create() returns") else {
        panic!("create() did not return an int");
    };
    n
}

/// The runtime error `create()` raises.
async fn create_fails(code: &str) -> String {
    let vm = Vm::new(test_config());
    vm.initialize_process_from_code("/master.c", code)
        .await
        .unwrap_err()
        .to_string()
}

#[tokio::test]
async fn inc_through_ref_writes_the_callers_local() {
    let n = create_returns(indoc! { r#"
        void inc(int ref x) { x++; }
        int create() { int y = 1; inc(ref y); inc(ref y); return y; }
    "# })
    .await;
    assert_eq!(n, 3);
}

#[tokio::test]
async fn assignment_through_ref_replaces_the_value() {
    let n = create_returns(indoc! { r#"
        void set(int ref x, int v) { x = v; }
        int create() { int y; set(ref y, 42); return y; }
    "# })
    .await;
    assert_eq!(n, 42);
}

#[tokio::test]
async fn a_ref_of_a_global_writes_the_global() {
    let n = create_returns(indoc! { r#"
        int g = 5;
        void dbl(int ref x) { x *= 2; }
        int create() { dbl(ref g); return g; }
    "# })
    .await;
    assert_eq!(n, 10);
}

#[tokio::test]
async fn a_ref_of_a_captured_local_shares_the_cell_with_the_closure() {
    let n = create_returns(indoc! { r#"
        void inc(int ref x) { x++; }
        int create() {
            int y = 1;
            function get = (: y :);
            inc(ref y);
            return get();
        }
    "# })
    .await;
    assert_eq!(n, 2);
}

#[tokio::test]
async fn a_closure_capturing_a_ref_parameter_aliases_the_caller_after_return() {
    let n = create_returns(indoc! { r#"
        function keep(int ref x) { return (: x++ :); }
        int create() {
            int y = 1;
            function bump = keep(ref y);
            bump();
            bump();
            return y;
        }
    "# })
    .await;
    assert_eq!(n, 3);
}

#[tokio::test]
async fn a_capture_and_a_ref_parameter_coexist_in_one_frame() {
    let n = create_returns(indoc! { r#"
        function keep(int a, int ref x) { x += a; return (: a :); }
        int create() { int y = 1; function g = keep(5, ref y); return y * 100 + g(); }
    "# })
    .await;
    assert_eq!(n, 605);
}

#[tokio::test]
async fn a_ref_passes_on_by_ref() {
    let n = create_returns(indoc! { r#"
        void inc(int ref x) { x++; }
        void twice(int ref x) { inc(ref x); inc(ref x); }
        int create() { int y; twice(ref y); return y; }
    "# })
    .await;
    assert_eq!(n, 2);
}

#[tokio::test]
async fn recursion_through_ref_accumulates() {
    let n = create_returns(indoc! { r#"
        void count(int n, int ref acc) { if (n == 0) return; acc += n; count(n - 1, ref acc); }
        int create() { int total; count(4, ref total); return total; }
    "# })
    .await;
    assert_eq!(n, 10);
}

#[tokio::test]
async fn the_callee_sees_a_write_the_caller_makes_during_a_callback() {
    let n = create_returns(indoc! { r#"
        int y;
        void poke() { y = 7; }
        int peek(int ref x) { poke(); return x; }
        int create() { y = 1; return peek(ref y); }
    "# })
    .await;
    assert_eq!(n, 7);
}

#[tokio::test]
async fn an_inherited_ref_function_called_directly_works() {
    // `inherit` always compiles its target from disk (never from the object
    // space), so `/base` here is `tests/fixtures/code/base.c`.
    let vm = Vm::new(test_config());
    let proc = vm
        .initialize_process_from_code(
            "/child.c",
            indoc! { r#"
                inherit "/base";
                int create() { int y = 4; inc(ref y); return y; }
            "# },
        )
        .await
        .unwrap();
    assert_eq!(proc.result(), Some(LpcRef::from(5)));
}

#[tokio::test]
async fn call_other_into_a_ref_function_is_a_runtime_error() {
    // Same `vm` for both: `find_object` resolves `"/target"` from the
    // object space, and a second `Vm` would never see it there.
    let vm = Vm::new(test_config());
    vm.initialize_process_from_code("/target.c", "void inc(int ref x) { x++; }")
        .await
        .unwrap();
    let err = vm
        .initialize_process_from_code(
            "/master.c",
            indoc! { r#"
                int create() { int y; return "/target"->inc(y); }
            "# },
        )
        .await
        .unwrap_err()
        .to_string();
    assert!(
        err.contains("argument 1 of `inc` must be passed by reference"),
        "{err}"
    );
}

#[tokio::test]
async fn a_pointer_to_a_ref_function_refuses_to_fire() {
    let err = create_fails(indoc! { r#"
        void inc(int ref x) { x++; }
        int create() { function p = &inc(); int y; return p(y); }
    "# })
    .await;
    assert!(
        err.contains("`inc` takes argument 1 by reference; call it directly"),
        "{err}"
    );
}

#[tokio::test]
async fn call_other_with_too_few_args_into_a_ref_function_is_a_runtime_error() {
    // Same `vm` for both: `find_object` resolves `"/target"` from the
    // object space, and a second `Vm` would never see it there.
    let vm = Vm::new(test_config());
    vm.initialize_process_from_code("/target.c", "void inc(int ref x) { x++; }")
        .await
        .unwrap();
    let err = vm
        .initialize_process_from_code(
            "/master.c",
            indoc! { r#"
                int create() { return "/target"->inc(); }
            "# },
        )
        .await
        .unwrap_err()
        .to_string();
    assert!(
        err.contains("argument 1 of `inc` must be passed by reference"),
        "{err}"
    );
}

#[tokio::test]
async fn a_child_override_does_not_intercept_the_bases_ref_call() {
    // `inherit` always compiles its target from disk (never from the object
    // space), so `/base` here is `tests/fixtures/code/base.c`.
    let vm = Vm::new(test_config());
    let result = vm
        .initialize_process_from_code(
            "/child.c",
            indoc! { r#"
                inherit "/base";
                void inc(int x) { }
                int create() { return run(); }
            "# },
        )
        .await;
    // `run()`'s call to `inc` binds statically to `/base.c`'s own `inc` at
    // compile time, not virtually to the child's override, so `y` becomes 2.
    let task = result.unwrap();
    assert_eq!(task.result(), Some(LpcRef::from(2)));
}

#[tokio::test]
async fn sscanf_writes_an_array_element() {
    let n = create_returns(indoc! { r#"
        int create() { int *a = ({ 7, 8 }); sscanf("1 2", "%d %d", a[0], a[1]); return a[0] * 10 + a[1]; }
    "# })
    .await;
    assert_eq!(n, 12);
}

#[tokio::test]
async fn sscanf_treats_an_element_exactly_like_a_variable_on_a_partial_match() {
    let n = create_returns(indoc! { r#"
        int create() {
            int x = 7, y = 8;
            int *a = ({ 7, 8 });
            int nx = sscanf("1", "%d %d", x, y);
            int na = sscanf("1", "%d %d", a[0], a[1]);
            return nx == na && x == a[0] && y == a[1];
        }
    "# })
    .await;
    assert_eq!(n, 1);
}

#[tokio::test]
async fn sscanf_writes_a_mapping_element_and_a_global_element() {
    let n = create_returns(indoc! { r#"
        int *g = ({ 7 });
        int create() {
            mapping m = ([ "k": 5 ]);
            sscanf("3", "%d", m["k"]);
            sscanf("2", "%d", g[0]);
            return m["k"] * 10 + g[0];
        }
    "# })
    .await;
    assert_eq!(n, 32);
}

#[tokio::test]
async fn an_indexed_target_is_evaluated_once_before_the_call() {
    let n = create_returns(indoc! { r#"
        int create() { int i = 0; int *a = ({ 7, 8 }); sscanf("5", "%d", a[i++]); return a[0] * 10 + i; }
    "# })
    .await;
    assert_eq!(n, 51);
}

#[tokio::test]
async fn a_nested_element_is_written_in_place() {
    let n = create_returns(indoc! { r#"
        int create() { mixed *a = ({ ({ 1, 2 }) }); sscanf("9", "%d", a[0][1]); return a[0][1]; }
    "# })
    .await;
    assert_eq!(n, 9);
}

#[tokio::test]
async fn a_hidden_cell_does_not_disturb_a_closures_capture() {
    let n = create_returns(indoc! { r#"
        int create() {
            int *a = ({ 7 });
            int c = 1;
            function f = (: c :);
            sscanf("3", "%d", a[0]);
            return a[0] * 10 + f();
        }
    "# })
    .await;
    assert_eq!(n, 31);
}

#[tokio::test]
async fn parse_command_writes_an_array_element() {
    let n = create_returns(indoc! { r#"
        int create() {
            mixed *t = ({ 0 });
            int n = parse_command("foo", ({ }), "%w", t[0]);
            return n * 10 + (t[0] == "foo");
        }
    "# })
    .await;
    assert_eq!(n, 11);
}
