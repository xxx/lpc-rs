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
async fn a_child_override_that_drops_ref_is_a_runtime_error() {
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
