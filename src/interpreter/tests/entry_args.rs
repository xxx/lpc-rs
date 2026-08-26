//! Arguments that enter a task at its seed — `call_other`, applies, process
//! init — must land where the function declares them, like a direct call's.

use indoc::indoc;

use crate::{interpreter::vm::Vm, test_support::test_config};

/// `create()`'s result for `master` compiled as `/master.c`, with `target`
/// already loaded as `/target` on the same `Vm`.
async fn call_target(target: &str, master: &str) -> Option<crate::interpreter::lpc_ref::LpcRef> {
    let vm = Vm::new(test_config());
    vm.initialize_process_from_code("/target.c", target)
        .await
        .unwrap();
    vm.initialize_process_from_code("/master.c", master)
        .await
        .unwrap()
        .result()
}

#[tokio::test]
async fn call_other_seeds_a_parameter_the_callee_captures() {
    let result = call_target(
        indoc! { r#"
            int twice(int a) { function g = (: a * 2 :); return g(); }
        "# },
        indoc! { r#"
            int create() { return "/target"->twice(21); }
        "# },
    )
    .await;
    assert_eq!(result, Some(42.into()));
}

#[tokio::test]
async fn call_other_seeds_argv_for_an_ellipsis_function() {
    let result = call_target(
        indoc! { r#"
            int count(...) { return sizeof(argv); }
        "# },
        indoc! { r#"
            int create() { return "/target"->count(1, 2, 3); }
        "# },
    )
    .await;
    assert_eq!(result, Some(3.into()));
}

#[tokio::test]
async fn call_other_seeds_the_declared_and_ellipsis_parts_together() {
    let result = call_target(
        indoc! { r#"
            int total(int a, ...) { return a * 100 + sizeof(argv); }
        "# },
        indoc! { r#"
            int create() { return "/target"->total(7, "x", "y"); }
        "# },
    )
    .await;
    assert_eq!(result, Some(702.into()));
}
