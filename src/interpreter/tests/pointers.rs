//! Function pointer calls through variables, indexed values, and partial application.

use indoc::indoc;

use super::{fails, run};
use crate::interpreter::lpc_ref::LpcRef;

const ADD3: &str = "int add3(int a, int b, int c) { return a + 10 * b + 100 * c; }\n";

#[tokio::test]
async fn an_indexed_array_function_can_be_called() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function *callbacks = ({ &add3() });
                    return ({ callbacks[0](1, 2, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn an_indexed_mapping_function_can_be_called() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    mapping callbacks = ([ "add": &add3(1) ]);
                    return ({ callbacks["add"](2, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn indexed_calls_compose_with_nested_indexes_and_call_chains() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function make() { return (: $1 + 1 :); }
            mapping callbacks() { return ([ "make": ({ &make() }) ]); }
            mixed *create() {
                return ({ callbacks()["make"][0]()(41) });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn an_indexed_callee_is_evaluated_once_before_its_arguments() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            int order;
            int add(int a) { return a + 1; }
            function *callbacks() { order = order * 10 + 1; return ({ &add() }); }
            int index() { order = order * 10 + 2; return 0; }
            int argument() { order = order * 10 + 3; return 41; }
            mixed *create() {
                int result = callbacks()[index()](argument());
                return ({ result, order });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42), LpcRef::from(123)]);
}

#[tokio::test]
async fn an_indexed_call_captures_its_collection_in_a_closure() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function make() {
                mapping callbacks = ([ "add": (: $1 + 1 :) ]);
                return (: callbacks["add"]($1) :);
            }
            mixed *create() { return ({ make()(41) }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn indexed_calls_compose_with_object_receivers() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            function *callbacks() { return ({ &this_object() }); }
            int answer() { return 42; }
            mixed *create() {
                return ({ this_object()->callbacks()[0]()->answer() });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(42)]);
}

#[tokio::test]
async fn an_indexed_call_spreads_after_bound_arguments() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function *callbacks = ({ &add3(1) });
                    int *args = ({ 2, 3 });
                    return ({ callbacks[0](args...) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn an_indexed_call_rejects_reference_arguments() {
    let e = fails(
        "",
        &[],
        "mixed *create() { function *fs = ({ (: $1 :) }); int x; return ({ fs[0](ref x) }); }",
    )
    .await;
    assert!(
        e.contains("a function pointer cannot take an argument by reference"),
        "{e}"
    );
}

#[tokio::test]
async fn an_indexed_non_function_is_a_runtime_error() {
    let e = fails(
        "",
        &[],
        "mixed *create() { mapping callbacks = ([ \"value\": 5 ]); return ({ callbacks[\"value\"]() }); }",
    )
    .await;
    assert!(e.contains("callfp instruction on non-function: 5"), "{e}");
}

#[tokio::test]
async fn a_pointer_over_a_function_value_appends_its_partial_list() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3();
                    function g = &f(, 2);
                    function h = &g();
                    return ({ g(1, 3), h(1, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321), LpcRef::from(321)]);
}

#[tokio::test]
async fn a_pointer_over_a_bound_function_value_keeps_its_bindings() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3(1);
                    function g = &f(, 3);
                    return ({ g(2) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn a_pointer_over_a_captured_function_value_works_inside_a_closure() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD3}{}",
            indoc! { r#"
                mixed *create() {
                    function f = &add3();
                    function mk = (: &f(, 2) :);
                    function g = mk();
                    return ({ g(1, 3) });
                }
            "# }
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(321)]);
}

#[tokio::test]
async fn applying_arguments_to_a_non_function_is_a_runtime_error() {
    let e = fails(
        "",
        &[],
        "mixed *create() { mixed f = 5; function g = &f(1); return ({ g }); }",
    )
    .await;
    assert!(
        e.contains("cannot apply arguments to int"),
        "unexpected error: {e}"
    );
}
