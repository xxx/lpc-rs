//! `&f(, x)` over a function-typed variable: the new pointer is built over
//! the value, appending to its own partial list.

use indoc::indoc;

use super::{fails, run};
use crate::interpreter::lpc_ref::LpcRef;

const ADD3: &str = "int add3(int a, int b, int c) { return a + 10 * b + 100 * c; }\n";

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
