//! Argument spreading, `f(a, xs...)`, through every call door.

use indoc::indoc;

use super::{fails, run};
use crate::interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef};

fn n(i: i64) -> LpcRef {
    LpcRef::Int(LpcInt(i))
}

const ADD: &str = "int add(int a, int b, int c) { return a + b + c; }";

#[tokio::test]
async fn a_spread_passes_each_element_as_an_argument() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD} mixed *create() {{ int *xs = ({{ 2, 3 }}); return ({{ add(1, xs...) }}); }}"
        ),
    )
    .await;
    assert_eq!(r, vec![n(6)]);
}

#[tokio::test]
async fn a_spread_may_sit_first_or_between_arguments() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD} mixed *create() {{ int *xs = ({{ 1, 2 }}); return ({{ add(xs..., 4), add(xs..., ({{ 10 }})...) }}); }}"
        ),
    )
    .await;
    assert_eq!(r, vec![n(7), n(13)]);
}

#[tokio::test]
async fn an_empty_array_spreads_to_nothing() {
    let r = run(
        "",
        &[],
        &format!("{ADD} mixed *create() {{ return ({{ add(1, ({{ }})..., 2) }}); }}"),
    )
    .await;
    assert_eq!(r, vec![n(3)]);
}

#[tokio::test]
async fn spread_extras_land_in_argv() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            int count(int a, ...) { return sizeof(argv); }
            mixed *create() { int *xs = ({ 1, 2, 3 }); return ({ count(xs...), count(0, xs...) }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![n(2), n(3)]);
}

/// `inherit` compiles its target from disk, so this names
/// `tests/fixtures/code/spread_base.c`, not the `objects` list's object space.
#[tokio::test]
async fn an_inherited_call_spreads() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            inherit "/spread_base" base;
            int add(int a, int b, int c) { return 100; }
            mixed *create() { int *xs = ({ 1, 2, 3 }); return ({ ::add(xs...), base::add(xs...) }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![n(6), n(6)]);
}

#[tokio::test]
async fn spreading_a_non_array_is_a_runtime_error() {
    let e = fails(
        "",
        &[],
        &format!("{ADD} mixed *create() {{ mixed x = 5; return ({{ add(x...) }}); }}"),
    )
    .await;
    assert!(e.contains("cannot spread int: `...` takes an array"), "{e}");
}

const X: (&str, &str) = ("/x.c", ADD);

#[tokio::test]
async fn an_efun_call_spreads() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { mixed *args = ({ ({ "a", "b" }), "-" }); return ({ implode(args...) }); }"#,
    )
    .await;
    assert_eq!(r, vec![super::s("a-b")]);
}

#[tokio::test]
async fn a_bare_variable_after_a_spread_is_not_an_efun_lvalue() {
    let e = fails(
        "",
        &[],
        r#"mixed *create() { int a; mixed *pre = ({ "12", "%d" }); sscanf(pre..., a); return ({ a }); }"#,
    )
    .await;
    assert!(
        e.contains("argument 3 of `sscanf` must be passed by reference"),
        "{e}"
    );
}

#[tokio::test]
async fn a_spread_element_cannot_land_on_an_efun_ref_parameter() {
    let e = fails(
        "",
        &[],
        r#"mixed *create() { mixed *all = ({ "12", "%d", 0 }); sscanf(all...); return ({ 0 }); }"#,
    )
    .await;
    assert!(
        e.contains("argument 3 of `sscanf` must be passed by reference"),
        "{e}"
    );
}

#[tokio::test]
async fn call_other_spreads_in_both_forms() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { int *xs = ({ 2, 3 }); return ({ "/x"->add(1, xs...), call_other("/x", "add", xs..., 4) }); }"#,
    )
    .await;
    assert_eq!(r, vec![n(6), n(9)]);
}

#[tokio::test]
async fn a_function_pointer_spreads_after_its_bound_arguments() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD} mixed *create() {{ int *xs = ({{ 2, 3 }}); function bare = &add(); function bound = &add(10); return ({{ bare(1, xs...), bound(xs...) }}); }}"
        ),
    )
    .await;
    assert_eq!(r, vec![n(6), n(15)]);
}

#[tokio::test]
async fn a_chained_call_spreads() {
    let r = run(
        "",
        &[],
        &format!(
            "{ADD} function make() {{ return &add(); }} mixed *create() {{ int *xs = ({{ 2, 3 }}); return ({{ make()(1, xs...) }}); }}"
        ),
    )
    .await;
    assert_eq!(r, vec![n(6)]);
}

#[tokio::test]
async fn an_efun_pointer_spreads() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { function fp = &implode(); mixed *args = ({ ({ "a", "b" }), "+" }); return ({ fp(args...) }); }"#,
    )
    .await;
    assert_eq!(r, vec![super::s("a+b")]);
}

#[tokio::test]
async fn a_pointer_call_refuses_a_non_array_spread() {
    let e = fails(
        "",
        &[],
        &format!("{ADD} mixed *create() {{ function fp = &add(); mixed x = 5; return ({{ fp(x...) }}); }}"),
    )
    .await;
    assert!(e.contains("cannot spread int: `...` takes an array"), "{e}");
}
