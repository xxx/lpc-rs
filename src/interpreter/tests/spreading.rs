//! Argument spreading, `f(a, xs...)`, through every call door.

use indoc::indoc;

use super::{fails, run};
use crate::{
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef},
    test_support::PERMISSIVE_MASTER,
};

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
        &format!("{ADD} mixed *create() {{ return ({{ add(1, ({{ }})..., 2, 3) }}); }}"),
    )
    .await;
    assert_eq!(r, vec![n(6)]);
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

#[tokio::test]
async fn a_spread_is_held_to_the_callee_s_parameter_count() {
    for (xs, received) in [("({ 1, 2 })", 2), ("({ 1, 2, 3, 4 })", 4)] {
        let e = fails(
            "",
            &[],
            &format!("{ADD} mixed *create() {{ int *xs = {xs}; return ({{ add(xs...) }}); }}"),
        )
        .await;
        assert!(
            e.contains(&format!(
                "incorrect argument count in call to `add`: expected: 3, received: {received}"
            )),
            "{e}"
        );
    }
}

#[tokio::test]
async fn a_spread_to_a_varargs_function_may_fall_short_but_not_run_over() {
    const VAR: &str = "varargs int add(int a, int b, int c) { return a + b + c; }";
    let r = run(
        "",
        &[],
        &format!("{VAR} mixed *create() {{ return ({{ add(({{ 1 }})...), add(({{ }})...) }}); }}"),
    )
    .await;
    assert_eq!(r, vec![n(1), n(0)]);

    let e = fails(
        "",
        &[],
        &format!("{VAR} mixed *create() {{ return ({{ add(({{ 1, 2, 3, 4 }})...) }}); }}"),
    )
    .await;
    assert!(e.contains("expected: 3, received: 4"), "{e}");
}

const X: (&str, &str) = ("/x.c", ADD);

#[tokio::test]
async fn call_other_drops_a_spread_s_extras() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x"->add(({ 1, 2, 3, 4 })...), "/x"->add(({ 1 })...) }); }"#,
    )
    .await;
    assert_eq!(r, vec![n(6), n(1)]);
}

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

/// A collection receiver's argument list is read by `arg_values`, not
/// `push_call_frame`'s door.
#[tokio::test]
async fn a_collection_call_other_spreads() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { int *xs = ({ 2, 3 }); mixed *r = ({ "/x", "/x" })->add(1, xs...); return ({ r[0], r[1] }); }"#,
    )
    .await;
    assert_eq!(r, vec![n(6), n(6)]);
}

#[tokio::test]
async fn a_collection_receiver_spread_past_the_ceiling_is_a_clean_runtime_error() {
    let e = fails(
        "",
        &[X],
        r#"mixed *create() { mixed *xs = allocate(65535); mixed *r = ({ "/x", "/x" })->add(xs...); return r; }"#,
    )
    .await;
    assert!(e.contains("cannot pass"), "{e}");
    assert!(e.contains("the limit is"), "{e}");
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

const LOCALS: &str = "int f(int a) { int q; int w; int z; q = 1; w = 2; z = 3; return q + w + z; }";

#[tokio::test]
async fn a_spread_past_the_register_ceiling_is_a_clean_runtime_error() {
    let e = fails(
        "",
        &[],
        &format!(
            "{LOCALS} mixed *create() {{ mixed *xs = allocate(65535); return ({{ f(xs...) }}); }}"
        ),
    )
    .await;
    assert!(e.contains("cannot pass"), "{e}");
    assert!(e.contains("the limit is"), "{e}");
}

#[tokio::test]
async fn a_spread_past_the_register_ceiling_through_a_pointer_is_a_clean_runtime_error() {
    let e = fails(
        "",
        &[],
        &format!(
            "{LOCALS} mixed *create() {{ mixed *xs = allocate(65535); function fp = &f(); return ({{ fp(xs...) }}); }}"
        ),
    )
    .await;
    assert!(e.contains("cannot pass"), "{e}");
    assert!(e.contains("the limit is"), "{e}");
}

#[tokio::test]
async fn a_large_spread_well_under_the_ceiling_still_works() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            int count(int a, ...) { int q; int w; int z; q = 1; w = 2; z = 3; return sizeof(argv); }
            mixed *create() { mixed *xs = allocate(1000); return ({ count(xs...) }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![n(999)]);
}

/// `&->add()`'s receiver is unbound until the call, which is what routes
/// this through the slow door that loads `/spread_base`.
#[tokio::test]
async fn a_spread_past_the_ceiling_through_an_unloaded_dynamic_pointer_is_a_clean_runtime_error() {
    let e = fails(
        PERMISSIVE_MASTER,
        &[],
        r#"mixed *create() { function fp = &->add(); mixed *xs = allocate(65535); return ({ fp("/spread_base", xs...) }); }"#,
    )
    .await;
    assert!(e.contains("cannot pass"), "{e}");
    assert!(e.contains("the limit is"), "{e}");
}
