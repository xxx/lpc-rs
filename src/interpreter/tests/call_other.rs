//! `call_other` (`->`) through the compiler: the result's static type and
//! a receiver that is itself a call.

use indoc::indoc;

use super::run;
use crate::interpreter::lpc_ref::LpcRef;

const X: (&str, &str) = (
    "/x.c",
    indoc! { r#"
        mixed *list() { return ({ 1, 2 }); }
        int pair(int a, int b) { return a * 10 + b; }
        int count(...) { return sizeof(argv); }
    "# },
);

#[tokio::test]
async fn the_receiver_is_evaluated_before_the_arguments() {
    let r = run(
        "",
        &[X],
        indoc! { r#"
            string order = "";
            object rcvr() { order += "r"; return find_object("/x"); }
            int arg() { order += "a"; return 1; }
            mixed *create() { rcvr()->pair(arg(), arg()); return ({ order }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![super::s("raa")]);
}

#[tokio::test]
async fn a_call_other_result_is_accepted_where_an_array_is_expected() {
    let r = run("", &[X], r#"mixed *create() { return "/x"->list(); }"#).await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(2)]);
}

#[tokio::test]
async fn a_mixed_variable_is_accepted_where_an_array_is_expected() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { mixed m = ({ 3 }); mixed *a = m; return a; }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(3)]);
}

#[tokio::test]
async fn a_call_other_on_a_call_result_passes_its_own_arguments() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ find_object("/x")->pair(1, 2) }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(12)]);
}

#[tokio::test]
async fn a_call_other_on_a_call_result_passes_no_arguments_when_given_none() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ find_object("/x")->count() }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0)]);
}
