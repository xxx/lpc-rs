//! `calling_function` and `calling_program`: the function that called
//! through the door `previous_object` names, and its file.

use indoc::indoc;

use super::{fails, run, s};
use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
    test_support::{committed_global, run_prog, test_config},
};

/// `/x.c`: answers which function called it, directly and through a local
/// call.
const X: (&str, &str) = (
    "/x.c",
    indoc! { r#"
        string who() { return calling_function(); }
        string whose() { return calling_program(); }
        string inner() { return calling_function(); }
        string via_local() { return inner(); }
    "# },
);

#[tokio::test]
async fn a_call_other_callee_sees_the_calling_function_and_its_file() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x"->who(), "/x"->whose() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("create"), s("/main.c")]);
}

#[tokio::test]
async fn a_local_call_keeps_the_callers_calling_function() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x"->via_local() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("create")]);
}

#[tokio::test]
async fn the_program_is_the_file_that_defines_the_calling_function() {
    let r = run(
        "",
        &[X],
        indoc! { r#"
            inherit "/calling_base";
            mixed *create() { return ({ ask() }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("/calling_base.c")]);
}

#[tokio::test]
async fn create_sees_the_function_that_cloned() {
    let r = run(
        "",
        &[(
            "/y.c",
            indoc! { r#"
                string made_by;
                void create() { made_by = calling_function(); }
                string made_by() { return made_by; }
            "# },
        )],
        indoc! { r#"
            string make() { return clone_object("/y")->made_by(); }
            mixed *create() { return ({ make() }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("make")]);
}

#[tokio::test]
async fn the_driver_entry_has_no_calling_function() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { return ({ calling_function(), calling_program() }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0)]);
}

#[tokio::test]
async fn a_step_walks_the_chain_and_minus_one_is_all_of_it() {
    let r = run(
        "",
        &[
            (
                "/x.c",
                indoc! { r#"
                    mixed one() { return calling_function(1); }
                    mixed all() { return implode(calling_function(-1), " "); }
                    mixed files() { return implode(calling_program(-1), " "); }
                    mixed past() { return calling_function(5); }
                "# },
            ),
            (
                "/a.c",
                indoc! { r#"
                    mixed one() { return "/x"->one(); }
                    mixed all() { return "/x"->all(); }
                    mixed files() { return "/x"->files(); }
                    mixed past() { return "/x"->past(); }
                "# },
            ),
        ],
        r#"mixed *create() { return ({ "/a"->one(), "/a"->all(), "/a"->files(), "/a"->past() }); }"#,
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("create"),
            s("all create"),
            s("/a.c /main.c"),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn another_negative_step_is_an_error() {
    let err = fails(
        "",
        &[],
        r#"mixed *create() { return ({ calling_function(-2) }); }"#,
    )
    .await;
    assert!(
        err.contains("calling_function: expected a step back or -1, got -2"),
        "{err}"
    );
}

#[tokio::test]
async fn a_closure_call_sees_the_firing_function() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            mixed *create() {
                function f = (: calling_function() :);
                return ({ f() });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("create")]);
}

#[tokio::test]
async fn a_simul_efun_sees_the_function_that_called_it() {
    let task = run_prog(r#"string seen; void create() { seen = simul_calling(); }"#).await;
    assert_eq!(committed_global(&task, "seen"), LpcRef::from("create"));
}

/// A `call_out` is fired by the driver: the owner is the previous object,
/// the function unknown.
#[tokio::test]
async fn a_call_out_has_no_calling_function() {
    let vm = Vm::new(test_config());
    let y = vm
        .initialize_process_from_code(
            "/y.c",
            r#"mixed seen = "unset"; void note() { seen = calling_function(); }"#,
        )
        .await
        .unwrap()
        .context
        .process;
    vm.initialize_process_from_code(
        "/w.c",
        r#"void create() { call_out(papplyv(&->note(), ({ "/y" })), 100); }"#,
    )
    .await
    .unwrap();
    let gs = vm.global_state.clone();
    let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
    gs.prioritize_call_out(id).await.await.unwrap();
    assert_eq!(gs.committed_global(&y, 0), LpcRef::from(0));
}
