//! `previous_object`: the object that called the current function through
//! a door — `->`, a function pointer, a simul efun, or a task entry — with
//! local calls transparent, and the chain behind it.

use std::sync::Arc;

use indoc::indoc;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run, s};
use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef},
    test_config_builder,
    test_support::run_prog_with_config,
};

/// `/x.c`: answers who called it, directly and through a local call.
const X: (&str, &str) = (
    "/x.c",
    indoc! { r#"
        string who() { return file_name(previous_object()); }
        string inner() { return file_name(previous_object()); }
        string via_local() { return inner(); }
    "# },
);

#[tokio::test]
async fn a_call_other_callee_sees_its_caller() {
    let r = run("", &[X], r#"mixed *create() { return ({ "/x"->who() }); }"#).await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn a_local_call_keeps_the_callers_previous_object() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x"->via_local() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn a_call_other_to_this_object_is_its_own_previous_object() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            string me() { return file_name(previous_object()); }
            mixed *create() { return ({ this_object()->me() }); }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn each_collection_element_sees_the_caller() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x", "/x" })->who(); }"#,
    )
    .await;
    assert_eq!(r, vec![s("/main"), s("/main")]);
}

#[tokio::test]
async fn the_caller_is_unchanged_after_its_own_call_other() {
    let r = run(
        "",
        &[
            X,
            (
                "/a.c",
                r#"string ask() { "/x"->who(); return file_name(previous_object()); }"#,
            ),
        ],
        r#"mixed *create() { return ({ "/a"->ask() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn a_closure_call_sees_the_firer() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            mixed *create() {
                function f = (: file_name(previous_object()) :);
                return ({ f() });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn a_pointer_to_another_objects_function_sees_the_firer() {
    let r = run(
        "",
        &[X],
        indoc! { r#"
            mixed *create() {
                function f = papplyv(&->who(), ({ "/x" }));
                return ({ f() });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

/// `run_prog` seats the simul efuns; naming the file with its `.c` is what
/// makes the compiler emit the simul-efun call rather than a local one.
#[tokio::test]
async fn a_simul_efun_sees_its_caller() {
    let config = test_config_builder!()
        .simul_efun_file("/secure/simul_efuns.c")
        .build()
        .unwrap();
    let task = run_prog_with_config(
        r#"string seen; void create() { seen = file_name(simul_previous()); }"#,
        Arc::new(config),
    )
    .await;
    let process = task.context.process.clone();
    assert_eq!(
        task.context.global_state.committed_global(&process, 0u16),
        s("/my_file")
    );
}

/// `/x.c`: remembers who was there when it was created.
const MADE: (&str, &str) = (
    "/x.c",
    indoc! { r#"
        string maker;
        void create() { maker = previous_object() ? file_name(previous_object()) : "none"; }
        string who_made() { return maker; }
    "# },
);

#[tokio::test]
async fn create_sees_the_cloner() {
    let r = run(
        "",
        &[MADE],
        r#"mixed *create() { return ({ clone_object("/x")->who_made() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("/main")]);
}

#[tokio::test]
async fn the_driver_entry_has_no_previous_object() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { return ({ previous_object() }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0)]);
}
