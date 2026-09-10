//! Call-stack inspection includes local calls and survives nested task entries.

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
async fn a_local_call_sees_its_immediate_calling_function() {
    let r = run(
        "",
        &[X],
        r#"mixed *create() { return ({ "/x"->via_local() }); }"#,
    )
    .await;
    assert_eq!(r, vec![s("via_local")]);
}

#[tokio::test]
async fn a_local_call_can_name_this_object_while_previous_object_names_another() {
    let r = run(
        "",
        &[(
            "/x.c",
            indoc! { r#"
                mixed *inner() {
                    return ({
                        calling_object() == this_object(),
                        file_name(previous_object()),
                        calling_function(),
                        calling_program(),
                        file_name(calling_object(1)),
                        calling_function(1),
                        calling_program(1),
                        calling_object(2),
                    });
                }
                mixed *outer() { return inner(); }
            "# },
        )],
        r#"mixed *create() { return "/x"->outer(); }"#,
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("/main"),
            s("outer"),
            s("/x.c"),
            s("/main"),
            s("create"),
            s("/main.c"),
            LpcRef::from(0),
        ]
    );
}

#[tokio::test]
async fn recursive_local_calls_occupy_separate_stack_positions() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            mixed *recurse(int depth) {
                if (depth) { return recurse(depth - 1); }
                object *objects = calling_object(-1);
                return ({
                    implode(calling_function(-1), " "),
                    implode(calling_program(-1), " "),
                    sizeof(objects),
                    objects[0] == this_object() && objects[1] == this_object()
                        && objects[2] == this_object(),
                    previous_object(),
                });
            }
            mixed *create() { return recurse(2); }
        "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("recurse recurse create"),
            s("/main.c /main.c /main.c"),
            LpcRef::from(3),
            LpcRef::from(1),
            LpcRef::from(0),
        ]
    );
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
async fn inherited_local_calls_keep_the_object_and_use_the_defining_program() {
    let r = run(
        "",
        &[(
            "/x.c",
            indoc! { r#"
                inherit "/calling_base";
                mixed *inspect() {
                    return ({ calling_object() == this_object(), calling_function(),
                        calling_program(), calling_function(1), calling_program(1) });
                }
                mixed *go() { return inspect_local(); }
            "# },
        )],
        r#"mixed *create() { return "/x"->go(); }"#,
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("inspect_local"),
            s("/calling_base.c"),
            s("go"),
            s("/x.c")
        ]
    );
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
async fn nested_tasks_retain_local_frames_on_both_sides_of_the_boundary() {
    let r = run(
        "",
        &[(
            "/y.c",
            indoc! { r#"
                mixed *seen;
                void inspect() {
                    object *objects = calling_object(-1);
                    seen = ({ implode(calling_function(-1), " "),
                        implode(calling_program(-1), " "), sizeof(objects),
                        objects[0] == this_object(), file_name(objects[1]),
                        file_name(objects[2]), file_name(previous_object()),
                        sizeof(previous_object(-1)) });
                }
                void create() { if (previous_object()) { inspect(); } }
                mixed *seen() { return seen; }
            "# },
        )],
        indoc! { r#"
            mixed *make() { return clone_object("/y")->seen(); }
            mixed *create() { return make(); }
        "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("create make create"),
            s("/y.c /main.c /main.c"),
            LpcRef::from(3),
            LpcRef::from(1),
            s("/main"),
            s("/main"),
            s("/main"),
            LpcRef::from(1),
        ]
    );
}

#[tokio::test]
async fn the_driver_entry_has_no_calling_function() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { return ({ calling_object(), calling_function(), calling_program(), sizeof(calling_object(-1)) }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0); 4]);
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
async fn calling_object_rejects_an_invalid_negative_step() {
    let err = fails(
        "",
        &[],
        r#"mixed *create() { return ({ calling_object(-2) }); }"#,
    )
    .await;
    assert!(
        err.contains("calling_object: expected a step back or -1, got -2"),
        "{err}"
    );
}

#[tokio::test]
async fn efun_pointers_inspect_the_frame_that_fired_them() {
    let r = run(
        "",
        &[("/x.c", r#"mixed *fire(function ob, function name, function program) { return ({ file_name(ob()), name(), program() }); }"#)],
        r#"mixed *create() { return "/x"->fire(&calling_object(), &calling_function(), &calling_program()); }"#,
    ).await;
    assert_eq!(r, vec![s("/x"), s("fire"), s("/x.c")]);
}

#[tokio::test]
async fn destructed_callers_keep_their_stack_position_and_function() {
    let r = run(
        "",
        &[
            ("/a.c", r#"mixed *go() { return "/x"->inspect(); }"#),
            (
                "/x.c",
                indoc! { r#"
                mixed *inspect() {
                    destruct(calling_object());
                    object *objects = calling_object(-1);
                    return ({ calling_object(), sizeof(objects), objects[0],
                        file_name(objects[1]), calling_function(), calling_program() });
                }
            "# },
            ),
        ],
        r#"mixed *create() { return clone_object("/a")->go(); }"#,
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(0),
            LpcRef::from(2),
            LpcRef::from(0),
            s("/main"),
            s("go"),
            s("/a.c")
        ]
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

/// The driver fires an efun pointer through an entry frame: a callback it
/// runs has no calling function, though `previous_object` names the owner.
#[tokio::test]
async fn a_callback_of_a_fired_efun_pointer_has_no_calling_function() {
    let r = run(
        "",
        &[],
        indoc! { r#"
            mixed *create() {
                function f = &map();
                mixed *names = f(({ 1 }), (: calling_function() :));
                mixed *files = f(({ 1 }), (: calling_program() :));
                mixed *objects = f(({ 1 }), (: previous_object() :));
                return ({ names[0], files[0], file_name(objects[0]) });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0), s("/main")]);
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

/// Inside the whole chain a driver-fired link is a 0 in its place.
#[tokio::test]
async fn a_driver_fired_link_is_zero_inside_the_whole_chain() {
    let vm = Vm::new(test_config());
    vm.initialize_process_from_code(
        "/x.c",
        indoc! { r#"
            string all() {
                mixed *a = calling_function(-1);
                return sprintf("%d:%s:%d", sizeof(a), a[0], a[1] == 0);
            }
        "# },
    )
    .await
    .unwrap();
    let y = vm
        .initialize_process_from_code(
            "/y.c",
            r#"mixed seen = "unset"; void note() { seen = "/x"->all(); }"#,
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
    assert_eq!(gs.committed_global(&y, 0), LpcRef::from("2:note:1"));
}
