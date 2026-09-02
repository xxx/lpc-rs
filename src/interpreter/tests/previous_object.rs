//! `previous_object`: the object that called the current function through
//! a door — `->`, a function pointer, a simul efun, or a task entry — with
//! local calls transparent, and the chain behind it.

use std::sync::Arc;

use indoc::indoc;
use lpc_rs_utils::config::ConfigBuilder;

use super::{fails, loading::write, run, run_with, s};
use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
    test_config_builder,
    test_support::{
        PERMISSIVE_MASTER, TempLib, committed_string, run_prog_with_config, temp_lib_config,
        test_config,
    },
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
async fn each_mapping_value_sees_the_caller() {
    let r = run(
        "",
        &[X],
        indoc! { r#"
            mixed *create() {
                mapping m = ([ "a": "/x", "b": "/x" ])->who();
                return ({ m["a"], m["b"] });
            }
        "# },
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

/// An efun pointer runs as its owner: the firer is its previous object,
/// then the firer's chain.
#[tokio::test]
async fn an_efun_pointer_fired_elsewhere_sees_the_firer() {
    let r = run(
        "",
        &[(
            "/b.c",
            r#"mixed *fire(function a, function b) { return ({ file_name(a()), file_name(b()) }); }"#,
        )],
        r#"mixed *create() { return "/b"->fire(&previous_object(), &previous_object(1)); }"#,
    )
    .await;
    assert_eq!(r, vec![s("/b"), s("/main")]);
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

/// Records the whole chain as `create()` saw it, file names joined by spaces.
const CHAIN_ON_CREATE: &str = indoc! { r#"
    string seen = "";
    void create() {
        object *all = previous_object(-1);
        for (int i = 0; i < sizeof(all); i++) seen += file_name(all[i]) + " ";
    }
    string seen() { return seen; }
"# };

/// Records the whole chain as `init()` saw it, file names joined by spaces.
const CHAIN_ON_INIT: &str = indoc! { r#"
    string seen = "";
    void init() {
        object *all = previous_object(-1);
        for (int i = 0; i < sizeof(all); i++) seen += file_name(all[i]) + " ";
    }
    string seen() { return seen; }
"# };

/// What a task started by another object's code is entered with.
mod entries {
    use super::*;

    #[tokio::test]
    async fn create_through_a_call_other_miss_sees_the_caller() {
        let root = TempLib::new("previous-object-miss");
        write(&root, "x.c", MADE.1);
        let r = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"mixed *create() { return ({ "/x"->who_made() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/main")]);
    }

    #[tokio::test]
    async fn create_through_find_object_sees_the_finder() {
        let root = TempLib::new("previous-object-find");
        write(&root, "x.c", MADE.1);
        let r = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"mixed *create() { return ({ find_object("/x")->who_made() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/main")]);
    }

    /// The chain is the executing frame's, not the task entry's.
    #[tokio::test]
    async fn a_nested_task_sees_the_call_other_callee_not_the_entry() {
        let r = run(
            "",
            &[
                ("/y.c", MADE.1),
                (
                    "/x.c",
                    r#"string make() { return clone_object("/y")->who_made(); }"#,
                ),
            ],
            r#"mixed *create() { return ({ "/x"->make() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/x")]);
    }

    #[tokio::test]
    async fn a_master_apply_sees_the_asking_object() {
        let root = TempLib::new("previous-object-master");
        write(&root, "x.c", "int x;\n");
        let master = indoc! { r#"
            string asker;
            int valid_load(string p, string f, object c, string g) {
                asker = file_name(previous_object());
                return 1;
            }
            string who_asked() { return asker; }
        "# };
        let r = run_with(
            temp_lib_config(&root),
            master,
            &[],
            r#"mixed *create() { clone_object("/x"); return ({ "/secure/master"->who_asked() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/main")]);
    }

    #[tokio::test]
    async fn compile_object_and_the_instance_see_the_requester() {
        let master = indoc! { r#"
            string asker;
            string compile_object(string path, string func, object caller, string program) {
                asker = file_name(previous_object());
                return "/x";
            }
            string who_asked() { return asker; }
        "# };
        let r = run(
            master,
            &[MADE],
            indoc! { r#"
                mixed *create() {
                    string made = find_object("/inst/1/x")->who_made();
                    return ({ made, "/secure/master"->who_asked() });
                }
            "# },
        )
        .await;
        assert_eq!(r, vec![s("/main"), s("/main")]);
    }

    const LISTENER: (&str, &str) = (
        "/p.c",
        indoc! { r#"
            string from;
            void catch_tell(string m) { from = file_name(previous_object()); }
            string who() { return from; }
        "# },
    );

    #[tokio::test]
    async fn catch_tell_sees_the_teller() {
        let r = run(
            "",
            &[LISTENER],
            indoc! { r#"
                mixed *create() {
                    object p = clone_object("/p");
                    tell_object(p, "hi");
                    string told = p->who();
                    set_this_player(p);
                    write("hi");
                    return ({ told, p->who() });
                }
            "# },
        )
        .await;
        assert_eq!(r, vec![s("/main"), s("/main")]);
    }

    /// The receiver is another object, so the owner is told apart from it.
    #[tokio::test]
    async fn a_call_out_sees_its_scheduler() {
        let vm = Vm::new(test_config());
        let y = vm
            .initialize_process_from_code(
                "/y.c",
                r#"string seen; void note() { seen = file_name(previous_object()); }"#,
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
        assert_eq!(committed_string(&vm, &y, 0), "/w");
    }

    /// The handler lives in the room, so the actor is told apart from it.
    #[tokio::test]
    async fn a_command_handler_sees_the_actor() {
        let r = run(
            "",
            &[(
                "/room.c",
                indoc! { r#"
                    string seen;
                    void init() { add_action("do_look", "look"); }
                    int do_look(string a) { seen = file_name(previous_object()); return 1; }
                    string who() { return seen; }
                "# },
            )],
            indoc! { r#"
                mixed *create() {
                    set_this_player(this_object());
                    enable_commands();
                    move_object("/room");
                    command("look");
                    return ({ "/room"->who() });
                }
            "# },
        )
        .await;
        assert_eq!(r, vec![s("/main")]);
    }

    /// `init` is entered for the living, in front of the chain where
    /// `move_object` was called.
    #[tokio::test]
    async fn init_sees_the_living_then_the_mover_chain() {
        let r = run(
            "",
            &[
                ("/room.c", CHAIN_ON_INIT),
                (
                    "/a.c",
                    r#"string go() { enable_commands(); move_object("/room"); return "/room"->seen(); }"#,
                ),
            ],
            r#"mixed *create() { return ({ "/a"->go() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/a /a /main ")]);
    }

    /// An object a pointer call creates is entered for the pointer's owner,
    /// then the firer — the owner twice when it fired its own pointer.
    #[tokio::test]
    async fn a_pointer_receiver_miss_sees_the_owner_then_the_firer() {
        let root = TempLib::new("previous-object-pointer-miss");
        write(&root, "x.c", CHAIN_ON_CREATE);
        let r = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            indoc! { r#"
                mixed *create() {
                    function f = papplyv(&->seen(), ({ "/x" }));
                    return ({ f() });
                }
            "# },
        )
        .await;
        assert_eq!(r, vec![s("/main /main ")]);
    }

    #[tokio::test]
    async fn a_destructed_previous_object_is_zero() {
        let r = run(
            "",
            &[
                ("/a.c", r#"mixed *go() { return "/x"->kill_and_ask(); }"#),
                (
                    "/x.c",
                    indoc! { r#"
                        mixed *kill_and_ask() {
                            destruct(previous_object());
                            object *all = previous_object(-1);
                            return ({ previous_object(), sizeof(all), all[0], file_name(all[1]) });
                        }
                    "# },
                ),
            ],
            r#"mixed *create() { object a = clone_object("/a"); return a->go(); }"#,
        )
        .await;
        assert_eq!(
            r,
            vec![
                LpcRef::from(0),
                LpcRef::from(2),
                LpcRef::from(0),
                s("/main")
            ]
        );
    }
}

/// `previous_object(n)`: `n` steps back, `-1` the whole chain.
mod steps {
    use super::*;

    const A: (&str, &str) = ("/a.c", r#"mixed *chain() { return "/b"->chain(); }"#);
    const B: (&str, &str) = ("/b.c", r#"mixed *chain() { return "/c"->chain(); }"#);

    #[tokio::test]
    async fn a_step_counts_back_through_the_chain() {
        let c = (
            "/c.c",
            indoc! { r#"
                mixed *chain() {
                    return ({
                        file_name(previous_object(0)),
                        file_name(previous_object(1)),
                        file_name(previous_object(2)),
                        previous_object(3),
                    });
                }
            "# },
        );
        let r = run(
            "",
            &[A, B, c],
            r#"mixed *create() { return "/a"->chain(); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/b"), s("/a"), s("/main"), LpcRef::from(0)]);
    }

    #[tokio::test]
    async fn minus_one_is_the_whole_chain() {
        let c = (
            "/c.c",
            indoc! { r#"
                mixed *chain() {
                    object *all = previous_object(-1);
                    return ({ sizeof(all), file_name(all[0]), file_name(all[1]), file_name(all[2]) });
                }
            "# },
        );
        let r = run(
            "",
            &[A, B, c],
            r#"mixed *create() { return "/a"->chain(); }"#,
        )
        .await;
        assert_eq!(r, vec![LpcRef::from(3), s("/b"), s("/a"), s("/main")]);
    }

    #[tokio::test]
    async fn the_chain_crosses_a_task_entry() {
        let r = run(
            "",
            &[
                ("/y.c", CHAIN_ON_CREATE),
                (
                    "/x.c",
                    r#"string make() { return clone_object("/y")->seen(); }"#,
                ),
            ],
            r#"mixed *create() { return ({ "/x"->make() }); }"#,
        )
        .await;
        assert_eq!(r, vec![s("/x /main ")]);
    }

    #[tokio::test]
    async fn the_drivers_chain_is_empty() {
        let r = run(
            "",
            &[],
            r#"mixed *create() { return ({ sizeof(previous_object(-1)), previous_object(1) }); }"#,
        )
        .await;
        assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0)]);
    }

    #[tokio::test]
    async fn another_negative_step_is_an_error() {
        let err = fails(
            "",
            &[],
            r#"mixed *create() { previous_object(-2); return ({}); }"#,
        )
        .await;
        assert!(
            err.contains("previous_object: expected a step back or -1, got -2"),
            "{err}"
        );
    }
}
