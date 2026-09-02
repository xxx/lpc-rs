//! What the master hears before a file is compiled into an object, and what
//! happens when it says no.

use std::sync::Arc;

use indoc::indoc;

use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef, process::Process, vm::Vm},
    test_support::{TempLib, committed_string, temp_lib_config},
};

/// Records every loading question and allows it. Globals, by register:
/// 0 `seen_path`, 1 `seen_func`, 2 `seen_caller`, 3 `seen_program`,
/// 4 `inherit_path`, 5 `inherit_from`, 6 `include_path`, 7 `include_caller`,
/// 8 `include_from`, 9 `loads`, 10 `inherits`, 11 `includes`.
pub(crate) const RECORDING_MASTER: &str = indoc! { r#"
    string seen_path; string seen_func; string seen_caller; mixed seen_program;
    string inherit_path; string inherit_from;
    string include_path; mixed include_caller; string include_from;
    int loads; int inherits; int includes;
    int valid_load(string path, string func, object caller, mixed program) {
        seen_path = path; seen_func = func; seen_caller = file_name(caller);
        seen_program = program; loads++;
        return 1;
    }
    int valid_inherit(string path, string from) {
        inherit_path = path; inherit_from = from; inherits++;
        return 1;
    }
    int valid_read(string path, string func, object caller, mixed program) {
        include_path = path; include_caller = caller; include_from = program; includes++;
        return 1;
    }
"# };

pub(crate) const SEEN_PATH: u16 = 0;
pub(crate) const SEEN_FUNC: u16 = 1;
pub(crate) const SEEN_CALLER: u16 = 2;
pub(crate) const SEEN_PROGRAM: u16 = 3;
pub(crate) const INHERIT_PATH: u16 = 4;
pub(crate) const INHERIT_FROM: u16 = 5;
pub(crate) const INCLUDE_PATH: u16 = 6;
pub(crate) const INCLUDE_CALLER: u16 = 7;
pub(crate) const INCLUDE_FROM: u16 = 8;
pub(crate) const LOADS: u16 = 9;
pub(crate) const INHERITS: u16 = 10;
pub(crate) const INCLUDES: u16 = 11;

/// `code` at `root/rel`, directories made as needed.
pub(crate) fn write(root: &TempLib, rel: &str, code: &str) {
    let path = root.join(rel);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, code).unwrap();
}

/// A lib at `root` with `/x.c` (`int x = 1; void f() {}`); the recording master.
pub(crate) async fn recording_master(vm: &Vm, root: &TempLib) -> Arc<Process> {
    write(root, "x.c", "int x = 1;\nvoid f() {}\n");
    vm.initialize_process_from_code("/secure/master.c", RECORDING_MASTER)
        .await
        .unwrap()
        .context
        .process
}

/// Load `code` as `path`; its `create()` performs the load under test.
pub(crate) async fn run(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
        .context
        .process
}

/// `master`'s global `reg` as an int.
pub(crate) fn count(vm: &Vm, master: &Arc<Process>, reg: u16) -> i64 {
    match vm.global_state.committed_global(master, reg) {
        LpcRef::Int(i) => i.0,
        other => panic!("an int in register {reg}: {other:?}"),
    }
}

/// `/a.c` whose `create()` clones `/x`, catching the error into global 0.
const CLONER: &str = r#"mixed err; void create() { err = catch(clone_object("/x")); }"#;

/// What `CLONER` caught: `0` on success, the error string otherwise.
async fn cloner_caught(vm: &Vm) -> LpcRef {
    let a = run(vm, "/a.c", CLONER).await;
    vm.global_state.committed_global(&a, 0u16)
}

fn message(caught: &LpcRef) -> String {
    caught
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| panic!("an error string, got {caught:?}"))
}

fn lib_with_x(name: &str) -> TempLib {
    let root = TempLib::new(name);
    write(&root, "x.c", "int x = 1;\nvoid f() {}\n");
    root
}

mod gate_states {
    use super::*;

    #[tokio::test]
    async fn no_master_refuses_a_load() {
        let root = lib_with_x("load-no-master");
        let vm = Vm::new(temp_lib_config(&root));
        let err = message(&cloner_caught(&vm).await);
        assert!(err.contains("clone_object: permission denied"), "{err}");
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    #[tokio::test]
    async fn a_master_without_valid_load_refuses() {
        let root = lib_with_x("load-no-apply");
        let vm = Vm::new(temp_lib_config(&root));
        run(&vm, "/secure/master.c", "int unrelated() { return 1; }").await;
        let err = message(&cloner_caught(&vm).await);
        assert!(err.contains("clone_object: permission denied"), "{err}");
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    #[tokio::test]
    async fn an_error_in_valid_load_is_the_callers() {
        let root = lib_with_x("load-throws");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { throw("not today"); }"#,
        )
        .await;
        let err = message(&cloner_caught(&vm).await);
        assert!(err.contains("not today"), "{err}");
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    #[tokio::test]
    async fn a_refusing_master_leaves_the_object_unloaded() {
        let root = lib_with_x("load-deny");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { return 0; }"#,
        )
        .await;
        let err = message(&cloner_caught(&vm).await);
        assert!(err.contains("clone_object: permission denied"), "{err}");
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    #[tokio::test]
    async fn an_allowing_master_loads() {
        let root = lib_with_x("load-allow");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { return 1; }"#,
        )
        .await;
        assert_eq!(cloner_caught(&vm).await, LpcRef::from(0));
        assert!(vm.global_state.object_space.lookup("/x").is_some());
    }
}

mod doors {
    use super::*;

    #[tokio::test]
    async fn clone_object_reports_itself_the_caller_and_its_file() {
        let root = TempLib::new("door-clone");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(&vm, "/a.c", r#"void create() { clone_object("/x"); }"#).await;
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/x.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "clone_object");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/a");
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
        assert_eq!(count(&vm, &master, LOADS), 1);
    }

    #[tokio::test]
    async fn a_string_receiver_reports_call_other() {
        let root = TempLib::new("door-arrow");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(&vm, "/a.c", r#"void create() { "/x"->f(); }"#).await;
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/x.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "call_other");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/a");
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
    }

    #[tokio::test]
    async fn a_collection_of_receivers_asks_once_each() {
        let root = TempLib::new("door-collection");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "y.c", "void f() {}\n");
        run(&vm, "/a.c", r#"void create() { ({ "/x", "/y" })->f(); }"#).await;
        assert_eq!(count(&vm, &master, LOADS), 2);
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "call_other");
    }

    #[tokio::test]
    async fn move_object_and_tell_object_report_their_names() {
        let root = TempLib::new("door-move-tell");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "room.c", "int r;\n");
        run(&vm, "/a.c", r#"void create() { move_object("/room"); }"#).await;
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/room.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "move_object");
        write(&root, "ear.c", "void catch_tell(string s) {}\n");
        run(
            &vm,
            "/b.c",
            r#"void create() { tell_object("/ear", "hi"); }"#,
        )
        .await;
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/ear.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "tell_object");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/b");
    }

    #[tokio::test]
    async fn find_objects_denied_load_is_zero() {
        let root = lib_with_x("door-find");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { return f != "find_object"; }"#,
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"mixed got; void create() { got = find_object("/x"); }"#,
        )
        .await;
        assert_eq!(vm.global_state.committed_global(&a, 0u16), LpcRef::from(0));
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    /// An error out of `valid_load` is a failed load, which `find_object`
    /// reports as `0`.
    #[tokio::test]
    async fn find_objects_throwing_valid_load_is_zero() {
        let root = lib_with_x("door-find-throw");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { throw("not today"); }"#,
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"mixed got; void create() { got = find_object("/x"); }"#,
        )
        .await;
        assert_eq!(vm.global_state.committed_global(&a, 0u16), LpcRef::from(0));
        assert!(vm.global_state.object_space.lookup("/x").is_none());
    }

    /// A pointer holding `/x` as its receiver, fired inside a task, loads
    /// for the code that wrote it.
    #[tokio::test]
    async fn a_pointer_fired_in_a_task_loads_for_its_writer() {
        let root = TempLib::new("door-pointer-task");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(
            &vm,
            "/w.c",
            r#"function make() { return papplyv(&->f(), ({ "/x" })); }"#,
        )
        .await;
        run(
            &vm,
            "/u.c",
            indoc! { r#"
                void create() { function p = find_object("/w")->make(); p(); }
            "# },
        )
        .await;
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/x.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "call_other");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/w");
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/w.c");
    }

    /// `call_out(papplyv(&->f(), ({ "/x" })), ...)` resolves its receiver before any task runs;
    /// the load still answers to the pointer's writer.
    #[tokio::test]
    async fn a_pointer_fired_from_call_out_loads_for_its_writer() {
        let root = TempLib::new("door-pointer-call-out");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(
            &vm,
            "/w.c",
            r#"void create() { call_out(papplyv(&->f(), ({ "/x" })), 100); }"#,
        )
        .await;
        let gs = vm.global_state.clone();
        let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
        gs.prioritize_call_out(id).await.await.unwrap();
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/x.c");
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "call_other");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/w");
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/w.c");
        assert!(vm.global_state.object_space.lookup("/x").is_some());
    }

    /// The object the resolve committed is the one the firing runs in.
    #[tokio::test]
    async fn a_call_out_receiver_is_loaded_once_and_run_in() {
        let root = TempLib::new("door-pointer-call-out-once");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "x.c", "int x = 1;\nvoid f() { x = 2; }\n");
        run(
            &vm,
            "/w.c",
            r#"void create() { call_out(papplyv(&->f(), ({ "/x" })), 100); }"#,
        )
        .await;
        let gs = vm.global_state.clone();
        let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
        gs.prioritize_call_out(id).await.await.unwrap();
        assert_eq!(count(&vm, &master, LOADS), 1);
        let x = vm.global_state.object_space.lookup("/x").unwrap();
        assert_eq!(vm.global_state.committed_global(&x, 0u16), LpcRef::from(2));
    }

    #[tokio::test]
    async fn a_refused_call_out_receiver_stays_unloaded() {
        let root = lib_with_x("door-pointer-call-out-deny");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, string g) { return 0; }"#,
        )
        .await;
        run(
            &vm,
            "/w.c",
            r#"void create() { call_out(papplyv(&->f(), ({ "/x" })), 100); }"#,
        )
        .await;
        let gs = vm.global_state.clone();
        let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
        let outcome = gs.prioritize_call_out(id).await.await;
        assert!(vm.global_state.object_space.lookup("/x").is_none());
        // How the firing reports a refusal is the call-out path's business.
        let _ = outcome;
    }

    #[tokio::test]
    async fn a_resident_prototype_is_cloned_without_asking() {
        let root = TempLib::new("door-resident");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(&vm, "/x.c", "int x = 1;\n").await;
        run(&vm, "/a.c", r#"void create() { clone_object("/x"); }"#).await;
        assert_eq!(count(&vm, &master, LOADS), 0);
    }
}

mod paths {
    use super::*;

    /// The `/x.c` case asks nothing: it shares the resident object's key.
    #[tokio::test]
    async fn the_master_hears_the_dot_c_source() {
        let root = TempLib::new("path-source");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "x.h.c", "int hc;\n");
        for (i, (arg, expected, loads)) in [
            ("/x", "/x.c", 1),
            ("/x.c", "/x.c", 1),
            ("/x.h", "/x.h.c", 2),
        ]
        .into_iter()
        .enumerate()
        {
            let code = format!(r#"void create() {{ find_object("{arg}"); }}"#);
            run(&vm, &format!("/probe{i}.c"), &code).await;
            assert_eq!(committed_string(&vm, &master, SEEN_PATH), expected, "{arg}");
            assert_eq!(count(&vm, &master, LOADS), loads, "{arg}");
        }
    }

    #[tokio::test]
    async fn a_bare_receiver_is_an_absolute_object_name() {
        let root = TempLib::new("path-bare");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "d/a.c", r#"void create() { "x"->f(); }"#);
        vm.initialize_process_from_path(&lpc_rs_core::lpc_path::LpcPath::new_in_game(
            "/d/a",
            "/",
            vm.global_state.config.lib_dir.as_str(),
        ))
        .await
        .unwrap();
        assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/x.c");
    }

    /// Leaving the lib is a mechanical error, not a policy question.
    #[tokio::test]
    async fn an_escape_errors_at_both_doors_without_consulting_the_master() {
        let root = TempLib::new("path-escape");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        let a = run(
            &vm,
            "/a.c",
            indoc! { r#"
                string e1; string e2;
                void create() {
                    e1 = catch(clone_object("/../../../../../../../../etc/passwd"));
                    e2 = catch("/../../../../../../../../etc/passwd"->f());
                }
            "# },
        )
        .await;
        let lib_dir = vm.global_state.config.lib_dir.as_str();
        let e1 = committed_string(&vm, &a, 0);
        assert!(
            e1.contains("clone_object: `/../../../../../../../../etc/passwd` is not a valid path"),
            "{e1}"
        );
        assert!(!e1.contains(lib_dir), "server path leaked: {e1}");
        let e2 = committed_string(&vm, &a, 1);
        assert!(
            e2.contains("call_other: `/../../../../../../../../etc/passwd` is not a valid path"),
            "{e2}"
        );
        assert!(!e2.contains(lib_dir), "server path leaked: {e2}");
        assert_eq!(count(&vm, &master, LOADS), 0, "the master was never asked");
    }

    #[tokio::test]
    async fn a_missing_object_is_named_in_game() {
        let root = TempLib::new("path-missing");
        let vm = Vm::new(temp_lib_config(&root));
        recording_master(&vm, &root).await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(clone_object("/missing")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("Cannot read file `/missing.c`"), "{e}");
        assert!(!e.contains(vm.global_state.config.lib_dir.as_str()), "{e}");
    }
}

mod provenance {
    use super::*;

    #[tokio::test]
    async fn an_inherited_function_loads_under_the_parents_file() {
        let root = TempLib::new("prov-inherit");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(
            &root,
            "parent.c",
            r#"void parent_load() { clone_object("/x"); }"#,
        );
        run(
            &vm,
            "/child.c",
            "inherit \"/parent\";\nvoid create() { parent_load(); }\n",
        )
        .await;
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/parent.c");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/child");
    }

    #[tokio::test]
    async fn a_closure_loads_under_the_file_it_was_written_in() {
        let root = TempLib::new("prov-closure");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(
            &vm,
            "/a.c",
            r#"function make() { return (: clone_object($1) :); }"#,
        )
        .await;
        run(
            &vm,
            "/b.c",
            r#"void create() { function f = find_object("/a")->make(); f("/x"); }"#,
        )
        .await;
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
    }

    /// An efun pointer fired by `call_out` has no LPC frame under it.
    #[tokio::test]
    async fn an_efun_pointer_from_call_out_reports_zero() {
        let root = TempLib::new("prov-call-out-efun");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        run(
            &vm,
            "/timer.c",
            r#"void create() { call_out(&clone_object("/x"), 100); }"#,
        )
        .await;
        let gs = vm.global_state.clone();
        let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
        gs.prioritize_call_out(id).await.await.unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, SEEN_PROGRAM),
            LpcRef::from(0)
        );
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/timer");
    }
}

mod compile_time {
    use super::*;

    /// The `inherit` line sits in a header; the program being loaded is `from`.
    #[tokio::test]
    async fn an_inherit_is_asked_for_the_program_being_compiled() {
        let root = TempLib::new("ct-inherit");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "parent.c", "int p;\n");
        write(&root, "hdr.h", "inherit \"/parent\";\n");
        write(&root, "child.c", "#include \"/hdr.h\"\nint c;\n");
        run(&vm, "/u.c", r#"void create() { clone_object("/child"); }"#).await;
        assert_eq!(count(&vm, &master, INHERITS), 1);
        assert_eq!(committed_string(&vm, &master, INHERIT_PATH), "/parent.c");
        assert_eq!(committed_string(&vm, &master, INHERIT_FROM), "/child.c");
    }

    #[tokio::test]
    async fn an_include_is_a_read_by_the_includer_with_no_caller() {
        let root = TempLib::new("ct-include");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "a.h", "#include \"b.h\"\n");
        write(&root, "b.h", "#define B 1\n");
        write(&root, "y.c", "#include \"/a.h\"\nint y = B;\n");
        run(&vm, "/u.c", r#"void create() { clone_object("/y"); }"#).await;
        assert_eq!(count(&vm, &master, INCLUDES), 2);
        // The last question was b.h's, asked by a.h.
        assert_eq!(committed_string(&vm, &master, INCLUDE_PATH), "/b.h");
        assert_eq!(committed_string(&vm, &master, INCLUDE_FROM), "/a.h");
        assert_eq!(
            vm.global_state.committed_global(&master, INCLUDE_CALLER),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn a_denied_inherit_fails_the_load_at_the_directive() {
        let root = TempLib::new("ct-inherit-deny");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            indoc! { r#"
                int valid_load(string p, string f, object c, string g) { return 1; }
                int valid_inherit(string path, string from) { return 0; }
            "# },
        )
        .await;
        write(&root, "parent.c", "int p;\n");
        write(&root, "child.c", "inherit \"/parent\";\n");
        let u = run(
            &vm,
            "/u.c",
            r#"string e; void create() { e = catch(clone_object("/child")); }"#,
        )
        .await;
        let e = committed_string(&vm, &u, 0);
        assert!(
            e.contains("inherit \"/parent.c\": permission denied"),
            "{e}"
        );
        assert!(vm.global_state.object_space.lookup("/child").is_none());
    }

    #[tokio::test]
    async fn a_denied_include_fails_the_load_at_the_directive() {
        let root = TempLib::new("ct-include-deny");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            indoc! { r#"
                int valid_load(string p, string f, object c, string g) { return 1; }
                int valid_read(string path, string func, object caller, string program) {
                    return func != "include";
                }
            "# },
        )
        .await;
        write(&root, "secret.h", "#define S 1\n");
        write(&root, "child.c", "#include \"/secret.h\"\nint c = S;\n");
        let u = run(
            &vm,
            "/u.c",
            r#"string e; void create() { e = catch(clone_object("/child")); }"#,
        )
        .await;
        let e = committed_string(&vm, &u, 0);
        assert!(
            e.contains("#include \"/secret.h\": permission denied"),
            "{e}"
        );
        assert!(vm.global_state.object_space.lookup("/child").is_none());
    }

    /// The same program compiles ungated through the driver's own loader.
    #[tokio::test]
    async fn a_driver_compile_asks_nothing() {
        let root = TempLib::new("ct-driver");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "parent.c", "int p;\n");
        write(&root, "h.h", "#define H 1\n");
        run(
            &vm,
            "/child.c",
            "inherit \"/parent\";\n#include \"/h.h\"\nint c = H;\n",
        )
        .await;
        assert_eq!(count(&vm, &master, INHERITS), 0);
        assert_eq!(count(&vm, &master, INCLUDES), 0);
        assert_eq!(count(&vm, &master, LOADS), 0);
    }
}

mod boot {
    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;

    /// The master compiles before any master exists — inherits and includes
    /// and all — and after boot, everything it did not allow is refused.
    #[tokio::test]
    async fn a_master_that_inherits_and_includes_boots_ungated() {
        let root = lib_with_x("boot-master");
        write(&root, "secure/base.c", "int base;\n");
        write(&root, "secure/defs.h", "#define D 1\n");
        write(
            &root,
            "secure/master.c",
            "inherit \"/secure/base\";\n#include \"/secure/defs.h\"\nint d = D;\n",
        );
        let vm = Vm::new(temp_lib_config(&root));
        let lib_dir = vm.global_state.config.lib_dir.as_str();
        vm.initialize_process_from_path(&LpcPath::new_in_game("/secure/master", "/", lib_dir))
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));
        assert!(vm.global_state.object_space.master_object().is_some());

        let err = message(&cloner_caught(&vm).await);
        assert!(err.contains("clone_object: permission denied"), "{err}");
    }
}
