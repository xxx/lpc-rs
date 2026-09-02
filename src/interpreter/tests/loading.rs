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
pub(crate) const LOADS: u16 = 9;

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

    #[tokio::test]
    async fn the_master_hears_the_dot_c_source() {
        let root = TempLib::new("path-source");
        let vm = Vm::new(temp_lib_config(&root));
        let master = recording_master(&vm, &root).await;
        write(&root, "x.h.c", "int hc;\n");
        for (i, (arg, expected)) in [("/x", "/x.c"), ("/x.c", "/x.c"), ("/x.h", "/x.h.c")]
            .into_iter()
            .enumerate()
        {
            let code = format!(r#"void create() {{ find_object("{arg}"); }}"#);
            run(&vm, &format!("/probe{i}.c"), &code).await;
            assert_eq!(committed_string(&vm, &master, SEEN_PATH), expected, "{arg}");
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
