//! A path with no source file becomes a blueprint's program under that
//! name, when the master says which blueprint.

use std::sync::Arc;

use indoc::indoc;

use super::loading::{count, run, write};
use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef, process::Process, vm::Vm},
    test_support::{TempLib, committed_string, temp_lib_config},
};

/// Answers `/inst/<n>/<rest>` with `/<rest>` and declines everything else;
/// records what it heard; allows every load and read. Globals, by register:
/// 0 `seen_path`, 1 `seen_func`, 2 `seen_caller`, 3 `seen_program`, 4 `asks`,
/// 5 `load_path`, 6 `load_func`, 7 `load_caller`, 8 `load_program`, 9 `loads`,
/// 10 `read_program`, 11 `read_caller`, 12 `deny_path` (a load of this source
/// is refused).
const INSTANCING_MASTER: &str = indoc! { r#"
    string seen_path; string seen_func; string seen_caller; mixed seen_program; int asks;
    string load_path; string load_func; string load_caller; mixed load_program; int loads;
    mixed read_program; string read_caller;
    string deny_path;
    mixed compile_object(string path, string func, object caller, mixed program) {
        int n; string rest;
        seen_path = path; seen_func = func; seen_caller = file_name(caller);
        seen_program = program; asks++;
        if (sscanf(path, "/inst/%d/%s", n, rest) != 2) return 0;
        return "/" + rest;
    }
    int valid_load(string path, string func, object caller, mixed program) {
        load_path = path; load_func = func; load_caller = file_name(caller);
        load_program = program; loads++;
        return path != deny_path;
    }
    int valid_inherit(string path, string from) { return 1; }
    int valid_read(string path, string func, object caller, mixed program) {
        read_program = program; read_caller = file_name(caller);
        return 1;
    }
    void deny(string path) { deny_path = path; }
"# };

const SEEN_PATH: u16 = 0;
const SEEN_FUNC: u16 = 1;
const SEEN_CALLER: u16 = 2;
const SEEN_PROGRAM: u16 = 3;
const ASKS: u16 = 4;
const LOAD_PATH: u16 = 5;
const LOAD_FUNC: u16 = 6;
const LOAD_CALLER: u16 = 7;
const LOAD_PROGRAM: u16 = 8;
const LOADS: u16 = 9;

/// `/d/room1.c`: counts its creates, and `f()` finds `room2` relative to
/// itself; `/d/room2.c`.
const ROOM1: &str = indoc! { r#"
    int creates;
    void create() { creates++; }
    object f() { return find_object("room2"); }
    string rd() { return read_file("/data.txt"); }
"# };
const ROOM2: &str = "int r = 2;\n";

/// A lib with `/d/room1.c`, `/d/room2.c`, `/data.txt`, and the instancing
/// master; the master.
async fn instancing_lib(name: &str) -> (TempLib, Vm, Arc<Process>) {
    let root = TempLib::new(name);
    write(&root, "d/room1.c", ROOM1);
    write(&root, "d/room2.c", ROOM2);
    write(&root, "data.txt", "hello\n");
    let vm = Vm::new(temp_lib_config(&root));
    let master = vm
        .initialize_process_from_code("/secure/master.c", INSTANCING_MASTER)
        .await
        .unwrap()
        .context
        .process;
    (root, vm, master)
}

/// The resident object at `key`.
fn resident(vm: &Vm, key: &str) -> Arc<Process> {
    vm.global_state
        .object_space
        .lookup(key)
        .unwrap_or_else(|| panic!("{key} is resident"))
}

mod the_apply {
    use super::*;

    #[tokio::test]
    async fn a_fileless_path_is_the_blueprints_program_under_its_own_name() {
        let (_root, vm, master) = instancing_lib("virt-basic").await;
        let a = run(
            &vm,
            "/a.c",
            indoc! { r#"
                string name;
                void create() {
                    find_object("/d/room1");
                    name = file_name(find_object("/inst/17/d/room1"));
                }
            "# },
        )
        .await;
        assert_eq!(committed_string(&vm, &a, 0), "/inst/17/d/room1");
        let blueprint = resident(&vm, "/d/room1");
        let instance = resident(&vm, "/inst/17/d/room1");
        assert_eq!(instance.filename(), "/inst/17/d/room1");
        assert!(!instance.is_clone());
        assert!(Arc::ptr_eq(&instance.program, &blueprint.program));
        assert!(vm.global_state.is_initialized(&instance));
        assert_eq!(
            committed_string(&vm, &master, SEEN_PATH),
            "/inst/17/d/room1"
        );
        assert_eq!(committed_string(&vm, &master, SEEN_FUNC), "find_object");
        assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/a");
        assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
        assert_eq!(count(&vm, &master, ASKS), 1);
    }

    /// The blueprint was resident, so nothing was compiled and `valid_load`
    /// heard nothing about the fileless path.
    #[tokio::test]
    async fn valid_load_is_not_asked_for_a_fileless_path() {
        let (_root, vm, master) = instancing_lib("virt-no-valid-load").await;
        run(&vm, "/a.c", r#"void create() { find_object("/d/room1"); }"#).await;
        assert_eq!(count(&vm, &master, LOADS), 1);
        run(
            &vm,
            "/b.c",
            r#"void create() { find_object("/inst/17/d/room1"); }"#,
        )
        .await;
        assert_eq!(count(&vm, &master, LOADS), 1);
        assert_eq!(count(&vm, &master, ASKS), 1);
    }

    #[tokio::test]
    async fn a_declining_master_fails_as_a_missing_file() {
        let (_root, vm, master) = instancing_lib("virt-decline").await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(clone_object("/nowhere")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("Cannot read file `/nowhere.c`"), "{e}");
        assert_eq!(count(&vm, &master, ASKS), 1);
        assert!(vm.global_state.object_space.lookup("/nowhere").is_none());
    }

    #[tokio::test]
    async fn a_master_without_compile_object_fails_as_a_missing_file() {
        let root = TempLib::new("virt-undefined");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            r#"int valid_load(string p, string f, object c, mixed g) { return 1; }"#,
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/room1")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("Cannot read file `/inst/1/d/room1.c`"), "{e}");
    }

    /// With no master at all, a fileless path fails as every load does.
    #[tokio::test]
    async fn no_master_refuses_as_it_refuses_every_load() {
        let root = TempLib::new("virt-no-master");
        let vm = Vm::new(temp_lib_config(&root));
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/room1")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("move_object: permission denied"), "{e}");
    }

    #[tokio::test]
    async fn a_throwing_compile_object_is_the_callers_error() {
        let root = TempLib::new("virt-throw");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            indoc! { r#"
                int valid_load(string p, string f, object c, mixed g) { return 1; }
                mixed compile_object(string p, string f, object c, mixed g) { throw("no instances today"); }
            "# },
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/room1")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("no instances today"), "{e}");
    }

    #[tokio::test]
    async fn a_non_path_answer_is_a_runtime_error() {
        let root = TempLib::new("virt-wrong-type");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            indoc! { r#"
                int valid_load(string p, string f, object c, mixed g) { return 1; }
                mixed compile_object(string p, string f, object c, mixed g) { return 42; }
            "# },
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/room1")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("compile_object: expected a path, got 42"), "{e}");
    }

    /// The blueprint is not resurrected to serve the instance.
    #[tokio::test]
    async fn a_blueprint_destructed_in_this_task_is_an_error() {
        let (_root, vm, _master) = instancing_lib("virt-destructed").await;
        let a = run(
            &vm,
            "/a.c",
            indoc! { r#"
                string e;
                void create() {
                    destruct(find_object("/d/room1"));
                    e = catch(move_object("/inst/17/d/room1"));
                }
            "# },
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(
            e.contains("compile_object: `/d/room1` was destructed in this task"),
            "{e}"
        );
    }

    #[tokio::test]
    async fn a_clone_suffixed_path_never_asks() {
        let (_root, vm, master) = instancing_lib("virt-clone-suffix").await;
        let a = run(
            &vm,
            "/a.c",
            r#"mixed got; void create() { got = find_object("/inst/1/d/room1#3"); }"#,
        )
        .await;
        assert_eq!(vm.global_state.committed_global(&a, 0u16), LpcRef::from(0));
        assert_eq!(count(&vm, &master, ASKS), 0);
    }
}

mod the_blueprint {
    use super::*;

    #[tokio::test]
    async fn a_non_resident_blueprint_is_loaded_for_the_requester() {
        let (_root, vm, master) = instancing_lib("virt-attribution").await;
        run(
            &vm,
            "/a.c",
            r#"void create() { find_object("/inst/17/d/room1"); }"#,
        )
        .await;
        assert_eq!(committed_string(&vm, &master, LOAD_PATH), "/d/room1.c");
        assert_eq!(committed_string(&vm, &master, LOAD_FUNC), "find_object");
        assert_eq!(committed_string(&vm, &master, LOAD_CALLER), "/a");
        assert_eq!(committed_string(&vm, &master, LOAD_PROGRAM), "/a.c");
        assert_eq!(count(&vm, &master, LOADS), 1);
        let blueprint = resident(&vm, "/d/room1");
        assert!(vm.global_state.is_initialized(&blueprint));
        resident(&vm, "/inst/17/d/room1");
    }

    #[tokio::test]
    async fn a_denied_blueprint_fails_with_nothing_inserted() {
        let (_root, vm, master) = instancing_lib("virt-denied").await;
        run(
            &vm,
            "/w.c",
            r#"void create() { "/secure/master"->deny("/d/room1.c"); }"#,
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/17/d/room1")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("move_object: permission denied"), "{e}");
        assert!(vm.global_state.object_space.lookup("/d/room1").is_none());
        assert!(
            vm.global_state
                .object_space
                .lookup("/inst/17/d/room1")
                .is_none()
        );
        assert_eq!(count(&vm, &master, ASKS), 1);
    }

    #[tokio::test]
    async fn an_answer_naming_no_file_is_a_missing_file() {
        let (_root, vm, master) = instancing_lib("virt-ghost").await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/17/ghost")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("Cannot read file `/ghost.c`"), "{e}");
        assert_eq!(
            count(&vm, &master, ASKS),
            1,
            "the answer is not put back to compile_object"
        );
        assert!(
            vm.global_state
                .object_space
                .lookup("/inst/17/ghost")
                .is_none()
        );
    }

    #[tokio::test]
    async fn an_escaping_answer_is_refused_without_a_server_path() {
        let root = TempLib::new("virt-escape");
        let vm = Vm::new(temp_lib_config(&root));
        run(
            &vm,
            "/secure/master.c",
            indoc! { r#"
                int valid_load(string p, string f, object c, mixed g) { return 1; }
                mixed compile_object(string p, string f, object c, mixed g) {
                    return "/../../../../../../../../etc/passwd";
                }
            "# },
        )
        .await;
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/x")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(
            e.contains("compile_object: `/../../../../../../../../etc/passwd` is not a valid path"),
            "{e}"
        );
        assert!(
            !e.contains(vm.global_state.config.lib_dir.as_str()),
            "server path leaked: {e}"
        );
    }

    #[tokio::test]
    async fn a_no_clone_blueprint_is_refused() {
        let (root, vm, _master) = instancing_lib("virt-no-clone").await;
        write(&root, "d/locked.c", "#pragma no_clone\nint r;\n");
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/locked")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(
            e.contains("`#pragma no_clone` enabled, and so cannot be instantiated"),
            "{e}"
        );
        assert!(
            vm.global_state
                .object_space
                .lookup("/inst/1/d/locked")
                .is_none()
        );
    }

    /// The instance's `create()` runs on its own globals; the blueprint's
    /// stay its own.
    #[tokio::test]
    async fn the_blueprints_globals_are_its_own() {
        let (_root, vm, _master) = instancing_lib("virt-globals").await;
        run(
            &vm,
            "/a.c",
            r#"void create() { find_object("/inst/17/d/room1"); find_object("/inst/18/d/room1"); }"#,
        )
        .await;
        let blueprint = resident(&vm, "/d/room1");
        let seventeen = resident(&vm, "/inst/17/d/room1");
        let eighteen = resident(&vm, "/inst/18/d/room1");
        for ob in [&blueprint, &seventeen, &eighteen] {
            assert_eq!(
                vm.global_state.committed_global(ob, 0u16),
                LpcRef::from(1),
                "{ob}"
            );
        }
    }

    #[tokio::test]
    async fn a_throwing_create_leaves_the_instance_absent() {
        let (root, vm, _master) = instancing_lib("virt-bad-create").await;
        write(
            &root,
            "d/bad.c",
            indoc! { r#"
                void create() {
                    string s;
                    if (sscanf(file_name(this_object()), "/inst/%s", s) == 1) throw("boom");
                }
            "# },
        );
        let a = run(
            &vm,
            "/a.c",
            r#"string e; void create() { e = catch(move_object("/inst/1/d/bad")); }"#,
        )
        .await;
        let e = committed_string(&vm, &a, 0);
        assert!(e.contains("boom"), "{e}");
        resident(&vm, "/d/bad");
        assert!(
            vm.global_state
                .object_space
                .lookup("/inst/1/d/bad")
                .is_none()
        );
    }
}
