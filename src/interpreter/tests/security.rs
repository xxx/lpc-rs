//! Who the master hears from: the defining file of the calling code, and
//! the canonical path, for every shape of call.

use std::sync::Arc;

use indoc::indoc;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{CommittedReader, lpc_ref::LpcRef, process::Process, vm::Vm},
    test_support::{TempLib, committed_string, temp_lib_config},
};

/// Records every `valid_read` argument: path, efun, caller's file name,
/// program (`mixed`, so a 0 lands too).
const RECORDING_MASTER: &str = indoc! { r#"
    string seen_path;
    string seen_efun;
    string seen_caller;
    mixed seen_program;
    int valid_read(string path, string func, object caller, mixed program) {
        seen_path = path;
        seen_efun = func;
        seen_caller = file_name(caller);
        seen_program = program;
        return 1;
    }
"# };

const SEEN_PATH: u16 = 0;
const SEEN_CALLER: u16 = 2;
const SEEN_PROGRAM: u16 = 3;

/// A lib at `root` with `/data.txt` and the recording master; the master.
async fn recording_master(vm: &Vm, root: &TempLib) -> Arc<Process> {
    std::fs::write(root.join("data.txt"), "hello\n").unwrap();
    vm.initialize_process_from_code("/secure/master.c", RECORDING_MASTER)
        .await
        .unwrap()
        .context
        .process
}

/// Load `code` as `path`; its `create()` performs the read under test.
async fn run(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap()
        .context
        .process
}

#[tokio::test]
async fn a_direct_call_reports_the_callers_own_file() {
    let root = TempLib::new("prov-direct");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(&vm, "/a.c", r#"void create() { read_file("/data.txt"); }"#).await;
    assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
    assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/a");
}

#[tokio::test]
async fn an_inherited_function_reports_the_parents_file() {
    let root = TempLib::new("prov-inherit");
    std::fs::write(
        root.join("parent.c"),
        "string parent_read(string p) { return read_file(p); }\n",
    )
    .unwrap();
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/child.c",
        indoc! { r#"
            inherit "/parent";
            string got;
            void create() { got = parent_read("/data.txt"); }
        "# },
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/parent.c");
    assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/child");
}

#[tokio::test]
async fn a_closure_reports_the_file_it_was_written_in() {
    let root = TempLib::new("prov-closure");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/a.c",
        r#"function make() { return (: read_file($1) :); }"#,
    )
    .await;
    run(
        &vm,
        "/b.c",
        indoc! { r#"
            string got;
            void create() {
                function f = find_object("/a")->make();
                got = f("/data.txt");
            }
        "# },
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
}

/// A pointer `u` wrote and `x` fired acts for `u`'s code, exactly as a
/// closure would.
#[tokio::test]
async fn an_efun_pointer_fired_by_another_object_reports_the_file_that_wrote_it() {
    let root = TempLib::new("prov-pointer");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/u.c",
        r#"function make() { return &read_file("/data.txt"); }"#,
    )
    .await;
    run(
        &vm,
        "/x.c",
        indoc! { r#"
            string got;
            void create() {
                function f = find_object("/u")->make();
                got = f();
            }
        "# },
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/u.c");
    assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/u");
}

/// `b` asking `a` to read for it hands the master `a`'s code, not `b`'s.
#[tokio::test]
async fn a_call_other_trampoline_reports_the_callees_file() {
    let root = TempLib::new("prov-trampoline");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/a.c",
        r#"string via(string p) { return read_file(p); }"#,
    )
    .await;
    run(
        &vm,
        "/b.c",
        r#"string got; void create() { got = find_object("/a")->via("/data.txt"); }"#,
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PROGRAM), "/a.c");
    assert_eq!(committed_string(&vm, &master, SEEN_CALLER), "/a");
}

#[tokio::test]
async fn a_simul_efun_reports_the_simul_efun_file() {
    let root = TempLib::new("prov-simul");
    std::fs::create_dir_all(root.join("secure")).unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "string sread(string p) { return read_file(p); }\n",
    )
    .unwrap();
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.global_state
        .initialize_simul_efuns()
        .await
        .expect("configured")
        .expect("compiles");
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/user.c",
        r#"string got; void create() { got = sread("/data.txt"); }"#,
    )
    .await;
    assert_eq!(
        committed_string(&vm, &master, SEEN_PROGRAM),
        "/secure/simul_efuns.c"
    );
    assert_eq!(
        committed_string(&vm, &master, SEEN_CALLER),
        "/secure/simul_efuns"
    );
}

/// A function the simul-efun file inherits runs in the simul-efun object
/// but is defined by its base file, and `program` says so.
#[tokio::test]
async fn a_function_the_simul_efun_file_inherits_reports_its_own_file() {
    let root = TempLib::new("prov-simul-base");
    std::fs::create_dir_all(root.join("secure")).unwrap();
    std::fs::write(
        root.join("secure/se_base.c"),
        "string sread(string p) { return read_file(p); }\n",
    )
    .unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "inherit \"/secure/se_base\";\n",
    )
    .unwrap();
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.global_state
        .initialize_simul_efuns()
        .await
        .expect("configured")
        .expect("compiles");
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/user.c",
        r#"string got; void create() { got = sread("/data.txt"); }"#,
    )
    .await;
    assert_eq!(
        committed_string(&vm, &master, SEEN_PROGRAM),
        "/secure/se_base.c"
    );
    assert_eq!(
        committed_string(&vm, &master, SEEN_CALLER),
        "/secure/simul_efuns"
    );
}

/// An efun pointer fired by `call_out` has no LPC frame under it.
#[tokio::test]
async fn an_efun_pointer_from_call_out_reports_zero() {
    let root = TempLib::new("prov-call-out");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/timer.c",
        r#"void create() { call_out(&read_file("/data.txt"), 100); }"#,
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

#[tokio::test]
async fn a_relative_path_resolves_against_the_callers_directory() {
    let root = TempLib::new("path-relative");
    std::fs::create_dir_all(root.join("dir")).unwrap();
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/dir/obj.c",
        r#"void create() { read_file("../data.txt"); }"#,
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/data.txt");
}

#[tokio::test]
async fn dot_segments_canonicalize_before_the_master_sees_them() {
    let root = TempLib::new("path-dots");
    let vm = Vm::new(temp_lib_config(&root));
    let master = recording_master(&vm, &root).await;
    run(
        &vm,
        "/a.c",
        r#"void create() { read_file("/dir/./../data.txt"); }"#,
    )
    .await;
    assert_eq!(committed_string(&vm, &master, SEEN_PATH), "/data.txt");
}

/// Leaving the lib is a mechanical error, not a policy question.
#[tokio::test]
async fn an_escape_from_the_lib_errors_without_consulting_the_master() {
    let root = TempLib::new("path-escape");
    let vm = Vm::new(temp_lib_config(&root));
    let master = vm
        .initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int consulted;
                int valid_read(string p, string e, object c, string g) {
                    consulted = 1;
                    return 1;
                }
            "# },
        )
        .await
        .unwrap()
        .context
        .process;
    let reader = run(
        &vm,
        "/a.c",
        indoc! { r#"
            string err;
            void create() {
                err = catch(read_file("/../../../../../../../../etc/passwd"));
            }
        "# },
    )
    .await;
    let err = committed_string(&vm, &reader, 0);
    assert!(
        err.contains("read_file: `/../../../../../../../../etc/passwd` is not a valid path"),
        "{err}"
    );
    assert!(
        !err.contains(vm.global_state.config.lib_dir.as_str()),
        "server path leaked: {err}"
    );
    assert_eq!(
        vm.global_state.committed_global(&master, 0u16),
        LpcRef::from(0),
        "the master was never asked"
    );
}
