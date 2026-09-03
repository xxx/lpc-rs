//! The simul-efun file: named in the config with or without its `.c`, its
//! functions run in the simul-efun object rather than in the caller, and a
//! call by bare name resolves own and inherited functions first, then simul
//! efuns, then efuns.

use std::sync::Arc;

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{process::Process, vm::Vm},
    test_support::{TempLib, committed_string, permissive_master, test_config},
};

/// A simul efun answering what `this_object()` names.
const ME: &str = "string me() { return file_name(this_object()); }\n";

/// `/user.c`: `got` is what `me()` returns.
const USER_ME: &str = "string got; void create() { got = me(); }";

/// A lib holding `files` (path under the lib, source), with a `secure/` dir.
fn lib_holding(lib: &str, files: &[(&str, &str)]) -> TempLib {
    let root = TempLib::new(lib);
    std::fs::create_dir_all(root.join("secure")).unwrap();
    for (path, source) in files {
        std::fs::write(root.join(path), source).unwrap();
    }
    root
}

/// The string global `name` of `process`.
fn string_global(vm: &Vm, process: &Arc<Process>, name: &str) -> String {
    // By name: an inherited global may hold register 0.
    let Some(RegisterVariant::Global(register)) = process.program.global_variables[name].location
    else {
        panic!("`{name}` is a global");
    };
    committed_string(vm, process, register.index())
}

/// The `got` global of `/user.c` compiled from `user`, in a lib holding
/// `files` (path under the lib, source) whose simul-efun file the config
/// names as `spelling`.
async fn got_from_user(lib: &str, spelling: &str, files: &[(&str, &str)], user: &str) -> String {
    let root = lib_holding(lib, files);
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file(spelling)
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.global_state
        .initialize_simul_efuns()
        .await
        .expect("configured")
        .expect("compiles");
    let user = vm
        .initialize_process_from_code("/user.c", user)
        .await
        .unwrap()
        .context
        .process;
    string_global(&vm, &user, "got")
}

/// The `got` global of `/secure/master.c` compiled from `master`, after a
/// boot of a lib holding `files` (path under the lib, source).
async fn got_from_master(lib: &str, files: &[(&str, &str)], master: &str) -> String {
    let root = lib_holding(lib, files);
    std::fs::write(root.join("secure/master.c"), master).unwrap();
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .master_object(ustr::ustr("/secure/master.c"))
        .build()
        .unwrap();
    let mut vm = Vm::new(config);
    let master = vm.bootstrap().await.expect("boots").process;
    string_global(&vm, &master, "got")
}

/// What `me()` returns to `/user.c` when the config names the simul-efun
/// file as `spelling`.
async fn me_from_user(lib: &str, spelling: &str) -> String {
    got_from_user(lib, spelling, &[("secure/simul_efuns.c", ME)], USER_ME).await
}

#[tokio::test]
async fn a_simul_efun_runs_in_the_simul_efun_object() {
    assert_eq!(
        me_from_user("simul-plain", "/secure/simul_efuns").await,
        "/secure/simul_efuns"
    );
}

#[tokio::test]
async fn the_config_may_name_the_file_with_its_extension() {
    assert_eq!(
        me_from_user("simul-dot-c", "/secure/simul_efuns.c").await,
        "/secure/simul_efuns"
    );
}

#[tokio::test]
async fn the_config_may_omit_the_leading_slash() {
    assert_eq!(
        me_from_user("simul-no-slash", "secure/simul_efuns").await,
        "/secure/simul_efuns"
    );
}

/// A simul efun calling one of its own file's functions is a local call: no
/// door, so the inner one's `previous_object()` is still the user.
#[tokio::test]
async fn a_call_between_simul_efuns_is_local() {
    let simul = "string inner() { return file_name(previous_object()); }\n\
                 string outer() { return inner(); }\n";
    let got = got_from_user(
        "simul-sibling",
        "/secure/simul_efuns",
        &[("secure/simul_efuns.c", simul)],
        "string got; void create() { got = outer(); }",
    )
    .await;
    assert_eq!(got, "/user");
}

#[tokio::test]
async fn a_function_the_simul_efun_file_inherits_is_a_simul_efun() {
    let got = got_from_user(
        "simul-inherits",
        "/secure/simul_efuns",
        &[
            ("secure/se_base.c", ME),
            ("secure/simul_efuns.c", "inherit \"/secure/se_base\";\n"),
        ],
        USER_ME,
    )
    .await;
    assert_eq!(got, "/secure/simul_efuns");
}

/// Ordinary inheritance: the inheritor runs its own copy, on its own globals.
#[tokio::test]
async fn an_object_inheriting_the_simul_efun_file_runs_its_own_copy() {
    let simul = "string tag = \"simul\";\nstring me() { return tag; }\n";
    let got = got_from_user(
        "simul-inherited",
        "/secure/simul_efuns",
        &[("secure/simul_efuns.c", simul)],
        "inherit \"/secure/simul_efuns\";\n\
         string got; void create() { tag = \"user\"; got = me(); }",
    )
    .await;
    assert_eq!(got, "user");
}

#[tokio::test]
async fn an_own_function_shadows_a_simul_efun() {
    let got = got_from_user(
        "simul-shadow-own",
        "/secure/simul_efuns",
        &[(
            "secure/simul_efuns.c",
            "string me() { return \"simul\"; }\n",
        )],
        "string me() { return \"own\"; }\nstring got; void create() { got = me(); }",
    )
    .await;
    assert_eq!(got, "own");
}

#[tokio::test]
async fn a_simul_efun_shadows_an_efun() {
    let got = got_from_user(
        "simul-shadow-efun",
        "/secure/simul_efuns",
        &[(
            "secure/simul_efuns.c",
            "string explode(string s, string d) { return \"simul\"; }\n",
        )],
        "string got; void create() { got = explode(\"a,b\", \",\"); }",
    )
    .await;
    assert_eq!(got, "simul");
}

/// Inside the simul-efun file a call to a function of a file it inherits is
/// a local call: the base's `previous_object()` is still the user.
#[tokio::test]
async fn a_simul_efun_calling_its_base_stays_local() {
    let got = got_from_user(
        "simul-base-local",
        "/secure/simul_efuns",
        &[
            (
                "secure/se_base.c",
                "string inner() { return file_name(previous_object()); }\n",
            ),
            (
                "secure/simul_efuns.c",
                "inherit \"/secure/se_base\";\nstring outer() { return inner(); }\n",
            ),
        ],
        "string got; void create() { got = outer(); }",
    )
    .await;
    assert_eq!(got, "/user");
}

#[tokio::test]
async fn a_function_the_simul_efun_file_inherits_sees_its_caller() {
    let got = got_from_user(
        "simul-inherits-caller",
        "/secure/simul_efuns",
        &[
            (
                "secure/se_base.c",
                "string who() { return file_name(previous_object()); }\n",
            ),
            ("secure/simul_efuns.c", "inherit \"/secure/se_base\";\n"),
        ],
        "string got; void create() { got = who(); }",
    )
    .await;
    assert_eq!(got, "/user");
}

/// `::me()` names the inherited copy, not the resident simul-efun object.
#[tokio::test]
async fn an_object_inheriting_the_simul_efun_file_may_call_the_parent_form() {
    let simul = "string tag = \"simul\";\nstring me() { return tag; }\n";
    let got = got_from_user(
        "simul-inherited-parent-form",
        "/secure/simul_efuns",
        &[("secure/simul_efuns.c", simul)],
        "inherit \"/secure/simul_efuns\";\n\
         string got; void create() { tag = \"user\"; got = ::me(); }",
    )
    .await;
    assert_eq!(got, "user");
}

/// A simul-efun file with an initialized global and a `create()` that counts
/// its runs.
const COUNTED: &str = "int n = 7; int created;\n\
    void create() { created++; }\n\
    string seven() { return \"\" + n; }\n\
    string creations() { return \"\" + created; }\n";

const SIMUL_COUNTED: (&str, &str) = ("secure/simul_efuns.c", COUNTED);

#[tokio::test]
async fn a_simul_efun_global_is_initialized_at_boot() {
    assert_eq!(
        got_from_user(
            "simul-init-global",
            "/secure/simul_efuns",
            &[SIMUL_COUNTED],
            "string got; void create() { got = seven(); }",
        )
        .await,
        "7"
    );
}

/// Boot runs `create()`; a later `->` finds the object initialized and does
/// not run it again.
#[tokio::test]
async fn the_simul_efun_file_is_created_once_at_boot() {
    let user = r#"string got;
        void create() {
            string before = creations();
            "/secure/simul_efuns"->creations();
            got = before + "," + creations();
        }"#;
    assert_eq!(
        got_from_user(
            "simul-create-once",
            "/secure/simul_efuns",
            &[SIMUL_COUNTED],
            user
        )
        .await,
        "1,1"
    );
}

#[tokio::test]
async fn the_master_is_created_after_the_simul_efun_object() {
    assert_eq!(
        got_from_master(
            "simul-before-master",
            &[SIMUL_COUNTED],
            "string got; void create() { got = seven() + \",\" + creations(); }",
        )
        .await,
        "7,1"
    );
}

/// The first version of the simul-efun file: defines `gone()`.
const V1: &str = "string gone() { return \"v1\"; }\n";
/// A later version keeping `gone()`.
const V2_KEEPS: &str = "string gone() { return \"v2\"; }\n";
/// A later version without `gone()`.
const V2_DROPS: &str = "string other() { return \"v2\"; }\n";

/// Boot with [`V1`], compile `/user.c` (whose `later()` calls `gone()`)
/// against it, destruct the simul-efun object, and given `reload` rewrite
/// the file as it and load it again through a `->` in its own task; then ask
/// `"/user"->later()` from a fresh task: its answer, or the error.
async fn later_after(lib: &str, reload: Option<&str>) -> Result<String, String> {
    let root = lib_holding(lib, &[("secure/simul_efuns.c", V1)]);
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
    permissive_master(&vm.global_state.object_space).await;
    vm.initialize_process_from_code("/user.c", "string later() { return gone(); }")
        .await
        .unwrap();
    vm.initialize_process_from_code(
        "/destroyer.c",
        r#"void create() { destruct(find_object("/secure/simul_efuns")); }"#,
    )
    .await
    .unwrap();
    if let Some(v2) = reload {
        std::fs::write(root.join("secure/simul_efuns.c"), v2).unwrap();
        vm.initialize_process_from_code(
            "/reloader.c",
            r#"void create() { "/secure/simul_efuns"->gone(); }"#,
        )
        .await
        .unwrap();
    }
    let asker = vm
        .initialize_process_from_code(
            "/asker.c",
            r#"string got; void create() { got = "/user"->later(); }"#,
        )
        .await
        .map_err(|e| e.to_string())?;
    Ok(string_global(&vm, &asker.context.process, "got"))
}

#[tokio::test]
async fn a_destructed_simul_efun_object_fails_calls_from_later_tasks() {
    assert_eq!(
        later_after("simul-destructed", None).await,
        Err("runtime error: call to simul efun `gone`: no simul-efun object is loaded".into())
    );
}

/// An object compiled against the old version links by name to the new one.
#[tokio::test]
async fn a_reloaded_simul_efun_object_serves_callers_compiled_against_the_old_one() {
    assert_eq!(
        later_after("simul-reloaded", Some(V2_KEEPS)).await,
        Ok("v2".into())
    );
}

#[tokio::test]
async fn a_name_the_reloaded_simul_efun_file_dropped_fails_at_the_call() {
    assert_eq!(
        later_after("simul-dropped", Some(V2_DROPS)).await,
        Err("runtime error: call to unknown simul efun `gone`".into())
    );
}

#[tokio::test]
async fn the_simul_efun_object_is_the_resident_for_its_own_initializer() {
    let vm = Vm::new(test_config());
    let task = vm
        .initialize_process_from_code("/secure/simul_efuns.c", "int n = 7;")
        .await
        .unwrap();
    assert!(task.context.simul_efuns().is_some());
}
