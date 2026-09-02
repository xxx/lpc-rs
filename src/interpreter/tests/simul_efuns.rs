//! The simul-efun file: named in the config with or without its `.c`, its
//! functions run in the simul-efun object rather than in the caller, and a
//! call by bare name resolves own and inherited functions first, then simul
//! efuns, then efuns.

use lpc_rs_core::register::RegisterVariant;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::vm::Vm,
    test_support::{TempLib, committed_string},
};

/// A simul efun answering what `this_object()` names.
const ME: &str = "string me() { return file_name(this_object()); }\n";

/// `/user.c`: `got` is what `me()` returns.
const USER_ME: &str = "string got; void create() { got = me(); }";

/// The `got` global of `/user.c` compiled from `user`, in a lib holding
/// `files` (path under the lib, source) whose simul-efun file the config
/// names as `spelling`.
async fn got_from_user(lib: &str, spelling: &str, files: &[(&str, &str)], user: &str) -> String {
    let root = TempLib::new(lib);
    std::fs::create_dir_all(root.join("secure")).unwrap();
    for (path, source) in files {
        std::fs::write(root.join(path), source).unwrap();
    }
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
    // An inherited global comes before `/user.c`'s own.
    let Some(RegisterVariant::Global(register)) = user.program.global_variables["got"].location
    else {
        panic!("`got` is a global");
    };
    committed_string(&vm, &user, register.index())
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
