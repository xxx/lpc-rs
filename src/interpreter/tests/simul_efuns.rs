//! The simul-efun file: named in the config with or without its `.c`, the
//! functions it defines run in the simul-efun object rather than in the caller.

use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::vm::Vm,
    test_support::{TempLib, committed_string},
};

/// What `me()` — a simul efun answering `file_name(this_object())` — returns
/// to `/user.c` when the config names the simul-efun file as `spelling`.
async fn me_from_user(lib: &str, spelling: &str) -> String {
    let root = TempLib::new(lib);
    std::fs::create_dir_all(root.join("secure")).unwrap();
    std::fs::write(
        root.join("secure/simul_efuns.c"),
        "string me() { return file_name(this_object()); }\n",
    )
    .unwrap();
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
        .initialize_process_from_code("/user.c", "string got; void create() { got = me(); }")
        .await
        .unwrap()
        .context
        .process;
    committed_string(&vm, &user, 0)
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
