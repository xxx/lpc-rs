use std::sync::Arc;

use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, task_template::TaskTemplate},
        vm::global_state::GlobalState,
    },
    util::process_builder::process_insert_and_initialize_program,
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};

#[macro_export]
macro_rules! assert_regex {
    ($string:expr, $regex:expr) => {
        let re = regex::Regex::new($regex).unwrap();
        assert!(
            re.is_match($string),
            "Expected '{}' to match '{}'",
            $string,
            $regex
        )
    };
}

/// A master that allows every load, inherit and read: what these tests'
/// programs load under.
pub const PERMISSIVE_MASTER: &str = "\
int valid_load(string path, string func, object caller, string program) { return 1; }
int valid_inherit(string path, string from) { return 1; }
int valid_read(string path, string func, object caller, string program) { return 1; }
";

/// Physically insert [`PERMISSIVE_MASTER`] at the configured master path.
/// It has no state, so it needs no initializer.
pub async fn permissive_master(object_space: &ObjectSpace) -> Arc<Process> {
    object_space
        .create_process_from_code("/secure/master.c", PERMISSIVE_MASTER)
        .await
        .expect("the permissive master compiles")
}

pub fn test_config_builder() -> ConfigBuilder {
    ConfigBuilder::default().lib_dir("./tests/fixtures/code")
}

pub fn test_config() -> Config {
    test_config_builder().build().unwrap()
}

pub async fn compile_prog_custom<P>(code: &str, path: P, config: Config) -> Program
where
    P: Into<LpcPath>,
{
    let compiler = CompilerBuilder::default().config(config).build().unwrap();
    compiler
        .compile_string(path, code)
        .await
        .expect("Failed to compile.")
        .program
}

pub async fn run_prog_custom<P>(code: &str, path: P, config: Config) -> Task<MAX_CALL_STACK_SIZE>
where
    P: Into<LpcPath>,
{
    let (tx, _rx) = tokio::sync::mpsc::channel(128);
    let program = compile_prog_custom(code, path, config).await;
    let global_state = GlobalState::new(test_config(), tx);

    process_insert_and_initialize_program(
        Arc::new(Process::new(program)),
        TaskTemplate::from(global_state),
    )
    .await
    .unwrap_or_else(|e| {
        e.emit_diagnostics();
        panic!("failed to initialize");
    })
}

pub async fn run_prog(code: &str) -> Task<MAX_CALL_STACK_SIZE> {
    run_prog_custom(code, "/my_file.c", test_config()).await
}
