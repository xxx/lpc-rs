

use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        program::Program, task::Task,
    },
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};

use lpc_rs::interpreter::task::initialize_task::InitializeProgramBuilder;

#[macro_export]
macro_rules! assert_regex {
    ($string:expr, $regex:expr) => {
        let re = regex::Regex::new($regex).unwrap();
        assert!(
            re.is_match($string),
            "Expected '{}' to match '{}'",
            $regex,
            $string
        )
    };
}

pub fn test_config_builder() -> ConfigBuilder {
    let mut builder = ConfigBuilder::default();
    builder.lib_dir("./tests/fixtures/code");
    builder
}

pub fn test_config() -> Config {
    test_config_builder().build().unwrap()
}

pub fn compile_prog_custom<P>(code: &str, path: P, config: Config) -> Program
where
    P: Into<LpcPath>,
{
    let compiler = CompilerBuilder::default().config(config).build().unwrap();
    compiler
        .compile_string(path, code)
        .expect("Failed to compile.")
}

pub async fn run_prog_custom<P>(code: &str, path: P, config: Config) -> Task<MAX_CALL_STACK_SIZE>
where
    P: Into<LpcPath>,
{
    let (tx, _rx) = tokio::sync::mpsc::channel(128);
    let program = compile_prog_custom(code, path, config);

    InitializeProgramBuilder::default()
        .program(program)
        .config(test_config())
        .tx(tx)
        .build()
        .await
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            panic!("failed to initialize");
        })
}

pub async fn run_prog(code: &str) -> Task<MAX_CALL_STACK_SIZE> {
    run_prog_custom(code, "/my_file.c", test_config()).await
}
