use std::sync::Arc;

use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
        program::Program, task::Task,
    },
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use parking_lot::RwLock;

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
    let upvalues = RwLock::new(GcBank::default());
    let (tx, _rx) = tokio::sync::mpsc::channel(128);
    let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));
    let program = compile_prog_custom(code, path, config);

    Task::initialize_program(
        program,
        test_config(),
        RwLock::new(ObjectSpace::default()),
        Memory::default(),
        upvalues,
        call_outs,
        tx,
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
