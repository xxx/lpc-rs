use std::rc::Rc;

use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
        program::Program, task::Task, task_context::TaskContext,
    },
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use qcell::QCellOwner;

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

pub fn compile_prog_custom<P>(
    code: &str,
    path: P,
    config: Config,
    cell_key: &mut QCellOwner,
) -> Program
where
    P: Into<LpcPath>,
{
    let compiler = CompilerBuilder::default().config(config).build().unwrap();
    compiler
        .compile_string(path, code, cell_key)
        .expect("Failed to compile.")
}

pub fn run_prog_custom<P>(
    code: &str,
    path: P,
    config: Config,
    cell_key: &mut QCellOwner,
) -> (Task<MAX_CALL_STACK_SIZE>, TaskContext)
where
    P: Into<LpcPath>,
{
    let upvalues = cell_key.cell(GcBank::default());
    let (tx, _) = std::sync::mpsc::channel();
    let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
    let mut task = Task::new(Memory::default(), upvalues);
    let program = compile_prog_custom(code, path, config, cell_key);
    let ctx = task
        .initialize_program(
            program,
            test_config(),
            cell_key.cell(ObjectSpace::default()),
            call_outs,
            tx,
            cell_key,
        )
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            panic!("failed to initialize");
        });

    (task, ctx)
}

pub fn run_prog(code: &str, cell_key: &mut QCellOwner) -> (Task<MAX_CALL_STACK_SIZE>, TaskContext) {
    run_prog_custom(code, "/my_file.c", test_config(), cell_key)
}
