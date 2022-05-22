use lpc_rs::compiler::Compiler;
use lpc_rs::interpreter::memory::Memory;
use lpc_rs::interpreter::object_space::ObjectSpace;
use lpc_rs::interpreter::program::Program;
use lpc_rs::interpreter::task::Task;
use lpc_rs::interpreter::task_context::TaskContext;
use lpc_rs::util::config::Config;
use lpc_rs::interpreter::MAX_CALL_STACK_SIZE;

pub fn test_config() -> Config {
    Config::default().with_lib_dir("./tests/fixtures/code")
}

pub fn compile_prog(code: &str) -> Program {
    let config = test_config();
    let compiler = Compiler::new(config.into());
    compiler
        .compile_string("/my_file.c", code)
        .expect("Failed to compile.")
}

pub fn run_prog(code: &str) -> (Task<MAX_CALL_STACK_SIZE>, TaskContext) {
    let mut task = Task::new(Memory::default());
    let program = compile_prog(code);
    let ctx = task
        .initialize_program(program, test_config(), ObjectSpace::default())
        .expect("failed to initialize");

    (task, ctx)
}
