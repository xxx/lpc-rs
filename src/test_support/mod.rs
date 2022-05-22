use crate::compiler::Compiler;
use crate::interpreter::memory::Memory;
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::program::Program;
use crate::interpreter::task::Task;
use crate::interpreter::task_context::TaskContext;
use crate::util::config::Config;
use crate::interpreter::MAX_CALL_STACK_SIZE;

/// Module for various test utilities that are shared among unit tests.

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
