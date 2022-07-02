use lpc_rs::{
    compiler::Compiler,
    interpreter::{
        memory::Memory, object_space::ObjectSpace, program::Program, task::Task,
        task_context::TaskContext,
    },
    util::config::Config,
};
use lpc_rs::compile_time_config::MAX_CALL_STACK_SIZE;

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
