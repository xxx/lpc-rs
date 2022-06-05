use std::rc::Rc;
use crate::{
    compiler::Compiler,
    interpreter::{
        memory::Memory, object_space::ObjectSpace, program::Program, task::Task,
        task_context::TaskContext, MAX_CALL_STACK_SIZE,
    },
    util::config::Config,
};
use crate::util::path_maker::LpcPath;

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

pub fn compile_prog(code: &str) -> (Program, Rc<Config>) {
    let config = Rc::new(test_config());
    let compiler = Compiler::new(config.clone());
    let path = LpcPath::new_in_game("/my_file.c", "/", config.lib_dir());
    let program = compiler
        .compile_string(path, code)
        .expect("Failed to compile.");

    println!("filename: {:?}", program.filename);

    (program, config)
}

pub fn run_prog(code: &str) -> (Task<MAX_CALL_STACK_SIZE>, TaskContext) {
    let mut task = Task::new(Memory::default());
    let (program, config) = compile_prog(code);
    let ctx = task
        .initialize_program(program, config, ObjectSpace::default())
        .expect("failed to initialize");

    (task, ctx)
}
