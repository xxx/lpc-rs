use std::{cell::RefCell, rc::Rc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::Config;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::Compiler,
    interpreter::{
        memory::Memory, object_space::ObjectSpace, process::Process, program::Program, task::Task,
        task_context::TaskContext,
    },
};

pub mod factories;

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
    Config::default()
        .with_lib_dir("./tests/fixtures/code")
        .with_simul_efun_file(Some("/secure/simul_efuns"))
}

fn compile_simul_efuns(config: &Rc<Config>) -> Program {
    let compiler = Compiler::new(config.clone());
    let path = LpcPath::new_in_game(config.simul_efun_file().unwrap(), "/", config.lib_dir());
    compiler.compile_in_game_file(&path, None).unwrap()
}

pub fn compile_prog(code: &str) -> (Program, Rc<Config>, Rc<RefCell<Process>>) {
    let config = Rc::new(test_config());
    let simul_efuns = compile_simul_efuns(&config);
    let se_proc = Rc::new(RefCell::new(Process::new(simul_efuns)));

    let compiler = Compiler::new(config.clone()).with_simul_efuns(Some(se_proc.clone()));
    let path = LpcPath::new_in_game("/my_file.c", "/", config.lib_dir());
    let program = compiler
        .compile_string(path, code)
        .expect("Failed to compile.");

    (program, config, se_proc)
}

pub fn run_prog(code: &str) -> (Task<MAX_CALL_STACK_SIZE>, TaskContext) {
    let mut task = Task::new(Memory::default());
    let (program, config, se_proc) = compile_prog(code);

    let mut object_space = ObjectSpace::default();
    object_space.insert_process(se_proc);

    let ctx = task
        .initialize_program(program, config, object_space)
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            panic!("failed to initialize");
        });

    (task, ctx)
}
