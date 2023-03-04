use qcell::QCellOwner;
use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        memory::Memory, object_space::ObjectSpace, program::Program, task::Task,
        task_context::TaskContext,
    },
};
use lpc_rs_utils::config::{Config, ConfigBuilder};

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
    ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .build()
        .unwrap()
}

pub fn compile_prog(code: &str, cell_key: &mut QCellOwner) -> Program {
    let config = test_config();
    let compiler = CompilerBuilder::default().config(config).build().unwrap();
    compiler
        .compile_string("/my_file.c", code, cell_key)
        .expect("Failed to compile.")
}

pub fn run_prog<'a>(code: &str, cell_key: &mut QCellOwner) -> (Task<'a, MAX_CALL_STACK_SIZE>, TaskContext) {
    let mut task = Task::new(Memory::default());
    let program = compile_prog(code, cell_key);
    let ctx = task
        .initialize_program(program, test_config(), cell_key.cell(ObjectSpace::default()), cell_key)
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            panic!("failed to initialize");
        });

    (task, ctx)
}
