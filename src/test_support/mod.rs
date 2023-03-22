use std::rc::Rc;

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use qcell::{QCell, QCellOwner};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace, process::Process,
        program::Program, task::Task, task_context::TaskContext,
    },
};

pub mod factories;

/// Module for various test utilities that are shared among unit tests.

/// init() acts as a global test setup.
#[ctor::ctor]
fn init() {
    tracing::subscriber::set_global_default(
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_writer(std::io::stdout)
            // .with_env_filter("lpc_rs::interpreter::vm=trace")
            // .with_env_filter("lpc_rs::interpreter::task=trace,[populate_upvalues]=trace")
            .finish(),
    )
    .expect("setting tracing default failed");
}

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

#[macro_export]
macro_rules! test_config_builder {
    () => {
        ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .simul_efun_file("/secure/simul_efuns")
    };
}

pub fn test_config() -> Config {
    test_config_builder!().build().unwrap()
}

fn compile_simul_efuns(config: &Rc<Config>, cell_key: &mut QCellOwner) -> Program {
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();
    let path = LpcPath::new_in_game(
        config.simul_efun_file.as_ref().unwrap(),
        "/",
        &config.lib_dir,
    );
    compiler
        .compile_in_game_file(&path, None, cell_key)
        .unwrap()
}

pub fn compile_prog(
    code: &str,
    cell_key: &mut QCellOwner,
) -> (Program, Rc<Config>, Rc<QCell<Process>>) {
    let config = Rc::new(test_config());
    let simul_efuns = compile_simul_efuns(&config, cell_key);
    let se_proc = Rc::new(cell_key.cell(Process::new(simul_efuns)));

    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .simul_efuns(Some(se_proc.clone()))
        .build()
        .unwrap();
    let path = LpcPath::new_in_game("/my_file.c", "/", &config.lib_dir);
    let program = compiler
        .compile_string(path, code, cell_key)
        .expect("Failed to compile.");

    (program, config, se_proc)
}

pub fn run_prog<'pool>(
    code: &str,
    cell_key: &mut QCellOwner,
) -> (Task<'pool, MAX_CALL_STACK_SIZE>, TaskContext) {
    let upvalues = GcBank::default();
    let mut task = Task::new(Memory::default(), cell_key.cell(upvalues));
    let (program, config, se_proc) = compile_prog(code, cell_key);

    let object_space = ObjectSpace::default();
    let object_space: Rc<QCell<ObjectSpace>> = cell_key.cell(object_space).into();
    ObjectSpace::insert_process(&object_space, se_proc, cell_key);

    let ctx = task
        .initialize_program(program, config, object_space, cell_key)
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            eprintln!("{:?}", e);
            panic!("failed to initialize");
        });

    (task, ctx)
}
