//! Module for various test utilities that are shared among unit tests.

use std::sync::Arc;

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use tokio::sync::mpsc::Receiver;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::{
        CompilerBuilder, compilation_context::CompilationContext, semantic::scope_tree::ScopeTree,
    },
    interpreter::{
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, task_template::TaskTemplate},
        vm::{global_state::GlobalState, vm_op::VmOp},
    },
    util::process_builder::process_insert_and_initialize_program,
};

pub mod factories;

// /// init() acts as a global test setup.
// #[ctor::ctor]
// fn init() {
// }

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

/// Make a rendered diagnostic machine-independent: the canonical lib_dir
/// prefix becomes the in-game `/`.
pub fn strip_lib_dir(rendered: &str) -> String {
    let canon = std::fs::canonicalize("./tests/fixtures/code").unwrap();
    rendered.replace(&canon.to_string_lossy().into_owned(), "")
}

async fn compile_simul_efuns(config: &Arc<Config>) -> Program {
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();
    let path = LpcPath::new_in_game(
        config.simul_efun_file.unwrap().as_str(),
        "/",
        &*config.lib_dir,
    );
    compiler.compile_in_game_file(&path, None).await.unwrap()
}

pub async fn compile_prog(code: &str) -> (Program, Arc<Config>, Arc<Process>) {
    let config = Arc::new(test_config());
    compile_prog_with_config(code, config).await
}

pub async fn compile_prog_with_config(
    code: &str,
    config: Arc<Config>,
) -> (Program, Arc<Config>, Arc<Process>) {
    let simul_efuns = compile_simul_efuns(&config).await;
    let se_proc = Arc::new(Process::new(simul_efuns));

    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .simul_efuns(Some(se_proc.clone()))
        .build()
        .unwrap();
    let path = LpcPath::new_in_game("/my_file.c", "/", &*config.lib_dir);
    let program = compiler
        .compile_string(path, code)
        .await
        .expect("Failed to compile.");

    (program, config, se_proc)
}

pub async fn run_prog(code: &str) -> Task<MAX_CALL_STACK_SIZE> {
    run_prog_with_config(code, Arc::new(test_config())).await
}

pub async fn run_prog_with_config(code: &str, config: Arc<Config>) -> Task<MAX_CALL_STACK_SIZE> {
    expect_initialized(try_run_prog_with_config(code, config).await)
}

/// `run_prog` without the panic: the initializer's error comes back.
pub async fn try_run_prog(code: &str) -> Result<Task<MAX_CALL_STACK_SIZE>> {
    try_run_prog_with_config(code, Arc::new(test_config())).await
}

pub async fn try_run_prog_with_config(
    code: &str,
    config: Arc<Config>,
) -> Result<Task<MAX_CALL_STACK_SIZE>> {
    run_prog_core(code, config).await.map(|(task, _vm_rx)| task)
}

/// `run_prog`, also handing back the VM inbox the run's timers post to.
pub async fn run_prog_with_vm_rx(code: &str) -> (Task<MAX_CALL_STACK_SIZE>, Receiver<VmOp>) {
    expect_initialized(run_prog_core(code, Arc::new(test_config())).await)
}

/// Compile `code` as `/my_file.c` with the simul efuns physically inserted,
/// then bootstrap it on a fresh `GlobalState`.
async fn run_prog_core(
    code: &str,
    config: Arc<Config>,
) -> Result<(Task<MAX_CALL_STACK_SIZE>, Receiver<VmOp>)> {
    let (program, config, se_proc) = compile_prog_with_config(code, config).await;

    let (tx, rx) = tokio::sync::mpsc::channel(128);
    let global_state = GlobalState::new(config, tx);
    ObjectSpace::insert_process_physical(&global_state.object_space, se_proc);

    initialize_program(program, global_state)
        .await
        .map(|task| (task, rx))
}

fn expect_initialized<T>(result: Result<T>) -> T {
    result.unwrap_or_else(|e| {
        e.emit_diagnostics();
        eprintln!("{:?}", e);
        panic!("failed to initialize");
    })
}

/// Bootstrap `program` the way the driver does (physical insert, then its
/// initializer in a fresh task), on a call stack of `N` frames.
pub async fn initialize_program<const N: usize>(
    program: impl Into<Arc<Program>>,
    global_state: impl Into<Arc<GlobalState>>,
) -> Result<Task<N>> {
    let process = Arc::new(Process::new(program.into()));
    process_insert_and_initialize_program(process, TaskTemplate::from(global_state.into())).await
}

/// A helper to make an empty [`CompilationContext`] with a single empty scope.
pub fn empty_compilation_context() -> CompilationContext {
    let mut scopes = ScopeTree::default();
    scopes.push_new();
    CompilationContext {
        scopes,
        ..CompilationContext::default()
    }
}
