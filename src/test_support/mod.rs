//! Module for various test utilities that are shared among unit tests.

use std::{net::ToSocketAddrs, sync::Arc};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use tokio::sync::mpsc::{Receiver, UnboundedReceiver};

use async_trait::async_trait;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::{
        CompilerBuilder,
        ast::program_node::ProgramNode,
        codegen::{
            codegen_walker::CodegenWalker,
            function_prototype_walker::FunctionPrototypeWalker,
            inheritance_walker::InheritanceWalker,
            scope_walker::ScopeWalker,
            semantic_check_walker::SemanticCheckWalker,
            tree_walker::{ContextHolder, Pass, apply},
        },
        compilation_context::CompilationContext,
        semantic::scope_tree::ScopeTree,
    },
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{Task, task_template::TaskTemplate},
        vm::{Vm, global_state::GlobalState, vm_op::VmOp},
    },
    telnet::{connection::Connection, ops::ConnectionOp},
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

/// A connection bound to a process by [`connect`]; what the process is sent
/// arrives on `rx` after each commit.
pub struct Connected {
    /// The connection's outgoing operations.
    pub rx: UnboundedReceiver<ConnectionOp>,
    /// The connection itself.
    pub connection: Arc<Connection>,
}

/// Bind a fresh connection to `process` through the attach path.
pub async fn connect(vm: &Vm, process: &Arc<Process>) -> Connected {
    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    let connection = Arc::new(Connection::new(
        "127.0.0.1:23123"
            .to_socket_addrs()
            .expect("a literal address")
            .next()
            .expect("one address"),
        tx,
    ));
    vm.global_state
        .attach(connection.clone(), process.clone())
        .await;
    assert_eq!(
        rx.try_recv(),
        Ok(ConnectionOp::Attached),
        "attach announces the body"
    );
    Connected { rx, connection }
}

/// A master whose `valid_exec` allows everything.
pub async fn allow_exec(vm: &Vm) -> Arc<Process> {
    vm.global_state
        .initialize_process_from_code(
            "/secure/master.c",
            "int valid_exec(object caller, object new, object old) { return 1; }",
        )
        .await
        .expect("the master compiles")
        .context
        .process
}

/// A master that allows every load, inherit and include: what `run_prog`'s
/// programs load under.
pub const PERMISSIVE_MASTER: &str = "\
int valid_load(string path, string func, object caller, string program) { return 1; }
int valid_inherit(string path, string from) { return 1; }
int valid_read(string path, string func, object caller, string program) { return func == \"include\"; }
";

/// [`PERMISSIVE_MASTER`] at the default master path, inserted without an
/// initializer — it has no state.
pub async fn permissive_master(object_space: &ObjectSpace) -> Arc<Process> {
    object_space
        .create_process_from_code("/secure/master.c", PERMISSIVE_MASTER)
        .await
        .expect("the permissive master compiles")
}

pub fn test_config() -> Config {
    test_config_builder!().build().unwrap()
}

/// A [`Config`] whose lib is `root`, without simul efuns or a master.
pub fn temp_lib_config(root: &TempLib) -> Config {
    ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .build()
        .unwrap()
}

/// The string in `process`'s committed global `reg`; anything else panics.
pub fn committed_string(vm: &Vm, process: &Arc<Process>, reg: u16) -> String {
    match vm.global_state.committed_global(process, reg) {
        LpcRef::String(s) => s.to_str().to_owned(),
        other => panic!("a string in register {reg}: {other:?}"),
    }
}

async fn compile_simul_efuns(config: &Arc<Config>) -> Program {
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();
    let path = config.simul_efun_source().unwrap();
    compiler
        .compile_in_game_file(&path, None)
        .await
        .unwrap()
        .program
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
        .expect("Failed to compile.")
        .program;

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
    let global_state: Arc<GlobalState> = GlobalState::new(config, tx).into();
    // Inserted and initialized the way boot does it.
    process_insert_and_initialize_program::<MAX_CALL_STACK_SIZE>(
        se_proc,
        TaskTemplate::from(global_state.clone()),
    )
    .await?;
    permissive_master(&global_state.object_space).await;

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

/// Parse `code` as `/my_test.c` under the canonical test config.
async fn parse_test_program(code: &str) -> Result<(ProgramNode, CompilationContext)> {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .simul_efun_file("/secure/simul_efuns")
        .build()?;
    let compiler = CompilerBuilder::default().config(config).build()?;

    compiler
        .parse_string(
            &LpcPath::new_in_game("/my_test.c", "/", "./tests/fixtures/code"),
            code,
        )
        .await
}

/// Compile a source string through the real pipeline, up to and including
/// `Self` — every pass lenient, so recorded diagnostics stay inspectable.
#[async_trait]
pub trait CompileThrough: Pass {
    /// Run the pipeline prefix and `Self` over an already-parsed program.
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self>;

    /// Parse `code` as `/my_test.c`, then run the pipeline through `Self`.
    async fn compile_through(code: &str) -> Result<Self> {
        let (mut program, context) = parse_test_program(code).await?;
        Self::compile_through_parsed(&mut program, context).await
    }
}

#[async_trait]
impl CompileThrough for InheritanceWalker {
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self> {
        apply(program, context, false).await
    }
}

#[async_trait]
impl CompileThrough for FunctionPrototypeWalker {
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self> {
        let context = InheritanceWalker::compile_through_parsed(program, context)
            .await?
            .into_context();
        apply(program, context, false).await
    }
}

#[async_trait]
impl CompileThrough for ScopeWalker {
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self> {
        let context = FunctionPrototypeWalker::compile_through_parsed(program, context)
            .await?
            .into_context();
        apply(program, context, false).await
    }
}

#[async_trait]
impl CompileThrough for SemanticCheckWalker {
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self> {
        let context = ScopeWalker::compile_through_parsed(program, context)
            .await?
            .into_context();
        apply(program, context, false).await
    }
}

#[async_trait]
impl CompileThrough for CodegenWalker {
    async fn compile_through_parsed(
        program: &mut ProgramNode,
        context: CompilationContext,
    ) -> Result<Self> {
        let context = SemanticCheckWalker::compile_through_parsed(program, context)
            .await?
            .into_context();
        apply(program, context, false).await
    }
}

/// A uniquely named scratch directory (test name + pid + counter) for a
/// filesystem-backed fixture; removed on drop, panic included.
pub struct TempLib(std::path::PathBuf);

impl TempLib {
    pub fn new(name: &str) -> Self {
        // The counter keeps two tests that share a name from racing on
        // one directory.
        static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
        let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!("lpc-rs-{name}-{}-{n}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        Self(root)
    }
}

impl std::ops::Deref for TempLib {
    type Target = std::path::Path;

    fn deref(&self) -> &std::path::Path {
        &self.0
    }
}

impl Drop for TempLib {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[cfg(test)]
mod compile_through_tests {
    use super::*;

    #[tokio::test]
    async fn the_harness_compiles_through_codegen() {
        let walker = CodegenWalker::compile_through("int marf() { return 42; }")
            .await
            .unwrap();
        let program = walker.into_program().unwrap();
        assert!(program.functions.values().any(|f| f.name() == "marf"));
    }
}
