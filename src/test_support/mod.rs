use std::sync::Arc;

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use parking_lot::RwLock;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        call_outs::CallOuts,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{initialize_program::InitializeProgramBuilder, Task},
    },
};

pub mod factories;

/// Module for various test utilities that are shared among unit tests.

/// init() acts as a global test setup.
#[ctor::ctor]
fn init() {
    // console_subscriber::init();
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
    let (program, config, se_proc) = compile_prog(code).await;

    let object_space = ObjectSpace::default();
    let object_space: Arc<ObjectSpace> = object_space.into();
    let (tx, _rx) = tokio::sync::mpsc::channel(128);
    let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));
    ObjectSpace::insert_process(&object_space, se_proc);

    InitializeProgramBuilder::default()
        .program(program)
        .config(config)
        .object_space(object_space)
        .call_outs(call_outs)
        .tx(tx)
        .build()
        .await
        .unwrap_or_else(|e| {
            e.emit_diagnostics();
            eprintln!("{:?}", e);
            panic!("failed to initialize");
        })
}
