use std::sync::Arc;

use clap::Parser;
use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcBank, heap::Heap, object_space::ObjectSpace, task::Task,
    },
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::ConfigBuilder;
use parking_lot::RwLock;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The file to compile and execute
    #[clap(value_parser, value_name = "FILE", value_hint = clap::ValueHint::DirPath)]
    filename: String,

    /// Use a specific configuration file
    #[clap(short, long, value_parser)]
    config: Option<String>,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let config = ConfigBuilder::default()
        .load_env(args.config)
        .build()
        .unwrap();

    let config = Arc::new(config);

    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();

    let lpc_path = LpcPath::new_server(&args.filename);

    let (tx, _rx) = tokio::sync::mpsc::channel(1024);
    let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));

    let upvalues = RwLock::new(GcBank::default());

    match compiler.compile_in_game_file(&lpc_path, None).await {
        Ok(program) => {
            let memory = Heap::default();
            let object_space = ObjectSpace::default();
            if let Err(e) = Task::<MAX_CALL_STACK_SIZE>::initialize_program(
                program,
                config,
                object_space,
                memory,
                upvalues,
                call_outs,
                tx,
            )
            .await
            {
                e.emit_diagnostics();
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", &args.filename, e),
    }
}
