#![forbid(unsafe_code)]

use std::sync::Arc;

use clap::Parser;
use lpc_rs::{interpreter::vm::Vm};
use lpc_rs::util::process_builder::ProcessInitializer;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::ConfigBuilder;

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
        .await
        .build()
        .unwrap();

    let config = Arc::new(config);

    let lpc_path = LpcPath::new_server(&args.filename);

    let vm = Vm::new(config);
    vm.process_initialize_from_path(&lpc_path)
        .await
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
}
