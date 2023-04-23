#![forbid(unsafe_code)]

use clap::Parser;
use lpc_rs::interpreter::vm::Vm;
use lpc_rs_utils::config::ConfigBuilder;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Use a specific configuration file
    #[clap(short, long, value_parser)]
    env: Option<String>,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let config_override = args.env;

    let built = ConfigBuilder::default().load_env(config_override).build();

    let config = match built {
        Ok(c) => c,
        Err(e) => {
            e.emit_diagnostics();
            std::process::exit(1);
        }
    };

    config.init_tracing_subscriber();

    let mut vm = Vm::new(config);

    vm.boot().await.unwrap_or_else(|e| {
        eprintln!("unable to boot VM: {e:?}");
        std::process::exit(1);
    });
}
