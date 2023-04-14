use clap::Parser;
use if_chain::if_chain;
use lpc_rs::interpreter::vm::Vm;
use lpc_rs_utils::config::{Config, ConfigBuilder};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The directory to use as LIB_DIR for includes, etc. Defaults to what is
    /// contained in the config file.
    #[clap(short, long, value_parser, value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    lib_dir: Option<String>,

    /// Use a specific configuration file
    #[clap(short, long, value_parser)]
    config: Option<String>,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let config_override = args.config;

    let built = ConfigBuilder::default().load_env(config_override).build();

    let config = match built {
        Ok(c) => c,
        Err(e) => {
            e.emit_diagnostics();
            std::process::exit(1);
        }
    };

    println!("config: {:#?}", config);

    init_tracing_subscriber(&config);

    let mut vm = Vm::new(config);

    vm.boot().await.unwrap_or_else(|e| {
        eprintln!("unable to boot VM: {e:?}");
        std::process::exit(1);
    });
}

fn init_tracing_subscriber(config: &Config) {
    if_chain! {
        if let Some(level) = config.log_level;
        if let Some(file) = &config.log_file;
        then {
            match file.as_str() {
                "STDOUT" => {
                    tracing::subscriber::set_global_default(
                        tracing_subscriber::fmt()
                            .with_max_level(level)
                            // .with_env_filter("lpc_rs::interpreter::task=trace,[populate_upvalues]=trace")
                            .with_writer(std::io::stdout)
                            .finish(),
                    )
                    .expect("setting tracing default failed");
                }
                s => {
                    tracing::subscriber::set_global_default(
                        tracing_subscriber::fmt()
                            .with_max_level(level)
                            .with_writer(std::fs::File::create(s).unwrap())
                            .finish(),
                    )
                    .expect("setting tracing default failed");
                }
            }
        }
    }
}
