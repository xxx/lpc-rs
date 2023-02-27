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

fn main() {
    let args = Args::parse();

    let config_override = args.config;

    let built = ConfigBuilder::default()
        .path(config_override)
        .build();

    let config = match built {
        Ok(c) => c,
        Err(e) => {
            e.emit_diagnostics();
            std::process::exit(1);
        }
    };

    if_chain! {
        if let Some(level) = config.driver_log_level();
        if let Some(file) = config.driver_log_file();
        then {
            match file {
                "STDOUT" => {
                    tracing::subscriber::set_global_default(
                        tracing_subscriber::fmt()
                            .with_max_level(level)
                            .with_writer(std::io::stdout)
                            .finish(),
                    )
                    .expect("setting tracing default failed");
                }
                _ => {
                    tracing::subscriber::set_global_default(
                        tracing_subscriber::fmt()
                            .with_max_level(level)
                            .with_writer(std::fs::File::create(file).unwrap())
                            .finish(),
                    )
                    .expect("setting tracing default failed");
                }
            }
        }
    }

    let mut vm = Vm::new(config);

    vm.initialize().unwrap_or_else(|e| {
        eprintln!("unable to initialize VM: {e:?}");
        std::process::exit(1);
    });
}
