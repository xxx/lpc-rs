use if_chain::if_chain;
use std::rc::Rc;
use lpc_rs::{
    util::{config::Config, path_maker::LpcPath},
};
use lpc_rs::interpreter::vm::Vm;

fn main() {
    // let args: Vec<String> = env::args().collect();

    let config = match Config::new(None::<&str>) {
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

    let config = Rc::new(config);
    let mut vm = Vm::new(config);

    vm.initialize().unwrap_or_else(|e| {
        eprintln!("unable to initialize VM: {:?}", e);
        std::process::exit(1);
    });
}
