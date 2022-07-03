use lpc_rs::{
    compiler::Compiler,
    util::{config::Config, path_maker::LpcPath},
};

use if_chain::if_chain;
use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{memory::Memory, object_space::ObjectSpace, task::Task},
};
use std::rc::Rc;

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
    let compiler = Compiler::new(config.clone());
    let lpc_path = LpcPath::new_in_game(config.master_object(), "/", config.lib_dir());

    match compiler.compile_in_game_file(&lpc_path, None) {
        Ok(program) => {
            let memory = Memory::default();
            let object_space = ObjectSpace::default();
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&memory);
            if let Err(e) = task.initialize_program(program, config, object_space) {
                e.emit_diagnostics();
            }
        }
        Err(e) => {
            eprintln!(
                "unable to compile {}",
                lpc_path.as_server(config.lib_dir()).display(),
            );
            e.emit_diagnostics();
        }
    }
}
