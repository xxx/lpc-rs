use clap::Parser;

use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::Compiler,
    interpreter::{memory::Memory, object_space::ObjectSpace, task::Task},
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::Config;
use std::rc::Rc;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The file to compile and execute
    #[clap(value_parser, value_name = "FILE", value_hint = clap::ValueHint::DirPath)]
    filename: String,

    /// The directory to use as LIB_DIR for includes, etc.
    #[clap(default_value_t = String::from("."), short, long, value_parser, value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    lib_dir: String,

    /// Use a specific configuration file
    #[clap(short, long, value_parser)]
    config: Option<String>,
}

fn main() {
    let args = Args::parse();

    let config_override = if let Some(config_path) = args.config {
        Some(config_path)
    } else {
        None
    };

    let config = Config::new(config_override)
        .unwrap()
        .with_lib_dir(args.lib_dir);

    let config = Rc::new(config);

    let compiler = Compiler::new(config.clone());

    let lpc_path = LpcPath::new_server(&args.filename);

    match compiler.compile_in_game_file(&lpc_path, None) {
        Ok(program) => {
            let memory = Memory::default();
            let object_space = ObjectSpace::default();
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&memory);
            if let Err(e) = task.initialize_program(program, config, object_space) {
                e.emit_diagnostics();
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", &args.filename, e),
    }
}
