use std::rc::Rc;

use clap::Parser;
use lpc_rs::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{memory::Memory, object_space::ObjectSpace, task::Task},
};
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_utils::config::ConfigBuilder;
use qcell::QCellOwner;
use lpc_rs::interpreter::gc::gc_bank::GcBank;

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

    let config_override = args.config;

    let config = ConfigBuilder::default()
        .path(config_override)
        .lib_dir(args.lib_dir)
        .build()
        .unwrap();

    let config = Rc::new(config);

    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();

    let lpc_path = LpcPath::new_server(&args.filename);

    let mut cell_key = QCellOwner::new();

    let upvalues = cell_key.cell(GcBank::default());

    match compiler.compile_in_game_file(&lpc_path, None, &mut cell_key) {
        Ok(program) => {
            let memory = Memory::default();
            let object_space = ObjectSpace::default();
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&memory, upvalues);
            if let Err(e) =
                task.initialize_program(program, config, cell_key.cell(object_space), &mut cell_key)
            {
                e.emit_diagnostics();
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", &args.filename, e),
    }
}
