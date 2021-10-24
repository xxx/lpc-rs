use std::env;

use lpc_rs::{
    compiler::Compiler, errors, util::config::Config,
};
use std::rc::Rc;
use lpc_rs::interpreter::task::Task;
use lpc_rs::interpreter::MAX_CALL_STACK_SIZE;
use lpc_rs::interpreter::memory::Memory;
use lpc_rs::interpreter::object_space::ObjectSpace;
use lpc_rs::util::path_maker::LpcPath;

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = Rc::new(Config::new(None::<&str>).unwrap());
    // println!("config {:?}", config);

    let compiler = Compiler::new(config.clone());

    let filename = args.get(1).map_or(DEFAULT_FILE, |name| name);
    let lpc_path = LpcPath::new_server(filename);

    match compiler.compile_in_game_file(&lpc_path, None) {
        Ok(program) => {
            let memory = Memory::default();
            let object_space = ObjectSpace::default();
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(&memory);
            if let Err(e) = task.initialize_program(program, config, object_space) {
                errors::emit_diagnostics(&[e]);
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename, e),
    }
}
