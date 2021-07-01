use std::env;

use lpc_rs::{
    compiler::Compiler, errors, interpreter::asm_interpreter::AsmInterpreter, util::config::Config,
};
use std::rc::Rc;

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = Rc::new(Config::new(None::<&str>).unwrap());
    println!("config {:?}", config);

    let compiler = Compiler::new(config.clone());

    let filename = args.get(1).map_or(DEFAULT_FILE, |name| name);

    match compiler.compile_in_game_file(filename, "/", None) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::new(config);

            // println!("{:?}", program);
            if let Err(e) = interpreter.init_program(program) {
                errors::emit_diagnostics(&[e]);
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename, e),
    }
}
