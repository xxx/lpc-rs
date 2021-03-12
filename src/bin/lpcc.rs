use std::{env};

use lpc_rs::{compiler::compile_file, errors, interpreter::asm_interpreter::AsmInterpreter};

const DEFAULT_FILE: &str = "mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if let Some(name) = args.get(1) {
        name
    } else {
        DEFAULT_FILE
    };

    match compile_file(filename) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::default();

            // println!("{:?}", program);
            interpreter.load(program);

            if let Err(e) = interpreter.exec() {
                errors::emit_diagnostics(&[e]);
            }
        }
        Err(e) => panic!("unable to compile {}: {:?}", filename, e),
    }
}
