use std::env;

use lpc_rs::{compiler::compile_file, errors, interpreter::asm_interpreter::AsmInterpreter};

const DEFAULT_FILE: &str = "local/mathfile.c";

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = args.get(1).map_or(DEFAULT_FILE, |name| name);

    match compile_file(filename) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::default();

            let c2 = compile_file("local/mathfile2.c").unwrap();
            let _ = interpreter.init_program(c2);

            // println!("{:?}", program);
            if let Err(e) = interpreter.init_program(program) {
                errors::emit_diagnostics(&[e]);
            }
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename, e),
    }
}
