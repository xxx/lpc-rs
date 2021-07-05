// use std::env;

use lpc_rs::{
    compiler::Compiler, errors, interpreter::asm_interpreter::AsmInterpreter, util::config::Config,
};
use std::rc::Rc;

fn main() {
    // let args: Vec<String> = env::args().collect();

    let config = match Config::new(None::<&str>) {
        Ok(c) => c,
        Err(e) => {
            errors::emit_diagnostics(&[e]);
            std::process::exit(1);
        }
    };

    let config = Rc::new(config);

    let compiler = Compiler::new(config.clone());

    let filename = config.master_object();

    match compiler.compile_in_game_file(filename, "/", None) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::new(config);

            // println!("{:?}", program);
            if let Err(e) = interpreter.init_program(program) {
                errors::emit_diagnostics(&[e]);
            }

            println!("procs {:?}", interpreter.processes);
            let master = interpreter
                .processes
                .get("/secure/master.c")
                .unwrap()
                .clone();
            let ob = interpreter.apply(master, "thing", &Vec::new());
            println!("ob??? {:?}", ob);
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename, e),
    }
}
