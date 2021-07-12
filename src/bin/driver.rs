// use std::env;

use lpc_rs::{
    compiler::Compiler,
    errors,
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    util::{config::Config, path_maker::LpcPath},
    value_to_ref,
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

    let lpc_path = LpcPath::new_in_game(config.master_object());
    let filename = lpc_path.as_server(config.lib_dir());

    match compiler.compile_in_game_file(&filename, None) {
        Ok(program) => {
            let mut interpreter = AsmInterpreter::new(config);

            // println!("{:?}", program);
            if let Err(e) = interpreter.init_master(program) {
                errors::emit_diagnostics(&[e]);
            }

            // println!("procs {:?}", interpreter.processes);
            // let master = interpreter
            //     .processes
            //     .get("/secure/master.c")
            //     .unwrap()
            //     .clone();
            // let mut mapping = HashMap::new();
            // mapping.insert(
            //     value_to_ref!(LpcValue::from("foo"), interpreter.memory),
            //     value_to_ref!(LpcValue::from("bar"), interpreter.memory),
            // );
            // mapping.insert(
            //     value_to_ref!(LpcValue::from("baz"), interpreter.memory),
            //     value_to_ref!(
            //         LpcValue::from(vec![
            //             LpcRef::Int(12938),
            //             value_to_ref!(LpcValue::from("a str"), interpreter.memory)
            //         ]),
            //         interpreter.memory
            //     ),
            // );
            //
            // let args = vec![
            //     value_to_ref!(LpcValue::Int(69), interpreter.memory),
            //     value_to_ref!(LpcValue::from("marfin"), interpreter.memory),
            //     value_to_ref!(LpcValue::Object(master.clone()), interpreter.memory),
            //     value_to_ref!(LpcValue::Mapping(mapping), interpreter.memory),
            // ];
            // let ob = interpreter.apply(master, "thing", &args);
            // println!("ob??? {:?}", ob);
        }
        Err(e) => eprintln!("unable to compile {}: {:?}", filename.display(), e),
    }
}
