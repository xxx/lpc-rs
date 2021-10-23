use lpc_rs::{
    compiler::Compiler,
    errors,
    util::{config::Config, path_maker::LpcPath},
};

use std::rc::Rc;
use lpc_rs::interpreter::function_evaluator::FunctionEvaluator;
use lpc_rs::interpreter::MAX_CALL_STACK_SIZE;
use lpc_rs::interpreter::memory::Memory;
use lpc_rs::interpreter::object_space::ObjectSpace;


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
    let lpc_path = LpcPath::new_in_game(config.master_object(), "/", config.lib_dir());

    match compiler.compile_in_game_file(&lpc_path, None) {
        Ok(program) => {
            let memory = Memory::default();
            let object_space = ObjectSpace::default();
            let mut task: FunctionEvaluator<MAX_CALL_STACK_SIZE> = FunctionEvaluator::new(&memory);
            if let Err(e) = task.initialize_program(program, config, object_space) {
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
        Err(e) => eprintln!("unable to compile {}: {:?}", lpc_path.as_server(config.lib_dir()).display(), e),
    }
}
