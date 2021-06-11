use crate::interpreter::asm_interpreter::AsmInterpreter;
use crate::try_extract_value;
use crate::Result;
use crate::interpreter::lpc_ref::LpcRef;
use crate::errors::LpcError;
use crate::compiler::compile_file;
use crate::compiler::compiler_error::CompilerError;
use crate::interpreter::lpc_value::LpcValue;
use std::cell::RefCell;
use refpool::PoolRef;

pub fn clone_object(interpreter: &mut AsmInterpreter) -> Result<()> {
    let frame = interpreter.stack.last().unwrap();

    let lpc_ref = interpreter.register_to_lpc_ref(1);
    if let LpcRef::String(s) = lpc_ref {
        let r = s.borrow();
        let path = try_extract_value!(*r, LpcValue::String);

        // load and initialize the master if necessary
        if !interpreter.processes.contains_key(path) {
            match compile_file(path) {
                Ok(prog) => {
                    interpreter.init_program(prog)?;
                }
                Err(e) => {
                    let err = match e {
                        CompilerError::LpcError(x) => x,
                        // TODO: make this handle all of these errors
                        CompilerError::Collection(mut x) => x.swap_remove(0)
                    };
                    return Err(err);
                }
            }
        }

        let master = interpreter.processes.get(path).unwrap();
        let mut new_clone = master.program.clone();

        let new_path = format!("{}#{}", path, interpreter.clone_count);
        new_clone.filename = new_path;

        let new_proc = interpreter.init_program(new_clone)?;
        interpreter.clone_count += 1;

        let v = LpcValue::Object(new_proc);
        let result = LpcRef::Object(PoolRef::new(&interpreter.memory, RefCell::new(v)));

        interpreter.return_efun_result(result);

        // check if master is loaded
        // if not, load and initialize it first
        // clone the entire master program (not the Rc), then initialize the clone,
        // create an LpcRef for the object, and put that into r0
    } else {
        return Err(
            LpcError::new(format!("Invalid argument passed to `clone_object`: {}", lpc_ref)
        ).with_span(frame.process.current_debug_span()));
    }

    Ok(())
}
