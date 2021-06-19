use crate::{
    compiler::{compile_file, compiler_error::CompilerError},
    errors::LpcError,
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value, Result,
};
use refpool::PoolRef;
use std::cell::RefCell;

pub fn clone_object(interpreter: &mut AsmInterpreter) -> Result<()> {
    let frame = interpreter.stack.last().unwrap();

    let arg = interpreter.register_to_lpc_ref(1);

    if let LpcRef::String(s) = arg {
        let r = s.borrow();
        let path = try_extract_value!(*r, LpcValue::String);

        let master = match interpreter.processes.get(path) {
            Some(proc) => proc.clone(),
            None => {
                match compile_file(path) {
                    Ok(prog) => {
                        interpreter.init_program_with_clean_stack(prog)?
                    }
                    Err(e) => {
                        let debug_span = frame.process.current_debug_span();

                        let err = match e {
                            CompilerError::LpcError(x) => x,
                            // TODO: make this handle all of these errors
                            CompilerError::Collection(mut x) => x.swap_remove(0),
                        }
                        .with_span(debug_span);

                        return Err(err);
                    }
                }
            }
        };

        let mut new_clone = master.program.clone();

        let new_path = format!("{}#{}", path, interpreter.clone_count);
        new_clone.filename = new_path;
        interpreter.clone_count += 1;

        // Set up the return value
        let new_proc = interpreter.init_program_with_clean_stack(new_clone)?;
        let v = LpcValue::Object(new_proc);
        let result = LpcRef::Object(PoolRef::new(&interpreter.memory, RefCell::new(v)));

        interpreter.return_efun_result(result);
    } else {
        return Err(LpcError::new(format!(
            "Invalid argument passed to `clone_object`: {}",
            arg
        ))
        .with_span(frame.process.current_debug_span()));
    }

    Ok(())
}
