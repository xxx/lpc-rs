use crate::{
    compiler::{compile_file, compiler_error::CompilerError},
    errors::LpcError,
    interpreter::{asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue},
    try_extract_value, Result,
};
use refpool::PoolRef;
use std::cell::RefCell;

/// `clone_object`, the efun for creating new object instances.
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
                    Ok(prog) => interpreter.init_program_with_clean_stack(prog)?,
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

        if master.program.pragmas.no_clone() {
            return Err(LpcError::new(format!(
                "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                master.program.filename
            ))
            .with_span(interpreter.process.current_debug_span()));
        }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{interpreter::stack_frame::StackFrame, semantic::function_symbol::FunctionSymbol};
    use std::rc::Rc;

    #[test]
    fn returns_error_if_no_clone() {
        let mut interpreter = AsmInterpreter::default();
        let sym = FunctionSymbol {
            name: "clone_object".to_string(),
            num_args: 1,
            num_locals: 0,
            address: 0,
        };

        let mut frame = StackFrame::new(interpreter.process.clone(), Rc::new(sym), 0);

        let path = LpcRef::String(PoolRef::new(
            &interpreter.memory,
            RefCell::new(LpcValue::from("./tests/fixtures/code/no_clone.c")),
        ));
        frame.registers[1] = path;

        interpreter.push_frame(frame);

        assert_eq!(
            &clone_object(&mut interpreter).unwrap_err().to_string(),
            "./tests/fixtures/code/no_clone.c has `#pragma no_clone` enabled, and so cannot be cloned."
        )
    }
}
