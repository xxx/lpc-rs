use crate::{
    compiler::{compiler_error::CompilerError, Compiler},
    errors::LpcError,
    interpreter::{
        asm_interpreter::AsmInterpreter, lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
    },
    try_extract_value,
    util::path_maker::LpcPath,
    value_to_ref, Result,
};
use refpool::PoolRef;
use std::{cell::RefCell, rc::Rc};

fn load_master(interpreter: &mut AsmInterpreter, path: &str) -> Result<Rc<Process>> {
    let frame = interpreter.stack.last().unwrap();
    let compiler = Compiler::new(interpreter.config.clone());

    let full_path = LpcPath::new_in_game_with_cwd(path, interpreter.in_game_cwd()?);
    let path_str: &str = full_path.as_ref();

    match interpreter.lookup_process(path_str) {
        Ok(proc) => Ok(proc.clone()),
        Err(_) => {
            match compiler.compile_in_game_file(full_path, interpreter.process.current_debug_span())
            {
                Ok(prog) => {
                    let closure = |interpreter: &mut AsmInterpreter| {
                        let process = interpreter.load_master(prog);
                        let init_result = interpreter.init();

                        match init_result {
                            Ok(_) => Ok(process),
                            Err(e) => Err(e),
                        }
                    };
                    interpreter.with_clean_stack(closure)
                }
                Err(e) => {
                    let debug_span = frame.process.current_debug_span();

                    let err = match e {
                        CompilerError::LpcError(x) => x,
                        // TODO: make this handle all of these errors
                        CompilerError::Collection(mut x) => x.swap_remove(0),
                    }
                    .with_span(debug_span);

                    Err(err)
                }
            }
        }
    }
}

/// `clone_object`, the efun for creating new object instances.
pub fn clone_object(interpreter: &mut AsmInterpreter) -> Result<()> {
    let arg = interpreter.register_to_lpc_ref(1);

    if let LpcRef::String(s) = arg {
        let r = s.borrow();
        let path = try_extract_value!(*r, LpcValue::String);

        let master = load_master(interpreter, path)?;

        if master.program.pragmas.no_clone() {
            return Err(LpcError::new(format!(
                "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                master.program.filename
            ))
            .with_span(interpreter.process.current_debug_span()));
        }

        let new_clone = master.program.clone();

        // Set up the return value
        let closure = |interpreter: &mut AsmInterpreter| {
            let process = interpreter.load_clone(new_clone);
            let init_result = interpreter.init();

            match init_result {
                Ok(_) => Ok(process),
                Err(e) => Err(e),
            }
        };
        let new_proc = interpreter.with_clean_stack(closure)?;
        let v = LpcValue::Object(new_proc);
        let result = value_to_ref!(v, &interpreter.memory);

        interpreter.return_efun_result(result);
    } else {
        let frame = interpreter.stack.last().unwrap();

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
    use crate::{
        interpreter::{program::Program, stack_frame::StackFrame},
        semantic::function_symbol::FunctionSymbol,
        util::config::Config,
    };
    use fs_err as fs;
    use regex::Regex;
    use std::rc::Rc;

    fn fixture() -> AsmInterpreter {
        let config = Config::new(None::<&str>).unwrap().with_lib_dir("./tests");

        let program = Program {
            instructions: vec![],
            filename: fs::canonicalize("./tests/fixtures/code/empty.c")
                .unwrap()
                .to_string_lossy()
                .to_string(),
            debug_spans: vec![],
            labels: Default::default(),
            functions: Default::default(),
            num_globals: 0,
            num_init_registers: 0,
            pragmas: Default::default(),
        };

        let mut interpreter = AsmInterpreter::new(config.into());
        interpreter.load_master(program);

        interpreter
    }

    #[test]
    fn does_not_create_multiple_master_objects() {
        let mut interpreter = fixture();

        let sym = FunctionSymbol {
            name: "clone_object".to_string(),
            num_args: 1,
            num_locals: 0,
            address: 0,
        };

        let mut frame = StackFrame::new(interpreter.process.clone(), Rc::new(sym), 0);

        let path = value_to_ref!(LpcValue::from("./example"), &interpreter.memory);
        frame.registers[1] = path;

        interpreter.push_frame(frame.clone()).expect("stack overflow");
        assert!(clone_object(&mut interpreter).is_ok());
        interpreter.pop_frame();

        interpreter.push_frame(frame).expect("stack overflow");
        assert!(clone_object(&mut interpreter).is_ok());

        // procs are empty.c, example.c, example.c#0, example.c#1
        assert_eq!(interpreter.processes.len(), 4);
    }

    #[test]
    fn returns_error_if_no_clone() {
        let mut interpreter = fixture();

        let sym = FunctionSymbol {
            name: "clone_object".to_string(),
            num_args: 1,
            num_locals: 0,
            address: 0,
        };

        let mut frame = StackFrame::new(interpreter.process.clone(), Rc::new(sym), 0);

        let path = value_to_ref!(LpcValue::from("./no_clone.c"), &interpreter.memory);
        frame.registers[1] = path;

        interpreter.push_frame(frame).expect("stack overflow");

        let re =
            Regex::new(r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\.")
                .unwrap();

        assert!(re.is_match(&clone_object(&mut interpreter).unwrap_err().to_string()));
    }
}
