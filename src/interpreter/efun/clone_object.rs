use crate::{
    compiler::{compiler_error::CompilerError, Compiler},
    errors::LpcError,
    interpreter::{
        lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
    },
    try_extract_value,
    util::path_maker::LpcPath,
    Result,
};
use std::{cell::RefCell, rc::Rc};
use crate::interpreter::efun::efun_context::EfunContext;
use crate::interpreter::function_evaluator::FunctionEvaluator;
use crate::codegen::codegen_walker::INIT_PROGRAM;
use crate::interpreter::MAX_CALL_STACK_SIZE;
use crate::interpreter::task_context::TaskContext;

fn load_master(context: &mut EfunContext, path: &str) -> Result<Rc<RefCell<Process>>> {
    let compiler = Compiler::new(context.config());

    let full_path = LpcPath::new_in_game_with_cwd(path, context.in_game_cwd()?);
    println!("full path in load_master {:?} {:?}", full_path, context.in_game_cwd()?);
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str) {
        Some(proc) => Ok(proc.clone()),
        None => {
            println!("load master path {:?}", full_path);
            match compiler
                .compile_in_game_file(full_path, context.current_debug_span())
            {
                Ok(prog) => {
                    let mut evaluator: FunctionEvaluator<MAX_CALL_STACK_SIZE> = FunctionEvaluator::new(context.memory());
                    let process: Rc<RefCell<Process>> = Process::new(prog).into();
                    context.insert_process(process.clone());
                    let borrowed = process.borrow();
                    let function = borrowed.lookup_function(INIT_PROGRAM);
                    match function {
                        Some(prog_function) => {
                            let new_context = context.clone_task_context().with_process(process.clone());
                            evaluator.eval(
                                prog_function.clone(),
                                &[],
                                new_context,
                            )?;

                            Ok(process.clone())
                        }
                        None => Err(LpcError::new("Init function not found on master?"))
                    }
                }
                Err(e) => {
                    let debug_span = context.current_debug_span();

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
pub fn clone_object(context: &mut EfunContext) -> Result<()> {
    let arg = context.resolve_lpc_ref(1_usize);

    if let LpcRef::String(s) = arg {
        let r = s.borrow();
        let path = try_extract_value!(*r, LpcValue::String);

        let master = load_master(context, path)?;

        {
            let borrowed = master.borrow();
            if borrowed.program.pragmas.no_clone() {
                return Err(context.runtime_error(format!(
                    "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                    borrowed.program.filename
                )));
            }
        }

        let new_prog = master.borrow().program.clone();
        let new_clone: Rc<RefCell<Process>> = Process::new(new_prog).into();
        context.insert_process(new_clone.clone());

        let mut evaluator: FunctionEvaluator<MAX_CALL_STACK_SIZE> = FunctionEvaluator::new(context.memory());
        {
            let borrowed = new_clone.borrow();
            let function = borrowed.lookup_function(INIT_PROGRAM);

            match function {
                Some(prog_function) => {
                    let new_context = context.clone_task_context().with_process(new_clone.clone());
                    evaluator.eval(
                        prog_function.clone(),
                        &[],
                        new_context,
                    )?;
                }
                None => return Err(LpcError::new("Init function not found in clone?"))
            }
        }

        // Set up the return value
        let v = LpcValue::Object(new_clone);
        let result = context.value_to_ref(v);

        context.return_efun_result(result);
    } else {
        return Err(context.runtime_error(format!(
            "invalid argument passed to `clone_object`: {}",
            arg
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::{program::Program},
        util::config::Config,
    };
    use crate::interpreter::memory::Memory;
    use crate::interpreter::task_context::TaskContext;
    use indoc::indoc;
    use crate::interpreter::object_space::ObjectSpace;

    fn compile_prog(code: &str, config: Rc<Config>) -> Program {
        let compiler = Compiler::new(config);
        compiler
            .compile_string("./my_file.c", code)
            .expect("Failed to compile.")
    }

    fn task_context_fixture(program: Program, config: Rc<Config>) -> TaskContext {
        let process = Process::new(program);

        TaskContext::new(config, process, ObjectSpace::default())
    }

    fn fixture<'pool>() -> FunctionEvaluator<'pool, MAX_CALL_STACK_SIZE> {
        FunctionEvaluator::new(Memory::new(10))
    }

    #[test]
    fn does_not_create_multiple_master_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let config: Rc<Config> = Config::new(None::<&str>).unwrap().with_lib_dir("./tests/fixtures/code").into();

        let program = compile_prog(prog, config.clone());
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config);
        let mut task = fixture();

        let result = task.eval(func, &[], context.clone());

        assert_eq!(context.object_space().borrow().len(), 4);

        println!("result \n\n {:?} \n\n {:?}", result, task);
        //
        // let sym = ProgramFunction::new("clone_object", 1, 0);
        //
        // let mut frame = StackFrame::new(interpreter.process.clone(), Rc::new(sym));
        //
        // let path = value_to_ref!(LpcValue::from("./example"), &interpreter.memory);
        // frame.registers[1] = path;
        //
        // interpreter
        //     .push_frame(frame.clone())
        //     .expect("stack overflow");
        // assert!(clone_object(&mut interpreter).is_ok());
        // interpreter.pop_frame();
        //
        // interpreter.push_frame(frame).expect("stack overflow");
        // assert!(clone_object(&mut interpreter).is_ok());
        //
        // // procs are empty.c, example.c, example.c#0, example.c#1
        // assert_eq!(interpreter.processes.len(), 4);
    }

    // #[test]
    // fn returns_error_if_no_clone() {
    //     let mut interpreter = fixture();
    //
    //     let sym = ProgramFunction::new("clone_object", 1, 0);
    //
    //     let mut frame = StackFrame::new(interpreter.process.clone(), Rc::new(sym));
    //
    //     let path = value_to_ref!(LpcValue::from("./no_clone.c"), &interpreter.memory);
    //     frame.registers[1] = path;
    //
    //     interpreter.push_frame(frame).expect("stack overflow");
    //
    //     let re =
    //         Regex::new(r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\.")
    //             .unwrap();
    //
    //     assert!(re.is_match(&clone_object(&mut interpreter).unwrap_err().to_string()));
    // }
}
