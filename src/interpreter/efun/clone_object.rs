use crate::{
    codegen::codegen_walker::INIT_PROGRAM,
    compiler::{compiler_error::CompilerError, Compiler},
    errors::LpcError,
    interpreter::{
        efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
        task::Task, MAX_CALL_STACK_SIZE,
    },
    try_extract_value,
    util::path_maker::LpcPath,
    Result,
};
use std::{cell::RefCell, rc::Rc};

fn load_master(context: &mut EfunContext, path: &str) -> Result<Rc<RefCell<Process>>> {
    let compiler = Compiler::new(context.config());

    let full_path = LpcPath::new_in_game(path, context.in_game_cwd()?, context.config().lib_dir());
    // TODO: non-UTF8 filesystems could have problems here
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str) {
        Some(proc) => Ok(proc),
        None => {
            match compiler.compile_in_game_file(&full_path, context.current_debug_span()) {
                Ok(prog) => {
                    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context.memory());
                    let process: Rc<RefCell<Process>> = Process::new(prog).into();
                    context.insert_process(process.clone());
                    let borrowed = process.borrow();
                    let function = borrowed.lookup_function(INIT_PROGRAM);
                    match function {
                        Some(prog_function) => {
                            let new_context =
                                context.clone_task_context().with_process(process.clone());
                            let eval_context =
                                task.eval(prog_function.clone(), &[], new_context)?;

                            context
                                .increment_instruction_count(eval_context.instruction_count())?;

                            Ok(process.clone())
                        }
                        None => Err(LpcError::new("Init function not found on master?")),
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

        let new_clone = context.insert_clone(new_prog);

        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context.memory());
        {
            let borrowed = new_clone.borrow();
            let function = borrowed.lookup_function(INIT_PROGRAM);

            match function {
                Some(prog_function) => {
                    let new_context = context.clone_task_context().with_process(new_clone.clone());
                    let eval_context = task.eval(prog_function.clone(), &[], new_context)?;
                    context.increment_instruction_count(eval_context.instruction_count())?;
                }
                None => return Err(LpcError::new("Init function not found in clone?")),
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
        interpreter::{
            memory::Memory, object_space::ObjectSpace, program::Program, task_context::TaskContext,
        },
        util::config::Config,
    };
    use indoc::indoc;
    use regex::Regex;

    fn compile_prog(code: &str, config: Rc<Config>) -> Program {
        let compiler = Compiler::new(config);
        compiler
            .compile_string("./tests/fixtures/code/my_file.c", code)
            .expect("Failed to compile.")
    }

    fn task_context_fixture(program: Program, config: Rc<Config>) -> TaskContext {
        let process = Process::new(program);

        TaskContext::new(config, process, ObjectSpace::default())
    }

    fn fixture<'pool>() -> Task<'pool, MAX_CALL_STACK_SIZE> {
        Task::new(Memory::new(10))
    }

    #[test]
    fn does_not_create_multiple_master_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let config: Rc<Config> = Config::new(None::<&str>)
            .unwrap()
            .with_lib_dir("./tests/fixtures/code")
            .into();

        let program = compile_prog(prog, config.clone());
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config);
        let mut task = fixture();

        let _result1 = task.eval(func.clone(), &[], context.clone());
        let _result2 = task.eval(func, &[], context.clone());

        // procs are /example, /example#0, /example#1
        assert_eq!(context.object_space().borrow().len(), 3);
    }

    #[test]
    fn returns_error_if_no_clone() {
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let config: Rc<Config> = Config::new(None::<&str>)
            .unwrap()
            .with_lib_dir("./tests/fixtures/code")
            .into();

        let program = compile_prog(prog, config.clone());
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config);
        let mut task = fixture();

        let result = task.eval(func.clone(), &[], context.clone());

        let re =
            Regex::new(r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\.")
                .unwrap();

        assert!(re.is_match(&result.unwrap_err().to_string()));
    }
}
