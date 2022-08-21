use std::{cell::RefCell, rc::Rc};

use lpc_rs_core::{call_namespace::CallNamespace, lpc_path::LpcPath, INIT_PROGRAM};
use lpc_rs_errors::{LpcError, Result};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::Compiler,
    interpreter::{
        efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
        task::Task,
    },
    try_extract_value,
};

fn load_master<const N: usize>(
    context: &mut EfunContext<N>,
    path: &str,
) -> Result<Rc<RefCell<Process>>> {
    let compiler = Compiler::new(context.config());

    let full_path = LpcPath::new_in_game(path, context.in_game_cwd(), context.config().lib_dir());
    // TODO: non-UTF8 filesystems could have problems here
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str) {
        Some(proc) => Ok(proc),
        None => match compiler.compile_in_game_file(&full_path, context.current_debug_span()) {
            Ok(prog) => {
                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context.memory());
                let process: Rc<RefCell<Process>> = Process::new(prog).into();
                context.insert_process(process.clone());
                let borrowed = process.borrow();
                let function = borrowed.lookup_function(INIT_PROGRAM, &CallNamespace::Local);
                match function {
                    Some(prog_function) => {
                        let new_context =
                            context.clone_task_context().with_process(process.clone());
                        let eval_context = task.eval(prog_function.clone(), &[], new_context)?;

                        context.increment_instruction_count(eval_context.instruction_count())?;

                        Ok(process.clone())
                    }
                    None => Err(LpcError::new("Init function not found on master?")),
                }
            }
            Err(e) => {
                let debug_span = context.current_debug_span();

                Err(e.with_span(debug_span))
            }
        },
    }
}

/// `clone_object`, the efun for creating new object instances.
pub fn clone_object<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
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
            let function = borrowed.lookup_function(INIT_PROGRAM, &CallNamespace::Local);

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
    use indoc::indoc;
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        assert_regex,
        interpreter::{
            memory::Memory, object_space::ObjectSpace, program::Program, task_context::TaskContext,
        },
        test_support::compile_prog,
    };

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

        let (program, config, _) = compile_prog(prog);
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config.clone());
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

        let (program, config, _) = compile_prog(prog);
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config);
        let mut task = fixture();

        let result = task.eval(func.clone(), &[], context.clone());

        assert_regex!(
            &result.as_ref().unwrap_err().to_string(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }
}
