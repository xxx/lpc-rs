use std::rc::Rc;

use lpc_rs_core::{call_namespace::CallNamespace, lpc_path::LpcPath, INIT_PROGRAM};
use lpc_rs_errors::{LpcError, Result};
use qcell::{QCell, QCellOwner};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        efun::efun_context::EfunContext, lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
        task::Task,
    },
    try_extract_value,
};

fn load_master<const N: usize>(
    context: &mut EfunContext<N>,
    path: &str,
    cell_key: &mut QCellOwner,
) -> Result<Rc<QCell<Process>>> {
    let compiler = CompilerBuilder::default()
        .config(context.config())
        .build()?;

    let full_path = LpcPath::new_in_game(
        path,
        context.in_game_cwd(cell_key),
        &context.config().lib_dir,
    );
    // TODO: non-UTF8 filesystems could have problems here
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str, cell_key) {
        Some(proc) => Ok(proc),
        None => {
            match compiler.compile_in_game_file(&full_path, context.current_debug_span(), cell_key)
            {
                Ok(prog) => {
                    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context.memory());
                    let process: Rc<QCell<Process>> = cell_key.cell(Process::new(prog)).into();
                    context.insert_process(process.clone(), cell_key);
                    let function = {
                        let borrowed = process.ro(cell_key);
                        borrowed
                            .lookup_function(INIT_PROGRAM, &CallNamespace::Local)
                            .cloned()
                    };
                    match function {
                        Some(prog_function) => {
                            let new_context =
                                context.clone_task_context().with_process(process.clone());
                            let eval_context =
                                task.eval(prog_function, &[], new_context, cell_key)?;

                            context
                                .increment_instruction_count(eval_context.instruction_count())?;

                            Ok(process)
                        }
                        None => Err(LpcError::new("Init function not found on master?")),
                    }
                }
                Err(e) => {
                    let debug_span = context.current_debug_span();

                    Err(e.with_span(debug_span))
                }
            }
        }
    }
}

/// `clone_object`, the efun for creating new object instances.
pub fn clone_object<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    if let LpcRef::String(s) = arg {
        let r = s.borrow();
        let path = try_extract_value!(*r, LpcValue::String);

        let master = load_master(context, path, cell_key)?;

        {
            let borrowed = master.ro(cell_key);
            if borrowed.program.pragmas.no_clone() {
                return Err(context.runtime_error(format!(
                    "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                    borrowed.program.filename
                )));
            }
        }

        let new_prog = master.ro(cell_key).program.clone();

        let new_clone = context.insert_clone(new_prog, cell_key);

        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context.memory());
        {
            let function = {
                let borrowed = new_clone.ro(cell_key);
                borrowed
                    .lookup_function(INIT_PROGRAM, &CallNamespace::Local)
                    .cloned()
            };

            match function {
                Some(prog_function) => {
                    let new_context = context.clone_task_context().with_process(new_clone.clone());
                    let eval_context = task.eval(prog_function, &[], new_context, cell_key)?;
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
        return Err(
            context.runtime_error(format!("invalid argument passed to `clone_object`: {arg}"))
        );
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

    fn task_context_fixture(
        program: Program,
        config: Rc<Config>,
        cell_key: &QCellOwner,
    ) -> TaskContext {
        let process = Process::new(program);

        TaskContext::new(
            config,
            cell_key.cell(process),
            cell_key.cell(ObjectSpace::default()),
            cell_key,
        )
    }

    fn fixture<'pool>() -> Task<'pool, MAX_CALL_STACK_SIZE> {
        Task::new(Memory::new(10))
    }

    #[test]
    fn does_not_create_multiple_master_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let mut cell_key = QCellOwner::new();
        let (program, config, _) = compile_prog(prog, &mut cell_key);
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let context = task_context_fixture(program, config, &cell_key);
        let mut task = fixture();

        task.eval(func.clone(), &[], context.clone(), &mut cell_key)
            .expect("first task failed");
        task.eval(func, &[], context.clone(), &mut cell_key)
            .expect("second task failed");

        // procs are /example, /example#0, /example#1
        assert_eq!(context.object_space().ro(&cell_key).len(), 3);
    }

    #[test]
    fn returns_error_if_no_clone() {
        let mut cell_key = QCellOwner::new();
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog, &mut cell_key);
        let func = program
            .functions
            .get(INIT_PROGRAM)
            .expect("no init found?")
            .clone();
        let mut cell_key = QCellOwner::new();
        let context = task_context_fixture(program, config, &cell_key);
        let mut task = fixture();

        let result = task.eval(func, &[], context, &mut cell_key);

        assert_regex!(
            result.as_ref().unwrap_err().as_ref(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }
}
