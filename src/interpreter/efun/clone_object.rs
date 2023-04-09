use std::rc::Rc;

use lpc_rs_core::lpc_path::LpcPath;
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
    util::keyable::Keyable,
};

fn load_master<const N: usize>(
    context: &mut EfunContext<N>,
    path: &str,
    cell_key: &mut QCellOwner,
) -> Result<Rc<QCell<Process>>> {
    let full_path = LpcPath::new_in_game(
        path,
        context.in_game_cwd(cell_key),
        &*context.config().lib_dir,
    );
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str, cell_key) {
        Some(proc) => Ok(proc),
        None => {
            let compiler = CompilerBuilder::default()
                .config(context.config())
                .build()?;

            match compiler.compile_in_game_file(&full_path, context.current_debug_span(), cell_key)
            {
                Ok(prog) => {
                    let Some(prog_function) = prog.initializer.clone() else {
                        return Err(LpcError::new("Init function not found on master?"));
                    };
                    let process: Rc<QCell<Process>> = cell_key.cell(Process::new(prog)).into();
                    context.insert_process(process.clone(), cell_key);

                    let new_context = context.clone_task_context().with_process(process.clone());
                    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(new_context);
                    task.eval(prog_function, &[], cell_key)?;

                    context.set_instruction_count(task.context.instruction_count())?;

                    Ok(process)
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
        let path = path.to_str();

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
        let Some(initializer) = new_prog.initializer.clone() else {
            return Err(LpcError::new("Init function not found on clone?"));
        };

        let new_clone = context.insert_clone(new_prog, cell_key);

        let new_context = context.clone_task_context().with_process(new_clone.clone());
        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(new_context);
        task.eval(initializer, &[], cell_key)?;

        context.set_instruction_count(task.context.instruction_count())?;

        // Set up the return value
        let v = LpcValue::Object(new_clone);
        let result = context.value_to_ref(v);

        context.return_efun_result(result);
    } else {
        return Err(context.runtime_error(format!(
            "invalid argument passed to `clone_object`: {}",
            arg.with_key(cell_key)
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
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            program::Program, task_context::TaskContext,
        },
        test_support::compile_prog,
    };

    fn task_context_fixture(
        program: Program,
        config: Rc<Config>,
        cell_key: &QCellOwner,
    ) -> TaskContext {
        let process = Process::new(program);

        let (tx, _) = tokio::sync::mpsc::channel();
        TaskContext::new(
            config,
            cell_key.cell(process),
            cell_key.cell(ObjectSpace::default()),
            Memory::new(10),
            cell_key.cell(GcBank::default()),
            Rc::new(cell_key.cell(CallOuts::new(tx.clone()))),
            tx,
            cell_key,
        )
    }

    #[test]
    fn does_not_create_multiple_master_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let mut cell_key = QCellOwner::new();
        let (program, config, _) = compile_prog(prog, &mut cell_key);
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, &cell_key);

        let mut task = Task::<10>::new(context.clone());
        task.eval(func.clone(), &[], &mut cell_key)
            .expect("first task failed");

        let mut task = Task::<10>::new(context);
        task.eval(func, &[], &mut cell_key)
            .expect("second task failed");

        // procs are /example, /example#0, /example#1
        assert_eq!(task.context.object_space().ro(&cell_key).len(), 3);
    }

    #[test]
    fn returns_error_if_no_clone() {
        let mut cell_key = QCellOwner::new();
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog, &mut cell_key);
        let func = program.initializer.clone().expect("no init found?");
        let mut cell_key = QCellOwner::new();
        let context = task_context_fixture(program, config, &cell_key);
        let mut task = Task::<10>::new(context);

        let result = task.eval(func, &[], &mut cell_key);

        assert_regex!(
            result.as_ref().unwrap_err().as_ref(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }
}
