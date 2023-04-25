use std::sync::Arc;

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    compiler::CompilerBuilder,
    interpreter::{
        efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
        process::Process, task::Task,
    },
};

async fn load_master<const N: usize>(
    context: &mut EfunContext<'_, N>,
    path: &str,
) -> Result<Arc<Process>> {
    let full_path = LpcPath::new_in_game(path, context.in_game_cwd(), &*context.config().lib_dir);
    let path_str: &str = full_path.as_ref();

    match context.lookup_process(path_str) {
        Some(proc) => Ok(proc),
        None => {
            let compiler = CompilerBuilder::default()
                .config(context.config().clone())
                .build()?;

            match compiler
                .compile_in_game_file(&full_path, context.current_debug_span())
                .await
            {
                Ok(prog) => {
                    let Some(prog_function) = prog.initializer.clone() else {
                        return Err(LpcError::new("Init function not found on master?"));
                    };
                    let process: Arc<Process> = Process::new(prog).into();
                    context.insert_process(process.clone());

                    let new_context = context.clone_task_context().with_process(process);
                    let max_execution_time = context.config().max_execution_time;
                    let mut task = Task::<MAX_CALL_STACK_SIZE>::new(new_context);
                    task.timed_eval(prog_function, &[], max_execution_time).await?;

                    let process = task.context.process;

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
pub async fn clone_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1_usize);

    if let LpcRef::String(s) = arg {
        let path = {
            let path = s.read();
            // TODO: is there any way to avoid this clone? Added due to async
            path.to_string()
        };

        let master = load_master(context, &path).await?;

        {
            if master.program.pragmas.no_clone() {
                return Err(context.runtime_error(format!(
                    "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                    master.program.filename
                )));
            }
        }

        let new_prog = master.program.clone();
        let Some(initializer) = new_prog.initializer.clone() else {
            return Err(LpcError::new("Init function not found on clone?"));
        };

        let new_clone = context.insert_clone(new_prog);

        let new_context = context.clone_task_context().with_process(new_clone);
        let max_execution_time = context.config().max_execution_time;
        let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(new_context);
        task.timed_eval(initializer, &[], max_execution_time).await?;

        // Set up the return value
        let return_val = task.context.process;
        let v = Arc::downgrade(&return_val);
        let result = v.into_lpc_ref(context.memory());

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
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        assert_regex,
        interpreter::{
            call_outs::CallOuts,
            heap::Heap,
            program::Program,
            task_context::{TaskContext, TaskContextBuilder},
            vm::vm_op::VmOp,
        },
        test_support::compile_prog,
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        TaskContextBuilder::default()
            .config(config)
            .process(process)
            .memory(Heap::new(10))
            .call_outs(RwLock::new(CallOuts::new(tx.clone())))
            .tx(tx)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn does_not_create_multiple_master_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 300)
            .await
            .expect("first task failed");

        let mut task = Task::<10>::new(context);
        task.timed_eval(func, &[], 300)
            .await
            .expect("second task failed");

        // procs are /example, /example#0, /example#1
        assert_eq!(task.context.object_space().len(), 3);
    }

    #[tokio::test]
    async fn returns_error_if_no_clone() {
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let context = task_context_fixture(program, config, tx);
        let mut task = Task::<10>::new(context);

        let result = task.timed_eval(func, &[], 300).await;

        assert_regex!(
            result.as_ref().unwrap_err().as_ref(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }
}
