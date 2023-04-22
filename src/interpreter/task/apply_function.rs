use std::sync::Arc;

use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_ref::LpcRef,
        process::Process,
        task::{task_template::TaskTemplate, Task},
        task_context::TaskContext,
    },
};

/// Apply function `f` in process `proc`, to arguments `args`, using context
/// information from `template`.
/// Returns the result of the function.
///
/// This function uses timed evaluation, and will timeout if execution takes too long.
///
/// # Arguments
///
/// * `f` - The [`ProgramFunction`] to apply.
/// * `args` - A slice of [`LpcRef`]s to apply the function to.
/// * `proc` - The [`Process`] to apply the function in.
/// * `template` - The [`TaskTemplate`] that holds the rest of the context information.
///
/// # Returns
///
/// * `Ok(LpcRef)` - The result of the function.
/// * `Err(LpcError)` - The error that occurred.
pub async fn apply_function(
    f: Arc<ProgramFunction>,
    args: &[LpcRef],
    proc: Arc<Process>,
    template: TaskTemplate,
) -> Result<LpcRef> {
    let ctx = TaskContext::from_template(template, proc);
    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(ctx);

    task.timed_eval(f, args)
        .await
        .map(|_| task.result().cloned().unwrap())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use parking_lot::RwLock;

    use super::*;
    use crate::{interpreter::call_outs::CallOuts, test_support::compile_prog};

    #[tokio::test]
    async fn test_apply_function() {
        let code = indoc! {"
            int my_cool_func(int x) {
                return x * 10;
            }
       "};
        let (prog, config, _proc) = compile_prog(code).await;
        let f = prog
            .unmangled_functions
            .get("my_cool_func")
            .unwrap()
            .clone();
        let process = Process::new(prog);
        let (tx, _rx) = tokio::sync::mpsc::channel(10);

        let template = TaskTemplate {
            config,
            object_space: Arc::new(Default::default()),
            vm_upvalues: Arc::new(Default::default()),
            call_outs: Arc::new(RwLock::new(CallOuts::new(tx.clone()))),
            memory: Arc::new(Default::default()),
            tx,
        };

        let args = vec![LpcRef::from(42)];
        // We could use `proc` as the process, but the language supports functions being applied
        // in different processes, so we'll use a new one. Note that this can lead to mismatches
        // with global variables, but that's the nature of the beast.
        let result = apply_function(f, &args, Arc::new(process), template)
            .await
            .unwrap();

        assert_eq!(result, LpcRef::from(420));
    }
}
