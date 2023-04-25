use std::sync::Arc;

use lpc_rs_errors::{lpc_error, Result};
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_ref::LpcRef,
        process::Process,
        task::{into_task_context::IntoTaskContext, task_template::TaskTemplate, Task},
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
/// * `template` - The template that holds the rest of the context information.
/// * `timeout` - The maximum amount of time to allow the function to execute, in milliseconds.
///
/// # Returns
///
/// * `Ok(LpcRef)` - The result of the function.
/// * `Err(LpcError)` - The error that occurred.
pub async fn apply_function<T>(
    f: Arc<ProgramFunction>,
    args: &[LpcRef],
    proc: Arc<Process>,
    template: T,
    timeout: Option<u64>,
) -> Result<LpcRef>
where
    T: IntoTaskContext,
{
    let ctx = template.into_task_context(proc);
    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(ctx);

    if let Some(timeout) = timeout {
        task.timed_eval(f, args, timeout)
            .await
            .map(|_| task.result().cloned().unwrap())
    } else {
        task.eval(f, args)
            .await
            .map(|_| task.result().cloned().unwrap())
    }
}

/// Apply function named `name`, in process `proc`, to arguments `args`, using context
/// information from `template`.
/// Returns the result of the function.
///
/// This function uses timed evaluation, and will timeout if execution takes too long.
///
/// # Arguments
///
/// * `name` - The name of the function to apply. This is assumed to be an unmangled name.
/// * `args` - A slice of [`LpcRef`]s to apply the function to.
/// * `proc` - The [`Process`] to apply the function in.
/// * `template` - The template that holds the rest of the context information.
/// * `timeout` - The maximum amount of time to allow the function to execute, in milliseconds.
///
/// # Returns
///
/// * `Some(Ok(LpcRef))` - The result of the function.
/// * `Some(Err(LpcError))` - The error that occurred.
/// * `None` - The function is not defined in `proc`.
pub async fn apply_function_by_name<S, T>(
    name: S,
    args: &[LpcRef],
    proc: Arc<Process>,
    template: T,
    timeout: Option<u64>,
) -> Option<Result<LpcRef>>
where
    S: AsRef<str>,
    T: IntoTaskContext,
{
    let Some(f) = proc.program.unmangled_functions.get(name.as_ref()) else {
        return None;
    };

    Some(apply_function(f.clone(), args, proc, template, timeout).await)
}

/// Apply function named `name`, in the master object, to arguments `args`, using context
/// information from `template`.
/// Returns the result of the function.
///
/// This function uses timed evaluation, and will timeout if execution takes too long.
///
/// # Arguments
///
/// * `name` - The name of the function to apply. This is assumed to be an unmangled name.
/// * `args` - A slice of [`LpcRef`]s to apply the function to.
/// * `template` - The [`TaskTemplate`] that holds the rest of the context information.
///
/// # Returns
///
/// * `Some(Ok(LpcRef))` - The result of the function.
/// * `Some(Err(LpcError))` - The error that occurred.
/// * `None` - The function is not defined in the master object.
pub async fn apply_function_in_master<S>(
    name: S,
    args: &[LpcRef],
    template: TaskTemplate,
    timeout: Option<u64>,
) -> Option<Result<LpcRef>>
where
    S: AsRef<str>,
{
    let master = {
        let Some(master) = template.object_space.master_object() else {
            return Some(Err(lpc_error!("No master object defined.")));
        };

        master.clone()
    };

    apply_function_by_name(name, args, master, template, timeout).await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        interpreter::{call_outs::CallOuts, task::task_template::TaskTemplateBuilder},
        test_support::compile_prog,
    };

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

        let template = TaskTemplateBuilder::default()
            .config(config)
            .call_outs(Arc::new(RwLock::new(CallOuts::new(tx.clone()))))
            .tx(tx)
            .build()
            .unwrap();

        let args = vec![LpcRef::from(42)];
        // We could use `proc` as the process, but the language supports functions being applied
        // in different processes, so we'll use a new one. Note that this can lead to mismatches
        // with global variables, but that's the nature of the beast.
        let result = apply_function(f, &args, Arc::new(process), template, None)
            .await
            .unwrap();

        assert_eq!(result, LpcRef::from(420));
    }
}
