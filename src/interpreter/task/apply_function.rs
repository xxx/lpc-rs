use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_errors::{lpc_error, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use termcolor::Buffer;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        into_lpc_ref::IntoLpcRef,
        lpc_mapping::LpcMapping,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        object_space::ObjectSpace,
        process::Process,
        task::{into_task_context::IntoTaskContext, Task},
        ERROR_HANDLER,
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
/// * `template` - The object that will eventually become the
///   [`TaskContext`](crate::interpreter::task_context::TaskContext).
///
/// # Returns
///
/// * `Some(Ok(LpcRef))` - The result of the function.
/// * `Some(Err(LpcError))` - The error that occurred.
/// * `None` - The function is not defined in the master object.
pub async fn apply_function_in_master<S, T>(
    name: S,
    args: &[LpcRef],
    template: T,
    timeout: Option<u64>,
) -> Option<Result<LpcRef>>
where
    S: AsRef<str>,
    T: IntoTaskContext,
{
    let Some(master) = AsRef::<ObjectSpace>::as_ref(&template).master_object() else {
        return Some(Err(lpc_error!("No master object defined.")));
    };

    apply_function_by_name(name, args, master, template, timeout).await
}

/// Send a runtime error to the master object's `error_handler` function.
pub async fn apply_runtime_error<T>(
    error: &LpcError,
    proc: Option<Arc<Process>>,
    template: T,
) -> Option<Result<LpcRef>>
where
    T: IntoTaskContext,
{
    let mut mapping = IndexMap::new();
    let master = AsRef::<ObjectSpace>::as_ref(&template).master_object()?;
    let ctx = template.into_task_context(master);

    mapping.insert(
        LpcString::from("error").into_lpc_ref(ctx.memory()),
        LpcString::from(error.to_string()).into_lpc_ref(ctx.memory()),
    );

    // get the path and line number from the span, stripping off the lib dir so
    // the path is an in-game path.
    let span_string = error
        .span
        .and_then(|s| {
            s.to_string()
                .strip_prefix(ctx.config().lib_dir.as_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_else(|| String::from("<unknown>"));

    mapping.insert(
        LpcString::from("location").into_lpc_ref(ctx.memory()),
        LpcString::from(span_string).into_lpc_ref(ctx.memory()),
    );

    let object = proc
        .map(|pr| Arc::downgrade(&pr).into_lpc_ref(ctx.memory()))
        .unwrap_or_else(|| LpcString::from("<no object>").into_lpc_ref(ctx.memory()));
    mapping.insert(LpcString::from("object").into_lpc_ref(ctx.memory()), object);

    let mut buffer = Buffer::ansi();
    let diagnostics = error.to_diagnostics();

    lpc_rs_errors::output_diagnostics(&diagnostics, &mut buffer);
    let s = std::str::from_utf8(buffer.as_slice()).unwrap_or("<diagnostic with invalid utf8?>");

    mapping.insert(
        LpcString::from("diagnostics").into_lpc_ref(ctx.memory()),
        LpcString::from(s).into_lpc_ref(ctx.memory()),
    );

    let args = [LpcMapping::new(mapping).into_lpc_ref(ctx.memory())];
    // TODO wire the timeout up to config
    apply_function_in_master(ERROR_HANDLER, &args, ctx, Some(300)).await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{task::task_template::TaskTemplateBuilder, vm::global_state::GlobalState},
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
        let global_state = GlobalState::new(config, tx);

        let template = TaskTemplateBuilder::default()
            .global_state(global_state)
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
