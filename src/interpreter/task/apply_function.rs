use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_errors::{LpcError, Result, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use termcolor::Buffer;

use super::{SeedArg, TaskSeed};
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        ERROR_HANDLER,
        apply::in_game_location,
        lpc_mapping::LpcMapping,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        task::{Task, task_template::TaskTemplate},
        task_context::TaskContext,
    },
};

/// Apply function `f` in `ctx` (whose `process` is the object the function
/// runs in), to arguments `args`.
/// Returns the result of the function.
///
/// # Arguments
///
/// * `f` - The [`ProgramFunction`] to apply.
/// * `args` - A slice of [`LpcRef`]s to apply the function to.
/// * `ctx` - The [`TaskContext`] to run the function in.
/// * `timeout` - The execution limit in milliseconds; `None` for no limit.
///
/// # Returns
///
/// * `Ok(LpcRef)` - The result of the function.
/// * `Err(LpcError)` - The error that occurred.
pub async fn apply_function(
    f: Arc<ProgramFunction>,
    args: &[LpcRef],
    ctx: TaskContext,
    timeout: Option<u64>,
) -> Result<LpcRef> {
    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(ctx);

    task.timed_eval(f, args, timeout.unwrap_or(0))
        .await
        .map(|_| task.result().unwrap())
}

/// As [`apply_function`], with [`SeedArg`] arguments: a
/// [`SeedArg::FreshMapping`] is minted into each attempt's transaction.
pub async fn apply_function_seeded(
    f: Arc<ProgramFunction>,
    args: Vec<SeedArg>,
    ctx: TaskContext,
    timeout: Option<u64>,
) -> Result<LpcRef> {
    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(ctx);
    let seed = TaskSeed {
        process: task.context.process().clone(),
        function: f,
        args,
        initializes: false,
    };
    task.timed_eval_seed(seed, timeout.unwrap_or(0))
        .await
        .map(|_| task.result().unwrap())
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
pub async fn apply_function_by_name<S>(
    name: S,
    args: &[LpcRef],
    proc: Arc<Process>,
    template: TaskTemplate,
    timeout: Option<u64>,
) -> Option<Result<LpcRef>>
where
    S: AsRef<str>,
{
    let f = proc.program.unmangled_functions.get(name.as_ref())?.clone();

    Some(apply_function(f, args, template.into_task_context(proc), timeout).await)
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
/// * `template` - The template that holds the rest of the context information.
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
    let Some(master) = template.global_state.object_space.master_object() else {
        return Some(Err(lpc_error!("No master object defined.")));
    };

    apply_function_by_name(name, args, master, template, timeout).await
}

/// Send a runtime error to the master object's `error_handler` function.
pub async fn apply_runtime_error(
    error: &LpcError,
    proc: Option<Arc<Process>>,
    template: TaskTemplate,
) -> Option<Result<LpcRef>> {
    let mut mapping = IndexMap::new();
    let master = template.global_state.object_space.master_object()?;
    let error_handler = master
        .program
        .unmangled_functions
        .get(ERROR_HANDLER)?
        .clone();
    let ctx = template.into_task_context(master);

    mapping.insert(
        LpcString::from("error").into(),
        LpcString::from(error.to_string()).into(),
    );

    mapping.insert(
        LpcString::from("location").into(),
        LpcString::from(in_game_location(error.span())).into(),
    );

    let object = proc
        .map(|pr| Arc::downgrade(&pr).into())
        .unwrap_or_else(|| LpcString::from("<no object>").into());
    mapping.insert(LpcString::from("object").into(), object);

    let mut buffer = Buffer::ansi();
    let diagnostics = error.to_diagnostics();

    lpc_rs_errors::output_diagnostics(&diagnostics, &mut buffer);
    let s = std::str::from_utf8(buffer.as_slice()).unwrap_or("<diagnostic with invalid utf8?>");

    mapping.insert(
        LpcString::from("diagnostic").into(),
        LpcString::from(s).into(),
    );

    // A cell minted into `ctx`'s transaction here is discarded when the
    // task opens its own.
    let args = vec![SeedArg::FreshMapping(LpcMapping::new(mapping))];
    // TODO wire the timeout up to config
    Some(apply_function_seeded(error_handler, args, ctx, Some(300)).await)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{CommittedReader, vm::Vm, vm::global_state::GlobalState},
        test_support::{compile_prog, test_config},
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

        let template = TaskTemplate::from(global_state);

        let args = vec![LpcRef::from(42)];
        // We could use `proc` as the process, but the language supports functions being applied
        // in different processes, so we'll use a new one. Note that this can lead to mismatches
        // with global variables, but that's the nature of the beast.
        let result = apply_function(
            f,
            &args,
            template.into_task_context(Arc::new(process)),
            None,
        )
        .await
        .unwrap();

        assert_eq!(result, LpcRef::from(420));
    }

    #[tokio::test]
    async fn the_error_handler_can_read_its_mapping() {
        let vm = Vm::new(test_config());
        let master = vm
            .global_state
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string last;
                    string diagnostic;
                    void error_handler(mapping m) {
                        last = m["error"];
                        diagnostic = m["diagnostic"];
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;

        let err = lpc_rs_errors::LpcError::runtime("boom");
        let result =
            apply_runtime_error(&err, None, TaskTemplate::from(vm.global_state.clone())).await;

        assert!(matches!(result, Some(Ok(_))), "the handler ran: {result:?}");
        let LpcRef::String(s) = vm.global_state.committed_global(&master, 0u16) else {
            panic!("a string");
        };
        assert!(s.to_str().contains("boom"), "read the mapping: {s}");
        let LpcRef::String(d) = vm.global_state.committed_global(&master, 1u16) else {
            panic!("a string");
        };
        assert!(
            d.to_str().contains("runtime error: boom"),
            "the rendered key: {d}"
        );
    }
}
