use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_errors::{LpcError, Result, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::lpc_string::LpcString;

use super::{SeedArg, SeedEntry, TaskSeed};
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        ERROR_HANDLER,
        apply::{diagnostics, in_game_location},
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::LpcRef,
        process::Process,
        stm::TxnHandle,
        task::{Task, task_template::TaskTemplate},
        task_context::{Caller, TaskContext},
    },
};

/// A function's result with the transaction it was computed in: the
/// pre-commit snapshot plus the task's own writes, which no GC pass touches.
/// A committed read of the same result can find it reclaimed, since nothing
/// roots a bare answer.
#[derive(Debug)]
pub(crate) struct Applied {
    value: LpcRef,
    txn: TxnHandle,
}

impl Applied {
    /// The function's result.
    pub(crate) fn value(&self) -> &LpcRef {
        &self.value
    }

    /// The function's result, without its transaction.
    pub(crate) fn into_value(self) -> LpcRef {
        self.value
    }

    /// The result as an array; `None` for any other type.
    pub(crate) fn array(&self) -> Option<LpcArray> {
        self.read_array(&self.value)
    }

    /// The result as a mapping; `None` for any other type.
    pub(crate) fn mapping(&self) -> Option<LpcMapping> {
        self.read_mapping(&self.value)
    }

    /// The array named by `value`, a member of the result; `None` for any
    /// other type.
    pub(crate) fn read_array(&self, value: &LpcRef) -> Option<LpcArray> {
        value.with_array(&self.txn, LpcArray::clone).ok()
    }

    /// The mapping named by `value`, a member of the result; `None` for any
    /// other type.
    pub(crate) fn read_mapping(&self, value: &LpcRef) -> Option<LpcMapping> {
        value.with_mapping(&self.txn, LpcMapping::clone).ok()
    }
}

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
    applied(f, args, ctx, timeout)
        .await
        .map(Applied::into_value)
}

/// As [`apply_function`], with the result readable through the returned
/// [`Applied`].
pub(crate) async fn applied(
    f: Arc<ProgramFunction>,
    args: &[LpcRef],
    ctx: TaskContext,
    timeout: Option<u64>,
) -> Result<Applied> {
    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(ctx);

    task.timed_eval(f, args, timeout.unwrap_or(0))
        .await
        .map(|_| Applied {
            value: task.result().unwrap(),
            txn: task.context.txn().clone(),
        })
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
        entry: SeedEntry::Function(f),
        args,
        initializes: false,
    };
    task.timed_eval_seed(seed, timeout.unwrap_or(0))
        .await
        .map(|_| task.result().unwrap())
}

/// Apply function named `name` on process `proc`, to arguments `args`, using
/// context information from `template`. The name is resolved through `proc`'s
/// shadow chain inside each attempt, so a shadow defining it publicly runs
/// instead of `proc`.
/// Returns the result of the function.
///
/// This function uses timed evaluation, and will timeout if execution takes too long.
///
/// # Arguments
///
/// * `name` - The name of the function to apply. This is assumed to be an unmangled name.
/// * `args` - A slice of [`LpcRef`]s to apply the function to.
/// * `proc` - The [`Process`] to apply the function on.
/// * `template` - The template that holds the rest of the context information.
/// * `timeout` - The maximum amount of time to allow the function to execute, in milliseconds.
///
/// # Returns
///
/// * `Some(Ok(LpcRef))` - The result of the function.
/// * `Some(Err(LpcError))` - The error that occurred.
/// * `None` - Nothing in `proc`'s chain defines the function.
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
    applied_by_name(name, args, proc, template, timeout)
        .await
        .map(|result| result.map(Applied::into_value))
}

/// As [`apply_function_by_name`], with the result readable through the
/// returned [`Applied`].
pub(crate) async fn applied_by_name<S>(
    name: S,
    args: &[LpcRef],
    proc: Arc<Process>,
    template: TaskTemplate,
    timeout: Option<u64>,
) -> Option<Result<Applied>>
where
    S: AsRef<str>,
{
    let name = name.as_ref();
    if !proc.ever_in_a_chain() && !proc.program.unmangled_functions.contains_key(name) {
        diagnostics::missing(name, Some(&proc));
        return None;
    }

    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(template.into_task_context(proc.clone()));
    let seed = TaskSeed {
        process: proc,
        entry: SeedEntry::Named(name.to_owned()),
        args: args.iter().cloned().map(SeedArg::Value).collect(),
        initializes: false,
    };
    match task.timed_eval_seed(seed, timeout.unwrap_or(0)).await {
        Err(e) => Some(Err(e)),
        Ok(()) => task.result().map(|value| {
            Ok(Applied {
                value,
                txn: task.context.txn().clone(),
            })
        }),
    }
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
    applied_in_master(name, args, template, timeout)
        .await
        .map(|result| result.map(Applied::into_value))
}

/// As [`apply_function_in_master`], with the result readable through the
/// returned [`Applied`].
pub(crate) async fn applied_in_master<S>(
    name: S,
    args: &[LpcRef],
    template: TaskTemplate,
    timeout: Option<u64>,
) -> Option<Result<Applied>>
where
    S: AsRef<str>,
{
    let Some(master) = template.global_state.object_space.master_object() else {
        diagnostics::missing(name.as_ref(), None);
        return Some(Err(lpc_error!("No master object defined.")));
    };

    applied_by_name(name, args, master, template, timeout).await
}

/// Send a runtime error to the master object's `error_handler` function.
pub async fn apply_runtime_error(
    error: &LpcError,
    proc: Option<Arc<Process>>,
    template: TaskTemplate,
) -> Option<Result<LpcRef>> {
    let mut mapping = IndexMap::new();
    let Some(master) = template.global_state.object_space.master_object() else {
        diagnostics::missing(ERROR_HANDLER, None);
        return None;
    };
    let Some(error_handler) = master
        .program
        .unmangled_functions
        .get(ERROR_HANDLER)
        .cloned()
    else {
        diagnostics::missing(ERROR_HANDLER, Some(&master));
        return None;
    };
    let mut ctx = template.into_task_context(master);
    ctx.callers = proc.clone().map(|erring| Caller::link(erring, None));

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

    mapping.insert(
        LpcString::from("diagnostic").into(),
        LpcString::from(error.diagnostic_string()).into(),
    );

    // A cell minted into `ctx`'s transaction here is discarded when the
    // task opens its own.
    let args = vec![SeedArg::FreshMapping(LpcMapping::new(mapping))];
    // TODO wire the timeout up to config
    Some(apply_function_seeded(error_handler, args, ctx, Some(300)).await)
}

/// Log an uncaught error to the server and notify the master's `error_handler`,
/// falling back to the debug log when the handler is absent or throws.
pub async fn report_runtime_error(
    error: &LpcError,
    proc: Option<Arc<Process>>,
    template: TaskTemplate,
) {
    tracing::error!(
        target: "lpc_rs::applies",
        object = %proc.as_deref().map_or_else(|| "<no object>".into(), Process::filename),
        "Uncaught LPC error:\n{}", error.diagnostic_string()
    );
    let config = template.global_state.config.clone();
    match apply_runtime_error(error, proc, template).await {
        Some(Ok(_)) => {}
        None => config.debug_log(error.diagnostic_string()).await,
        Some(Err(handler_error)) => {
            tracing::error!(
                target: "lpc_rs::applies",
                apply = ERROR_HANDLER,
                "Error handler failed:\n{}", handler_error.diagnostic_string()
            );
            config.debug_log(error.diagnostic_string()).await;
            config
                .debug_log(format!(
                    "error_handler failed: {}",
                    handler_error.diagnostic_string()
                ))
                .await;
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};
    use tracing::instrument::WithSubscriber;

    use super::*;
    use crate::{
        interpreter::{CommittedReader, vm::Vm, vm::global_state::GlobalState},
        test_support::{compile_prog, log_capture::LogCapture, test_config},
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
    async fn an_applied_answer_is_readable_after_a_gc_pass() {
        let vm = Vm::new(test_config());
        let process = vm
            .initialize_process_from_code(
                "/stats.c",
                r#"mapping stats() { return ([ "PORTS": ({ "4000", 4001 }) ]); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let applied = applied_by_name("stats", &[], process, template, None)
            .await
            .unwrap()
            .unwrap();
        let LpcRef::Mapping(cell) = applied.value().clone() else {
            panic!("stats() answers a mapping, got {:?}", applied.value());
        };

        vm.global_state.gc().await.unwrap().unwrap();

        assert!(
            vm.global_state.committed_mapping(cell.id).is_none(),
            "nothing roots a bare answer, so the pass reclaims it"
        );
        let mapping = applied.mapping().unwrap();
        let ports = applied
            .read_array(mapping.get(&LpcRef::from("PORTS")).unwrap())
            .unwrap();
        assert_eq!(
            ports.to_vec(),
            vec![LpcRef::from("4000"), LpcRef::from(4001)]
        );
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
        assert!(!d.to_str().contains('\u{1b}'), "plain text: {d:?}");
    }

    #[tokio::test]
    async fn a_silent_error_handler_cannot_hide_an_apply_failure_from_the_server_log() {
        let vm = Vm::new(test_config());
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                "int handled; void error_handler(mapping e) { handled++; }",
            )
            .await
            .unwrap()
            .context
            .process;
        let login = vm
            .initialize_process_from_code("/login.c", "int logon() { throw(\"login exploded\"); }")
            .await
            .unwrap()
            .context
            .process;
        let log = LogCapture::default();
        async {
            let template = TaskTemplate::from(vm.global_state.clone());
            let error = apply_function_by_name("logon", &[], login.clone(), template.clone(), None)
                .await
                .unwrap()
                .unwrap_err();
            report_runtime_error(&error, Some(login), template).await;
        }
        .with_subscriber(log.subscriber("info"))
        .await;

        let logged = log.contents();
        assert!(logged.contains("ERROR lpc_rs::applies"), "{logged}");
        assert!(
            logged.contains("Apply failed apply=\"logon\" object=/login"),
            "{logged}"
        );
        assert!(logged.contains("Uncaught LPC error"), "{logged}");
        assert!(logged.contains("login exploded"), "{logged}");
        assert!(logged.contains("/login.c:1:"), "{logged}");
        assert!(logged.contains("throw(\"login exploded\")"), "{logged}");
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn a_failing_error_handler_logs_both_diagnostics_without_recursing() {
        let config = crate::test_config_builder!()
            .debug_log(DebugLog::new(tokio::io::sink()))
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.initialize_process_from_code(
            "/secure/master.c",
            "void error_handler(mapping e) { throw(\"handler exploded\"); }",
        )
        .await
        .unwrap();
        let log = LogCapture::default();
        report_runtime_error(
            &LpcError::runtime("original failure"),
            None,
            TaskTemplate::from(vm.global_state.clone()),
        )
        .with_subscriber(log.subscriber("info"))
        .await;

        let logged = log.contents();
        assert_eq!(logged.matches("Uncaught LPC error").count(), 1, "{logged}");
        assert_eq!(
            logged.matches("Error handler failed:").count(),
            1,
            "{logged}"
        );
        assert!(logged.contains("original failure"), "{logged}");
        assert!(logged.contains("handler exploded"), "{logged}");
        assert!(logged.contains("/secure/master.c:1:"), "{logged}");
    }

    #[tokio::test]
    async fn errors_without_a_master_still_reach_the_server_log() {
        let config = crate::test_config_builder!()
            .debug_log(DebugLog::new(tokio::io::sink()))
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let log = LogCapture::default();
        report_runtime_error(
            &LpcError::runtime("no master available"),
            None,
            TaskTemplate::from(vm.global_state.clone()),
        )
        .with_subscriber(log.subscriber("info"))
        .await;

        let logged = log.contents();
        assert!(logged.contains("no master available"), "{logged}");
        assert!(logged.contains("object=<no object>"), "{logged}");
    }

    #[tokio::test]
    async fn object_initialization_errors_are_logged_before_returning_to_the_loader() {
        let vm = Vm::new(test_config());
        let log = LogCapture::default();
        let result = vm
            .initialize_process_from_code(
                "/broken.c",
                "void create() { throw(\"broken initializer\"); }",
            )
            .with_subscriber(log.subscriber("info"))
            .await;

        assert!(result.is_err());
        let logged = log.contents();
        assert!(logged.contains("Apply failed"), "{logged}");
        assert!(logged.contains("object=/broken"), "{logged}");
        assert!(logged.contains("broken initializer"), "{logged}");
    }
}
