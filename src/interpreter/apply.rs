//! Applies made on a command's behalf: nested in the caller's transaction,
//! bounded by the configured execution time and `MAX_TASK_CHAIN` levels.

use std::sync::Arc;

use indexmap::IndexMap;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    interpreter::{
        CATCH_TELL, WARNING_HANDLER,
        function_type::function_ptr::FunctionPtr,
        lpc_mapping::LpcMapping,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        stm::Effect,
        task::apply_function::apply_function,
        task_context::{Caller, Callers, TaskContext},
    },
    telnet::ops::ConnectionOp,
};

/// The chain a handler the driver runs for `actor` is entered with: the
/// actor in front of `ctx`'s own.
pub(crate) fn as_actor(ctx: &TaskContext, actor: &Arc<Process>) -> Callers {
    Some(Caller::link(actor.clone(), ctx.callers.clone()))
}

/// Apply `function` on `target`, entered through `callers`, joining `ctx`'s
/// transaction; `this_player` is whatever it was.
pub(crate) async fn apply_nested(
    ctx: &TaskContext,
    callers: Callers,
    target: &Arc<Process>,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    timed(ctx, ctx.nested(callers, target.clone())?, function, args).await
}

/// Apply `function` on `target` with `this_player` set, entered through
/// `callers`, joining `ctx`'s transaction.
pub(crate) async fn apply_on(
    ctx: &TaskContext,
    callers: Callers,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    let nested = ctx.nested(callers, target.clone())?;
    nested.this_player.store(Some(this_player.clone()));
    timed(ctx, nested, function, args).await
}

/// `target->name(args)` as `apply_on`, entered through `callers`; `None`
/// when `target` does not define `name`.
pub(crate) async fn apply_hook(
    ctx: &TaskContext,
    callers: Callers,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
    name: &str,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let Some(function) = target.program.unmangled_functions.get(name).cloned() else {
        return Ok(None);
    };
    apply_on(ctx, callers, target, this_player, function, args)
        .await
        .map(Some)
}

/// Fire `pointer` as `actor` (`this_player` and the caller too) with `args`;
/// `None` when the pointer no longer resolves.
pub(crate) async fn apply_pointer(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    pointer: &FunctionPtr,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let callers = as_actor(ctx, actor);
    let handler_ctx = ctx.nested(callers.clone(), actor.clone())?;
    handler_ctx.this_player.store(Some(actor.clone()));
    let Some(resolved) = pointer
        .prepare_call(args, &handler_ctx, || Ok(callers))
        .await?
    else {
        return Ok(None);
    };
    timed(
        ctx,
        handler_ctx.with_process(resolved.process),
        resolved.function,
        &resolved.args,
    )
    .await
    .map(Some)
}

/// `name(args)` on the master, entered through `callers`, joining `ctx`'s
/// transaction; `None` without a master or without the apply. The apply's
/// error is the caller's.
pub(crate) async fn master_apply(
    ctx: &TaskContext,
    callers: Callers,
    name: &str,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let Some(master) = ctx.object_space().master_object() else {
        return Ok(None);
    };
    let Some(function) = master.program.unmangled_functions.get(name).cloned() else {
        return Ok(None);
    };
    apply_nested(ctx, callers, &master, function, args)
        .await
        .map(Some)
}

/// The master's verdict on `name(args)`: truthiness of what it returns;
/// `false` without a master or without the apply; the apply's error is the
/// caller's.
pub(crate) async fn valid_apply(
    ctx: &TaskContext,
    callers: Callers,
    name: &str,
    args: &[LpcRef],
) -> Result<bool> {
    let verdict = master_apply(ctx, callers, name, args).await?;
    Ok(verdict.is_some_and(|v| v.is_truthy(ctx.txn())))
}

/// `message` to `target`: through `catch_tell` — entered through the chain
/// `callers` yields, applied with `this_player` set when one is given — else
/// its connection, else the debug log, as effects; a destructed target is
/// the log. Whether it was received; the log is not.
pub(crate) async fn deliver(
    ctx: &TaskContext,
    callers: impl FnOnce() -> Callers,
    target: &Arc<Process>,
    this_player: Option<&Arc<Process>>,
    message: &str,
) -> Result<bool> {
    if !target.is_live(ctx.txn()) {
        ctx.txn()
            .with(|t| t.record_effect(Effect::DebugLog(message.to_owned())));
        return Ok(false);
    }
    if let Some(function) = target.program.unmangled_functions.get(CATCH_TELL).cloned() {
        let args = [LpcString::from(message).into()];
        let callers = callers();
        match this_player {
            Some(player) => apply_on(ctx, callers, target, player, function, &args).await?,
            None => apply_nested(ctx, callers, target, function, &args).await?,
        };
        return Ok(true);
    }
    let connection = ctx.txn().with(|t| t.read_connection(target.connection.id));
    let (effect, received) = match connection {
        Some(connection) => (
            Effect::Socket {
                op: ConnectionOp::SendMessage(message.to_owned()),
                tx: connection.sender(),
            },
            true,
        ),
        None => (Effect::DebugLog(message.to_owned()), false),
    };
    ctx.txn().with(|t| t.record_effect(effect));
    Ok(received)
}

/// Each of `warnings`, raised compiling `file`, to the master's
/// `warning_handler`, entered through `callers`, nested in `ctx`'s
/// transaction; without the apply, to the debug log as effects.
pub(crate) async fn report_warnings(
    ctx: &TaskContext,
    callers: Callers,
    file: &LpcPath,
    warnings: Vec<LpcError>,
) -> Result<()> {
    let handler = ctx.object_space().master_object().and_then(|master| {
        let function = master
            .program
            .unmangled_functions
            .get(WARNING_HANDLER)
            .cloned()?;
        Some((master, function))
    });
    for warning in warnings {
        let Some((master, function)) = &handler else {
            ctx.txn()
                .with(|t| t.record_effect(Effect::DebugLog(warning.diagnostic_string())));
            continue;
        };
        let mapping = warning_mapping(ctx, file, &warning);
        apply_nested(ctx, callers.clone(), master, function.clone(), &[mapping]).await?;
    }
    Ok(())
}

/// The `warning_handler` argument — `message`, `location`, `file`,
/// `diagnostic` — minted in `ctx`'s transaction.
fn warning_mapping(ctx: &TaskContext, file: &LpcPath, warning: &LpcError) -> LpcRef {
    let lib_dir = ctx.config().lib_dir.as_str();
    let entries = [
        ("message", warning.message().to_owned()),
        ("location", in_game_location(warning.span())),
        ("file", file.as_in_game(lib_dir).display().to_string()),
        ("diagnostic", warning.diagnostic_string()),
    ];
    let mapping: IndexMap<LpcRef, LpcRef> = entries
        .into_iter()
        .map(|(key, value)| (LpcString::from(key).into(), LpcString::from(value).into()))
        .collect();
    let cell = ctx.txn().with(|t| t.mint_mapping(LpcMapping::new(mapping)));
    LpcRef::Mapping(cell)
}

/// `span` as an in-game `path:line:column`; `<unknown>` without one.
pub(crate) fn in_game_location(span: Option<Span>) -> String {
    span.map(|s| s.to_string())
        .unwrap_or_else(|| String::from("<unknown>"))
}

/// Run `function` in `nested` under `ctx`'s configured execution time.
async fn timed(
    ctx: &TaskContext,
    nested: TaskContext,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    let timeout = ctx.config().max_execution_time;
    apply_function(function, args, nested, Some(timeout)).await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        compile_time_config::MAX_TASK_CHAIN,
        interpreter::{CommittedReader, vm::Vm},
        test_support::{PERMISSIVE_MASTER, test_config},
    };

    /// A `catch_tell` that writes back nests until the budget refuses it.
    #[tokio::test]
    async fn nested_applies_stop_at_the_budget() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int depth;
            int refused_at;
            void catch_tell(string s) {
                depth++;
                if (catch(write("again"))) refused_at = depth;
            }
            void create() { set_this_player(this_object()); write("go"); }
        "# };
        let process = vm
            .initialize_process_from_code("/loop.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 1u16),
            LpcRef::from(i64::from(MAX_TASK_CHAIN)),
            "the level at the budget cannot nest again"
        );
    }

    #[tokio::test]
    async fn the_refused_nesting_is_a_runtime_error() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            void catch_tell(string s) { write("again"); }
            void create() { set_this_player(this_object()); write("go"); }
        "# };
        let err = vm
            .initialize_process_from_code("/loop.c", code)
            .await
            .unwrap_err();
        assert!(
            err.to_string()
                .contains(&format!("nested task depth of {MAX_TASK_CHAIN} exceeded")),
            "{err}"
        );
    }

    /// `/warns.c` raises two warnings; the master hears each, as the loader's
    /// `this_player`.
    #[tokio::test]
    async fn a_compile_warning_reaches_the_masters_warning_handler() {
        let vm = Vm::new(test_config());
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                format!(
                    "{}{PERMISSIVE_MASTER}",
                    indoc! { r#"
                        int count;
                        string message;
                        string location;
                        string file;
                        string diagnostic;
                        string who;
                        void warning_handler(mapping w) {
                            count++;
                            message = w["message"];
                            location = w["location"];
                            file = w["file"];
                            diagnostic = w["diagnostic"];
                            who = file_name(this_player());
                        }
                    "# },
                ),
            )
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code(
            "/loader.c",
            r#"void create() { set_this_player(this_object()); clone_object("/warns"); }"#,
        )
        .await
        .unwrap();

        let global = |reg: u16| match vm.global_state.committed_global(&master, reg) {
            LpcRef::String(s) => s.to_str().to_owned(),
            other => panic!("a string in {reg}: {other:?}"),
        };
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(2)
        );
        assert_eq!(
            global(1),
            "non-void function does not return a value. defaulting to 0."
        );
        assert_eq!(global(2), "/warns.c:5:1");
        assert_eq!(global(3), "/warns.c");
        assert!(
            global(4).starts_with("warning: non-void function does not return a value"),
            "{}",
            global(4)
        );
        assert!(global(4).contains("┌─ /warns.c:5:1"), "{}", global(4));
        assert_eq!(global(5), "/loader");
    }

    #[tokio::test]
    async fn a_master_without_the_apply_still_loads() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code(
            "/secure/master.c",
            format!("void create() {{}}\n{PERMISSIVE_MASTER}"),
        )
        .await
        .unwrap();
        let loader = vm
            .initialize_process_from_code(
                "/loader.c",
                r#"object o; void create() { o = clone_object("/warns"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(matches!(
            vm.global_state.committed_global(&loader, 0u16),
            LpcRef::Object(_)
        ));
    }

    #[tokio::test]
    async fn a_throwing_warning_handler_fails_the_load() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code(
            "/secure/master.c",
            format!(
                r#"void warning_handler(mapping w) {{ throw("no warnings allowed"); }}
{PERMISSIVE_MASTER}"#
            ),
        )
        .await
        .unwrap();
        let loader = vm
            .initialize_process_from_code(
                "/loader.c",
                indoc! { r#"
                    string err;
                    object o;
                    void create() {
                        err = catch(clone_object("/warns"));
                        o = find_object("/warns");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let LpcRef::String(err) = vm.global_state.committed_global(&loader, 0u16) else {
            panic!("the error string");
        };
        assert!(err.to_str().contains("no warnings allowed"), "{err}");
        assert!(
            !matches!(
                vm.global_state.committed_global(&loader, 1u16),
                LpcRef::Object(_)
            ),
            "nothing was inserted"
        );
    }

    #[tokio::test]
    async fn a_master_compiles_with_warnings_before_any_master_exists() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/secure/master.c", "int f() { }")
            .await
            .unwrap();
    }
}
