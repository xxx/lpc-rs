//! Applies made on a command's behalf: nested in the caller's transaction,
//! bounded by the configured execution time and `MAX_TASK_CHAIN` levels.

use std::sync::Arc;

use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    interpreter::{
        CATCH_TELL, function_type::function_ptr::FunctionPtr, lpc_ref::LpcRef,
        lpc_string::LpcString, process::Process, stm::Effect, task::apply_function::apply_function,
        task_context::TaskContext,
    },
    telnet::ops::ConnectionOp,
};

/// Apply `function` on `target`, joining `ctx`'s transaction; `this_player`
/// is whatever it was.
pub(crate) async fn apply_nested(
    ctx: &TaskContext,
    target: &Arc<Process>,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    timed(ctx, ctx.nested(target.clone())?, function, args).await
}

/// Apply `function` on `target` with `this_player` set, joining `ctx`'s
/// transaction.
pub(crate) async fn apply_on(
    ctx: &TaskContext,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    let nested = ctx.nested(target.clone())?;
    nested.this_player.store(Some(this_player.clone()));
    timed(ctx, nested, function, args).await
}

/// `target->name(args)` as `apply_on`; `None` when `target` does not define
/// `name`.
pub(crate) async fn apply_hook(
    ctx: &TaskContext,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
    name: &str,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let Some(function) = target.program.unmangled_functions.get(name).cloned() else {
        return Ok(None);
    };
    apply_on(ctx, target, this_player, function, args)
        .await
        .map(Some)
}

/// Fire `pointer` as `actor` (`this_player` too) with `args`; `None` when
/// the pointer no longer resolves.
pub(crate) async fn apply_pointer(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    pointer: &FunctionPtr,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let handler_ctx = ctx.nested(actor.clone())?;
    handler_ctx.this_player.store(Some(actor.clone()));
    let Some(resolved) = pointer.prepare_call(args, &handler_ctx).await? else {
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

/// `message` to `target`: through `catch_tell` — applied with `this_player`
/// set when one is given — else its connection, else the debug log, as
/// effects; a destructed target is the log. Whether it was received; the
/// log is not.
pub(crate) async fn deliver(
    ctx: &TaskContext,
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
        match this_player {
            Some(player) => apply_on(ctx, target, player, function, &args).await?,
            None => apply_nested(ctx, target, function, &args).await?,
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
        test_support::test_config,
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

    /// `->` over a collection still nests a task per element; the budget is
    /// the bound on its recursion.
    #[tokio::test]
    async fn collection_call_other_recursion_fills_the_budget_and_no_more() {
        let vm = Vm::new(test_config());
        let fits = format!(
            r#"
            int depth;
            int f(int n) {{
                if (n < {MAX_TASK_CHAIN}) {{
                    int *r = ({{ this_object() }})->f(n + 1);
                    return r[0];
                }}
                return n;
            }}
            void create() {{ depth = f(0); }}
            "#
        );
        let process = vm
            .initialize_process_from_code("/fits.c", &fits)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 0u16),
            LpcRef::from(i64::from(MAX_TASK_CHAIN))
        );

        let past = indoc! { r#"
            int f(int n) {
                if (n < 1000) {
                    int *r = ({ this_object() })->f(n + 1);
                    return r[0];
                }
                return n;
            }
            void create() { f(0); }
        "# };
        let err = vm
            .initialize_process_from_code("/past.c", past)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("nested task depth"), "{err}");
    }
}
