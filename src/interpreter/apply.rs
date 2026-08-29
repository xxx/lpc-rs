//! Applies made on a command's behalf: nested in the caller's transaction,
//! bounded by the configured execution time.

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
    let nested = ctx.clone().with_process(target.clone());
    timed(ctx, nested, function, args).await
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
    let nested = ctx.clone().with_process(target.clone());
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
    let handler_ctx = ctx.clone().with_process(actor.clone());
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

/// Deliver `message` to `actor` through `catch_tell`, else straight to its
/// connection, else the debug log, as effects.
pub(crate) async fn deliver(ctx: &TaskContext, actor: &Arc<Process>, message: &str) -> Result<()> {
    if apply_hook(
        ctx,
        actor,
        actor,
        CATCH_TELL,
        &[LpcString::from(message).into()],
    )
    .await?
    .is_some()
    {
        return Ok(());
    }
    let connection = ctx.txn().with(|t| t.read_connection(actor.connection.id));
    let effect = match connection {
        Some(connection) => Effect::Socket {
            op: ConnectionOp::SendMessage(message.to_owned()),
            tx: connection.tx.clone(),
        },
        None => Effect::DebugLog(message.to_owned()),
    };
    ctx.txn().with(|t| t.record_effect(effect));
    Ok(())
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
