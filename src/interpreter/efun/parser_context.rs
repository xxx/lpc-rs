//! Invocation context shared by the parser efuns.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process, task_context::TaskContext,
};

/// Select the command giver while retaining the invocation's callers and transaction.
pub(super) fn invocation<const N: usize>(
    context: &EfunContext<'_, N>,
    actor: &Arc<Process>,
    giver_slot: usize,
    name: &str,
) -> Result<TaskContext> {
    let giver = if context.arg_count() <= giver_slot {
        Some(actor.clone())
    } else {
        let value = context.arg(giver_slot);
        match value {
            LpcRef::Object(_) => value.live_object(context.txn()),
            _ if value.is_null_in(context.txn()) => None,
            _ => {
                return Err(context.runtime_error(format!(
                    "{name}: the command giver must be an object or null"
                )));
            }
        }
    };
    let mut invocation = context.task_context().clone();
    invocation.callers = Some(context.chain());
    invocation.this_player.store(giver);
    Ok(invocation)
}
