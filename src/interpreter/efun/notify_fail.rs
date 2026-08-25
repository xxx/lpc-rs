use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `notify_fail`, an efun that sets the message (or a closure producing it)
/// for the command in progress if nothing handles it. Returns 0, so
/// `return notify_fail("...")` reads as "not handled".
pub async fn notify_fail<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let message = context.resolve_local_register(1 as RegisterSize).clone();
    if !matches!(message, LpcRef::String(_) | LpcRef::Function(_)) {
        return Err(
            context.runtime_error("notify_fail: the message must be a string or a function")
        );
    }
    context.task_context().with_command(|state| {
        if let Some(state) = state {
            state.notify_fail = Some(message);
        }
    });
    Ok(())
}
