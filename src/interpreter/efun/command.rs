use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::dispatch::{Outcome, dispatch},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
};

/// `command`, an efun that runs a line as `this_player()` (or a given
/// living) inside the caller's transaction; 1 when handled.
pub async fn command<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(line) = context.resolve_local_register(1 as RegisterSize).clone() else {
        return Err(context.runtime_error("command: the command must be a string"));
    };
    // An omitted default argument arrives as a NULL-filled register, not an absent one.
    let actor = match context.try_resolve_local_register(2 as RegisterSize) {
        None => context.this_player().load_full(),
        Some(given) if given.is_null() => context.this_player().load_full(),
        Some(LpcRef::Object(weak)) => weak.upgrade(),
        Some(_) => return Err(context.runtime_error("command: the actor must be an object")),
    };
    let Some(actor) = actor else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    let outcome = dispatch(context.task_context(), actor, line.to_str()).await?;
    context.return_efun_result(LpcRef::from(outcome == Outcome::Handled));
    Ok(())
}
