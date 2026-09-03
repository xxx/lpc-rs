use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    process::Process,
};

/// `previous_object`, an efun for the object that called the current
/// function through a door — `->`, a pointer, a simul efun, or a task
/// entry — `0` when the driver did, or it is destructed. A step `n` names
/// the `n`th caller back; `-1` is the whole chain as an array.
pub fn previous_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let step = match context.try_resolve_local_register(1 as RegisterSize) {
        Some(LpcRef::Int(n)) => n.0,
        _ => 0,
    };
    let txn = context.txn();
    let live = |process: &Arc<Process>| {
        if process.is_live(txn) {
            LpcRef::from(Arc::downgrade(process))
        } else {
            NULL
        }
    };
    let result = match step {
        -1 => context.mint_array(context.previous_objects().map(live)),
        n if n >= 0 => usize::try_from(n)
            .ok()
            .and_then(|n| context.previous_objects().nth(n))
            .map_or(NULL, live),
        n => {
            return Err(context.runtime_error(format!(
                "previous_object: expected a step back or -1, got {n}"
            )));
        }
    };

    context.return_efun_result(result);

    Ok(())
}
