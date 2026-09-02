use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// `previous_object`, an efun for the object that called the current
/// function through a door — `->`, a pointer, a simul efun, or a task
/// entry — `0` when the driver did, or it is destructed.
pub async fn previous_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = context
        .previous_objects()
        .next()
        .filter(|process| process.is_live(context.txn()))
        .map_or(NULL, |process| LpcRef::from(Arc::downgrade(process)));

    context.return_efun_result(result);

    Ok(())
}
