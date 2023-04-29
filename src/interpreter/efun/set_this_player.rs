use std::sync::Arc;
use lpc_rs_core::RegisterSize;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    into_lpc_ref::IntoLpcRef,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
};

/// `set_this_player`, an efun for setting a new command giver for the current Task
pub async fn set_this_player<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let new = context.resolve_local_register(1  as RegisterSize);

    let new_value = match new {
        LpcRef::Int(LpcInt(0)) => None,
        LpcRef::Object(x) => Some(x.upgrade()),
        _ => {
            context.return_efun_result(NULL);
            return Ok(());
        }
    }
    .flatten();

    let prev = context.this_player().swap(new_value);

    if let Some(prev) = prev {
        let prev = Arc::downgrade(&prev);
        context.return_efun_result(prev.into_lpc_ref(context.memory()));
    } else {
        context.return_efun_result(NULL);
    }

    Ok(())
}
