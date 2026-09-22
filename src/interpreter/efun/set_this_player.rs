use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `set_this_player`, an efun for setting a new command giver for the current Task
pub fn set_this_player<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let new = context.arg(0);

    let new_value = match new {
        value if value == &context.txn().nil() => None,
        LpcRef::Object(x) => Some(x.upgrade()),
        _ => {
            context.return_efun_result(context.txn().nil());
            return Ok(());
        }
    }
    .flatten();

    let prev = context.this_player().swap(new_value);

    if let Some(prev) = prev {
        let prev = Arc::downgrade(&prev);
        context.return_efun_result(prev.into());
    } else {
        context.return_efun_result(context.txn().nil());
    }

    Ok(())
}
