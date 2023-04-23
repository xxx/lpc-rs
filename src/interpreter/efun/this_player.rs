use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
};

/// `this_player`, an efun for returning the command giver for the current Task
pub async fn this_player<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = if let Some(process) = &*context.this_player().load() {
        Arc::downgrade(process).into_lpc_ref(context.memory())
    } else {
        LpcRef::from(0)
    };

    context.return_efun_result(result);

    Ok(())
}
