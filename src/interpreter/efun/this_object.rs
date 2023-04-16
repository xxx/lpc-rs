use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef};

/// `this_object`, an efun for returning a reference to the object it was called
/// within.
pub async fn this_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = context.frame().process.clone();

    let result = Arc::downgrade(&proc).into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}
