use std::sync::Arc;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};


/// `this_object`, an efun for returning a reference to the object it was called
/// within.
pub async fn this_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let proc = context.frame().process.clone();

    let v = LpcValue::Object(Arc::downgrade(&proc));
    let result = context.value_to_ref(v);

    context.return_efun_result(result);

    Ok(())
}
