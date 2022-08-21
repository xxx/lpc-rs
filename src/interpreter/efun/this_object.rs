use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};

/// `this_object`, an efun for returning a reference to the object it was called
/// within.
pub fn this_object<const N: usize>(context: &mut EfunContext<N>) -> Result<()> {
    let proc = context.process().clone();
    let v = LpcValue::Object(proc);
    let result = context.value_to_ref(v);

    context.return_efun_result(result);

    Ok(())
}
