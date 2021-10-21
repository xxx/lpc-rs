use crate::{
    interpreter::{lpc_value::LpcValue},
    Result,
};
use crate::interpreter::efun::efun_context::EfunContext;

/// `this_object`, an efun for returning a reference to the object it was called within.
pub fn this_object(context: &mut EfunContext) -> Result<()> {
    let proc = context.process().clone();
    let v = LpcValue::Object(proc);
    let result = context.value_to_ref(v);

    context.return_efun_result(result);

    Ok(())
}
