use lpc_rs_errors::Result;
use qcell::QCellOwner;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};
use crate::interpreter::lpc_ref::LpcRef;
use crate::try_extract_value;
use lpc_rs_errors::LpcError;

/// `call_out`, an efun for calling a function at some future point in time
pub fn call_out<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let LpcRef::Function(func) = context.resolve_local_register(1_usize) else {
        return Err(context.runtime_error("invalid function sent to `call_out`"));
    };
    let ptr = try_extract_value!(&*func.borrow(), LpcValue::Function);

    let duration_ref = context.resolve_local_register(2_usize);
    let duration = match duration_ref {
        LpcRef::Int(x) => chrono::Duration::seconds(x),
        LpcRef::Float(x) => {
            let m = x * 1000.0;
            let millis = if m > i64::MAX as f64 {
                i64::MAX
            } else if m < i64::MIN as f64 {
                i64::MIN
            } else {
                m.into_inner() as i64
            };
            chrono::Duration::milliseconds(millis)
        },
        _ => return Err(context.runtime_error("invalid duration sent to `call_out`")),
    };

    let task = ();
    let call_outs = context.call_outs().rw(cell_key);
    let index = call_outs.schedule_task(task, duration)?;

    // TODO: limit the max number of call outs so we don't overflow this
    let result = LpcRef::Int(index as i64);
    context.return_efun_result(result);

    Ok(())
}
