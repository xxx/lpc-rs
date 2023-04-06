use std::rc::Weak;
use lpc_rs_errors::Result;
use qcell::{QCell, QCellOwner};

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};
use crate::interpreter::call_outs::CallOut;
use crate::interpreter::lpc_array::LpcArray;
use crate::interpreter::lpc_ref::LpcRef;
use crate::interpreter::process::Process;

/// `query_call_out`, an efun for return information about a single call out.
pub fn query_call_out<const N: usize>(
    context: &mut EfunContext<N>,
    cell_key: &mut QCellOwner,
) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1_usize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    match context.call_outs().ro(cell_key).get(idx as usize) {
        Some(call_out) => {
            let result = call_out_array_ref(context, &call_out)?;
            context.return_efun_result(result);
        }
        None => {
            context.return_efun_result(LpcRef::Int(0));
        }
    }

    Ok(())
}

fn call_out_array_ref<const N: usize>(context: &EfunContext<N>, call_out: &CallOut) -> Result<LpcRef> {
    let mut arr = Vec::new();
    let LpcRef::Function(f) = &call_out.func_ref else {
        return Err(context.runtime_bug("call out function is not a function. This shouldn't be reachable."));
    };

    // push the object that the call out was called from
    arr.push(context.value_to_ref(LpcValue::Object(call_out.process().clone())));

    // push the function
    arr.push(LpcRef::Function(f.clone()));

    // push the number of milliseconds remaining until the function runs
    arr.push(LpcRef::Int(call_out.time_remaining().map(|duration| duration.num_milliseconds()).unwrap_or(0)));

    // push the number of milliseconds between repeats
    arr.push(LpcRef::Int(call_out.repeat_duration().map(|duration| duration.num_milliseconds()).unwrap_or(0)));

    let result = context.value_to_ref(LpcValue::Array(LpcArray::new(arr)));
    Ok(result)
}
