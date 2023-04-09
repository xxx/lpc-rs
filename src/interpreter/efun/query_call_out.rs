use lpc_rs_errors::Result;
use qcell::QCellOwner;

use crate::interpreter::{
    call_outs::CallOut, efun::efun_context::EfunContext, lpc_array::LpcArray, lpc_ref::LpcRef,
    lpc_value::LpcValue,
};

/// `query_call_out`, an efun for returning information about a single call out.
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
            let result = call_out_array_ref(context, call_out)?;
            context.return_efun_result(result);
        }
        None => {
            context.return_efun_result(LpcRef::Int(0));
        }
    }

    Ok(())
}

/// get the result Array reference to returning from `query_call_out` and `query_call_outs`
pub fn call_out_array_ref<const N: usize>(
    context: &EfunContext<N>,
    call_out: &CallOut,
) -> Result<LpcRef> {
    let mut arr = Vec::new();
    let LpcRef::Function(f) = &call_out.func_ref else {
        return Err(context.runtime_bug("call out function is not a function. This shouldn't be reachable."));
    };

    // push the object that the call out was called from
    arr.push(context.value_to_ref(LpcValue::Object(call_out.process().clone())));

    // push the function
    arr.push(LpcRef::Function(f.clone()));

    // push the number of milliseconds remaining until the function runs
    arr.push(LpcRef::Int(
        call_out
            .time_remaining()
            .map(|duration| duration.num_milliseconds())
            .unwrap_or(0),
    ));

    // push the number of milliseconds between repeats
    arr.push(LpcRef::Int(
        call_out
            .repeat_duration()
            .map(|duration| duration.num_milliseconds())
            .unwrap_or(0),
    ));

    let result = context.value_to_ref(LpcValue::Array(LpcArray::new(arr)));
    Ok(result)
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use if_chain::if_chain;
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcBank, memory::Memory, object_space::ObjectSpace,
            task::Task,
        },
        test_support::compile_prog,
    };

    #[test]
    fn test_query_call_out() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            mixed create() {
                int id = call_out(call_out_test, 100);

                mixed *result = query_call_out(id);

                remove_call_out(id);

                return result;
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let call_outs = Arc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let task = Task::<10>::initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            Memory::default(),
            cell_key.cell(GcBank::default()),
            call_outs,
            tx,
            &mut cell_key,
        )
        .unwrap();

        if_chain! {
            if let LpcRef::Array(arr) = task.result().unwrap();
            if let LpcValue::Array(LpcArray { array, ..}) = &*arr.read();
            then {
                assert_eq!(array.len(), 4);
                assert!(matches!(array[0], LpcRef::Object(_)));
                assert!(matches!(array[1], LpcRef::Function(_)));
                assert!(matches!(array[2], LpcRef::Int(_)));
                assert_eq!(array[3], LpcRef::Int(0));
            }
            else {
                panic!("result is not an array");
            }
        }
    }
}
