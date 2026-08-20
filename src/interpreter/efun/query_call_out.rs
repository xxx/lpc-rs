use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    call_outs::CallOut,
    efun::efun_context::EfunContext,
    lpc_array::LpcArray,
    lpc_ref::{LpcRef, NULL},
};

/// `query_call_out`, an efun for returning information about a single call out.
pub async fn query_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx.0 < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    let result = context.with_call_outs(|co| match co.get(idx.0 as usize) {
        Some(call_out) => call_out_array_ref(context, call_out),
        None => Ok(NULL),
    })?;

    context.return_efun_result(result);

    Ok(())
}

/// get the result Array reference to returning from `query_call_out` and `query_call_outs`
pub fn call_out_array_ref<const N: usize>(
    context: &EfunContext<N>,
    call_out: &CallOut,
) -> Result<LpcRef> {
    let mut arr = Vec::new();
    let LpcRef::Function(f) = &call_out.func_ref else {
        return Err(context
            .runtime_bug("call out function is not a function. This shouldn't be reachable."));
    };

    // push the object that the call out was called from
    arr.push(call_out.process().clone().into());

    // push the function
    arr.push(LpcRef::Function(f.clone()));

    // push the number of milliseconds remaining until the function runs
    arr.push(LpcRef::Int(
        call_out
            .time_remaining()
            .map(|duration| duration.num_milliseconds())
            .unwrap_or(0)
            .into(),
    ));

    // push the number of milliseconds between repeats
    arr.push(LpcRef::Int(
        call_out
            .repeat_duration()
            .map(|duration| duration.num_milliseconds())
            .unwrap_or(0)
            .into(),
    ));

    let result = context
        .txn()
        .with(|t| LpcRef::Array(t.mint_array(LpcArray::new(arr))));
    Ok(result)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        interpreter::{
            lpc_int::LpcInt, task::initialize_program::InitializeProgramBuilder,
            vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_query_call_out() {
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
        let (program, config, _) = compile_prog(code).await;
        let global_state = GlobalState::new(config, tx);
        let task = InitializeProgramBuilder::<10>::default()
            .global_state(global_state)
            .program(program)
            .build()
            .await
            .unwrap();

        task.result()
            .unwrap()
            .with_array(&task.txn, |arr| {
                assert_eq!(arr.len(), 4);
                assert!(matches!(arr[0], LpcRef::Object(_)));
                assert!(matches!(arr[1], LpcRef::Function(_)));
                assert!(matches!(arr[2], LpcRef::Int(_)));
                assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
            })
            .unwrap();
    }
}
