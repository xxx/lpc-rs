use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, query_call_out::call_out_array_ref},
    lpc_array::LpcArray,
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
};

/// `query_call_outs`, an efun for returning information about all call outs in a specific object
pub async fn query_call_outs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match context.resolve_local_register(1 as RegisterSize) {
        LpcRef::Object(process) => process.upgrade(),
        LpcRef::Int(LpcInt(0)) => Some(context.frame().process.clone()),
        _ => return Err(context.runtime_error("non-object sent to `query_call_outs`")),
    };

    let Some(owner) = owner else {
        return Err(context.runtime_error("object in `query_call_outs` is already destructed"));
    };

    let vec = context.with_call_outs(|co| {
        co.queue()
            .iter()
            .filter_map(|(_idx, call_out)| {
                if let Some(process) = call_out.process().upgrade()
                    && Arc::ptr_eq(&process, &owner)
                {
                    Some(call_out_array_ref(context, call_out).unwrap())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    });

    let result = context
        .txn()
        .with(|t| LpcRef::Array(t.mint_array(LpcArray::new(vec))));

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        interpreter::{
            task::initialize_program::InitializeProgramBuilder, vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    /// `query_call_outs` reads the physical queue, which is populated only
    /// after the scheduling transaction commits. `create`
    /// schedules both call outs in its own transaction,
    /// which commits and materializes them. The query then runs in a fresh
    /// `timed_eval` over the now-physical queue.
    #[tokio::test]
    async fn test_query_call_out() {
        let code = r##"
            void create() {
                call_out(call_out_test, 100);
                call_out(call_out_test, 200);
            }

            mixed query() {
                return query_call_outs();
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let query_fn = program
            .lookup_function("query")
            .expect("no `query` found")
            .clone();
        let global_state = std::sync::Arc::new(GlobalState::new(config, tx));
        let mut task = InitializeProgramBuilder::<10>::default()
            .global_state(global_state.clone())
            .program(program)
            .build()
            .await
            .unwrap();

        // The initializer's transaction committed, so both call outs are physical.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 2));

        task.timed_eval(query_fn.clone(), &[], 500)
            .await
            .expect("query eval failed");

        task.result()
            .unwrap()
            .with_array(&task.txn, |array| {
                assert_eq!(array.len(), 2);

                for call_out in array.iter() {
                    call_out
                        .with_array(&task.txn, |arr| {
                            assert_eq!(arr.len(), 4);
                            assert!(matches!(arr[0], LpcRef::Object(_)));
                            assert!(matches!(arr[1], LpcRef::Function(_)));
                            assert!(matches!(arr[2], LpcRef::Int(_)));
                            assert_eq!(arr[3], LpcRef::Int(0.into()));
                        })
                        .unwrap();
                }
            })
            .unwrap();
    }
}
