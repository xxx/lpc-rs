use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

pub async fn query_call_outs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match context.resolve_local_register(1 as RegisterSize) {
        LpcRef::Object(process) => process.upgrade(),
        LpcRef::Int(LpcInt(0)) => Some(context.frame().process.clone()),
        _ => return Err(context.runtime_error("non-object sent to `query_call_outs`")),
    };

    let Some(owner) = owner else {
        return Err(context.runtime_error("object in `query_call_outs` is already destructed"));
    };

    let rows: Vec<LpcRef> = context
        .query_call_outs(&owner)
        .into_iter()
        .map(|fields| context.mint_array(fields))
        .collect();
    context.return_array(rows);

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::test_support::initialize_program;
    use crate::{interpreter::vm::global_state::GlobalState, test_support::compile_prog};

    /// The query sees this attempt's own pending call outs: the schedules
    /// are recorded but not yet in the physical queue.
    #[tokio::test]
    async fn test_query_call_outs_includes_own_pending() {
        let code = r##"
            mixed create() {
                call_out(call_out_test, 100);
                call_out(call_out_test, 200);
                return query_call_outs();
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = std::sync::Arc::new(GlobalState::new(config, tx));
        let task = initialize_program::<10>(program, global_state.clone())
            .await
            .unwrap();

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array.len(), 2);

                for call_out in array.iter() {
                    call_out
                        .with_array(task.context.txn(), |arr| {
                            assert_eq!(arr.len(), 4);
                            assert!(matches!(arr[0], LpcRef::Object(_)));
                            assert!(matches!(arr[1], LpcRef::Function(_)));
                            assert!(matches!(arr[2], LpcRef::Int(_)));
                            assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
                        })
                        .unwrap();
                }
            })
            .unwrap();

        // Both call outs materialized when the attempt committed.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 2));
    }

    /// A committed query over the physical queue from a fresh transaction.
    #[tokio::test]
    async fn test_query_call_outs() {
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
        let mut task = initialize_program::<10>(program, global_state.clone())
            .await
            .unwrap();

        // The initializer's transaction committed, so both call outs are physical.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 2));

        task.timed_eval(query_fn.clone(), &[], 500)
            .await
            .expect("query eval failed");

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array.len(), 2);

                for call_out in array.iter() {
                    call_out
                        .with_array(task.context.txn(), |arr| {
                            assert_eq!(arr.len(), 4);
                            assert!(matches!(arr[0], LpcRef::Object(_)));
                            assert!(matches!(arr[1], LpcRef::Function(_)));
                            assert!(matches!(arr[2], LpcRef::Int(_)));
                            assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
                        })
                        .unwrap();
                }
            })
            .unwrap();
    }
}
