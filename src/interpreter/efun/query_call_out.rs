use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_array::LpcArray,
    lpc_ref::{LpcRef, NULL},
};

/// `query_call_out`, an efun for returning information about a single call out.
pub async fn query_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `query_call_out`"));
    };

    if idx.0 < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `query_call_out`"
        )));
    }

    // The interface returns owned fields; the result cell is minted after
    // the queue scan, so no call-out lock is held across it.
    let fields = context.query_call_out(idx.0 as u64);
    let result = match fields {
        Some(fields) => context
            .txn()
            .with(|t| LpcRef::Array(t.mint_array(LpcArray::new(fields)))),
        None => NULL,
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::test_support::initialize_program;
    use crate::{
        interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::global_state::GlobalState},
        test_support::compile_prog,
    };

    /// The query sees this attempt's own pending call out: the schedule is
    /// recorded but not yet in the physical queue.
    #[tokio::test]
    async fn test_query_call_out_sees_own_pending() {
        let code = r##"
            mixed create() {
                int id = call_out(call_out_test, 100);
                return query_call_out(id);
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
            .with_array(task.context.txn(), |arr| {
                assert_eq!(arr.len(), 4);
                assert!(matches!(arr[0], LpcRef::Object(_)));
                assert!(matches!(arr[1], LpcRef::Function(_)));
                assert_eq!(arr[2], LpcRef::Int(LpcInt(100_000)));
                assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
            })
            .expect("expected an array result");

        // The initializer's transaction committed, so the call out is physical.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));
    }

    /// A committed call out is visible from a fresh transaction over the
    /// physical queue.
    #[tokio::test]
    async fn test_query_call_out() {
        let code = r##"
            void create() {
                call_out(call_out_test, 100);
            }

            mixed query() {
                return query_call_out(0);
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

        // The initializer's transaction committed, so the call out is physical.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));

        task.timed_eval(query_fn.clone(), &[], 500)
            .await
            .expect("query eval failed");

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                assert_eq!(arr.len(), 4);
                assert!(matches!(arr[0], LpcRef::Object(_)));
                assert!(matches!(arr[1], LpcRef::Function(_)));
                assert!(matches!(arr[2], LpcRef::Int(_)));
                assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
            })
            .unwrap();
    }
}
