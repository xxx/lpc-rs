use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::NULL};

pub fn query_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let id = context.call_out_id(1 as RegisterSize, "query_call_out")?;
    let result = match context.query_call_out(id) {
        Some(fields) => context.mint_array(fields),
        None => NULL,
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use crate::{
        interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef},
        test_support::run_prog,
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

        let task = run_prog(code).await;
        let global_state = task.context.global_state.clone();

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

        let mut task = run_prog(code).await;
        let global_state = task.context.global_state.clone();
        let query_fn = task
            .context
            .process
            .program
            .lookup_function("query")
            .expect("no `query` found")
            .clone();

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
