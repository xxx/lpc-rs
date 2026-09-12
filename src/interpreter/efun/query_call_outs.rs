use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

pub fn query_call_outs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match context.arg(0) {
        LpcRef::Object(process) => process.upgrade(),
        LpcRef::Int(LpcInt(0)) => Some(context.process().clone()),
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
    use crate::test_support::run_prog;

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

        let task = run_prog(code).await;
        let global_state = task.context.global_state.clone();

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array.len(), 2);

                for (id, call_out) in array.iter().enumerate() {
                    call_out
                        .with_array(task.context.txn(), |arr| {
                            assert_eq!(arr.len(), 5);
                            assert!(matches!(arr[0], LpcRef::Object(_)));
                            assert!(matches!(arr[1], LpcRef::Function(_)));
                            assert!(matches!(arr[2], LpcRef::Int(_)));
                            assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
                            assert_eq!(arr[4], LpcRef::from(id as i64));
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

        let mut task = run_prog(code).await;
        let global_state = task.context.global_state.clone();
        let query_fn = task
            .context
            .process
            .program
            .lookup_function("query")
            .expect("no `query` found")
            .clone();

        // The initializer's transaction committed, so both call outs are physical.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 2));

        task.timed_eval(query_fn.clone(), &[], 500)
            .await
            .expect("query eval failed");

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array.len(), 2);

                for (id, call_out) in array.iter().enumerate() {
                    call_out
                        .with_array(task.context.txn(), |arr| {
                            assert_eq!(arr.len(), 5);
                            assert!(matches!(arr[0], LpcRef::Object(_)));
                            assert!(matches!(arr[1], LpcRef::Function(_)));
                            assert!(matches!(arr[2], LpcRef::Int(_)));
                            assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
                            assert_eq!(arr[4], LpcRef::from(id as i64));
                        })
                        .unwrap();
                }
            })
            .unwrap();
    }

    #[tokio::test]
    async fn filtered_ids_remove_committed_and_pending_call_outs() {
        let code = r#"
            int kept;
            int committed;

            void keep_me() {}
            void cancel_me() {}

            void create() {
                int discarded = call_out(keep_me, 100);
                remove_call_out(discarded);
                kept = call_out(keep_me, 100, 10);
                committed = call_out(cancel_me, 100);
            }

            int prune() {
                int discarded = call_out(keep_me, 100);
                remove_call_out(discarded);
                int pending = call_out(cancel_me, 100, 5);
                mixed *matches = filter(query_call_outs(),
                    (: function_name($1[1]) == "cancel_me" :));
                if (sizeof(matches) != 2) throw("expected two matching call outs");

                foreach (mixed *row : matches) {
                    int id = row[4];
                    if (id != committed && id != pending) throw("wrong call out ID");
                    if (query_call_out(id)[4] != id) throw("query ID mismatch");
                    if (remove_call_out(id) < 0) throw("call out not removed");
                    if (query_call_out(id)) throw("removed call out still visible");
                }

                mixed *remaining = query_call_outs();
                if (sizeof(remaining) != 1 || remaining[0][4] != kept)
                    throw("wrong call outs remain");
                return kept;
            }
        "#;

        let mut task = run_prog(code).await;
        let prune = task
            .context
            .process
            .program
            .lookup_function("prune")
            .unwrap()
            .clone();
        task.timed_eval(prune, &[], 500)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        task.context.global_state.with_call_outs(|co| {
            let ids: Vec<_> = co.queue().iter().map(|(_, call_out)| call_out.id).collect();
            assert_eq!(ids, vec![1]);
        });
    }
}
