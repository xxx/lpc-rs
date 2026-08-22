use chrono::Duration;
use lpc_rs_core::{LpcFloatInner, LpcIntInner, RegisterSize};
use lpc_rs_errors::{Result, lpc_error};

use crate::interpreter::{
    efun::efun_context::EfunContext, function_type::function_address::FunctionAddress,
    lpc_int::LpcInt, lpc_ref::LpcRef,
};

/// `call_out`, an efun for calling a function at some future point in time
pub async fn call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let func_ref = context.resolve_local_register(1 as RegisterSize).clone();

    // Some validations
    {
        let LpcRef::Function(func) = func_ref.clone() else {
            return Err(context.runtime_error("invalid function sent to `call_out`"));
        };
        if let FunctionAddress::Dynamic(_) = func.address {
            return Err(lpc_error!(
                "cannot `call_out` to a function with a dynamic receiver",
            ));
        }
    }

    let duration_ref = context.resolve_local_register(2 as RegisterSize);
    let duration = match duration_ref {
        LpcRef::Int(x) => Duration::seconds(x.0),
        LpcRef::Float(x) => to_millis(x.0),
        _ => return Err(context.runtime_error("invalid duration sent to `call_out`")),
    };

    let repeat_ref = context.try_resolve_local_register(3 as RegisterSize);
    let repeat = if let Some(repeat_ref) = repeat_ref {
        match repeat_ref {
            LpcRef::Int(x) => {
                if x.0 <= 0 {
                    None
                } else {
                    Some(Duration::seconds(x.0))
                }
            }
            LpcRef::Float(x) => {
                if x.0 <= 0.0 {
                    None
                } else {
                    Some(to_millis(x.0))
                }
            }
            _ => return Err(context.runtime_error("invalid repeat sent to `call_out`")),
        }
    } else {
        None
    };

    let id = context.schedule_call_out(&context.frame().process, func_ref, duration, repeat);

    let result = LpcRef::Int(LpcInt(id as LpcIntInner));
    context.return_efun_result(result);

    Ok(())
}

fn to_millis(x: LpcFloatInner) -> Duration {
    let m = x * 1000.0;
    let millis = if m > i64::MAX as f64 {
        i64::MAX
    } else if m < i64::MIN as f64 {
        i64::MIN
    } else {
        m.into_inner() as i64
    };
    chrono::Duration::milliseconds(millis)
}

#[cfg(test)]
mod tests {

    use crate::test_support::initialize_program;
    use crate::{
        interpreter::{
            lpc_int::LpcInt,
            lpc_ref::LpcRef,
            vm::{global_state::GlobalState, vm_op::VmOp},
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_disallows_dynamic_receiver() {
        let code = r##"
            void create() {
                call_out(&->call_out_test(), 0.1);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = GlobalState::new(config, tx);
        let result = initialize_program::<10>(program, global_state).await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "cannot `call_out` to a function with a dynamic receiver"
        );
    }

    /// Scheduling is a deferred effect: the timer task and the queue entry
    /// materialize only when the task's transaction commits and its effects
    /// flush. After the init task commits, the queue holds exactly the one
    /// call out, and the timer task fires its materialization's slot (0).
    #[tokio::test]
    async fn test_enqueues_task_on_commit() {
        let code = r##"
            void create() {
                call_out(call_out_test, 0.001);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, mut rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = std::sync::Arc::new(GlobalState::new(config, tx));
        let _ = initialize_program::<5>(program, global_state.clone())
            .await
            .unwrap();

        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));

        // The timer task fires the slot its materialization returned (0),
        // not the explicit ID.
        let msg = rx.recv().await.unwrap();
        assert_eq!(msg, VmOp::PrioritizeCallOut(0));
    }

    /// The ID `call_out` returns is the one `remove_call_out` (in the same
    /// transaction) can cancel: the removed call out never materializes, the
    /// surviving one does. This is the load-bearing proof that the returned
    /// ID and the pending list's ID are the same ID.
    #[tokio::test]
    async fn test_cancellation_of_own_pending_call_out() {
        let code = r##"
            void create() {
                int a = call_out(call_out_test, 100);
                int b = call_out(call_out_test, 100);
                remove_call_out(a);
            }

            void call_out_test() {
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = std::sync::Arc::new(GlobalState::new(config, tx));
        let _ = initialize_program::<10>(program, global_state.clone())
            .await
            .unwrap();

        // Only the un-removed call out made it into the physical queue.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));
    }

    /// The query sees this attempt's own pending call out: the schedule is
    /// recorded but not yet in the physical queue.
    #[tokio::test]
    async fn test_same_attempt_query_sees_pending() {
        let code = r##"
            mixed create() {
                int id = call_out(call_out_test, 100);
                return query_call_out(id);
            }

            void call_out_test() {
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = std::sync::Arc::new(GlobalState::new(config, tx));
        let task = initialize_program::<10>(program, global_state.clone())
            .await
            .expect("init failed");

        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                assert_eq!(arr.len(), 4);
                assert!(matches!(arr[0], LpcRef::Object(_)));
                assert!(matches!(arr[1], LpcRef::Function(_)));
                // A pending entry's timer has not started: the full delay is
                // the remaining time.
                assert_eq!(arr[2], LpcRef::Int(LpcInt(100_000)));
                assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
            })
            .expect("expected an array result");

        // The schedule materialized when the attempt committed.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));
    }
}
