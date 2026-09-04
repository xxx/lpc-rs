use chrono::Duration;
use lpc_rs_core::{LpcFloatInner, LpcIntInner};
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef};

/// `call_out`, an efun for calling a function at some future point in time
pub fn call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let func_ref = context.arg(0).clone();

    // Some validations
    {
        let LpcRef::Function(func) = func_ref.clone() else {
            return Err(context.runtime_error("invalid function sent to `call_out`"));
        };
        if !func.receiver_bound() {
            return Err(context.runtime_error(
                "`call_out` needs the receiver of a dynamic function pointer bound",
            ));
        }
    }

    let duration_ref = context.arg(1);
    let duration = match duration_ref {
        LpcRef::Int(x) => Duration::seconds(x.0),
        LpcRef::Float(x) => to_millis(x.0),
        _ => return Err(context.runtime_error("invalid duration sent to `call_out`")),
    };

    let repeat_ref = context.try_arg(2);
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

    let id = context.schedule_call_out(context.process(), func_ref, duration, repeat);

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

    use crate::{
        interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::vm_op::VmOp},
        test_support::{run_prog, run_prog_with_vm_rx, try_run_prog},
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

        let result = try_run_prog(code).await;

        assert_eq!(
            result.unwrap_err().to_string(),
            "runtime error: `call_out` needs the receiver of a dynamic function pointer bound"
        );
    }

    /// Scheduling is a deferred effect: the timer task and the queue entry
    /// materialize only when the task's transaction commits and its effects
    /// flush. After the init task commits, the queue holds exactly the one
    /// call out, and the timer task fires its ID.
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

        let (task, mut rx) = run_prog_with_vm_rx(code).await;

        let id = task.context.global_state.with_call_outs(|co| {
            assert_eq!(co.len(), 1);
            co.queue().iter().next().unwrap().1.id
        });

        let msg = rx.recv().await.unwrap();
        assert_eq!(msg, VmOp::PrioritizeCallOut(id));
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

        let task = run_prog(code).await;
        let global_state = task.context.global_state.clone();

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

        let task = run_prog(code).await;
        let global_state = task.context.global_state.clone();

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
