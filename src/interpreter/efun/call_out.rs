use chrono::Duration;
use lpc_rs_core::{LpcFloatInner, LpcIntInner};
use lpc_rs_errors::Result;

use crate::interpreter::{
    call_outs::CallOuts, efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef,
};

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

    let duration = to_duration(context.arg(1))
        .ok_or_else(|| context.runtime_error("invalid duration sent to `call_out`"))?;
    let repeat = context
        .try_arg(2)
        .map(|value| {
            to_duration(value)
                .ok_or_else(|| context.runtime_error("invalid repeat sent to `call_out`"))
        })
        .transpose()?
        .filter(|duration| *duration != Duration::zero());

    if CallOuts::checked_deadlines(duration, repeat).is_none() {
        return Err(context.runtime_error("call_out: time exceeds the supported clock range"));
    }

    let id = context.schedule_call_out(context.process(), func_ref, duration, repeat);

    let result = LpcRef::Int(LpcInt(id as LpcIntInner));
    context.return_efun_result(result);

    Ok(())
}

fn to_duration(value: &LpcRef) -> Option<Duration> {
    match value {
        LpcRef::Int(value) => Duration::try_seconds(value.0.max(0)),
        LpcRef::Float(value) => to_millis(value.0),
        _ => None,
    }
}

fn to_millis(value: LpcFloatInner) -> Option<Duration> {
    let seconds = value.into_inner();
    if !seconds.is_finite() {
        return None;
    }
    let millis = seconds.max(0.0) * 1000.0;
    if millis >= i64::MAX as f64 {
        return None;
    }
    Duration::try_milliseconds(millis as i64)
}

#[cfg(test)]
mod tests {
    use super::{to_duration, to_millis};

    use crate::{
        interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::vm_op::VmOp},
        test_support::{run_prog, run_prog_with_vm_rx, try_run_prog},
    };

    #[test]
    fn duration_conversion_checks_numeric_bounds() {
        assert_eq!(
            to_duration(&LpcRef::from(i64::MIN)),
            Some(chrono::Duration::zero())
        );
        assert!(to_duration(&LpcRef::from(i64::MAX)).is_none());
        assert_eq!(
            to_millis(1.25.into()),
            Some(chrono::Duration::milliseconds(1250))
        );
        assert_eq!(to_millis((-1.0e100).into()), Some(chrono::Duration::zero()));
        for value in [
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MAX,
            i64::MAX as f64 / 1000.0,
        ] {
            assert!(to_millis(value.into()).is_none(), "{value}");
        }
    }

    #[tokio::test]
    async fn float_delays_and_repeats_keep_millisecond_precision() {
        let task = run_prog(
            r#"
            void tick() {}
            int create() {
                int id = call_out(tick, 1.25, 2.5);
                mixed *entry = query_call_out(id);
                return entry[2] == 1250 && entry[3] == 2500;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn extreme_nonpositive_delays_are_immediate() {
        for delay in ["(-9223372036854775807 - 1)", "-1.0e100"] {
            let code =
                format!("void tick() {{}} int create() {{ return call_out(tick, {delay}); }}");
            let (task, mut rx) = run_prog_with_vm_rx(&code).await;
            assert_eq!(task.result(), Some(LpcRef::from(1)));
            let message = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
                .await
                .unwrap()
                .unwrap();
            assert!(matches!(message, VmOp::PrioritizeCallOut(1)), "{message:?}");
        }
    }

    #[tokio::test]
    async fn times_outside_the_supported_clocks_are_runtime_errors() {
        for arguments in [
            "9223372036854775807",
            "0, 9223372036854775807",
            "1.0e100",
            "0, 1.0e100",
            "1000000000000000",
            "0, 1000000000000000",
        ] {
            let code = format!("void tick() {{}} void create() {{ call_out(tick, {arguments}); }}");
            let Err(error) = try_run_prog(&code).await else {
                panic!("call_out accepted unsupported times: {arguments}");
            };
            assert!(error.to_string().contains("call_out"), "{error}");
        }
    }

    #[tokio::test]
    async fn a_repeat_below_one_millisecond_is_retired_after_firing() {
        let task =
            run_prog("void tick() {} int create() { return call_out(tick, 10, 0.0001); }").await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        let state = &task.context.global_state;
        state.prioritize_call_out(1).await.await.unwrap();
        assert!(state.with_call_outs(|calls| calls.get_by_id(1).is_none()));
    }

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
                assert_eq!(arr.len(), 5);
                assert!(matches!(arr[0], LpcRef::Object(_)));
                assert!(matches!(arr[1], LpcRef::Function(_)));
                // A pending entry's timer has not started: the full delay is
                // the remaining time.
                assert_eq!(arr[2], LpcRef::Int(LpcInt(100_000)));
                assert_eq!(arr[3], LpcRef::Int(LpcInt(0)));
                assert_eq!(arr[4], LpcRef::from(1));
            })
            .expect("expected an array result");

        // The schedule materialized when the attempt committed.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));
    }

    #[tokio::test]
    async fn zero_never_queries_or_cancels_the_first_timer() {
        let code = r#"
            int id;

            void tick() {}

            int check_zero() {
                if (!id) throw("timer ID is false");
                if (query_call_out(0)) throw("zero matched a timer");
                if (remove_call_out(0) != -1) throw("zero cancelled a timer");
                if (!query_call_out(id)) throw("timer was lost");
                return id;
            }

            int create() {
                id = call_out(tick, 100);
                return check_zero();
            }
        "#;

        let mut task = run_prog(code).await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));

        let check = task
            .context
            .process
            .initial_program()
            .lookup_function("check_zero")
            .unwrap()
            .clone();
        task.timed_eval(check, &[], 500).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        task.context
            .global_state
            .with_call_outs(|co| assert_eq!(co.len(), 1));
    }
}
