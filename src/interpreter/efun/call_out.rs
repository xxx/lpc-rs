use std::sync::Arc;

use chrono::Duration;
use lpc_rs_core::{LpcFloatInner, LpcIntInner, RegisterSize};
use lpc_rs_errors::{Result, lpc_error};

use crate::interpreter::{
    efun::efun_context::EfunContext, function_type::function_address::FunctionAddress,
    lpc_int::LpcInt, lpc_ref::LpcRef, stm::CallOutSchedule,
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

    // Scheduling is a deferred effect: the ID is minted now, the timer task
    // and queue entry materialize only if this attempt commits. The ID is
    // not the queue's slot, which no one can know before the flush.
    let id = context.with_call_outs(|co| co.mint_id());
    let process = Arc::downgrade(&context.frame().process);
    context.txn().record_call_out(CallOutSchedule {
        id,
        process,
        func_ref,
        delay: duration,
        repeat,
    });

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
        interpreter::{
            task::initialize_program::InitializeProgramBuilder,
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
        let result = InitializeProgramBuilder::<10>::default()
            .global_state(global_state)
            .program(program)
            .build()
            .await;

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
        let _ = InitializeProgramBuilder::<5>::default()
            .global_state(global_state.clone())
            .program(program)
            .build()
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
        let _ = InitializeProgramBuilder::<10>::default()
            .global_state(global_state.clone())
            .program(program)
            .build()
            .await
            .unwrap();

        // Only the un-removed call out made it into the physical queue.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));
    }
}
