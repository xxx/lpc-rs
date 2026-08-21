use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::Effect};

/// `remove_call_out`, an efun for removing a call out.
/// This will cancel both upcoming and repeating call outs.
///
/// Cancellation is transactional, in two tiers:
/// - A call out this attempt recorded is dropped from the attempt's pending
///   list, so it will never materialize. It is also a no-op against the
///   physical queue (it isn't there yet).
/// - A committed call out (already in the physical queue) is removed via a
///   deferred [`Effect::CancelCallOut`], flushed only when this attempt
///   commits. An aborted attempt's removal is dropped with it, so the
///   committed call out survives a retry.
pub async fn remove_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx.0 < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    let id = idx.0 as u64;

    let ret = match context.txn().cancel_pending_call_out(id) {
        // This attempt's own pending call out: drop it (it will never
        // materialize). Return the full delay: it has not run yet.
        Some(ms) => ms,
        // Not one of this attempt's: look for a committed one. The read is
        // of committed state (the physical queue); the removal is deferred.
        // The closure must return an owned value, not a borrow from the
        // transient lock guard.
        None => {
            let remaining = context.with_call_outs(|co| {
                co.get_by_id(id).map(|call_out| {
                    call_out
                        .time_remaining()
                        .map(|duration| duration.num_milliseconds())
                        .unwrap_or(0)
                })
            });

            match remaining {
                Some(ms) => {
                    context.txn().record_effect(Effect::CancelCallOut { id });
                    ms
                }
                // Not pending and not committed: it is unknown (or already
                // fired and removed itself).
                None => -1,
            }
        }
    };

    let result = LpcRef::Int(ret.into());
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use crate::{
        interpreter::{
            lpc_int::LpcInt, lpc_ref::LpcRef, task::initialize_program::InitializeProgramBuilder,
            vm::global_state::GlobalState,
        },
        test_support::compile_prog,
    };

    #[tokio::test]
    async fn test_removes_task() {
        let code = r##"
            int create() {
                int id = call_out(call_out_test, 0.1);

                return remove_call_out(id);
            }

            void call_out_test() {
                dump("foobar");
            }
        "##;

        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let (program, config, _) = compile_prog(code).await;
        let global_state = Arc::new(GlobalState::new(config, tx));
        let task = InitializeProgramBuilder::<10>::default()
            .program(program)
            .global_state(global_state.clone())
            .build()
            .await
            .expect("init failed");

        // The same-attempt cancellation returns the call out's full delay
        // (it never ran), not -1 (which would mean "not found").
        let LpcRef::Int(ms) = task.result().expect("no result") else {
            panic!("expected int result");
        };
        assert_eq!(ms, LpcInt(100));

        // Nothing materialized: the cancelled call out never reached the
        // physical queue.
        global_state.with_call_outs(|co| {
            assert!(co.is_empty());
        });
    }
}
