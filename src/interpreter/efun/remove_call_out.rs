use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `remove_call_out`, an efun for removing a call out.
/// This will cancel both upcoming and repeating call outs.
pub async fn remove_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(idx) = context.resolve_local_register(1 as RegisterSize) else {
        return Err(context.runtime_bug("non-int call out ID sent to `remove_call_out`"));
    };

    if idx.0 < 0 {
        return Err(context.runtime_error(format!(
            "invalid call out ID `{idx}` sent to `remove_call_out`"
        )));
    }

    let ret = context.cancel_call_out(idx.0 as u64);

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

    /// Cancelling a committed call out hides it from a same-attempt query:
    /// the removal is deferred to the flush, so the shadow carries the view
    /// within this transaction.
    #[tokio::test]
    async fn test_same_attempt_cancel_hides_from_query() {
        let code = r##"
            void create() {
                call_out(call_out_test, 100);
            }

            mixed query() {
                remove_call_out(0);
                return query_call_out(0);
            }

            void call_out_test() {
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
            .expect("init failed");

        // `create` committed, so call out 0 is in the physical queue.
        global_state.with_call_outs(|co| assert_eq!(co.len(), 1));

        task.timed_eval(query_fn.clone(), &[], 500)
            .await
            .expect("query eval failed");

        // The shadowed query returned NULL.
        let LpcRef::Int(v) = task.result().expect("no result") else {
            panic!("expected int result");
        };
        assert_eq!(v, LpcInt(0));

        // The deferred removal flushed.
        global_state.with_call_outs(|co| assert!(co.is_empty()));
    }
}
