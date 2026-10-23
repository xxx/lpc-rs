use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

pub fn remove_call_out<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let id = context.call_out_id(0, "remove_call_out")?;
    let ret = context.cancel_call_out(id);

    context.return_efun_result(LpcRef::Int(ret.into()));

    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_int::LpcInt,
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        },
        test_config_builder,
        test_support::{run_prog, run_prog_with_config},
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

        let task = run_prog(code).await;
        let global_state = task.context.global_state.clone();

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
            int id;

            void create() {
                id = call_out(call_out_test, 100);
            }

            mixed query() {
                remove_call_out(id);
                return query_call_out(id);
            }

            void call_out_test() {
            }
        "##;

        let mut task = run_prog(code).await;
        let global_state = task.context.global_state.clone();
        let query_fn = task
            .context
            .process
            .initial_program()
            .lookup_function("query")
            .expect("no `query` found")
            .clone();

        // `create` committed, so the call out is in the physical queue.
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

    #[tokio::test]
    async fn repeated_pending_cancellation_returns_not_found() {
        let code = r#"
            void tick() {}
            int create() {
                int id = call_out(tick, 3600);
                if (remove_call_out(id) != 3600000)
                    throw("first cancellation failed");
                return remove_call_out(id);
            }
        "#;

        for mode in [NullMode::Zero, NullMode::Distinct] {
            let config = test_config_builder!().null_mode(mode).build().unwrap();
            let task = run_prog_with_config(code, config.into()).await;

            assert_eq!(task.result(), Some(LpcRef::from(-1)), "{mode}");
            task.context
                .global_state
                .with_call_outs(|calls| assert!(calls.is_empty()));
        }
    }

    #[tokio::test]
    async fn repeated_committed_cancellation_returns_not_found() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for repeat in [0, 60] {
                let code = format!(
                    r#"
                    int id;
                    void tick() {{}}
                    void create() {{ id = call_out(tick, 3600, {repeat}); }}
                    int cancel_twice() {{
                        int remaining = remove_call_out(id);
                        if (remaining < 0 || remaining > 3600000)
                            throw("first cancellation failed");
                        if (query_call_out(id) != nil || sizeof(query_call_outs()))
                            throw("cancelled timer is still visible");
                        return remove_call_out(id);
                    }}
                "#
                );
                let config = test_config_builder!().null_mode(mode).build().unwrap();
                let mut task = run_prog_with_config(&code, config.into()).await;
                let global_state = task.context.global_state.clone();
                global_state.with_call_outs(|calls| assert_eq!(calls.len(), 1));
                let function = task
                    .context
                    .process
                    .initial_program()
                    .lookup_function("cancel_twice")
                    .unwrap()
                    .clone();

                task.timed_eval(function, &[], 500).await.unwrap();

                assert_eq!(task.result(), Some(LpcRef::from(-1)), "{mode}, {repeat}");
                global_state.with_call_outs(|calls| assert!(calls.is_empty()));
            }
        }
    }

    #[tokio::test]
    async fn aborted_cancellation_leaves_timer_cancellable_in_later_transaction() {
        let code = r#"
            int id;
            void tick() {}
            void create() { id = call_out(tick, 3600); }
            void cancel_and_fail() {
                if (remove_call_out(id) < 0 || remove_call_out(id) != -1)
                    throw("incorrect cancellation results");
                throw("abort cancellation");
            }
            int cancel() { return remove_call_out(id); }
        "#;

        for mode in [NullMode::Zero, NullMode::Distinct] {
            let config = test_config_builder!().null_mode(mode).build().unwrap();
            let task = run_prog_with_config(code, config.into()).await;
            let global_state = task.context.global_state.clone();
            let process = task.context.process.clone();
            let template = TaskTemplate::from(global_state.clone());
            let error = apply_function_by_name(
                "cancel_and_fail",
                &[],
                process.clone(),
                template.clone(),
                Some(500),
            )
            .await
            .unwrap()
            .unwrap_err();
            assert!(error.to_string().contains("abort cancellation"), "{error}");
            global_state.with_call_outs(|calls| assert_eq!(calls.len(), 1));

            let result = apply_function_by_name("cancel", &[], process, template, Some(500))
                .await
                .unwrap()
                .unwrap();

            let LpcRef::Int(remaining) = result else {
                panic!("expected milliseconds remaining, got {result:?}");
            };
            assert!((0..=3600000).contains(&remaining.0), "{mode}, {remaining}");
            global_state.with_call_outs(|calls| assert!(calls.is_empty()));
        }
    }
}
