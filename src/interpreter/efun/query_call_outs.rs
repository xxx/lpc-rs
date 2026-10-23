use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

pub fn query_call_outs<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let owner = match context.arg(0) {
        LpcRef::Int(value) if value.0 == -1 => None,
        LpcRef::Object(process) => Some(process.upgrade().ok_or_else(|| {
            context.runtime_error("object in `query_call_outs` is already destructed")
        })?),
        value if value == &context.txn().nil() => Some(context.process().clone()),
        _ => return Err(context.runtime_error("query_call_outs: expected an object or -1")),
    };

    let rows: Vec<LpcRef> = context
        .query_call_outs(owner.as_ref())
        .into_iter()
        .map(|fields| context.mint_array(fields))
        .collect();
    context.return_array(rows);

    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::interpreter::lpc_int::LpcInt;

    use super::*;
    use crate::{
        interpreter::{
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::{permissive_master, run_prog},
    };

    const TIMER_OWNER: &str = r#"
        int id;
        void tick() {}
        void create() { id = call_out(tick, 3600, 10); }
        int timer_id() { return id; }
        int schedule() { return call_out(tick, 7200); }
    "#;

    async fn timer_vm(mode: NullMode) -> Vm {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        permissive_master(&vm.global_state.object_space).await;
        vm
    }

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
                            assert_eq!(arr[4], LpcRef::from(id as i64 + 1));
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
            .initial_program()
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
                            assert_eq!(arr[4], LpcRef::from(id as i64 + 1));
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
            .initial_program()
            .lookup_function("prune")
            .unwrap()
            .clone();
        task.timed_eval(prune, &[], 500)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));

        assert_eq!(task.result(), Some(LpcRef::from(2)));
        task.context.global_state.with_call_outs(|co| {
            let ids: Vec<_> = co.queue().iter().map(|(_, call_out)| call_out.id).collect();
            assert_eq!(ids, vec![2]);
        });
    }

    #[tokio::test]
    async fn global_query_lists_multiple_owners_and_preserves_owner_selectors() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            vm.initialize_process_from_code("/other.c", TIMER_OWNER)
                .await
                .unwrap();
            let mut task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int id;
                void tick() {}
                int check() {
                    object other = find_object("/other");
                    function query = query_call_outs;
                    mixed *rows = query_call_outs(-1);
                    if (sizeof(rows) != 2 || sizeof(query(-1)) != 2
                        || sizeof(query()) != 1 || query_call_outs()[0][4] != id
                        || query_call_outs(nil)[0][4] != id
                        || query_call_outs(this_object())[0][4] != id
                        || query_call_outs(other)[0][4] != other->timer_id()) return 0;
                    foreach (mixed *row : rows) {
                        if (sizeof(row) != 5 || !functionp(row[1])
                            || row[2] < 0 || row[2] > 3600000
                            || query_call_out(row[4])[0] != row[0]) return 0;
                        if (row[0] == this_object()) {
                            if (row[4] != id || row[3] != 0) return 0;
                        } else if (row[0] == other) {
                            if (row[4] != other->timer_id() || row[3] != 10000) return 0;
                        } else return 0;
                    }
                    return 1;
                }
                void create() {
                    id = call_out(tick, 3600);
                    if (!check()) throw("pending timer query failed");
                }
            "#,
                )
                .await
                .unwrap();
            let check = task
                .context
                .process
                .initial_program()
                .lookup_function("check")
                .unwrap()
                .clone();

            task.timed_eval(check, &[], 500).await.unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn global_query_reflects_pending_schedules_and_cancellations_across_owners() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            vm.initialize_process_from_code("/other.c", TIMER_OWNER)
                .await
                .unwrap();
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                void tick() {}
                int create() {
                    object other = find_object("/other");
                    int own = call_out(tick, 3600);
                    int pending = other->schedule();
                    mixed *rows = query_call_outs(-1);
                    if (sizeof(rows) != 3 || sizeof(query_call_outs(other)) != 2) return 0;
                    foreach (mixed *row : rows) {
                        if (row[4] == own) continue;
                        if (remove_call_out(row[4]) < 0) return 0;
                    }
                    rows = query_call_outs(-1);
                    if (sizeof(rows) != 1 || rows[0][4] != own
                        || query_call_out(pending) != nil
                        || query_call_out(other->timer_id()) != nil
                        || sizeof(query_call_outs(other))) return 0;
                    remove_call_out(own);
                    return sizeof(query_call_outs(-1)) == 0;
                }
            "#,
                )
                .await
                .unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
            vm.global_state
                .with_call_outs(|calls| assert!(calls.is_empty()));
        }
    }

    #[tokio::test]
    async fn global_query_omits_destroyed_owners_before_and_after_commit() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            let _other = vm
                .initialize_process_from_code("/other.c", TIMER_OWNER)
                .await
                .unwrap();
            let mut task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int id;
                void tick() {}
                int check() {
                    mixed *rows = query_call_outs(-1);
                    return sizeof(rows) == 1 && rows[0][0] == this_object() && rows[0][4] == id;
                }
                void create() {
                    object other = find_object("/other");
                    id = call_out(tick, 3600);
                    other->schedule();
                    destruct(other);
                    if (!check()) throw("destroyed owner is still visible");
                }
            "#,
                )
                .await
                .unwrap();
            let check = task
                .context
                .process
                .initial_program()
                .lookup_function("check")
                .unwrap()
                .clone();

            task.timed_eval(check, &[], 500).await.unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
            vm.global_state
                .with_call_outs(|calls| assert_eq!(calls.len(), 1));
        }
    }

    #[tokio::test]
    async fn queries_exclude_schedules_made_after_self_destruction() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                void tick() {}
                int create() {
                    function callback = tick;
                    destruct(this_object());
                    call_out(callback, 3600);
                    return sizeof(query_call_outs(-1)) == 0 && sizeof(query_call_outs()) == 0;
                }
            "#,
                )
                .await
                .unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
            vm.global_state
                .with_call_outs(|calls| assert!(calls.is_empty()));
        }
    }

    #[tokio::test]
    async fn global_query_does_not_retain_aborted_schedules_or_cancellations() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            vm.initialize_process_from_code("/other.c", TIMER_OWNER)
                .await
                .unwrap();
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                void tick() {}
                void abort() {
                    int own = call_out(tick, 3600);
                    remove_call_out(find_object("/other")->timer_id());
                    mixed *rows = query_call_outs(-1);
                    if (sizeof(rows) != 1 || rows[0][4] != own)
                        throw("wrong transaction view");
                    throw("abort timer changes");
                }
                int check() {
                    mixed *rows = query_call_outs(-1);
                    object other = find_object("/other");
                    return sizeof(rows) == 1 && rows[0][0] == other
                        && rows[0][4] == other->timer_id() && !sizeof(query_call_outs());
                }
            "#,
                )
                .await
                .unwrap();
            let process = task.context.process.clone();
            let template = TaskTemplate::from(vm.global_state.clone());
            let error =
                apply_function_by_name("abort", &[], process.clone(), template.clone(), Some(500))
                    .await
                    .unwrap()
                    .unwrap_err();
            assert!(error.to_string().contains("abort timer changes"), "{error}");

            let result = apply_function_by_name("check", &[], process, template, Some(500))
                .await
                .unwrap()
                .unwrap();

            assert_eq!(result, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn empty_global_query_returns_an_array() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    "int create() { mixed *rows = query_call_outs(-1); return arrayp(rows) && !sizeof(rows); }",
                )
                .await
                .unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn invalid_selectors_are_rejected_and_nil_remains_owner_local() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                void tick() {}
                int create() {
                    function query = query_call_outs;
                    foreach (mixed arg : ({ -2, 1, 1.0, "owner", ({}), ([]), tick })) {
                        mixed error = catch(query_call_outs(arg));
                        if (!stringp(error) || strindex(error, "query_call_outs") < 0) return 0;
                        if (!catch(query(arg))) return 0;
                    }
                    call_out(tick, 3600);
                    if (sizeof(query_call_outs(nil)) != 1 || sizeof(query_call_outs()) != 1) return 0;
                    mixed zero_error = catch(query_call_outs(0));
                    return (nil == 0) ? !zero_error : !!zero_error;
                }
            "#,
                )
                .await
                .unwrap();

            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn compiler_checks_selector_types_and_argument_count() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = timer_vm(mode).await;
            for (call, expected) in [
                ("query_call_outs(-1, 1)", "incorrect argument count"),
                ("query_call_outs(\"owner\")", "unexpected argument type"),
                ("query_call_outs(1.0)", "unexpected argument type"),
                ("query_call_outs(({}))", "unexpected argument type"),
            ] {
                let error = vm
                    .initialize_process_from_code(
                        "/caller.c",
                        format!("mixed *create() {{ return {call}; }}"),
                    )
                    .await
                    .unwrap_err()
                    .to_string();

                assert!(error.contains(expected), "{mode}, {call}: {error}");
            }
        }
    }
}
