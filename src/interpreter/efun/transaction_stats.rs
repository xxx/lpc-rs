use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
};

fn counter(value: impl TryInto<LpcIntInner>) -> LpcRef {
    value.try_into().unwrap_or(LpcIntInner::MAX).into()
}

pub async fn transaction_stats<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let state = &context.task_context().global_state;
    let commits = state.committer_stats().await?;
    let attempts = state.attempt_telemetry();
    let fields = [
        ("owning_tasks", counter(attempts.owning_tasks)),
        ("owning_attempts", counter(attempts.owning_attempts)),
        (
            "admission_acquisitions",
            counter(attempts.admission_acquisitions),
        ),
        (
            "admission_wait_ns",
            counter(attempts.admission_wait.as_nanos()),
        ),
        ("admission_timeouts", counter(attempts.admission_timeouts)),
        ("applies", counter(attempts.applies)),
        ("attempts", counter(attempts.attempts)),
        ("conflicts", counter(attempts.conflicts)),
        ("errors", counter(attempts.errors)),
        ("total_ns", counter(attempts.total.as_nanos())),
        (
            "backoff_yield_ns",
            counter(attempts.backoff_yield.as_nanos()),
        ),
        (
            "backoff_sleep_ns",
            counter(attempts.backoff_sleep.as_nanos()),
        ),
        (
            "backoff_sleep_requested_ns",
            counter(attempts.backoff_sleep_requested.as_nanos()),
        ),
        (
            "backoff_commit_wakes",
            counter(attempts.backoff_commit_wakes),
        ),
        (
            "backoff_cap_expiries",
            counter(attempts.backoff_cap_expiries),
        ),
        ("commits", counter(commits.commits)),
        ("read_only_commits", counter(commits.read_only_commits)),
        ("commit_conflicts", counter(commits.conflicts)),
        ("reply_failures", counter(commits.reply_failures)),
        ("busy_ns", counter(commits.busy_ns)),
        ("commit_service_ns", counter(commits.commit_service_ns)),
        (
            "validation_scanned_versions",
            counter(commits.validation_scanned_versions),
        ),
        ("queue_len", counter(state.committer_queue_len())),
        ("queue_peak", counter(commits.queue_peak)),
        ("evictions", counter(commits.evictions)),
        ("live_snapshots", counter(commits.live_snapshots)),
        ("versions_retained", counter(commits.versions_retained)),
    ];
    context.return_mapping(LpcMapping::new(
        fields
            .into_iter()
            .map(|(key, value)| (key.into(), value))
            .collect(),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            task::{apply_function::applied_by_name, task_template::TaskTemplate},
            vm::global_state::GlobalState,
        },
        test_support::{run_prog, test_config},
    };

    #[test]
    fn large_counters_saturate_instead_of_becoming_negative() {
        assert_eq!(counter(u64::MAX), LpcRef::from(LpcIntInner::MAX));
        assert_eq!(counter(u128::MAX), LpcRef::from(LpcIntInner::MAX));
        assert_eq!(counter(42usize), LpcRef::from(42));
    }

    #[tokio::test]
    async fn reports_finished_runs_retries_failures_and_read_only_commits() {
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(Arc::new(test_config()), tx, 8));
        let task = state
            .initialize_process_from_code(
                "/stats.c",
                indoc! { r#"
            int value = 1;
            int read() { return value; }
            void fail() { throw("stats test failure"); }
            mapping sample() { return transaction_stats(); }
        "# },
            )
            .await
            .unwrap();
        let process = task.context.process.clone();
        let template = TaskTemplate::from(state.clone());
        let read = applied_by_name("read", &[], process.clone(), template.clone(), None)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(read.value(), &LpcRef::from(1));
        assert!(
            applied_by_name("fail", &[], process.clone(), template.clone(), None)
                .await
                .unwrap()
                .is_err()
        );

        let attempts = state.attempt_telemetry();
        let commits = state.committer_stats().await.unwrap();
        let sample = applied_by_name("sample", &[], process, template, None)
            .await
            .unwrap()
            .unwrap();
        let fields = sample.mapping().unwrap();
        let get = |key: &str| fields.get(&LpcRef::from(key)).unwrap().clone();
        assert_eq!(get("applies"), counter(attempts.applies));
        assert_eq!(get("attempts"), counter(attempts.attempts));
        assert_eq!(get("conflicts"), counter(attempts.conflicts));
        assert_eq!(get("errors"), counter(attempts.errors));
        assert_eq!(get("commits"), counter(commits.commits));
        assert_eq!(get("read_only_commits"), counter(commits.read_only_commits));
        assert_eq!(get("commit_conflicts"), counter(commits.conflicts));
        assert_eq!(get("conflicts"), LpcRef::from(8));
        assert_eq!(get("total_ns"), counter(attempts.total.as_nanos()));
        assert_eq!(
            get("backoff_yield_ns"),
            counter(attempts.backoff_yield.as_nanos())
        );
        assert_eq!(
            get("backoff_sleep_ns"),
            counter(attempts.backoff_sleep.as_nanos())
        );
        assert!(attempts.errors > 0);
        assert!(commits.read_only_commits > 0);
        assert!(attempts.backoff_sleep.as_nanos() > 0);
        assert!(attempts.backoff_yield.as_nanos() > 0);
        assert_eq!(get("owning_tasks"), counter(attempts.owning_tasks));
        assert_eq!(get("owning_attempts"), counter(attempts.owning_attempts));
        assert_eq!(
            get("admission_acquisitions"),
            counter(attempts.admission_acquisitions)
        );
        assert_eq!(
            get("admission_wait_ns"),
            counter(attempts.admission_wait.as_nanos())
        );
        assert_eq!(
            get("admission_timeouts"),
            counter(attempts.admission_timeouts)
        );
        assert_eq!(fields.len(), 27);
        assert!(
            fields
                .values()
                .all(|v| matches!(v, LpcRef::Int(n) if n.0 >= 0))
        );
    }

    #[tokio::test]
    async fn repeated_reads_do_not_reset_counters_or_count_the_current_run() {
        let task = run_prog(indoc! { r#"
            int create() {
                mapping first = transaction_stats();
                first["applies"] = -1;
                mapping second = transaction_stats();
                mapping third = transaction_stats();
                return second["applies"] >= 0
                    && second["applies"] == third["applies"]
                    && second["attempts"] == third["attempts"]
                    && second["commits"] == third["commits"]
                    && second["commit_conflicts"] == third["commit_conflicts"];
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn owning_totals_exclude_nested_room_init_applies() {
        let vm = crate::interpreter::vm::Vm::new(test_config());
        vm.initialize_process_from_code("/stats_room.c", "void init() {}")
            .await
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/stats_actor.c",
                indoc! { r#"
            void create() { enable_commands(); }
            int outer() { move_object("/stats_room"); return 42; }
        "# },
            )
            .await
            .unwrap();
        let state = vm.global_state.clone();
        let before = state.attempt_telemetry();
        let result = applied_by_name(
            "outer",
            &[],
            task.context.process.clone(),
            TaskTemplate::from(state.clone()),
            None,
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result.value(), &LpcRef::from(42));
        let after = state.attempt_telemetry();
        assert_eq!(after.owning_tasks - before.owning_tasks, 1);
        assert_eq!(after.owning_attempts - before.owning_attempts, 1);
        assert_eq!(after.applies - before.applies, 2);
        assert_eq!(after.attempts - before.attempts, 2);
        assert_eq!(
            after.admission_acquisitions - before.admission_acquisitions,
            0
        );
    }
}
