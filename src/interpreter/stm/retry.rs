//! The attempt runner: re-runs a transaction's attempts until one commits.
//!
//! The runner owns the committer protocol (open, commit, release-after-reply,
//! re-base, stats); the body owns the attempt-level work and its physical
//! output. Production runs a [`Task`](crate::interpreter::task::Task) or a
//! `GlobalState::attach`; tests run a bare
//! [`Transaction`](crate::interpreter::stm::Transaction).

use std::{
    sync::{Arc, atomic::AtomicU64},
    time::Duration,
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{LpcError, Result, lpc_error};
use tokio::time::{Instant, timeout_at};

use crate::{
    command::registry::RuleList,
    interpreter::{
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        process::Process,
        stm::{
            CommitOrigin, Conflict, GcPassReply, VarId, Version, WorldRoot, WorldValue,
            admission::Admission,
            backoff::{Backoff, BackoffSpent},
            changeset::Changeset,
            committer::{CommitProtocol, CommitterStats, LiveSnapshot},
        },
        vm::global_state::GlobalState,
    },
    telnet::connection::Connection,
};

/// Per-attempt statistics, owned by the attempt loop: the committer can't
/// see retries, only this loop can.
#[derive(Debug, Default)]
pub(crate) struct RetryStats {
    /// Attempts whose evaluation was started.
    pub(crate) attempts: u64,
    /// Conflicts observed across attempts.
    pub(crate) conflicts: u64,
    /// Wall time of the whole loop, successful attempt included.
    pub(crate) duration: Duration,
    /// Realized backoff totals for the loop.
    pub(crate) backoff: BackoffSpent,
    pub(crate) phases: PhaseTimes,
    pub(crate) last_conflict: Option<Conflict>,
    pub(crate) admission: AdmissionStats,
}

#[derive(Debug, Default)]
pub(crate) struct AdmissionStats {
    pub(crate) acquisitions: u64,
    pub(crate) waited: Duration,
    pub(crate) timeouts: u64,
}

/// Attempt time includes snapshot acquisition; compilation is a subset of it.
#[derive(Debug, Default)]
pub(crate) struct PhaseTimes {
    pub(crate) attempt: Duration,
    pub(crate) compilation: Duration,
    pub(crate) commit: Duration,
    pub(crate) delivery: Duration,
}

/// Attempt-loop lifetime totals, recorded once per finished invocation.
#[derive(Debug, Default)]
pub struct AttemptTelemetry {
    applies: AtomicU64,
    attempts: AtomicU64,
    conflicts: AtomicU64,
    errors: AtomicU64,
    total_ns: AtomicU64,
    backoff_yield_ns: AtomicU64,
    backoff_sleep_ns: AtomicU64,
    backoff_sleep_requested_ns: AtomicU64,
    backoff_commit_wakes: AtomicU64,
    backoff_cap_expiries: AtomicU64,
    owning_tasks: AtomicU64,
    owning_attempts: AtomicU64,
    admission_acquisitions: AtomicU64,
    admission_wait_ns: AtomicU64,
    admission_timeouts: AtomicU64,
}

impl AttemptTelemetry {
    /// Fold one finished attempt loop into the totals.
    fn record(&self, stats: &RetryStats, errored: bool, owning: bool) {
        use std::sync::atomic::Ordering::Relaxed;
        if owning {
            self.owning_tasks.fetch_add(1, Relaxed);
            self.owning_attempts.fetch_add(stats.attempts, Relaxed);
            self.admission_acquisitions
                .fetch_add(stats.admission.acquisitions, Relaxed);
            self.admission_wait_ns
                .fetch_add(stats.admission.waited.as_nanos() as u64, Relaxed);
            self.admission_timeouts
                .fetch_add(stats.admission.timeouts, Relaxed);
        }
        self.applies.fetch_add(1, Relaxed);
        self.attempts.fetch_add(stats.attempts, Relaxed);
        self.conflicts.fetch_add(stats.conflicts, Relaxed);
        self.errors.fetch_add(u64::from(errored), Relaxed);
        self.total_ns
            .fetch_add(stats.duration.as_nanos() as u64, Relaxed);
        self.backoff_yield_ns
            .fetch_add(stats.backoff.yielded.as_nanos() as u64, Relaxed);
        self.backoff_sleep_ns
            .fetch_add(stats.backoff.slept.as_nanos() as u64, Relaxed);
        self.backoff_sleep_requested_ns
            .fetch_add(stats.backoff.sleep_requested.as_nanos() as u64, Relaxed);
        self.backoff_commit_wakes
            .fetch_add(stats.backoff.commit_wakes, Relaxed);
        self.backoff_cap_expiries
            .fetch_add(stats.backoff.cap_expiries, Relaxed);
    }

    /// Read the totals; fields are loaded individually, so an apply landing
    /// mid-read may straddle the snapshot.
    pub fn snapshot(&self) -> AttemptTelemetrySnapshot {
        use std::sync::atomic::Ordering::Relaxed;
        AttemptTelemetrySnapshot {
            owning_tasks: self.owning_tasks.load(Relaxed),
            owning_attempts: self.owning_attempts.load(Relaxed),
            admission_acquisitions: self.admission_acquisitions.load(Relaxed),
            admission_wait: Duration::from_nanos(self.admission_wait_ns.load(Relaxed)),
            admission_timeouts: self.admission_timeouts.load(Relaxed),
            applies: self.applies.load(Relaxed),
            attempts: self.attempts.load(Relaxed),
            conflicts: self.conflicts.load(Relaxed),
            errors: self.errors.load(Relaxed),
            total: Duration::from_nanos(self.total_ns.load(Relaxed)),
            backoff_yield: Duration::from_nanos(self.backoff_yield_ns.load(Relaxed)),
            backoff_sleep: Duration::from_nanos(self.backoff_sleep_ns.load(Relaxed)),
            backoff_sleep_requested: Duration::from_nanos(
                self.backoff_sleep_requested_ns.load(Relaxed),
            ),
            backoff_commit_wakes: self.backoff_commit_wakes.load(Relaxed),
            backoff_cap_expiries: self.backoff_cap_expiries.load(Relaxed),
        }
    }
}

/// One read of `AttemptTelemetry`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AttemptTelemetrySnapshot {
    /// Finished owning tasks, excluding nested applies.
    pub owning_tasks: u64,
    /// Started attempts of finished owning tasks.
    pub owning_attempts: u64,
    /// Retry turns acquired by finished owning tasks.
    pub admission_acquisitions: u64,
    /// Time acquiring retry turns, including waits that reached the deadline.
    pub admission_wait: Duration,
    /// Finished owning tasks whose admission wait exhausted their allowance.
    pub admission_timeouts: u64,
    /// Finished attempt loops.
    pub applies: u64,
    /// Started attempts across all finished loops, including nested applies.
    pub attempts: u64,
    /// Conflicts observed across all loops.
    pub conflicts: u64,
    /// Loops that ended in an error.
    pub errors: u64,
    /// Wall time summed over whole loops, backoff included.
    pub total: Duration,
    /// Wall time spent in the yield backoff tier.
    pub backoff_yield: Duration,
    /// Wall time spent in the sleep backoff tier.
    pub backoff_sleep: Duration,
    /// Sleep as the ladder requested it, before timer rounding.
    pub backoff_sleep_requested: Duration,
    /// Sleep-tier waits ended by a commit bump.
    pub backoff_commit_wakes: u64,
    /// Sleep-tier waits that ran the full cap.
    pub backoff_cap_expiries: u64,
}

/// One attempt of a transactional body. The body must keep two invariants
/// the interface cannot express:
///
/// - `begin_attempt` may return a commit only for work that re-runs
///   cleanly from scratch on rejection.
/// - the commit must be atomic with the body's writes: a rejected attempt
///   leaves nothing physical behind.
#[async_trait::async_trait]
pub(crate) trait AttemptBody {
    fn is_nested(&self) -> bool {
        false
    }

    fn origin(&self) -> Option<CommitOrigin> {
        None
    }

    fn describe_cell(&self, _cell: VarId) -> Option<String> {
        None
    }

    fn take_compilation_time(&mut self) -> Duration {
        Duration::ZERO
    }

    /// Wall-clock allowance for evaluation across all attempts; zero disables it.
    fn timeout_ms(&self) -> u64 {
        0
    }

    /// The evaluation-limit error, with any execution context the body retains.
    fn timeout_error(&self) -> LpcError {
        LpcError::runtime(format!(
            "evaluation limit of {}ms has been reached",
            self.timeout_ms()
        ))
    }

    /// Open one attempt against the committer's current world, reset the
    /// body, run its work. `None` means nothing to commit (a joiner, or a
    /// read that answered); the loop stops after that attempt.
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>>;

    /// Commit the attempt's changeset and take its physical output for
    /// delivery. The handle arrives disarmed: the committer releases the
    /// pin while processing the commit, after validation, so the history
    /// the commit needs is never evicted under it.
    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), Conflict>,
        Vec<crate::interpreter::stm::Effect>,
    )>;

    /// Deliver the committed attempt's physical output, exactly once.
    async fn deliver(&mut self, effects: Vec<crate::interpreter::stm::Effect>) -> Result<()>;
}

/// One VM's attempt execution, waiting policy, and measurements.
#[derive(Debug)]
pub(crate) struct AttemptRunner {
    tx: flume::Sender<CommitProtocol>,
    telemetry: AttemptTelemetry,
    commit_watch: Option<tokio::sync::watch::Receiver<Version>>,
    admission: Admission,
}

impl AttemptRunner {
    pub(crate) fn new(
        tx: flume::Sender<CommitProtocol>,
        commit_watch: Option<tokio::sync::watch::Receiver<Version>>,
    ) -> Self {
        Self {
            tx,
            telemetry: AttemptTelemetry::default(),
            commit_watch,
            admission: Admission::default(),
        }
    }

    pub(crate) fn telemetry(&self) -> AttemptTelemetrySnapshot {
        self.telemetry.snapshot()
    }

    /// Execute an owner or joiner within its original evaluation allowance.
    pub(crate) async fn run<B: AttemptBody>(&self, body: &mut B) -> (Result<()>, RetryStats) {
        self.execute(body, &self.telemetry).await
    }

    async fn execute<B: AttemptBody>(
        &self,
        body: &mut B,
        telemetry: &AttemptTelemetry,
    ) -> (Result<()>, RetryStats) {
        let started = std::time::Instant::now();
        let owning = !body.is_nested();
        let timeout_ms = body.timeout_ms();
        let deadline =
            (timeout_ms != 0).then(|| Instant::now() + Duration::from_millis(timeout_ms));
        let mut attempts = 0u64;
        let mut conflicts = 0u64;
        let mut phases = PhaseTimes::default();
        let mut admission = AdmissionStats::default();
        let mut last_conflict = None;
        let mut retry_cell = None;
        let mut backoff = match self.commit_watch.clone() {
            Some(watch) => Backoff::watching(watch),
            None => Backoff::new(),
        };

        let mut result = loop {
            if deadline.is_some_and(|deadline| Instant::now() >= deadline) {
                break Err(attempt_timeout(body, attempts, conflicts));
            }
            let turn = if let Some(cell) = retry_cell.take() {
                let waiting = Instant::now();
                let acquired = match deadline {
                    Some(deadline) => timeout_at(deadline, self.admission.acquire(cell)).await,
                    None => Ok(self.admission.acquire(cell).await),
                };
                admission.waited += waiting.elapsed();
                match acquired {
                    Ok(Ok(turn)) => {
                        admission.acquisitions += 1;
                        if deadline.is_some_and(|deadline| Instant::now() >= deadline) {
                            admission.timeouts += 1;
                            break Err(attempt_timeout(body, attempts, conflicts));
                        }
                        Some(turn)
                    }
                    Ok(Err(error)) => break Err(error),
                    Err(_) => {
                        admission.timeouts += 1;
                        break Err(attempt_timeout(body, attempts, conflicts));
                    }
                }
            } else {
                None
            };
            attempts += 1;

            let attempt_started = Instant::now();
            let attempt = match deadline {
                Some(deadline) => match timeout_at(deadline, body.begin_attempt(&self.tx)).await {
                    Ok(attempt) => attempt,
                    Err(_) => Err(attempt_timeout(body, attempts, conflicts)),
                },
                None => body.begin_attempt(&self.tx).await,
            };
            phases.attempt += attempt_started.elapsed();
            if owning {
                phases.compilation += body.take_compilation_time();
            }
            let live = match attempt {
                Ok(live) => live,
                Err(e) => break Err(e),
            };

            let Some(mut live) = live else {
                break Ok(());
            };

            // The commit releases the pin, including when its reply crosses the deadline.
            live.disarm();
            let commit_started = Instant::now();
            let committed = body.commit_phase(&self.tx, live).await;
            phases.commit += commit_started.elapsed();
            drop(turn);
            let (commit, effects) = match committed {
                Ok(c) => c,
                Err(e) => break Err(e),
            };

            match commit {
                Ok(()) => {
                    let delivery_started = Instant::now();
                    let delivered = body.deliver(effects).await;
                    phases.delivery += delivery_started.elapsed();
                    break delivered;
                }
                Err(conflict) => {
                    conflicts += 1;
                    if owning && let Conflict::ReadInvalidated { cell, .. } = &conflict {
                        retry_cell = Some(*cell);
                    }
                    last_conflict = Some(conflict);
                }
            }
            if retry_cell.is_none() {
                match deadline {
                    Some(deadline) => {
                        if timeout_at(deadline, backoff.stagger(conflicts))
                            .await
                            .is_err()
                        {
                            break Err(attempt_timeout(body, attempts, conflicts));
                        }
                    }
                    None => backoff.stagger(conflicts).await,
                }
            }
        };

        let stats = RetryStats {
            attempts,
            conflicts,
            duration: started.elapsed(),
            backoff: backoff.spent(),
            phases,
            last_conflict,
            admission,
        };
        telemetry.record(&stats, result.is_err(), owning);
        if owning && (result.is_err() || stats.conflicts > 0) {
            if let Err(error) = result {
                let summary = diagnostic_summary(body, &stats);
                tracing::warn!(target: "lpc_rs::transactions", diagnostic = %summary, error = %error, "Transaction failed");
                result = Err(error.with_note(summary));
            } else if tracing::enabled!(target: "lpc_rs::transactions", tracing::Level::DEBUG) {
                tracing::debug!(target: "lpc_rs::transactions", diagnostic = %diagnostic_summary(body, &stats), "Transaction committed after retries");
            }
        }
        (result, stats)
    }
}

/// An isolated runner for existing committer/body fixtures.
#[cfg(test)]
pub(crate) async fn run_attempts<B: AttemptBody>(
    tx: &flume::Sender<CommitProtocol>,
    telemetry: &AttemptTelemetry,
    commit_watch: Option<tokio::sync::watch::Receiver<Version>>,
    body: &mut B,
) -> (Result<()>, RetryStats) {
    AttemptRunner::new(tx.clone(), commit_watch)
        .execute(body, telemetry)
        .await
}

fn diagnostic_summary(body: &impl AttemptBody, stats: &RetryStats) -> String {
    let origin = body
        .origin()
        .map_or_else(|| "unknown".to_owned(), |origin| origin.to_string());
    let conflict = stats.last_conflict.as_ref().map_or_else(
        || "none".to_owned(),
        |conflict| {
            let label = conflict.cell().and_then(|cell| body.describe_cell(cell));
            match label {
                Some(label) => format!("{conflict}, field={label}"),
                None => conflict.to_string(),
            }
        },
    );
    let ms = |duration: Duration| duration.as_secs_f64() * 1000.0;
    format!(
        "Transaction {origin}: attempts={}, conflicts={}, elapsed_ms={:.3}, evaluation_and_snapshot_ms={:.3}, compilation_ms={:.3}, commit_phase_ms={:.3}, backoff_yield_ms={:.3}, backoff_sleep_ms={:.3}, admission_wait_ms={:.3}, delivery_ms={:.3}; last_conflict=[{conflict}]",
        stats.attempts,
        stats.conflicts,
        ms(stats.duration),
        ms(stats
            .phases
            .attempt
            .saturating_sub(stats.phases.compilation)),
        ms(stats.phases.compilation),
        ms(stats.phases.commit),
        ms(stats.backoff.yielded),
        ms(stats.backoff.slept),
        ms(stats.admission.waited),
        ms(stats.phases.delivery),
    )
}

fn attempt_timeout(body: &impl AttemptBody, attempts: u64, conflicts: u64) -> LpcError {
    let error = body.timeout_error().with_note(format!(
        "Transaction aborted after {attempts} attempt(s) and {conflicts} conflict(s); uncommitted changes and output were discarded."
    ));
    if conflicts > 0 {
        error.with_note("Concurrent changes forced retries. Check shared state accessed by this operation and reduce the work done in one transaction.")
    } else {
        error
    }
}

/// Send one request to the committer and await its reply on the runtime.
async fn request<R: Send + 'static>(
    tx: &flume::Sender<CommitProtocol>,
    message: impl FnOnce(flume::Sender<R>) -> CommitProtocol,
) -> Result<R> {
    let (reply_tx, reply_rx) = flume::bounded(1);
    tx.send(message(reply_tx))
        .map_err(|_| -> lpc_rs_errors::LpcError { lpc_error!("committer channel closed") })?;
    reply_rx
        .recv_async()
        .await
        .map_err(|_| -> lpc_rs_errors::LpcError { lpc_error!("no reply from committer") })
}

/// Start a transaction against the committer's current world and hand back
/// the release handle.
pub(crate) async fn start_txn(tx: &flume::Sender<CommitProtocol>) -> Result<LiveSnapshot> {
    request(tx, |reply| CommitProtocol::Start { reply }).await
}

/// Client side of a GC pass: send the [`CommitProtocol::GcPass`] message
/// and await its reply. `Ok(Err(_))` = refused, not quiescent.
pub(crate) async fn gc_pass(
    tx: &flume::Sender<CommitProtocol>,
    roots: Vec<WorldRoot>,
) -> Result<GcPassReply> {
    request(tx, |reply| CommitProtocol::GcPass { roots, reply }).await
}

/// The committer's lifetime commit totals; for bench measurement and tooling, not the hot path.
pub(crate) async fn committer_stats(tx: &flume::Sender<CommitProtocol>) -> Result<CommitterStats> {
    request(tx, |reply| CommitProtocol::Stats { reply }).await
}

/// Commit a changeset from a disarmed attempt and await the reply; the
/// commit carries the release of the base version's pin. `Ok(())` =
/// committed; `Ok(Err(Conflict))` = rejected.
pub(crate) async fn commit_changeset(
    tx: &flume::Sender<CommitProtocol>,
    changeset: Changeset,
) -> Result<std::result::Result<(), Conflict>> {
    request(tx, |reply| CommitProtocol::Commit {
        changeset,
        releases_base: true,
        reply,
    })
    .await
}

/// A sync read of the latest committed world for readers outside any
/// transaction: tests, debug and tooling. Never for a task's result: a GC
/// pass can reclaim an answer nothing roots before the read, so the driver
/// reads one through `Applied`. Do not use from an interpreter transaction.
pub trait CommittedReader {
    /// Number of global slots on `process`.
    fn global_slot_count(&self, process: &Process) -> usize;

    /// The committed value of one global slot (absent = `NULL`).
    fn committed_global(&self, process: &Process, reg: RegisterSize) -> LpcRef;

    /// Whether `process`'s initializer has run and committed.
    fn is_initialized(&self, process: &Process) -> bool;

    /// Whether a committed `enable_commands()` is in effect on `process`.
    fn commands_enabled(&self, process: &Process) -> bool;

    /// The committed contents of one array payload cell; `None` if the var is
    /// absent or holds a slot value.
    fn committed_array(&self, var_id: VarId) -> Option<LpcArray>;

    /// The committed contents of one mapping payload cell, as in
    /// [`committed_array`]([`CommittedReader::committed_array`]).
    fn committed_mapping(&self, var_id: VarId) -> Option<LpcMapping>;

    /// The committed object for a cell var, or `None` if the var is absent or
    /// the cell holds a non-object.
    fn committed_object(&self, var_id: VarId) -> Option<Arc<Process>>;

    /// The committed connection bound to `process`, if any.
    fn committed_connection(&self, process: &Process) -> Option<Arc<Connection>>;

    /// The committed environment of `process` (`None` if it has none).
    fn committed_environment(&self, process: &Process) -> Option<Arc<Process>>;

    /// The committed inventory of `process`: its contained objects, destructed
    /// members filtered out.
    fn committed_inventory(&self, process: &Process) -> Vec<Arc<Process>>;

    /// The committed rule list of `process`, empty when absent.
    fn committed_rules(&self, process: &Process) -> RuleList;

    /// The committed verb-attached rule list, empty when absent.
    fn committed_verb_rules(&self) -> RuleList;
}

impl CommittedReader for Arc<GlobalState> {
    fn global_slot_count(&self, process: &Process) -> usize {
        process.program.num_globals as usize
    }

    fn committed_global(&self, process: &Process, reg: RegisterSize) -> LpcRef {
        self.committed_value(process.var_id(reg))
            .map_or(NULL, WorldValue::lpc_ref)
    }

    fn is_initialized(&self, process: &Process) -> bool {
        self.committed_value(process.initialized.id).is_some()
    }

    fn commands_enabled(&self, process: &Process) -> bool {
        self.committed_value(process.commands_enabled.id).is_some()
    }

    fn committed_array(&self, var_id: VarId) -> Option<LpcArray> {
        self.committed_value(var_id)?
            .into_array()
            .map(|array| (*array).clone())
    }

    fn committed_mapping(&self, var_id: VarId) -> Option<LpcMapping> {
        self.committed_value(var_id)?
            .into_mapping()
            .map(|mapping| (*mapping).clone())
    }

    fn committed_object(&self, var_id: VarId) -> Option<Arc<Process>> {
        self.committed_value(var_id)?.into_process()
    }

    fn committed_connection(&self, process: &Process) -> Option<Arc<Connection>> {
        self.committed_value(process.connection.id)?
            .into_connection()
    }

    fn committed_environment(&self, process: &Process) -> Option<Arc<Process>> {
        let LpcRef::Object(weak) = self
            .committed_value(process.position.environment.id)
            .map(WorldValue::lpc_ref)?
        else {
            return None;
        };

        weak.upgrade()
    }

    fn committed_inventory(&self, process: &Process) -> Vec<Arc<Process>> {
        let Some(inventory) = self
            .committed_value(process.position.inventory.id)
            .and_then(WorldValue::into_array)
        else {
            return Vec::new();
        };

        inventory
            .iter()
            .filter_map(|item| {
                let LpcRef::Object(weak) = item else {
                    return None;
                };
                weak.upgrade()
            })
            .collect()
    }

    fn committed_rules(&self, process: &Process) -> RuleList {
        self.committed_value(process.rules.id)
            .and_then(WorldValue::into_rules)
            .unwrap_or_default()
    }

    fn committed_verb_rules(&self) -> RuleList {
        self.committed_value(self.object_space.verb_rules.id)
            .and_then(WorldValue::into_rules)
            .unwrap_or_default()
    }
}

impl GlobalState {
    /// One synchronous read of the latest committed world (start, read the
    /// snapshot, release); the blocking recv runs on a scoped thread, never
    /// the caller's. `None` = absent.
    fn committed_value(&self, var_id: VarId) -> Option<WorldValue> {
        std::thread::scope(|s| {
            let (reply_tx, reply_rx) = flume::bounded(1);
            self.committer_tx
                .send(CommitProtocol::Start { reply: reply_tx })
                .expect("committer channel closed");
            let live = s
                .spawn(move || reply_rx.recv())
                .join()
                .expect("start reply thread panicked")
                .expect("committer always answers a start");
            live.inner.read(var_id)
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Barrier};

    use lpc_rs_core::LpcIntInner;

    use super::run_attempts;
    use crate::interpreter::stm::tests::IncBody;
    use crate::interpreter::{
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{
            VarId, Version, WorldValue, changeset::Changeset, committer::CommitProtocol, tests::*,
        },
    };

    fn seed(tx: &flume::Sender<CommitProtocol>, v0: Version, var: VarId, value: LpcIntInner) {
        let mut seed = Changeset::new(v0);
        seed.write(var, WorldValue::ref_of(LpcRef::from(value)));
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Commit {
            changeset: seed,
            releases_base: false,
            reply: reply_tx,
        })
        .expect("committer channel closed");
        reply_rx
            .recv()
            .expect("no reply from committer")
            .expect("seed should commit");
    }

    #[tokio::test]
    async fn a_clean_attempt_commits_in_one_pass() {
        let (tx, v0, handle) = start_committer();
        let counter = VarId::new();
        seed(&tx, v0, counter, 5);

        let mut body = IncBody::new(counter);
        let (res, stats) =
            run_attempts(&tx, &super::AttemptTelemetry::default(), None, &mut body).await;

        assert!(res.is_ok());
        assert_eq!(stats.attempts, 1);
        assert_eq!(stats.conflicts, 0);
        let final_snapshot = close_committer(tx, handle);
        assert_eq!(
            final_snapshot.read(counter),
            Some(WorldValue::ref_of(LpcRef::from(6)))
        );
    }

    #[test]
    fn contended_increments_all_complete_and_conflicts_are_counted() {
        const WORKERS: usize = 8;
        const ROUNDS: usize = 3;

        let (tx, v0, handle) = start_committer();
        let counter = VarId::new();
        seed(&tx, v0, counter, 0);

        // The barrier aligns all 8 first-round bases, so by FIFO exactly one
        // commits and 7 rejections are guaranteed.
        let gate = Arc::new(Barrier::new(WORKERS));
        let mut total_attempts = 0u64;
        let mut total_conflicts = 0u64;
        let mut total_duration = std::time::Duration::ZERO;

        std::thread::scope(|s| {
            let mut handles = Vec::with_capacity(WORKERS);
            for _ in 0..WORKERS {
                let tx = tx.clone();
                let gate = gate.clone();
                handles.push(s.spawn(move || {
                    gate.wait();
                    // Each worker drives the loop on its own runtime: the
                    // loop's channel sends resolve off the calling thread.
                    // enable_time: the sleep-tier backoff needs the timer driver.
                    let rt = tokio::runtime::Builder::new_current_thread()
                        .enable_time()
                        .build()
                        .expect("runtime");
                    let mut attempts = 0u64;
                    let mut conflicts = 0u64;
                    let mut duration = std::time::Duration::ZERO;
                    for _ in 0..ROUNDS {
                        let mut body = IncBody::new(counter);
                        let (res, stats) = rt.block_on(run_attempts(
                            &tx,
                            &super::AttemptTelemetry::default(),
                            None,
                            &mut body,
                        ));
                        assert!(res.is_ok(), "attempt must commit");
                        attempts += stats.attempts;
                        conflicts += stats.conflicts;
                        duration += stats.duration;
                        // the last attempt always succeeded
                        assert!(attempts > conflicts);
                    }
                    (attempts, conflicts, duration)
                }));
            }
            for handle in handles {
                let (attempts, conflicts, duration) = handle.join().expect("worker panicked");
                total_attempts += attempts;
                total_conflicts += conflicts;
                total_duration += duration;
            }
        });

        let final_snapshot = close_committer(tx, handle);
        let WorldValue::Ref(LpcRef::Int(LpcInt(total))) =
            final_snapshot.read(counter).expect("counter cell missing")
        else {
            panic!("counter cell is not an int");
        };

        // every increment landed exactly once
        assert_eq!(total, (WORKERS * ROUNDS) as LpcIntInner);
        // the aligned first round produced exactly 7 rejections
        assert!(total_conflicts >= 7);
        assert_eq!(
            total_attempts,
            (WORKERS * ROUNDS + total_conflicts as usize) as u64
        );
        // the retry loop timed itself
        assert!(!total_duration.is_zero());
    }
}

#[cfg(test)]
mod async_tests {
    use lpc_rs_core::LpcIntInner;

    use super::*;
    use crate::interpreter::stm::tests::IncBody;
    use crate::interpreter::{
        lpc_ref::LpcRef,
        stm::{Changeset, CommitProtocol, Committer, VarId, WorldValue},
    };

    fn seed(committer: &mut Committer, var: VarId, value: LpcIntInner) {
        let mut seed = Changeset::new(committer.current_version());
        seed.write(var, WorldValue::ref_of(LpcRef::from(value)));
        committer.commit(seed).expect("seed should commit");
    }

    struct TimedBody {
        inner: IncBody,
        limit_ms: u64,
        work: Duration,
        commit_delay: Duration,
        delivered: usize,
    }

    #[async_trait::async_trait]
    impl AttemptBody for TimedBody {
        fn timeout_ms(&self) -> u64 {
            self.limit_ms
        }

        async fn begin_attempt(
            &mut self,
            tx: &flume::Sender<CommitProtocol>,
        ) -> Result<Option<LiveSnapshot>> {
            let live = self.inner.begin_attempt(tx).await?;
            tokio::time::sleep(self.work).await;
            Ok(live)
        }

        async fn commit_phase(
            &mut self,
            tx: &flume::Sender<CommitProtocol>,
            live: LiveSnapshot,
        ) -> Result<(
            std::result::Result<(), Conflict>,
            Vec<crate::interpreter::stm::Effect>,
        )> {
            tokio::time::sleep(self.commit_delay).await;
            self.inner.commit_phase(tx, live).await
        }

        async fn deliver(&mut self, effects: Vec<crate::interpreter::stm::Effect>) -> Result<()> {
            self.delivered += 1;
            self.inner.deliver(effects).await
        }
    }

    async fn run_timed_body(
        limit_ms: u64,
        work_ms: u64,
        commit_ms: u64,
        mut rejections: usize,
    ) -> (Result<()>, RetryStats, usize, LpcRef) {
        let (tx, rx) = flume::unbounded();
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 0);
        let committer_tx = tx.clone();
        let handle = tokio::spawn(async move {
            while let Ok(message) = rx.recv_async().await {
                if rejections > 0
                    && let CommitProtocol::Commit {
                        changeset,
                        releases_base,
                        reply,
                    } = message
                {
                    rejections -= 1;
                    if releases_base {
                        committer.process(
                            CommitProtocol::Drop(changeset.base_version()),
                            &committer_tx,
                        );
                    }
                    reply.send(Err(Conflict::Forced)).unwrap();
                } else if !committer.process(message, &committer_tx) {
                    break;
                }
            }
            committer.snapshot_clone()
        });
        let mut body = TimedBody {
            inner: IncBody::new(counter),
            limit_ms,
            work: Duration::from_millis(work_ms),
            commit_delay: Duration::from_millis(commit_ms),
            delivered: 0,
        };
        let telemetry = AttemptTelemetry::default();
        let (result, stats) = run_attempts(&tx, &telemetry, None, &mut body).await;
        assert_eq!(telemetry.snapshot().errors, u64::from(result.is_err()));
        assert_eq!(committer_stats(&tx).await.unwrap().live_snapshots, 0);
        tx.send(CommitProtocol::Close).unwrap();
        let snapshot = handle.await.unwrap();
        let Some(WorldValue::Ref(value)) = snapshot.read(counter) else {
            panic!("counter should remain an integer");
        };
        (result, stats, body.delivered, value)
    }

    #[tokio::test(start_paused = true)]
    async fn retries_share_one_evaluation_limit_and_release_the_interrupted_attempt() {
        let (result, stats, delivered, value) = run_timed_body(100, 40, 0, usize::MAX).await;
        let error = result.unwrap_err();
        assert_eq!(
            error.to_string(),
            "runtime error: evaluation limit of 100ms has been reached"
        );
        let diagnostic = error.diagnostic_string();
        assert!(
            diagnostic.contains("3 attempt(s) and 2 conflict(s)"),
            "{diagnostic}"
        );
        assert!(
            diagnostic.contains("Concurrent changes forced retries"),
            "{diagnostic}"
        );
        assert_eq!(stats.attempts, 3);
        assert_eq!(stats.conflicts, 2);
        assert_eq!(stats.phases.attempt, Duration::from_millis(100));
        assert_eq!(stats.last_conflict, Some(Conflict::Forced));
        assert!(
            diagnostic.contains("evaluation_and_snapshot_ms=100.000"),
            "{diagnostic}"
        );
        assert!(
            diagnostic.contains("last_conflict=[forced test rejection]"),
            "{diagnostic}"
        );
        assert_eq!(delivered, 0);
        assert_eq!(value, LpcRef::from(0));
    }

    #[tokio::test(start_paused = true)]
    async fn the_evaluation_limit_also_stops_backoff() {
        let (result, stats, delivered, value) = run_timed_body(100, 0, 0, usize::MAX).await;
        assert!(result.is_err());
        assert!(stats.conflicts >= 7);
        assert_eq!(delivered, 0);
        assert_eq!(value, LpcRef::from(0));
    }

    #[tokio::test(start_paused = true)]
    async fn zero_disables_the_limit_across_retries() {
        let (result, stats, delivered, value) = run_timed_body(0, 40, 0, 2).await;
        result.unwrap();
        assert_eq!(stats.attempts, 3);
        assert_eq!(delivered, 1);
        assert_eq!(value, LpcRef::from(1));
    }

    #[tokio::test(start_paused = true)]
    async fn a_commit_that_crosses_the_deadline_still_delivers_once() {
        let (result, stats, delivered, value) = run_timed_body(100, 40, 150, 0).await;
        result.unwrap();
        assert_eq!(stats.attempts, 1);
        assert_eq!(delivered, 1);
        assert_eq!(value, LpcRef::from(1));
        assert_eq!(stats.phases.attempt, Duration::from_millis(40));
        assert_eq!(stats.phases.commit, Duration::from_millis(150));
        assert!(stats.last_conflict.is_none());
    }

    #[tokio::test]
    async fn async_clean_attempt_commits_in_one_pass() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 5);
        // The committer needs its own sender clone (its `LiveSnapshot`s use
        // it for releases); keep `tx` so the test can still send `Close`.
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        let (res, stats) = {
            let mut body = IncBody::new(counter);
            run_attempts(&tx, &super::AttemptTelemetry::default(), None, &mut body).await
        };

        assert!(res.is_ok());
        assert_eq!(stats.attempts, 1);
        assert_eq!(stats.conflicts, 0);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");
        assert_eq!(
            final_snapshot.read(counter),
            Some(WorldValue::ref_of(LpcRef::from(6)))
        );
    }

    #[tokio::test]
    async fn async_rejection_reruns_until_commit() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 0);
        let committer_tx = tx.clone(); // keep `tx` for the final `Close`
        let handle = std::thread::spawn(move || {
            // synthetic abort: reject the first commit
            committer.run_with_rejections(committer_tx, rx, 1)
        });

        let (res, stats) = {
            let mut body = IncBody::new(counter);
            run_attempts(&tx, &super::AttemptTelemetry::default(), None, &mut body).await
        };

        assert!(res.is_ok());
        assert_eq!(
            stats.attempts, 2,
            "one forced rejection, then a clean commit"
        );
        assert_eq!(stats.conflicts, 1);
        assert!(!stats.duration.is_zero());

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");
        // the rejected attempt wrote nothing; the re-run incremented 0 -> 1
        assert_eq!(
            final_snapshot.read(counter),
            Some(WorldValue::ref_of(LpcRef::from(1)))
        );
    }

    #[tokio::test(start_paused = true)]
    async fn async_rejection_storm_crosses_the_sleep_tier_and_still_commits() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 0);
        let committer_tx = tx.clone(); // keep `tx` for the final `Close`
        // 8 rejections push the loop past MAX_YIELD_LOSSES into the sleep tier.
        let handle = std::thread::spawn(move || committer.run_with_rejections(committer_tx, rx, 8));

        let (res, stats) = {
            let mut body = IncBody::new(counter);
            run_attempts(&tx, &super::AttemptTelemetry::default(), None, &mut body).await
        };

        assert!(res.is_ok());
        assert_eq!(stats.attempts, 9);
        assert_eq!(stats.conflicts, 8);
        assert_eq!(stats.backoff.cap_expiries, 2, "losses 7 and 8 sleep");
        assert_eq!(stats.backoff.commit_wakes, 0);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");
        // every rejected attempt wrote nothing; the ninth incremented 0 -> 1
        assert_eq!(
            final_snapshot.read(counter),
            Some(WorldValue::ref_of(LpcRef::from(1)))
        );
    }

    #[tokio::test]
    async fn telemetry_aggregates_the_storm_and_splits_backoff_tiers() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 0);
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run_with_rejections(committer_tx, rx, 8));

        let telemetry = AttemptTelemetry::default();
        let (res, stats) = {
            let mut body = IncBody::new(counter);
            run_attempts(&tx, &telemetry, None, &mut body).await
        };
        assert!(res.is_ok());

        let snap = telemetry.snapshot();
        assert_eq!(snap.applies, 1);
        assert_eq!(snap.attempts, stats.attempts);
        assert_eq!(snap.conflicts, stats.conflicts);
        assert_eq!(snap.errors, 0);
        // Losses 2-6 yield and 7+ sleep, so eight rejections land time in both tiers.
        assert!(snap.backoff_yield > Duration::ZERO);
        assert!(snap.backoff_sleep > Duration::ZERO);
        // The requested-vs-realized split makes the timer's tick-rounding overshoot measurable.
        assert!(snap.backoff_sleep_requested > Duration::ZERO);
        assert!(snap.backoff_sleep >= snap.backoff_sleep_requested);
        assert_eq!(snap.backoff_cap_expiries, 2);
        assert_eq!(snap.backoff_commit_wakes, 0);
        assert!(snap.total >= snap.backoff_sleep);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        handle.join().expect("committer panicked");
    }

    #[tokio::test]
    async fn a_conflict_free_run_records_no_backoff() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        seed(&mut committer, counter, 0);
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run_with_rejections(committer_tx, rx, 0));

        let telemetry = AttemptTelemetry::default();
        for _ in 0..3 {
            let mut body = IncBody::new(counter);
            let (res, _) = run_attempts(&tx, &telemetry, None, &mut body).await;
            assert!(res.is_ok());
        }

        let snap = telemetry.snapshot();
        assert_eq!(snap.applies, 3);
        assert_eq!(snap.attempts, 3);
        assert_eq!(snap.conflicts, 0);
        assert_eq!(snap.backoff_yield, Duration::ZERO);
        assert_eq!(snap.backoff_sleep, Duration::ZERO);
        assert_eq!(snap.backoff_sleep_requested, Duration::ZERO);
        assert_eq!(snap.backoff_commit_wakes, 0);
        assert_eq!(snap.backoff_cap_expiries, 0);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        handle.join().expect("committer panicked");
    }
}

#[cfg(test)]
mod admission_tests {
    use std::{
        future::Future,
        pin::Pin,
        sync::atomic::{AtomicUsize, Ordering::SeqCst},
        task::{Context, Poll, Waker},
    };

    use tokio::sync::Barrier;

    use super::*;
    use crate::interpreter::stm::{Committer, Effect, Snapshot, tests::IncBody};

    struct Harness {
        runner: Arc<AttemptRunner>,
        handle: Option<tokio::task::JoinHandle<Snapshot>>,
    }

    impl Harness {
        fn new(cells: &[VarId]) -> Self {
            let mut committer = Committer::new();
            let mut seed = Changeset::new(committer.current_version());
            for cell in cells {
                seed.write(*cell, WorldValue::ref_of(0.into()));
            }
            committer.commit(seed).unwrap();
            let (tx, rx) = flume::unbounded();
            let runner = Arc::new(AttemptRunner::new(
                tx.clone(),
                Some(committer.commit_watch()),
            ));
            let handle = tokio::spawn(async move {
                while let Ok(message) = rx.recv_async().await {
                    if !committer.process(message, &tx) {
                        break;
                    }
                }
                committer.snapshot_clone()
            });
            Self {
                runner,
                handle: Some(handle),
            }
        }

        async fn finish(mut self) -> Snapshot {
            assert!(self.runner.admission.is_idle());
            assert_eq!(
                committer_stats(&self.runner.tx)
                    .await
                    .unwrap()
                    .live_snapshots,
                0
            );
            self.runner.tx.send(CommitProtocol::Close).unwrap();
            self.handle.take().unwrap().await.unwrap()
        }
    }

    impl Drop for Harness {
        fn drop(&mut self) {
            if self.handle.is_some() {
                let _ = self.runner.tx.send(CommitProtocol::Close);
            }
        }
    }

    #[derive(Clone, Copy, PartialEq)]
    enum PauseAt {
        FirstRejection,
        RetryEvaluation,
        RetryReply,
        Delivery,
    }

    struct Gate {
        entered: flume::Sender<()>,
        resume: flume::Receiver<()>,
    }

    impl Gate {
        fn new() -> (Self, flume::Receiver<()>, flume::Sender<()>) {
            let (entered, observed) = flume::bounded(1);
            let (resume, waiting) = flume::bounded(1);
            (
                Self {
                    entered,
                    resume: waiting,
                },
                observed,
                resume,
            )
        }

        async fn wait(&self) {
            self.entered.send(()).unwrap();
            self.resume.recv_async().await.unwrap();
        }
    }

    struct Body {
        inner: IncBody,
        runner: Arc<AttemptRunner>,
        invalidate: Vec<VarId>,
        begins: usize,
        timeout: u64,
        barrier: Option<Arc<Barrier>>,
        pause: Option<(PauseAt, Gate)>,
        fail_retry: bool,
        deliveries: Arc<AtomicUsize>,
    }

    impl Body {
        fn new(runner: &Arc<AttemptRunner>, cell: VarId) -> Self {
            Self {
                inner: IncBody::new(cell),
                runner: runner.clone(),
                invalidate: vec![cell],
                begins: 0,
                timeout: 0,
                barrier: None,
                pause: None,
                fail_retry: false,
                deliveries: Arc::new(AtomicUsize::new(0)),
            }
        }

        fn pause_at(&mut self, phase: PauseAt) -> (flume::Receiver<()>, flume::Sender<()>) {
            let (gate, entered, resume) = Gate::new();
            self.pause = Some((phase, gate));
            (entered, resume)
        }

        async fn pause(&self, phase: PauseAt) {
            if let Some((at, gate)) = &self.pause
                && *at == phase
            {
                gate.wait().await;
            }
        }
    }

    #[async_trait::async_trait]
    impl AttemptBody for Body {
        fn timeout_ms(&self) -> u64 {
            self.timeout
        }

        async fn begin_attempt(
            &mut self,
            tx: &flume::Sender<CommitProtocol>,
        ) -> Result<Option<LiveSnapshot>> {
            if let Some(cell) = self.invalidate.get(self.begins) {
                self.inner.counter = *cell;
            }
            self.begins += 1;
            let live = self.inner.begin_attempt(tx).await?;
            if self.begins == 1
                && let Some(barrier) = &self.barrier
            {
                barrier.wait().await;
            }
            if self.begins <= self.invalidate.len() {
                // A fresh owner invalidates this snapshot even when a retry turn is held.
                self.runner
                    .run(&mut IncBody::new(self.inner.counter))
                    .await
                    .0?;
            }
            if self.begins > 1 {
                self.pause(PauseAt::RetryEvaluation).await;
                if self.fail_retry {
                    return Err(lpc_error!("retry evaluation failed"));
                }
            }
            Ok(live)
        }

        async fn commit_phase(
            &mut self,
            tx: &flume::Sender<CommitProtocol>,
            live: LiveSnapshot,
        ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
            let result = self.inner.commit_phase(tx, live).await;
            if self.begins == 1 && matches!(result, Ok((Err(_), _))) {
                self.pause(PauseAt::FirstRejection).await;
            }
            if self.begins > 1 {
                self.pause(PauseAt::RetryReply).await;
            }
            result
        }

        async fn deliver(&mut self, _effects: Vec<Effect>) -> Result<()> {
            self.deliveries.fetch_add(1, SeqCst);
            self.pause(PauseAt::Delivery).await;
            Ok(())
        }
    }

    fn poll<F: Future>(future: Pin<&mut F>) -> Poll<F::Output> {
        future.poll(&mut Context::from_waker(Waker::noop()))
    }

    #[tokio::test]
    async fn aligned_owners_preserve_updates_with_one_retry_per_loser() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let barrier = Arc::new(Barrier::new(8));
        let mut tasks = tokio::task::JoinSet::new();
        for _ in 0..8 {
            let runner = harness.runner.clone();
            let mut body = Body::new(&runner, cell);
            body.invalidate.clear();
            body.barrier = Some(barrier.clone());
            tasks.spawn(async move {
                let (result, stats) = runner.run(&mut body).await;
                result.unwrap();
                assert_eq!(body.deliveries.load(SeqCst), 1);
                assert_eq!(stats.attempts, stats.conflicts + 1);
                assert_eq!(stats.admission.acquisitions, stats.conflicts);
            });
        }
        while let Some(result) = tasks.join_next().await {
            result.unwrap();
        }
        let totals = harness.runner.telemetry();
        assert_eq!(totals.owning_tasks, 8);
        assert_eq!(totals.owning_attempts, 15);
        assert_eq!(totals.conflicts, 7);
        assert_eq!(totals.admission_acquisitions, 7);
        assert_eq!(totals.backoff_sleep, Duration::ZERO);
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(8.into()))
        );
    }

    #[tokio::test(start_paused = true)]
    async fn admission_timeout_does_not_start_or_pin_another_attempt() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let held = harness.runner.admission.acquire(cell).await.unwrap();
        let mut body = Body::new(&harness.runner, cell);
        body.timeout = 10;
        let (result, stats) = harness.runner.run(&mut body).await;
        assert!(result.unwrap_err().to_string().contains("evaluation limit"));
        assert_eq!(body.begins, 1);
        assert_eq!(stats.attempts, 1);
        assert_eq!(stats.conflicts, 1);
        assert_eq!(stats.admission.acquisitions, 0);
        assert_eq!(stats.admission.timeouts, 1);
        assert_eq!(stats.admission.waited, Duration::from_millis(10));
        assert_eq!(body.deliveries.load(SeqCst), 0);
        assert_eq!(harness.runner.telemetry().admission_timeouts, 1);
        drop(held);
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(1.into()))
        );
    }

    #[tokio::test]
    async fn cancelling_a_queued_owner_releases_enrollment() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let held = harness.runner.admission.acquire(cell).await.unwrap();
        let mut body = Body::new(&harness.runner, cell);
        let (entered, resume) = body.pause_at(PauseAt::FirstRejection);
        let mut run = Box::pin(harness.runner.run(&mut body));
        tokio::select! {
            result = &mut run => panic!("finished before pause: {result:?}"),
            result = entered.recv_async() => result.unwrap(),
        }
        resume.send(()).unwrap();
        assert!(poll(run.as_mut()).is_pending());
        drop(run);
        assert_eq!(body.begins, 1);
        drop(held);
        harness.finish().await;
    }

    #[tokio::test(start_paused = true)]
    async fn cancelling_or_timing_out_admitted_evaluation_releases_turn_and_pin() {
        for timeout in [0, 10] {
            let cell = VarId::new();
            let harness = Harness::new(&[cell]);
            let mut body = Body::new(&harness.runner, cell);
            body.timeout = timeout;
            let (entered, _resume) = body.pause_at(PauseAt::RetryEvaluation);
            let mut run = Box::pin(harness.runner.run(&mut body));
            tokio::select! {
                result = &mut run => panic!("finished before pause: {result:?}"),
                result = entered.recv_async() => result.unwrap(),
            }
            assert_eq!(
                committer_stats(&harness.runner.tx)
                    .await
                    .unwrap()
                    .live_snapshots,
                1
            );
            if timeout != 0 {
                let (result, stats) = run.await;
                assert!(result.is_err());
                assert_eq!(stats.admission.acquisitions, 1);
                assert_eq!(stats.admission.timeouts, 0);
            } else {
                drop(run);
            }
            assert_eq!(body.deliveries.load(SeqCst), 0);
            assert_eq!(
                harness.finish().await.read(cell),
                Some(WorldValue::ref_of(1.into()))
            );
        }
    }

    #[tokio::test]
    async fn evaluation_errors_release_admission_without_committing() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let mut body = Body::new(&harness.runner, cell);
        body.fail_retry = true;
        let (result, stats) = harness.runner.run(&mut body).await;
        assert!(result.is_err());
        assert_eq!(stats.admission.acquisitions, 1);
        assert_eq!(body.deliveries.load(SeqCst), 0);
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(1.into()))
        );
    }

    #[tokio::test(start_paused = true)]
    async fn commit_reply_keeps_the_turn_past_the_evaluation_deadline() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let mut body = Body::new(&harness.runner, cell);
        body.timeout = 10;
        let (entered, resume) = body.pause_at(PauseAt::RetryReply);
        let mut run = Box::pin(harness.runner.run(&mut body));
        tokio::select! {
            result = &mut run => panic!("finished before pause: {result:?}"),
            result = entered.recv_async() => result.unwrap(),
        }
        tokio::time::advance(Duration::from_millis(20)).await;
        let mut next = Box::pin(harness.runner.admission.acquire(cell));
        assert!(poll(next.as_mut()).is_pending());
        assert!(poll(run.as_mut()).is_pending());
        resume.send(()).unwrap();
        run.await.0.unwrap();
        drop(next.await.unwrap());
        assert_eq!(body.deliveries.load(SeqCst), 1);
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(2.into()))
        );
    }

    #[tokio::test]
    async fn delivery_can_start_another_owner_that_retries_on_the_same_cell() {
        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let mut body = Body::new(&harness.runner, cell);
        let (entered, resume) = body.pause_at(PauseAt::Delivery);
        let mut run = Box::pin(harness.runner.run(&mut body));
        tokio::select! {
            result = &mut run => panic!("finished before pause: {result:?}"),
            result = entered.recv_async() => result.unwrap(),
        }
        let mut following = Body::new(&harness.runner, cell);
        let (result, stats) = harness.runner.run(&mut following).await;
        result.unwrap();
        assert_eq!(stats.admission.acquisitions, 1);
        resume.send(()).unwrap();
        run.await.0.unwrap();
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(4.into()))
        );
    }

    #[tokio::test(start_paused = true)]
    async fn switching_conflict_cells_releases_the_previous_turn_before_waiting() {
        let cells = [VarId::new(), VarId::new()];
        let harness = Harness::new(&cells);
        let held = harness.runner.admission.acquire(cells[1]).await.unwrap();
        let mut body = Body::new(&harness.runner, cells[0]);
        body.invalidate = cells.to_vec();
        body.timeout = 10;
        let (entered, resume) = body.pause_at(PauseAt::RetryReply);
        let mut run = Box::pin(harness.runner.run(&mut body));
        tokio::select! {
            result = &mut run => panic!("finished before pause: {result:?}"),
            result = entered.recv_async() => result.unwrap(),
        }
        resume.send(()).unwrap();
        assert!(poll(run.as_mut()).is_pending());
        let mut previous = Box::pin(harness.runner.admission.acquire(cells[0]));
        let Poll::Ready(turn) = poll(previous.as_mut()) else {
            panic!("held the old turn while waiting for the new cell");
        };
        drop(turn.unwrap());
        drop(previous);
        let (result, stats) = run.await;
        assert!(result.is_err());
        assert_eq!(stats.attempts, 2);
        assert_eq!(stats.conflicts, 2);
        assert_eq!(stats.admission.acquisitions, 1);
        assert_eq!(stats.admission.timeouts, 1);
        assert!(
            matches!(stats.last_conflict, Some(Conflict::ReadInvalidated { cell, .. }) if cell == cells[1])
        );
        drop(harness.runner.admission.acquire(cells[0]).await.unwrap());
        drop(held);
        harness.finish().await;
    }

    #[tokio::test(start_paused = true)]
    async fn non_read_backoff_uses_the_total_of_both_kinds_of_conflicts() {
        struct Mixed(Body);

        #[async_trait::async_trait]
        impl AttemptBody for Mixed {
            async fn begin_attempt(
                &mut self,
                tx: &flume::Sender<CommitProtocol>,
            ) -> Result<Option<LiveSnapshot>> {
                self.0.begin_attempt(tx).await
            }

            async fn commit_phase(
                &mut self,
                tx: &flume::Sender<CommitProtocol>,
                live: LiveSnapshot,
            ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
                if self.0.begins == 7 {
                    let mut changeset = Changeset::new(live.version());
                    changeset.merge(
                        self.0.inner.counter,
                        crate::interpreter::stm::MergeOp::ArrayAppend(vec![1.into()]),
                    );
                    Ok((commit_changeset(tx, changeset).await?, vec![]))
                } else {
                    self.0.commit_phase(tx, live).await
                }
            }

            async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
                self.0.deliver(effects).await
            }
        }

        let cell = VarId::new();
        let harness = Harness::new(&[cell]);
        let mut body = Body::new(&harness.runner, cell);
        body.invalidate = vec![cell; 6];
        let (result, stats) = harness.runner.run(&mut Mixed(body)).await;
        result.unwrap();
        assert_eq!(stats.attempts, 8);
        assert_eq!(stats.conflicts, 7);
        assert_eq!(stats.admission.acquisitions, 6);
        assert!(stats.backoff.sleep_requested > Duration::ZERO);
        assert!(matches!(
            stats.last_conflict,
            Some(Conflict::MergeMismatch { .. })
        ));
        assert_eq!(
            harness.finish().await.read(cell),
            Some(WorldValue::ref_of(7.into()))
        );
    }
}
