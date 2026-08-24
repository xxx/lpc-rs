//! The attempt runner: re-runs a transaction's attempts until one commits.
//!
//! The runner owns the committer protocol (open, commit, release-after-reply,
//! re-base, stats); the body owns the attempt-level work and its physical
//! output. Production runs a [`Task`](crate::interpreter::task::Task) or a
//! `GlobalState::takeover`; tests run a bare [`Transaction`].

use std::{
    sync::{Arc, atomic::AtomicU64},
    time::Duration,
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{Result, lpc_error};

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{
        GcPassReply, VarId, WorldRoot, WorldValue,
        backoff::{Backoff, BackoffSpent},
        changeset::Changeset,
        committer::{CommitProtocol, CommitterStats, Conflict, LiveSnapshot},
    },
    vm::global_state::GlobalState,
};

/// Per-attempt statistics, owned by the attempt loop: the committer can't
/// see retries, only this loop can.
#[derive(Debug, Default)]
pub(crate) struct RetryStats {
    /// The first attempt plus one per conflict.
    pub(crate) attempts: u64,
    /// Conflicts observed across attempts.
    pub(crate) conflicts: u64,
    /// Wall time of the whole loop, successful attempt included.
    pub(crate) duration: Duration,
    /// Realized backoff totals for the loop.
    pub(crate) backoff: BackoffSpent,
}

/// Attempt-loop lifetime totals, recorded once per apply by `run_attempts`.
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
}

impl AttemptTelemetry {
    /// Fold one finished attempt loop into the totals.
    fn record(&self, stats: &RetryStats, errored: bool) {
        use std::sync::atomic::Ordering::Relaxed;
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
    }

    /// Read the totals; fields are loaded individually, so an apply landing
    /// mid-read may straddle the snapshot.
    pub fn snapshot(&self) -> AttemptTelemetrySnapshot {
        use std::sync::atomic::Ordering::Relaxed;
        AttemptTelemetrySnapshot {
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
        }
    }
}

/// One read of `AttemptTelemetry`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AttemptTelemetrySnapshot {
    /// Finished attempt loops.
    pub applies: u64,
    /// Attempts across all loops: each apply's first plus one per conflict.
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

/// Re-run `body`'s attempts until one commits, staggering repeat losers by
/// [`Backoff`]; each attempt re-bases on the newest world. `None` from
/// `begin_attempt` stops after that single attempt without committing.
pub(crate) async fn run_attempts<B: AttemptBody>(
    tx: &flume::Sender<CommitProtocol>,
    telemetry: &AttemptTelemetry,
    body: &mut B,
) -> (Result<()>, RetryStats) {
    let started = std::time::Instant::now();
    let mut attempts = 0u64;
    let mut backoff = Backoff::new();

    let result = loop {
        attempts += 1;

        let live = match body.begin_attempt(tx).await {
            Ok(live) => live,
            Err(e) => break Err(e),
        };

        // A joiner: the caller's commit carries the writes.
        let Some(mut live) = live else {
            break Ok(());
        };

        // Disarmed: the commit releases the pin, not the handle's drop.
        live.disarm();
        let (commit, effects) = match body.commit_phase(tx, live).await {
            Ok(c) => c,
            Err(e) => break Err(e),
        };

        // The commit is permanent, so deliver now. A rejected attempt never
        // reaches this: its recorded output is dropped with the attempt.
        if commit.is_ok() {
            break body.deliver(effects).await;
        }
        backoff.stagger().await;
    };

    let stats = RetryStats {
        attempts,
        conflicts: backoff.losses(),
        duration: started.elapsed(),
        backoff: backoff.spent(),
    };
    telemetry.record(&stats, result.is_err());
    (result, stats)
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

/// A sync "read the latest committed world" API for consistency-agnostic
/// readers (test/debug/tooling). Do not use from an interpreter transaction.
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

    /// The committed environment of `process` (`None` if it has none).
    fn committed_environment(&self, process: &Process) -> Option<Arc<Process>>;

    /// The committed inventory of `process`: its contained objects, destructed
    /// members filtered out.
    fn committed_inventory(&self, process: &Process) -> Vec<Arc<Process>>;
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
use crate::interpreter::stm::Transaction;

#[cfg(test)]
/// A test-only body: one bare transaction per attempt, no task machinery.
pub(crate) struct IncBody {
    pub(crate) counter: VarId,
    attempt: Option<Transaction>,
}

#[cfg(test)]
impl IncBody {
    pub(crate) fn new(counter: VarId) -> Self {
        Self {
            counter,
            attempt: None,
        }
    }
}

#[async_trait::async_trait]
#[cfg(test)]
impl AttemptBody for IncBody {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let mut t = Transaction::new(live.inner.clone());
        let LpcRef::Int(n) = t.read(self.counter).expect("counter cell missing") else {
            panic!("counter cell is not an int");
        };
        t.write(self.counter, LpcRef::from(n.wrapping_add(1)));
        self.attempt = Some(t);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), Conflict>,
        Vec<crate::interpreter::stm::Effect>,
    )> {
        let (_, changeset) = self
            .attempt
            .take()
            .expect("attempt present until committed")
            .into_parts();
        let commit = commit_changeset(tx, changeset).await?;
        Ok((commit, Vec::new()))
    }

    async fn deliver(&mut self, _effects: Vec<crate::interpreter::stm::Effect>) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Barrier};

    use lpc_rs_core::LpcIntInner;

    use super::{IncBody, run_attempts};
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
        let (res, stats) = run_attempts(&tx, &super::AttemptTelemetry::default(), &mut body).await;

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
    use crate::interpreter::{
        lpc_ref::LpcRef,
        stm::{Changeset, CommitProtocol, Committer, VarId, WorldValue},
    };

    fn seed(committer: &mut Committer, var: VarId, value: LpcIntInner) {
        let mut seed = Changeset::new(committer.current_version());
        seed.write(var, WorldValue::ref_of(LpcRef::from(value)));
        committer.commit(seed).expect("seed should commit");
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
            run_attempts(&tx, &super::AttemptTelemetry::default(), &mut body).await
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
            run_attempts(&tx, &super::AttemptTelemetry::default(), &mut body).await
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
            run_attempts(&tx, &super::AttemptTelemetry::default(), &mut body).await
        };

        assert!(res.is_ok());
        assert_eq!(stats.attempts, 9);
        assert_eq!(stats.conflicts, 8);

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
            run_attempts(&tx, &telemetry, &mut body).await
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
            let (res, _) = run_attempts(&tx, &telemetry, &mut body).await;
            assert!(res.is_ok());
        }

        let snap = telemetry.snapshot();
        assert_eq!(snap.applies, 3);
        assert_eq!(snap.attempts, 3);
        assert_eq!(snap.conflicts, 0);
        assert_eq!(snap.backoff_yield, Duration::ZERO);
        assert_eq!(snap.backoff_sleep, Duration::ZERO);
        assert_eq!(snap.backoff_sleep_requested, Duration::ZERO);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        handle.join().expect("committer panicked");
    }
}
