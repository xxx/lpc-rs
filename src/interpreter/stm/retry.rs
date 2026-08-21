//! unbounded internal re-run of a transaction until it commits.

use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::{Result, lpc_error};

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    process::Process,
    stm::{
        Transaction, VarId, WorldValue,
        changeset::Changeset,
        committer::{CommitProtocol, LiveSnapshot},
    },
    vm::global_state::GlobalState,
};

/// Per-attempt statistics, owned by the retry loop: the transaction already
/// knows its read/write set sizes, so commit size is free, and conflict rate
/// is a property of this loop, not the committer.
#[derive(Debug, Default)]
pub(crate) struct RetryStats {
    /// The first attempt plus one per conflict.
    pub(crate) attempts: u64,
    /// Conflicts observed across attempts.
    pub(crate) conflicts: u64,
    /// Wall time of the whole retry loop, successful attempt included.
    pub(crate) duration: Duration,
}

/// Runs `f` against a fresh transaction, commits, and re-runs from scratch
/// on each conflict until one commits.
pub(crate) fn retry<T>(
    tx: &flume::Sender<CommitProtocol>,
    mut f: impl FnMut(&mut Transaction) -> T,
) -> (T, RetryStats) {
    let started = Instant::now();
    let mut attempts = 0;
    let mut conflicts = 0;

    loop {
        attempts += 1;

        // Each attempt starts against the current world, so a re-run
        // re-bases on the newest snapshot.
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: reply_tx })
            .expect("committer channel closed");
        let live = reply_rx.recv().expect("no reply from committer");

        let mut transaction = Transaction::new(live.inner.clone());
        let result = f(&mut transaction);
        let (_world, changeset) = transaction.into_parts();

        // Hold the release handle until the reply resolves: dropping it
        // earlier can let the committer evict history this commit still
        // needs, spurious-conflicting a sound attempt.
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Commit {
            changeset,
            reply: reply_tx,
        })
        .expect("committer channel closed");
        let commit_result = reply_rx.recv().expect("no reply from committer");
        drop(live);

        if commit_result.is_ok() {
            return (
                result,
                RetryStats {
                    attempts,
                    conflicts,
                    duration: started.elapsed(),
                },
            );
        }
        conflicts += 1;
    }
}

/// Start a transaction against the committer's current world and hand back
/// the release handle. The blocking `flume` recv runs off the runtime via
/// `spawn_blocking`.
pub(crate) async fn start_txn(tx: &flume::Sender<CommitProtocol>) -> Result<LiveSnapshot> {
    let (reply_tx, reply_rx) = flume::bounded(1);
    tx.send(CommitProtocol::Start { reply: reply_tx })
        .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("committer channel closed") })?;
    tokio::task::spawn_blocking(move || reply_rx.recv())
        .await
        .map_err(|e| -> Box<lpc_rs_errors::LpcError> {
            lpc_error!("committer reply task panicked: {}", e)
        })?
        .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("no reply from committer") })
}

/// Commit a changeset and await the reply. `Ok(())` = committed;
/// `Ok(Err(_))` = rejected (conflict).
pub(crate) async fn commit_changeset(
    tx: &flume::Sender<CommitProtocol>,
    changeset: Changeset,
) -> Result<std::result::Result<(), Changeset>> {
    let (reply_tx, reply_rx) = flume::bounded(1);
    tx.send(CommitProtocol::Commit {
        changeset,
        reply: reply_tx,
    })
    .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("committer channel closed") })?;
    tokio::task::spawn_blocking(move || reply_rx.recv())
        .await
        .map_err(|e| -> Box<lpc_rs_errors::LpcError> {
            lpc_error!("committer reply task panicked: {}", e)
        })?
        .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("no reply from committer") })
}

/// Query the committed value of a var (async wrapper over
/// [`CommitProtocol::Query`]). Absent vars read back as `NULL`.
/// Test-only: production committed reads go through
/// [`CommittedReader::committed_global`].
#[cfg(test)]
pub(crate) async fn query_var(
    tx: &flume::Sender<CommitProtocol>,
    var_id: crate::interpreter::stm::VarId,
) -> Result<crate::interpreter::stm::WorldValue> {
    let (reply_tx, reply_rx) = flume::bounded(1);
    tx.send(CommitProtocol::Query {
        var_id,
        reply: reply_tx,
    })
    .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("committer channel closed") })?;
    tokio::task::spawn_blocking(move || reply_rx.recv())
        .await
        .map_err(|e| -> Box<lpc_rs_errors::LpcError> {
            lpc_error!("committer reply task panicked: {}", e)
        })?
        .map_err(|_| -> Box<lpc_rs_errors::LpcError> { lpc_error!("no reply from committer") })
}

/// Fire-and-forget: remove a var from the world. Non-blocking send; safe to
/// call from sync code (e.g. the GC sweep).
pub(crate) fn drop_var(tx: &flume::Sender<CommitProtocol>, var_id: crate::interpreter::stm::VarId) {
    let _ = tx.send(CommitProtocol::DropVar(var_id));
}

/// A sync "read the latest committed world" API for consistency-agnostic
/// readers (test/debug/tooling). Do not use from an interpreter transaction.
pub trait CommittedReader {
    /// Number of global slots on `process`.
    fn global_slot_count(&self, process: &Process) -> usize;

    /// The committed value of one global slot (absent = `NULL`).
    fn committed_global(&self, process: &Process, reg: RegisterSize) -> LpcRef;

    /// The committed contents of one array payload cell; `None` if the var is
    /// absent or holds a slot value.
    fn committed_array(&self, var_id: VarId) -> Option<LpcArray>;

    /// The committed contents of one mapping payload cell, as in
    /// [`committed_array`]([`CommittedReader::committed_array`]).
    fn committed_mapping(&self, var_id: VarId) -> Option<LpcMapping>;

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
            .map(WorldValue::lpc_ref)
            .expect("committer always answers a query")
    }

    fn committed_array(&self, var_id: VarId) -> Option<LpcArray> {
        match self.committed_value(var_id)? {
            WorldValue::Array(array) => Some((*array).clone()),
            WorldValue::Ref(_) | WorldValue::Mapping(_) | WorldValue::Process(_) => None,
        }
    }

    fn committed_mapping(&self, var_id: VarId) -> Option<LpcMapping> {
        match self.committed_value(var_id)? {
            WorldValue::Mapping(mapping) => Some((*mapping).clone()),
            WorldValue::Ref(_) | WorldValue::Array(_) | WorldValue::Process(_) => None,
        }
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
        let Some(WorldValue::Array(inventory)) =
            self.committed_value(process.position.inventory.id)
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
    /// One synchronous round trip against the committer, exactly like the
    /// sync `retry()` helper's recvs: scope a thread so the blocking recv
    /// can never run on the calling thread. `None` only if the committer
    /// never answers (channel dead); absent vars reply as `NULL`.
    fn committed_value(&self, var_id: VarId) -> Option<WorldValue> {
        std::thread::scope(|s| {
            let (reply_tx, reply_rx) = flume::bounded(1);
            self.committer_tx
                .send(CommitProtocol::Query {
                    var_id,
                    reply: reply_tx,
                })
                .expect("committer channel closed");
            s.spawn(move || reply_rx.recv())
                .join()
                .expect("query reply thread panicked")
                .ok()
        })
    }
}

/// Async mirror of [`retry`]: run `f` (one attempt) against a fresh
/// transaction, commit it, and re-run `f` from scratch on each rejection
/// until one commits.
///
/// `f` opens the attempt's transaction (via [`start_txn`]), does the work,
/// and returns the `Transaction` plus the attempt's [`LiveSnapshot`]. The
/// loop commits the changeset and releases the `LiveSnapshot` after
/// the commit reply resolves.
pub(crate) async fn retry_async<F, Fut>(
    tx: &flume::Sender<CommitProtocol>,
    mut f: F,
) -> (Result<()>, RetryStats)
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<(Transaction, LiveSnapshot)>>,
{
    let started = Instant::now();
    let mut attempts = 0u64;
    let mut conflicts = 0u64;

    loop {
        attempts += 1;

        let transaction = match f().await {
            Ok(t) => t,
            Err(e) => {
                return (
                    Err(e),
                    RetryStats {
                        attempts,
                        conflicts,
                        duration: started.elapsed(),
                    },
                );
            }
        };
        let (live_txn, live) = transaction;
        let (_world, changeset) = live_txn.into_parts();

        let commit = match commit_changeset(tx, changeset).await {
            Ok(c) => c,
            Err(e) => {
                drop(live);
                return (
                    Err(e),
                    RetryStats {
                        attempts,
                        conflicts,
                        duration: started.elapsed(),
                    },
                );
            }
        };
        // Release the snapshot only after the commit reply has resolved.
        drop(live);

        if commit.is_ok() {
            return (
                Ok(()),
                RetryStats {
                    attempts,
                    conflicts,
                    duration: started.elapsed(),
                },
            );
        }
        conflicts += 1;
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Barrier};

    use lpc_rs_core::LpcIntInner;

    use super::{RetryStats, retry};
    use crate::interpreter::{
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{
            Transaction, VarId, Version, WorldValue, changeset::Changeset,
            committer::CommitProtocol, tests::*,
        },
    };

    /// `counter = counter + 1` with no atomics. Returns the written value.
    fn increment(t: &mut Transaction, counter: VarId) -> LpcInt {
        let LpcRef::Int(n) = t.read(counter).expect("counter cell missing") else {
            panic!("counter cell is not an int");
        };
        let next = n + LpcInt(1);
        t.write(counter, LpcRef::from(next));
        next
    }

    fn seed(tx: &flume::Sender<CommitProtocol>, v0: Version, var: VarId, value: LpcIntInner) {
        let mut seed = Changeset::new(v0);
        seed.write(var, WorldValue::ref_of(LpcRef::from(value)));
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Commit {
            changeset: seed,
            reply: reply_tx,
        })
        .expect("committer channel closed");
        reply_rx
            .recv()
            .expect("no reply from committer")
            .expect("seed should commit");
    }

    #[test]
    fn a_clean_attempt_commits_in_one_pass() {
        let (tx, v0, handle) = start_committer();
        let counter = VarId::new();
        seed(&tx, v0, counter, 5);

        let (value, stats) = retry(&tx, |t| increment(t, counter));

        assert_eq!(value, LpcInt(6));
        assert_eq!(stats.attempts, 1);
        assert_eq!(stats.conflicts, 0);
        close_committer(tx, handle);
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
                    let mut attempts = 0u64;
                    let mut conflicts = 0u64;
                    let mut duration = std::time::Duration::ZERO;
                    for _ in 0..ROUNDS {
                        let (_, stats): (_, RetryStats) = retry(&tx, |t| increment(t, counter));
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
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{Changeset, CommitProtocol, Committer, Transaction, VarId, WorldValue},
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
        // (Same pattern as B4's `committer.rs` async tests.)
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        let (res, stats) = retry_async(&tx, || async {
            let live = start_txn(&tx).await?;
            let mut t = Transaction::new(live.inner.clone());
            let LpcRef::Int(n) = t.read(counter).expect("counter cell missing") else {
                panic!("counter cell is not an int");
            };
            t.write(counter, LpcRef::from(n + LpcInt(1)));
            Ok::<_, Box<lpc_rs_errors::LpcError>>((t, live))
        })
        .await;

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
    async fn query_and_drop_var_roundtrip() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let var = VarId::new();
        seed(&mut committer, var, 7);
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        assert_eq!(
            query_var(&tx, var).await.unwrap(),
            WorldValue::ref_of(LpcRef::from(7))
        );

        drop_var(&tx, var);
        // The channel is FIFO, so the drop is processed before this query.
        assert_eq!(
            query_var(&tx, var).await.unwrap(),
            WorldValue::ref_of(LpcRef::Int(LpcInt(0)))
        );

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");
        assert_eq!(final_snapshot.read(var), None);
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

        let (res, stats) = retry_async(&tx, || async {
            let live = start_txn(&tx).await?;
            let mut t = Transaction::new(live.inner.clone());
            let LpcRef::Int(n) = t.read(counter).expect("counter cell missing") else {
                panic!("counter cell is not an int");
            };
            t.write(counter, LpcRef::from(n + LpcInt(1)));
            Ok::<_, Box<lpc_rs_errors::LpcError>>((t, live))
        })
        .await;

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
}
