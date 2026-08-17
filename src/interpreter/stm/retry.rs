//! D4: unbounded internal re-run of a transaction until it commits.

use std::time::{Duration, Instant};

use crate::interpreter::stm::{
    Transaction,
    committer::{CommitProtocol, send_commit},
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
        let commit_result = send_commit(tx, changeset);
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

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Barrier};

    use lpc_rs_core::LpcIntInner;

    use super::{RetryStats, retry};
    use crate::interpreter::{
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{
            Transaction, VarId, Version,
            changeset::Changeset,
            committer::{CommitProtocol, close_committer, send_commit, start_committer},
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
        seed.write(var, LpcRef::from(value));
        send_commit(tx, seed).expect("seed should commit");
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
        let LpcRef::Int(LpcInt(total)) =
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
