use std::collections::{BTreeMap, BTreeSet};

use imbl::OrdMap;

use crate::interpreter::stm::{VarId, Version, changeset::Changeset, snapshot::Snapshot};

pub enum CommitProtocol {
    /// Start a transaction against the current world state.
    Start { reply: flume::Sender<Snapshot> },
    Commit {
        changeset: Changeset,
        reply: flume::Sender<Result<(), Changeset>>,
    },
}

struct Committer {
    snapshot: Snapshot,
    write_history: BTreeMap<Version, BTreeSet<VarId>>,
    oldest_version: Version,
}

impl Committer {
    pub(crate) fn new() -> Self {
        let version = Version::new();
        let snapshot = Snapshot::new(version, OrdMap::new());
        Self {
            snapshot,
            write_history: BTreeMap::new(),
            oldest_version: version,
        }
    }

    /// Committer's main loop.
    ///
    /// Returns the final state once the channel closes (all senders dropped).
    pub(crate) fn run(mut self, rx: flume::Receiver<CommitProtocol>) -> Snapshot {
        while let Ok(msg) = rx.recv() {
            match msg {
                CommitProtocol::Start { reply } => {
                    // todo: refcount the snapshot so history can be evicted below it
                    let _ = reply.send(self.snapshot.clone());
                }
                CommitProtocol::Commit { changeset, reply } => {
                    let result = self.commit(changeset);
                    // todo: callers handle their own retries
                    let _ = reply.send(result);
                }
            }
        }
        self.snapshot
    }

    /// Apply the changeset to the snapshot.
    ///
    /// *Conflict Rule:* A changeset built from version `V` conflicts if any
    /// `VarId` it read was written by a changeset committed after `V`.
    ///
    /// *NOTE* Blind writes (var is written, but never read within the same transaction) are _not_
    /// conflict-checked, and are applied in the order they are committed.
    ///
    /// Returns `Ok` on success, or `Err(changeset)` if the changeset conflicts.
    pub(crate) fn commit(&mut self, changeset: Changeset) -> Result<(), Changeset> {
        // TODO: ensure empty changesets don't make it this far. They work fine, but
        //       are unnecessary processing on a critical path.
        // if changeset.read_vars().is_empty() && changeset.written_vars().is_empty() {
        //     return Ok(());
        // }
        // debug_assert!(!changeset.read_vars().is_empty() || !changeset.written_vars().is_empty(), "empty changeset");

        let current_version = self.snapshot.version();
        let changeset_version = changeset.base_version();

        // Should not occur in practice - it implies versions are being created in more than one place.
        if current_version < changeset_version {
            return Err(changeset);
        }

        // changeset's base evicted → not enough history to resolve the conflict rule
        if changeset_version < self.oldest_version {
            return Err(changeset);
        }

        // check the conflict rule
        for (version, written_vars) in self
            .write_history
            .range(changeset_version..=current_version)
        {
            if *version == changeset_version {
                continue;
            }

            if !written_vars.is_disjoint(changeset.read_set()) {
                return Err(changeset);
            }
        }

        if changeset.write_set().is_empty() {
            // Conflict-free read-only changeset, so we're done.
            // No need for a new version or empty history insert.
            return Ok(());
        }

        let written_vars = changeset.write_set().clone();
        let new_version = Version::new();
        let new_snapshot = self.snapshot.apply(new_version, changeset);
        self.snapshot = new_snapshot;

        // Keep history insert after the snapshot apply, else a problem in apply leads to
        // all transactions conflicting in the future.
        self.write_history.insert(new_version, written_vars);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::LpcIntInner;
    use tokio::{sync::Barrier, task::JoinSet};

    use super::*;
    use crate::interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, stm::Transaction};

    /// The full cycle a task runs: start a transaction from the committer's
    /// current world, run `f` over it, then hand back `f`'s result, the
    /// transaction's snapshot (to release), and its changeset (to commit).
    async fn run_txn<T>(
        tx: &flume::Sender<CommitProtocol>,
        f: impl FnOnce(&mut Transaction) -> T + Send,
    ) -> (T, Snapshot, Changeset) {
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: reply_tx })
            .expect("committer channel closed");
        let snapshot = tokio::task::spawn_blocking(move || reply_rx.recv())
            .await
            .expect("reply task panicked")
            .expect("no reply from committer");
        let mut transaction = Transaction::new(snapshot);
        let result = f(&mut transaction);
        let (snapshot, changeset) = transaction.into_parts();
        (result, snapshot, changeset)
    }

    /// `counter = counter + 1` with no atomics
    fn increment(t: &mut Transaction, counter: VarId) {
        let LpcRef::Int(n) = t.read(counter).expect("counter cell missing") else {
            panic!("counter cell is not an int");
        };
        t.write(counter, LpcRef::from(n + LpcInt(1)));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn disjoint_changesets_all_succeed() {
        let (tx, rx) = flume::bounded(4);
        let committer = Committer::new();
        let handle = std::thread::spawn(move || committer.run(rx));

        // Each worker starts its own transaction against the committer and
        // writes a cell no other worker touches, so all three must land.
        let txs = (0..3).map(|_| tx.clone()).collect::<Vec<_>>();
        let gate = Arc::new(Barrier::new(3));

        let mut set = JoinSet::new();
        for (worker, tx) in txs.into_iter().enumerate() {
            let gate = gate.clone();
            set.spawn(async move {
                gate.wait().await;
                let (var_id, snapshot, changeset) = run_txn(&tx, |t| {
                    let var_id = VarId::new();
                    t.write(var_id, LpcRef::from(LpcInt(worker as LpcIntInner)));
                    var_id
                })
                .await;
                let (reply_tx, reply_rx) = flume::bounded(1);
                tx.send(CommitProtocol::Commit {
                    changeset,
                    reply: reply_tx,
                })
                .expect("committer channel closed");
                let result = tokio::task::spawn_blocking(move || reply_rx.recv())
                    .await
                    .expect("reply task panicked")
                    .expect("no reply from committer");
                // release the transaction's snapshot when it ends
                let _ = snapshot;
                result.expect("disjoint changeset was rejected");
                (worker, var_id)
            });
        }
        drop(tx);

        let mut committed = Vec::with_capacity(3);
        while let Some(joined) = set.join_next().await {
            committed.push(joined.expect("worker panicked"));
        }

        // channel has drained, so we can join and get the final state
        let final_snapshot = handle.join().expect("committer panicked");

        for (worker, var_id) in committed {
            assert_eq!(
                final_snapshot.read(var_id),
                Some(LpcRef::from(worker as LpcIntInner))
            );
        }
    }

    #[test]
    fn overlapping_changesets_handle_conflicts_correctly() {
        let mut committer = Committer::new();
        let version = committer.snapshot.version();

        let mut changeset1 = Changeset::new(version);
        let var_id1 = VarId::new();
        changeset1.write(var_id1, LpcRef::from(123));

        let mut changeset2 = Changeset::new(version);
        let var_id2 = VarId::new();
        changeset2.write(var_id2, LpcRef::from(456));

        let mut changeset3 = Changeset::new(version);
        // ID 2 was written after this changeset was created, so we're invalid by the conflict rule.
        changeset3.track_read(var_id2);
        changeset3.write(var_id1, LpcRef::from("boo!"));

        committer.commit(changeset1).unwrap();
        committer.commit(changeset2).unwrap();
        assert!(committer.commit(changeset3).is_err());

        assert_eq!(committer.write_history.len(), 2);
        assert_eq!(committer.snapshot.read(var_id1).unwrap(), LpcRef::from(123));
        assert_eq!(committer.snapshot.read(var_id2).unwrap(), LpcRef::from(456));
    }

    #[test]
    fn overlapping_blind_writes_apply_in_commit_order() {
        let mut committer = Committer::new();
        let version = committer.snapshot.version();

        let mut changeset1 = Changeset::new(version);
        let var_id = VarId::new();
        changeset1.write(var_id, LpcRef::from(123));

        let mut changeset2 = Changeset::new(version);
        changeset2.write(var_id, LpcRef::from(456));

        committer.commit(changeset2).unwrap();
        committer.commit(changeset1).unwrap(); // last commit wins

        assert_eq!(committer.write_history.len(), 2);
        assert_eq!(committer.snapshot.read(var_id).unwrap(), LpcRef::from(123));
    }

    #[test]
    fn changeset_with_a_version_ahead_of_snapshot_version_conflicts() {
        let mut committer = Committer::new();

        // In practice, this would be a bug.
        // Changeset versions should always be copied from the current snapshot.
        let mut changeset1 = Changeset::new(Version::new());

        let var_id = VarId::new();
        changeset1.write(var_id, LpcRef::from(123));

        assert!(committer.commit(changeset1).is_err());

        assert!(committer.write_history.is_empty());
        assert!(committer.snapshot.read(var_id).is_none());
    }

    #[test]
    fn read_only_changesets_do_not_apply_or_write_history() {
        let mut committer = Committer::new();
        let version = committer.snapshot.version();
        let var_id = VarId::new();

        let mut changeset = Changeset::new(version);
        changeset.track_read(var_id);
        committer.commit(changeset).unwrap();

        assert!(committer.write_history.is_empty());
        assert!(committer.snapshot.read(var_id).is_none());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn overlapping_changesets_reject_the_conflicting_ones() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        let initial = committer.snapshot.clone();
        let initial_value = 100;

        let mut seed = Changeset::new(initial.version());
        seed.write(counter, LpcRef::from(initial_value));
        committer.commit(seed).unwrap();

        let handle = std::thread::spawn(move || committer.run(rx));

        // All four transactions read `counter`, so each one's read set is
        // invalidated by any commit made after its base. The FIFO channel
        // makes queue order the tiebreak: exactly one - the first in -
        // commits, and its value is the one that survives.
        let txs = (0..4).map(|_| tx.clone()).collect::<Vec<_>>();
        let gate = Arc::new(Barrier::new(4));

        // All four must be holding their snapshots before any commit is
        // queued, so that every transaction is based on the same version.
        // (A worker that starts after another's commit has a fresh base and
        // would be allowed to commit, which is correct - but not what this
        // test is checking.)
        let bases_aligned = Arc::new(Barrier::new(4));
        let mut set = JoinSet::new();
        for (worker, tx) in txs.into_iter().enumerate() {
            let gate = gate.clone();
            let bases_aligned = bases_aligned.clone();
            set.spawn(async move {
                gate.wait().await;
                let (_, snapshot, changeset) = run_txn(&tx, |t| {
                    let _old = t.read(counter).expect("counter cell missing");
                    t.write(
                        counter,
                        LpcRef::from(initial_value + 1 + worker as LpcIntInner),
                    );
                })
                .await;
                bases_aligned.wait().await;
                let (reply_tx, reply_rx) = flume::bounded(1);
                tx.send(CommitProtocol::Commit {
                    changeset,
                    reply: reply_tx,
                })
                .expect("committer channel closed");
                let result = tokio::task::spawn_blocking(move || reply_rx.recv())
                    .await
                    .expect("reply task panicked")
                    .expect("no reply from committer");
                let _ = snapshot;
                (worker, result.is_ok())
            });
        }
        drop(tx);

        let mut outcomes = Vec::with_capacity(4);
        while let Some(joined) = set.join_next().await {
            outcomes.push(joined.expect("worker panicked"));
        }
        let final_snapshot = handle.join().expect("committer panicked");

        assert_eq!(outcomes.iter().filter(|(_, ok)| *ok).count(), 1);
        let (winner, _) = outcomes
            .into_iter()
            .find(|(_, ok)| *ok)
            .expect("one worker should commit");
        let expected = LpcRef::from(initial_value + 1 + winner as LpcIntInner);
        assert_eq!(final_snapshot.read(counter), Some(expected));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn concurrent_increments_total_equals_n() {
        // The B4 mirror of lost_update_racy: 8 tasks x 50 increments of
        // `count = count + 1` with no atomics, each through the full
        // start / commit / retry cycle. Every increment is read-validated,
        // so a lost update is impossible - rejected attempts re-run until
        // the total is exactly the sum of all increments.
        const TASKS: usize = 8;
        const ITERATIONS: usize = 50;

        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let counter = VarId::new();
        let initial = committer.snapshot.clone();
        {
            let mut seed = Changeset::new(initial.version());
            seed.write(counter, LpcRef::from(0));
            committer.commit(seed).unwrap();
        }
        let handle = std::thread::spawn(move || committer.run(rx));

        let txs = (0..TASKS).map(|_| tx.clone()).collect::<Vec<_>>();
        let gate = Arc::new(Barrier::new(TASKS));
        let mut set = JoinSet::new();
        for tx in txs {
            let gate = gate.clone();
            set.spawn(async move {
                gate.wait().await;
                for _ in 0..ITERATIONS {
                    let (_, snapshot, changeset) = run_txn(&tx, |t| increment(t, counter)).await;
                    let mut changeset = changeset;
                    loop {
                        let (reply_tx, reply_rx) = flume::bounded(1);
                        tx.send(CommitProtocol::Commit {
                            changeset,
                            reply: reply_tx,
                        })
                        .expect("committer channel closed");
                        let result = tokio::task::spawn_blocking(move || reply_rx.recv())
                            .await
                            .expect("reply task panicked")
                            .expect("no reply from committer");
                        match result {
                            Ok(()) => break,
                            // Rejected: its read set was invalidated. Release the
                            // stale snapshot, re-base on the current world, and
                            // re-apply the increment - D4's "unbounded re-run" at
                            // closure level; B5 wires this to the task itself.
                            Err(_) => {
                                let _ = snapshot;
                                let (reply_tx, reply_rx) = flume::bounded(1);
                                tx.send(CommitProtocol::Start { reply: reply_tx })
                                    .expect("committer channel closed");
                                let new_snapshot =
                                    tokio::task::spawn_blocking(move || reply_rx.recv())
                                        .await
                                        .expect("reply task panicked")
                                        .expect("no reply from committer");
                                let mut fresh = Transaction::new(new_snapshot);
                                increment(&mut fresh, counter);
                                let (_snapshot, cs) = fresh.into_parts();
                                let _ = _snapshot;
                                changeset = cs;
                            }
                        }
                    }
                }
            });
        }
        drop(tx);

        while let Some(joined) = set.join_next().await {
            joined.expect("worker panicked");
        }
        let final_snapshot = handle.join().expect("committer panicked");

        let LpcRef::Int(LpcInt(total)) =
            final_snapshot.read(counter).expect("counter cell missing")
        else {
            panic!("counter cell is not an int");
        };
        assert_eq!(total, (TASKS * ITERATIONS) as LpcIntInner);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn reader_holds_old_snapshot_while_writers_advance() {
        let (tx, rx) = flume::bounded(4);
        let mut committer = Committer::new();
        let var_id = VarId::new();
        let initial = committer.snapshot.clone();
        {
            let mut seed = Changeset::new(initial.version());
            seed.write(var_id, LpcRef::from(1));
            committer.commit(seed).unwrap();
        }
        let handle = std::thread::spawn(move || committer.run(rx));

        // A reader starts a transaction and holds onto its snapshot...
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: reply_tx })
            .expect("committer channel closed");
        let reader_snapshot = tokio::task::spawn_blocking(move || reply_rx.recv())
            .await
            .expect("reply task panicked")
            .expect("no reply from committer");

        // ...while a writer's commit moves the world forward.
        let (_, snapshot, changeset) = run_txn(&tx, |t| {
            let _ = t.read(var_id);
            t.write(var_id, LpcRef::from(2));
        })
        .await;
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Commit {
            changeset,
            reply: reply_tx,
        })
        .expect("committer channel closed");
        let result = tokio::task::spawn_blocking(move || reply_rx.recv())
            .await
            .expect("reply task panicked")
            .expect("no reply from committer");
        result.expect("writer should commit");
        let _ = snapshot;

        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");

        // The reader's snapshot is an immutable world: it still sees the
        // pre-commit value, while the latest snapshot shows the new one.
        assert_eq!(reader_snapshot.read(var_id), Some(LpcRef::from(1)));
        assert_eq!(final_snapshot.read(var_id), Some(LpcRef::from(2)));
    }
}
