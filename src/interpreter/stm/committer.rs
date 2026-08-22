use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
};

use imbl::OrdMap;
use lpc_rs_core::RegisterSize;
use lpc_rs_errors::lpc_error;
use tracing::error;

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{VarId, Version, WorldValue, changeset::Changeset, snapshot::Snapshot},
};

/// A unit of work in the world mark: either a var whose contents to read, or
/// an `LpcRef` whose payload/function captures to follow.
enum MarkWork {
    Var(VarId),
    Ref(LpcRef),
}

/// A root for the world sweep: a `Var` is a world slot whose committed
/// contents to follow; a `Ref` is a value held outside the world (a call-out's
/// function) whose payload refs still count as reachable.
#[derive(Debug, Clone)]
pub(crate) enum WorldRoot {
    Var(VarId),
    Ref(LpcRef),
}

/// Committer's lifetime commit totals, read back over the `Stats` protocol message.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CommitterStats {
    /// Successful commits
    pub commits: usize,
    /// Commits that failed due to conflicts
    pub conflicts: usize,
    /// Non-conflict errors that occurred during all operations.
    pub errors: usize,
}

impl CommitterStats {
    fn commit(&mut self) {
        self.commits += 1;
    }

    fn conflict(&mut self) {
        self.conflicts += 1;
    }

    fn error(&mut self) {
        self.errors += 1;
    }
}

/// Channel protocol for communication with [`Committer`]s.
///
/// `pub(crate)` (not `pub`): the arms carry `pub(crate)` types, so the enum
/// cannot be public without leaking them.
#[derive(Debug)]
pub(crate) enum CommitProtocol {
    /// Start a transaction against the current world state.
    Start { reply: flume::Sender<LiveSnapshot> },
    /// Commit a changeset to the world state
    Commit {
        changeset: Changeset,
        reply: flume::Sender<Result<(), Changeset>>,
    },
    /// Drop the reference count of a [`Version`], called when a [`Snapshot`] is dropped.
    Drop(Version),
    /// Query the committed value of a var (consistency-agnostic readers:
    /// test/debug/tooling, Do not use this for normal operations).
    Query {
        var_id: VarId,
        reply: flume::Sender<WorldValue>,
    },
    /// How many transactions are in flight (held [`LiveSnapshot`]s). `0` means
    /// quiescent, the precondition for a GC pass.
    LiveCount { reply: flume::Sender<usize> },
    /// One GC pass, atomic: refuse unless quiescent, else drop the swept
    /// upvalue cells and mark the world from `roots` in this one message, so
    /// the cell drops cannot be reordered past the sweep. Replies the report.
    GcPass {
        dropped: Vec<VarId>,
        roots: Vec<WorldRoot>,
        reply: flume::Sender<GcPassReply>,
    },
    /// Lifetime commit totals; for bench measurement and tooling, not the hot path.
    Stats {
        reply: flume::Sender<CommitterStats>,
    },
    /// Shutdown.
    Close,
}

/// The pass's reply: the report, or the committer's refusal.
type GcPassReply = std::result::Result<GcReport, Box<lpc_rs_errors::LpcError>>;

/// The result of one GC pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcReport {
    /// Upvalue cells the slab half culled (their `VarId`s were dropped from the world).
    pub cells_swept: usize,
    /// Unreachable `Array`/`Mapping` payload vars the world half reclaimed.
    pub payload_vars_reclaimed: usize,
}

/// A snapshot the committer has handed out. Dropping it releases the
/// version so write history below it can be evicted.
#[derive(Debug)]
pub(crate) struct LiveSnapshot {
    pub(crate) inner: Snapshot,
    release: flume::Sender<CommitProtocol>,
}

impl LiveSnapshot {
    pub(crate) fn new(inner: Snapshot, release: flume::Sender<CommitProtocol>) -> Self {
        Self { inner, release }
    }
}

impl Deref for LiveSnapshot {
    type Target = Snapshot;

    fn deref(&self) -> &Snapshot {
        &self.inner
    }
}

impl Drop for LiveSnapshot {
    fn drop(&mut self) {
        let _ = self
            .release
            .send(CommitProtocol::Drop(self.inner.version()));
    }
}

/// Represents a `Committer` which is responsible for managing versioning and
/// tracking changes in data snapshots using a version control mechanism.
#[derive(Debug)]
pub(crate) struct Committer {
    /// The current snapshot of the world
    snapshot: Snapshot,
    /// The history of written variables by version, used to check for conflicts during commit operations.
    write_history: BTreeMap<Version, BTreeSet<VarId>>,
    /// The oldest retained version. If we are trying to commit a [`Changeset`] with a base version
    /// that is older than this, it automatically fails.
    oldest_retained: Version,
    /// The counts of live snapshots for each version. Used for maintaining `oldest_retained` and `write_history`
    live_versions: BTreeMap<Version, u32>,
    /// Statistics related to the committer's activities
    stats: CommitterStats,
}

impl Committer {
    /// Create a new committer.
    /// Sets up the initial [`Version`] and [`Snapshot`].
    pub(crate) fn new() -> Self {
        let version = Version::new();
        let snapshot = Snapshot::new(version, OrdMap::new());
        Self {
            snapshot,
            write_history: BTreeMap::new(),
            oldest_retained: version,
            live_versions: BTreeMap::new(),
            stats: CommitterStats::default(),
        }
    }

    /// Committer's main loop.
    /// Returns the final snapshot.
    pub(crate) fn run(
        mut self,
        tx: flume::Sender<CommitProtocol>,
        rx: flume::Receiver<CommitProtocol>,
    ) -> Snapshot {
        while let Ok(msg) = rx.recv() {
            if !self.process(msg, &tx) {
                break;
            }
        }
        self.snapshot
    }

    /// Process one message. Returns `false` on `Close`.
    pub(crate) fn process(
        &mut self,
        msg: CommitProtocol,
        tx: &flume::Sender<CommitProtocol>,
    ) -> bool {
        match msg {
            CommitProtocol::Start { reply } => {
                // Count before hand-out: a failed reply drops the
                // LiveSnapshot, which self-releases.
                *self
                    .live_versions
                    .entry(self.snapshot.version())
                    .or_insert(0) += 1;
                let live_snapshot = LiveSnapshot::new(self.snapshot.clone(), tx.clone());
                reply.send(live_snapshot).unwrap_or_else(|e| {
                    error!("Failed to send live snapshot: {e}");
                    self.stats.error()
                });
            }
            CommitProtocol::Commit { changeset, reply } => {
                let result = self.commit(changeset);
                if result.is_ok() {
                    self.evict_old_versions();
                }
                reply.send(result).unwrap_or_else(|e| {
                    error!("Failed to send commit result: {e}");
                    self.stats.error()
                });
            }
            CommitProtocol::Drop(version) => {
                let Some(count) = self.live_versions.get_mut(&version) else {
                    unreachable!("drop for version {version:?} with no live count");
                };
                *count -= 1;
                if *count == 0 {
                    self.live_versions.remove(&version);
                }
                self.evict_old_versions();
            }
            CommitProtocol::Query { var_id, reply } => {
                let value = self.snapshot.read(var_id).unwrap_or_else(WorldValue::null);
                reply.send(value).unwrap_or_else(|e| {
                    error!("Failed to send query reply: {e}");
                    self.stats.error()
                });
            }
            CommitProtocol::LiveCount { reply } => {
                // Total held snapshots across versions; `0` = quiescent.
                let live: usize = self.live_versions.values().map(|&c| c as usize).sum();
                reply.send(live).unwrap_or_else(|e| {
                    error!("Failed to send live count: {e}");
                    self.stats.error()
                });
            }
            CommitProtocol::GcPass {
                dropped,
                roots,
                reply,
            } => {
                // Same live-count expression as the `LiveCount` arm, checked
                // in this message so a task that starts after the client's
                // fast-refuse probe still finds the pass refused.
                let live: usize = self.live_versions.values().map(|&c| c as usize).sum();
                if live != 0 {
                    let _ = reply.send(Err(lpc_error!(
                        "gc refused: committer not quiescent ({live} transaction(s) in flight)"
                    )));
                    return true;
                }
                for var_id in &dropped {
                    self.snapshot.drop_var(*var_id);
                }
                let reclaimed = self.mark_world(&roots);
                let _ = reply.send(Ok(GcReport {
                    cells_swept: dropped.len(),
                    payload_vars_reclaimed: reclaimed,
                }));
            }
            CommitProtocol::Stats { reply } => {
                reply.send(self.stats).unwrap_or_else(|e| {
                    error!("Failed to send stats: {e}");
                    self.stats.error()
                });
            }
            CommitProtocol::Close => {
                return false;
            }
        }
        true
    }

    /// Mark the world from `roots` and drop the unreachable `Array`/`Mapping`
    /// payload vars. Returns the number of vars dropped.
    ///
    /// Only payload vars are reclaimed — object, connection, and upvalue-cell
    /// vars are never dropped.
    fn mark_world(&mut self, roots: &[WorldRoot]) -> usize {
        let mut marked: BTreeSet<VarId> = BTreeSet::new();
        let mut work: Vec<MarkWork> = Vec::with_capacity(roots.len());
        for root in roots {
            match root {
                WorldRoot::Var(var_id) => {
                    marked.insert(*var_id);
                    work.push(MarkWork::Var(*var_id));
                }
                WorldRoot::Ref(lpc_ref) => work.push(MarkWork::Ref(lpc_ref.clone())),
            }
        }

        while let Some(item) = work.pop() {
            match item {
                MarkWork::Var(var_id) => {
                    let Some(world_value) = self.snapshot.read(var_id) else {
                        continue;
                    };
                    work.extend(Self::edges_of(&world_value));
                }
                MarkWork::Ref(lpc_ref) => match lpc_ref {
                    LpcRef::Array(svar) => {
                        if marked.insert(svar.id) {
                            work.push(MarkWork::Var(svar.id));
                        }
                    }
                    LpcRef::Mapping(svar) => {
                        if marked.insert(svar.id) {
                            work.push(MarkWork::Var(svar.id));
                        }
                    }
                    LpcRef::Function(fun) => {
                        for arg_ref in fun.partial_args().iter().flatten() {
                            work.push(MarkWork::Ref(arg_ref.clone()));
                        }
                    }
                    LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::String(_) | LpcRef::Object(_) => {}
                },
            }
        }

        let to_drop: Vec<VarId> = self
            .snapshot
            .state()
            .filter(|(var_id, world_value)| {
                !marked.contains(*var_id)
                    && matches!(world_value, WorldValue::Array(_) | WorldValue::Mapping(_))
            })
            .map(|(var_id, _)| *var_id)
            .collect();
        let reclaimed = to_drop.len();
        for var_id in to_drop {
            self.snapshot.drop_var(var_id);
        }
        reclaimed
    }

    /// The outgoing edges from one world entry: the payload vars its contents
    /// name, and the child cells an identity entry owns.
    fn edges_of(world_value: &WorldValue) -> Vec<MarkWork> {
        match world_value {
            WorldValue::Ref(lpc_ref) => vec![MarkWork::Ref(lpc_ref.clone())],
            WorldValue::Array(array) => array
                .array
                .iter()
                .map(|member| MarkWork::Ref(member.clone()))
                .collect(),
            WorldValue::Mapping(mapping) => {
                let mut out = Vec::with_capacity(mapping.mapping.len() * 2);
                for (key, value) in mapping.mapping.iter() {
                    out.push(MarkWork::Ref(key.clone()));
                    out.push(MarkWork::Ref(value.clone()));
                }
                out
            }
            WorldValue::Process(process) => {
                // The object's globals are separate slot vars that must stay alive.
                let num_globals = process.program.num_globals as usize;
                let mut out = Vec::with_capacity(num_globals + 3);
                for reg in 0..num_globals {
                    out.push(MarkWork::Var(process.var_id(reg as RegisterSize)));
                }
                out.push(MarkWork::Var(process.position.environment.id));
                out.push(MarkWork::Var(process.position.inventory.id));
                out.push(MarkWork::Var(process.connection.id));
                out
            }
            WorldValue::Connection(maybe_connection) => {
                // The connection's `input_to` target is a strong ref: it keeps
                // its function alive for the next input line.
                if let Some(connection) = maybe_connection
                    && let Some(input_to) = connection.input_to.load_full()
                {
                    vec![MarkWork::Ref(LpcRef::Function(input_to.ptr.clone()))]
                } else {
                    Vec::new()
                }
            }
        }
    }

    /// Trim write history, and bookkeep oldest retained version.
    fn evict_old_versions(&mut self) {
        // watermark = oldest version still held by a live snapshot
        let watermark = self
            .live_versions
            .first_key_value()
            .map(|(k, _)| *k)
            .unwrap_or_else(|| self.snapshot.version()); // nothing live: retain nothing
        self.write_history.retain(|v, _| *v >= watermark);
        self.oldest_retained = watermark;
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
            self.stats.conflict();
            return Err(changeset);
        }

        // changeset's base evicted → not enough history to resolve the conflict rule
        if changeset_version < self.oldest_retained {
            self.stats.conflict();
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
                self.stats.conflict();
                return Err(changeset);
            }
        }

        if changeset.touched_vars().is_empty() {
            // Conflict-free read-only changeset, so we're done.
            // No need for a new version or empty history insert.
            // TODO: track stats for these?
            return Ok(());
        }

        let written_vars = changeset.touched_vars();
        let new_version = Version::new();
        let new_snapshot = self.snapshot.apply(new_version, changeset);
        self.snapshot = new_snapshot;

        // Keep history insert after the snapshot apply, else a problem in apply leads to
        // all transactions conflicting in the future.
        self.write_history.insert(new_version, written_vars);
        self.stats.commit();

        Ok(())
    }
}

#[cfg(test)]
impl Committer {
    /// How many versions' write sets are retained.
    pub(crate) fn retained_versions(&self) -> usize {
        self.write_history.len()
    }

    /// Whether a version's write set is retained.
    pub(crate) fn retains_version(&self, version: Version) -> bool {
        self.write_history.contains_key(&version)
    }

    /// The oldest retained version, i.e. the eviction watermark.
    pub(crate) fn oldest_retained_version(&self) -> Version {
        self.oldest_retained
    }

    /// The version of the current world snapshot.
    pub(crate) fn current_version(&self) -> Version {
        self.snapshot.version()
    }

    /// A clone of the current world snapshot (sibling test modules can't
    /// reach the field directly).
    pub(crate) fn snapshot_clone(&self) -> Snapshot {
        self.snapshot.clone()
    }

    /// Run the committer loop, rejecting the first `n` `Commit`s it
    /// receives. The rejection is a synthetic abort with the same
    /// `Err(changeset)` reply the conflict rule would send.
    pub(crate) fn run_with_rejections(
        mut self,
        tx: flume::Sender<CommitProtocol>,
        rx: flume::Receiver<CommitProtocol>,
        rejections: usize,
    ) -> Snapshot {
        let mut rejections_left = rejections;
        while let Ok(msg) = rx.recv() {
            if rejections_left > 0
                && let CommitProtocol::Commit { changeset, reply } = msg
            {
                rejections_left -= 1;
                let _ = reply.send(Err(changeset));
                continue;
            }
            if !self.process(msg, &tx) {
                break;
            }
        }
        self.snapshot
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::LpcIntInner;
    use tokio::{sync::Barrier, task::JoinSet};

    use super::*;
    use crate::interpreter::{
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{SVar, Transaction, WorldValue, tests::*},
    };

    /// Start a transaction against the committer's current world and hand
    /// back the [`LiveSnapshot`] handle.
    ///
    /// Drop the handle only after the commit reply is resolved - its release
    /// is queued after the commit on the same FIFO channel.
    async fn start_txn(tx: &flume::Sender<CommitProtocol>) -> LiveSnapshot {
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: reply_tx })
            .expect("committer channel closed");
        tokio::task::spawn_blocking(move || reply_rx.recv())
            .await
            .expect("reply task panicked")
            .expect("no reply from committer")
    }

    /// The full cycle a task runs: start a transaction from the committer's
    /// current world, run `f` over it, and hand back `f`'s result, the
    /// changeset, and the snapshot's release handle.
    async fn run_txn<T>(
        tx: &flume::Sender<CommitProtocol>,
        f: impl FnOnce(&mut Transaction) -> T + Send,
    ) -> (T, Changeset, LiveSnapshot) {
        let live = start_txn(tx).await;
        let mut transaction = Transaction::new(live.inner.clone());
        let result = f(&mut transaction);
        let (_world, changeset) = transaction.into_parts();
        (result, changeset, live)
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
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        // Each worker starts its own transaction against the committer and
        // writes a cell no other worker touches, so all three must land.
        let txs = (0..3).map(|_| tx.clone()).collect::<Vec<_>>();
        let gate = Arc::new(Barrier::new(3));

        let mut set = JoinSet::new();
        for (worker, tx) in txs.into_iter().enumerate() {
            let gate = gate.clone();
            set.spawn(async move {
                gate.wait().await;
                let (var_id, changeset, live) = run_txn(&tx, |t| {
                    let var_id = VarId::new();
                    t.write(var_id, LpcRef::from(worker as LpcIntInner));
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
                result.expect("disjoint changeset was rejected");
                // release the snapshot only after the commit reply resolves
                drop(live);
                (worker, var_id)
            });
        }

        let mut committed = Vec::with_capacity(3);
        while let Some(joined) = set.join_next().await {
            committed.push(joined.expect("worker panicked"));
        }

        // Close settles the channel, so joining the committer gets the final state
        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");

        for (worker, var_id) in committed {
            assert_eq!(
                final_snapshot.read(var_id),
                Some(WorldValue::ref_of(LpcRef::from(worker as LpcIntInner)))
            );
        }
    }

    #[test]
    fn overlapping_changesets_handle_conflicts_correctly() {
        let mut committer = Committer::new();
        let version = committer.snapshot.version();

        let mut changeset1 = Changeset::new(version);
        let var_id1 = VarId::new();
        changeset1.write(var_id1, WorldValue::ref_of(LpcRef::from(123)));

        let mut changeset2 = Changeset::new(version);
        let var_id2 = VarId::new();
        changeset2.write(var_id2, WorldValue::ref_of(LpcRef::from(456)));

        let mut changeset3 = Changeset::new(version);
        // ID 2 was written after this changeset was created, so we're invalid by the conflict rule.
        changeset3.track_read(var_id2);
        changeset3.write(var_id1, WorldValue::ref_of(LpcRef::from("boo!")));

        committer.commit(changeset1).unwrap();
        committer.commit(changeset2).unwrap();
        assert!(committer.commit(changeset3).is_err());

        assert_eq!(committer.write_history.len(), 2);
        assert_eq!(
            committer.snapshot.read(var_id1).unwrap(),
            WorldValue::ref_of(LpcRef::from(123))
        );
        assert_eq!(
            committer.snapshot.read(var_id2).unwrap(),
            WorldValue::ref_of(LpcRef::from(456))
        );
    }

    #[test]
    fn overlapping_blind_writes_apply_in_commit_order() {
        let mut committer = Committer::new();
        let version = committer.snapshot.version();

        let mut changeset1 = Changeset::new(version);
        let var_id = VarId::new();
        changeset1.write(var_id, WorldValue::ref_of(LpcRef::from(123)));

        let mut changeset2 = Changeset::new(version);
        changeset2.write(var_id, WorldValue::ref_of(LpcRef::from(456)));

        committer.commit(changeset2).unwrap();
        committer.commit(changeset1).unwrap(); // last commit wins

        assert_eq!(committer.write_history.len(), 2);
        assert_eq!(
            committer.snapshot.read(var_id).unwrap(),
            WorldValue::ref_of(LpcRef::from(123))
        );
    }

    #[test]
    fn changeset_with_a_version_ahead_of_snapshot_version_conflicts() {
        let mut committer = Committer::new();

        // In practice, this would be a bug.
        // Changeset versions should always be copied from the current snapshot.
        let mut changeset1 = Changeset::new(Version::new());

        let var_id = VarId::new();
        changeset1.write(var_id, WorldValue::ref_of(LpcRef::from(123)));

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
        seed.write(counter, WorldValue::ref_of(LpcRef::from(initial_value)));
        committer.commit(seed).unwrap();

        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

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
                let (_, changeset, live) = run_txn(&tx, |t| {
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
                drop(live);
                (worker, result.is_ok())
            });
        }

        let mut outcomes = Vec::with_capacity(4);
        while let Some(joined) = set.join_next().await {
            outcomes.push(joined.expect("worker panicked"));
        }
        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");

        assert_eq!(outcomes.iter().filter(|(_, ok)| *ok).count(), 1);
        let (winner, _) = outcomes
            .into_iter()
            .find(|(_, ok)| *ok)
            .expect("one worker should commit");
        let expected = WorldValue::ref_of(LpcRef::from(initial_value + 1 + winner as LpcIntInner));
        assert_eq!(final_snapshot.read(counter), Some(expected));
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn concurrent_increments_total_equals_n() {
        // The incremental mirror of lost_update_racy: 8 tasks x 50 increments of
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
            seed.write(counter, WorldValue::ref_of(LpcRef::from(0)));
            committer.commit(seed).unwrap();
        }
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        let txs = (0..TASKS).map(|_| tx.clone()).collect::<Vec<_>>();
        let gate = Arc::new(Barrier::new(TASKS));
        let mut set = JoinSet::new();
        for tx in txs {
            let gate = gate.clone();

            set.spawn(async move {
                gate.wait().await;

                for _ in 0..ITERATIONS {
                    let (_, changeset, live) = run_txn(&tx, |t| increment(t, counter)).await;
                    let mut changeset = changeset;
                    // live is re-bound each iteration; the previous attempt's
                    // snapshot is released only after its commit resolved.
                    let mut live = live;

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
                            Ok(()) => {
                                drop(live);
                                break;
                            }
                            // Rejected: re-base on the current world and
                            // re-apply the increment
                            Err(_) => {
                                let new_live = start_txn(&tx).await;
                                let mut fresh = Transaction::new(new_live.inner.clone());
                                increment(&mut fresh, counter);
                                let (_world, cs) = fresh.into_parts();
                                changeset = cs;
                                live = new_live;
                            }
                        }
                    }
                }
            });
        }

        while let Some(joined) = set.join_next().await {
            joined.expect("worker panicked");
        }
        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");

        let WorldValue::Ref(LpcRef::Int(LpcInt(total))) =
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
            seed.write(var_id, WorldValue::ref_of(LpcRef::from(1)));
            committer.commit(seed).unwrap();
        }
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        // A reader starts a transaction and holds onto its snapshot...
        let reader_live = start_txn(&tx).await;
        let reader_snapshot = reader_live.inner.clone();

        // ...while a writer's commit moves the world forward.
        let (_, changeset, live) = run_txn(&tx, |t| {
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
        drop(live);
        drop(reader_live);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let final_snapshot = handle.join().expect("committer panicked");

        // The reader's snapshot is an immutable world: it still sees the
        // pre-commit value, while the latest snapshot shows the new one.
        assert_eq!(
            reader_snapshot.read(var_id),
            Some(WorldValue::ref_of(LpcRef::from(1)))
        );
        assert_eq!(
            final_snapshot.read(var_id),
            Some(WorldValue::ref_of(LpcRef::from(2)))
        );
    }

    /// Eviction follows the oldest live snapshot, and write history stays
    /// bounded.
    ///
    /// Do not switch the watermark back to a query on write_history keys -
    /// the initial snapshot is never a key, and that inference falsely
    /// rejected changesets based on the first world state.
    #[test]
    fn eviction_bounds_history_to_the_oldest_live_snapshot() {
        let (tx, rx) = flume::unbounded();
        let mut committer = Committer::new();
        let v0 = committer.snapshot.version();
        let cell = VarId::new();

        // 50 commits, each snapshot released right after its commit.
        // Without eviction, write_history would hold all 50 entries.
        for _ in 0..50 {
            let (_base, result) = drive_txn(&mut committer, &tx, &rx, |t| {
                t.write(cell, LpcRef::from(1));
            });
            assert!(result.is_ok());
        }
        assert_eq!(committer.write_history.len(), 1);

        // A snapshot held across commits pins the watermark at its version;
        // a new transaction based on that watermark is still valid.
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(CommitProtocol::Start { reply: reply_tx }, &tx);
        let live = reply_rx.recv().expect("no reply from committer");
        let pin_version = live.inner.version();
        let (base, result) =
            drive_txn(&mut committer, &tx, &rx, |t| t.write(cell, LpcRef::from(2)));
        assert!(result.is_ok());
        assert_eq!(base, pin_version);
        assert_eq!(committer.write_history.len(), 2);
        drop(live);
        pump(&mut committer, &tx, &rx);

        // All live snapshots released: the watermark returns to current.
        assert_eq!(committer.oldest_retained, committer.snapshot.version());
        assert_eq!(committer.write_history.len(), 1);

        // A base older than the watermark is rejected, not checked.
        let mut stale = Changeset::new(v0);
        stale.write(cell, WorldValue::ref_of(LpcRef::from(3)));
        assert!(committer.commit(stale).is_err());
        assert_eq!(committer.stats.conflicts, 1);
    }

    #[test]
    fn query_returns_committed_value_and_gc_pass_drop_removes_it() {
        let (tx, _rx) = flume::unbounded();
        let mut committer = Committer::new();
        let var = VarId::new();

        // absent → NULL
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::Query {
                var_id: var,
                reply: reply_tx,
            },
            &tx,
        );
        assert_eq!(
            reply_rx.recv().unwrap(),
            WorldValue::ref_of(LpcRef::from(0))
        );

        // A committed value is visible
        let mut seed = Changeset::new(committer.snapshot.version());
        seed.write(var, WorldValue::ref_of(LpcRef::from(7)));
        committer.commit(seed).unwrap();
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::Query {
                var_id: var,
                reply: reply_tx,
            },
            &tx,
        );
        assert_eq!(
            reply_rx.recv().unwrap(),
            WorldValue::ref_of(LpcRef::from(7))
        );

        // A GC pass with `var` in `dropped` removes it from the world.
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                dropped: vec![var],
                roots: Vec::new(),
                reply: reply_tx,
            },
            &tx,
        );
        assert!(reply_rx.recv().unwrap().is_ok());
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::Query {
                var_id: var,
                reply: reply_tx,
            },
            &tx,
        );
        assert_eq!(
            reply_rx.recv().unwrap(),
            WorldValue::ref_of(LpcRef::from(0))
        );
    }

    #[test]
    fn gc_pass_refuses_when_not_quiescent() {
        let (tx, drop_rx) = flume::unbounded();
        let mut committer = Committer::new();
        let var = VarId::new();
        let mut seed = Changeset::new(committer.snapshot.version());
        seed.write(var, WorldValue::ref_of(LpcRef::from(7)));
        committer.commit(seed).unwrap();

        // A held snapshot is an in-flight transaction.
        let (start_tx, start_rx) = flume::bounded(1);
        committer.process(CommitProtocol::Start { reply: start_tx }, &tx);
        let live = start_rx.recv().unwrap();

        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                dropped: vec![var],
                roots: Vec::new(),
                reply: reply_tx,
            },
            &tx,
        );
        assert!(reply_rx.recv().unwrap().is_err());

        // Refused: the var survives the pass untouched.
        drop(live);
        pump(&mut committer, &tx, &drop_rx);
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::Query {
                var_id: var,
                reply: reply_tx,
            },
            &tx,
        );
        assert_eq!(
            reply_rx.recv().unwrap(),
            WorldValue::ref_of(LpcRef::from(7))
        );
    }

    #[test]
    fn gc_pass_drops_cells_and_reclaims_in_one_message() {
        let (tx, _rx) = flume::unbounded();
        let mut committer = Committer::new();

        // Two array payload vars: one will be a live root, one unreachable.
        let live_payload = SVar::<LpcArray>::new();
        let dead_payload = SVar::<LpcArray>::new();
        let cell = VarId::new();
        let mut seed = Changeset::new(committer.snapshot.version());
        seed.write(
            live_payload.id,
            WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(1)]))),
        );
        seed.write(
            dead_payload.id,
            WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(2)]))),
        );
        seed.write(cell, WorldValue::ref_of(LpcRef::from(9)));
        committer.commit(seed).unwrap();

        // Drop `cell` while sweeping, and root only the live payload. The
        // dead payload is reclaimed in the same message, not a later pass.
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                dropped: vec![cell],
                roots: vec![WorldRoot::Var(live_payload.id)],
                reply: reply_tx,
            },
            &tx,
        );
        let report = reply_rx.recv().unwrap().unwrap();
        assert_eq!(report.cells_swept, 1);
        assert_eq!(report.payload_vars_reclaimed, 1);

        let is_null = |committer: &mut Committer, var: VarId| -> bool {
            let (reply_tx, reply_rx) = flume::bounded(1);
            committer.process(
                CommitProtocol::Query {
                    var_id: var,
                    reply: reply_tx,
                },
                &tx,
            );
            reply_rx.recv().unwrap() == WorldValue::null()
        };
        // The live payload survives; the dead payload and the dropped cell read back NULL.
        assert!(!is_null(&mut committer, live_payload.id));
        assert!(is_null(&mut committer, dead_payload.id));
        assert!(is_null(&mut committer, cell));
    }
}
