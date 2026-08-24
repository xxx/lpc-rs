use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
};

use tokio::sync::watch;
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

/// Committer's lifetime totals and gauges, read back over the `Stats`
/// protocol message. Counters only grow; the two gauges (`live_snapshots`,
/// `versions_retained`) are refreshed at each read.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CommitterStats {
    /// Commits that wrote a new version.
    pub commits: usize,
    /// Conflict-free commits with nothing to write; no version is created.
    pub read_only_commits: usize,
    /// Commits that failed due to conflicts
    pub conflicts: usize,
    /// Replies the committer could not deliver (receiver gone).
    pub reply_failures: usize,
    /// Wall time servicing work messages; a `Stats` read accrues nothing.
    pub busy_ns: u64,
    /// Wall time inside the `Commit` arm alone.
    pub commit_service_ns: u64,
    /// History versions the conflict rule has walked, summed over commits.
    pub validation_scanned_versions: u64,
    /// Deepest backlog seen behind a message the run loop pulled.
    pub queue_peak: usize,
    /// Write-history versions evicted at the watermark.
    pub evictions: usize,
    /// Gauge: live snapshot pins at the last `Stats` read.
    pub live_snapshots: usize,
    /// Gauge: write-history versions held at the last `Stats` read.
    pub versions_retained: usize,
}

impl CommitterStats {
    fn commit(&mut self) {
        self.commits += 1;
    }

    fn read_only_commit(&mut self) {
        self.read_only_commits += 1;
    }

    fn conflict(&mut self) {
        self.conflicts += 1;
    }

    fn reply_failure(&mut self) {
        self.reply_failures += 1;
    }
}

/// A rejected commit: the changeset read state a later commit wrote. The
/// changeset itself is not returned — the loser re-runs from a fresh
/// snapshot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Conflict;

/// Channel protocol for communication with [`Committer`]s.
///
/// `pub(crate)` (not `pub`): the arms carry `pub(crate)` types, so the enum
/// cannot be public without leaking them.
#[derive(Debug)]
// A `Commit` moves its whole changeset through the channel: boxing it would
// buy back the size lint with an allocation per commit.
#[allow(clippy::large_enum_variant)]
pub(crate) enum CommitProtocol {
    /// Start a transaction against the current world state.
    Start { reply: flume::Sender<LiveSnapshot> },
    /// Commit a changeset to the world state
    Commit {
        changeset: Changeset,
        /// The commit carries the release of its base version's pin: true
        /// only when the sender's [`LiveSnapshot`] was disarmed. A bare
        /// commit (seed, probe) leaves other holders' pins alone.
        releases_base: bool,
        reply: flume::Sender<Result<(), Conflict>>,
    },
    /// Drop the reference count of a [`Version`], called when a [`Snapshot`] is dropped.
    Drop(Version),
    /// One GC pass, atomic: refuse unless quiescent, else mark the world from
    /// `roots` and drop what the walk did not reach. Replies the report.
    GcPass {
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
pub(crate) type GcPassReply = std::result::Result<GcReport, GcRefused>;

/// The committer refused a GC pass: `live` transactions still held snapshots.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcRefused {
    pub live: usize,
}

impl std::fmt::Display for GcRefused {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "gc refused: committer not quiescent ({} transaction(s) in flight)",
            self.live
        )
    }
}

/// The result of one GC pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcReport {
    /// World vars the walk did not reach: captured cells, dead objects'
    /// globals, and array/mapping payloads.
    pub reclaimed: usize,
}

/// A snapshot the committer has handed out. Dropping it releases the
/// version so write history below it can be evicted.
#[derive(Debug)]
pub(crate) struct LiveSnapshot {
    pub(crate) inner: Snapshot,
    release: Option<flume::Sender<CommitProtocol>>,
}

impl LiveSnapshot {
    pub(crate) fn new(inner: Snapshot, release: flume::Sender<CommitProtocol>) -> Self {
        Self {
            inner,
            release: Some(release),
        }
    }

    /// Disarm the drop-release: the commit built on this snapshot carries
    /// the release instead.
    pub(crate) fn disarm(&mut self) {
        self.release = None;
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
        if let Some(release) = self.release.take() {
            let _ = release.send(CommitProtocol::Drop(self.inner.version()));
        }
    }
}

/// Represents a `Committer` which is responsible for managing versioning and
/// tracking changes in data snapshots using a version control mechanism.
#[derive(Debug)]
pub(crate) struct Committer {
    /// The current snapshot of the world
    snapshot: Snapshot,
    /// The history of written variables by version, used to check for conflicts during commit operations.
    write_history: BTreeMap<Version, ahash::AHashSet<VarId>>,
    /// The oldest retained version. If we are trying to commit a [`Changeset`] with a base version
    /// that is older than this, it automatically fails.
    oldest_retained: Version,
    /// The counts of live snapshots for each version. Used for maintaining `oldest_retained` and `write_history`
    live_versions: BTreeMap<Version, u32>,
    /// Statistics related to the committer's activities
    stats: CommitterStats,

    /// Fan-out watermark: bumped once per written commit for losers to await.
    commit_watch: watch::Sender<Version>,
}

impl Committer {
    /// Create a new committer.
    /// Sets up the initial [`Version`] and [`Snapshot`].
    pub(crate) fn new() -> Self {
        let version = Version::new();
        let snapshot = Snapshot::new(version, imbl::HashMap::new());
        let (commit_watch, _) = watch::channel(version);
        Self {
            snapshot,
            write_history: BTreeMap::new(),
            oldest_retained: version,
            live_versions: BTreeMap::new(),
            stats: CommitterStats::default(),
            commit_watch,
        }
    }

    /// Subscribe to the per-commit watermark bump.
    pub(crate) fn commit_watch(&self) -> watch::Receiver<Version> {
        self.commit_watch.subscribe()
    }

    /// Committer's main loop.
    /// Returns the final snapshot.
    pub(crate) fn run(
        mut self,
        tx: flume::Sender<CommitProtocol>,
        rx: flume::Receiver<CommitProtocol>,
    ) -> Snapshot {
        while let Ok(msg) = rx.recv() {
            self.stats.queue_peak = self.stats.queue_peak.max(rx.len());
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
        // A Stats read accrues nothing: two reads with no work between
        // them must compare equal.
        if matches!(msg, CommitProtocol::Stats { .. }) {
            return self.dispatch(msg, tx);
        }
        let started = std::time::Instant::now();
        let keep_running = self.dispatch(msg, tx);
        self.stats.busy_ns += started.elapsed().as_nanos() as u64;
        keep_running
    }

    fn dispatch(&mut self, msg: CommitProtocol, tx: &flume::Sender<CommitProtocol>) -> bool {
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
                    self.stats.reply_failure()
                });
            }
            CommitProtocol::Commit {
                changeset,
                releases_base,
                reply,
            } => {
                let started = std::time::Instant::now();
                let base = changeset.base_version();
                let result = self.commit(changeset);
                if releases_base {
                    self.release(base);
                }
                if self.live_versions.is_empty() {
                    self.evict_old_versions();
                }
                reply.send(result).unwrap_or_else(|e| {
                    error!("Failed to send commit result: {e}");
                    self.stats.reply_failure()
                });
                self.stats.commit_service_ns += started.elapsed().as_nanos() as u64;
            }
            CommitProtocol::Drop(version) => self.release(version),
            CommitProtocol::GcPass { roots, reply } => {
                // Held snapshots across versions; `0` = quiescent.
                let live: usize = self.live_versions.values().map(|&c| c as usize).sum();
                if live != 0 {
                    let _ = reply.send(Err(GcRefused { live }));
                    return true;
                }
                let reclaimed = self.mark_world(&roots);
                let _ = reply.send(Ok(GcReport { reclaimed }));
            }
            CommitProtocol::Stats { reply } => {
                self.stats.live_snapshots = self.live_versions.values().map(|&c| c as usize).sum();
                self.stats.versions_retained = self.write_history.len();
                reply.send(self.stats).unwrap_or_else(|e| {
                    error!("Failed to send stats: {e}");
                    self.stats.reply_failure()
                });
            }
            CommitProtocol::Close => {
                return false;
            }
        }
        true
    }

    /// Mark the world from `roots` and drop every unreachable `Ref`, `Array`
    /// and `Mapping` var; object and connection identities are never dropped.
    /// Returns the number of vars dropped.
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
                        for cell in &fun.upvalue_ptrs {
                            if marked.insert(*cell) {
                                work.push(MarkWork::Var(*cell));
                            }
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
                    && matches!(
                        world_value,
                        WorldValue::Ref(_) | WorldValue::Array(_) | WorldValue::Mapping(_)
                    )
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
            WorldValue::Process(process) => process
                .world_var_ids()
                .into_iter()
                .map(MarkWork::Var)
                .collect(),
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

    /// Release one live pin on `version`; a version with no pin is a no-op.
    /// History is evicted only when the watermark actually moves.
    fn release(&mut self, version: Version) {
        let was_min = self
            .live_versions
            .first_key_value()
            .is_some_and(|(k, _)| *k == version);
        let Some(count) = self.live_versions.get_mut(&version) else {
            return;
        };
        *count -= 1;
        if *count == 0 {
            self.live_versions.remove(&version);
            if was_min {
                self.evict_old_versions();
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
        let before = self.write_history.len();
        self.write_history.retain(|v, _| *v >= watermark);
        self.stats.evictions += before - self.write_history.len();
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
    /// Returns `Ok` on success, or `Err(Conflict)` if the changeset conflicts.
    pub(crate) fn commit(&mut self, changeset: Changeset) -> Result<(), Conflict> {
        let current_version = self.snapshot.version();
        let changeset_version = changeset.base_version();

        // Should not occur in practice - it implies versions are being created in more than one place.
        if current_version < changeset_version {
            self.stats.conflict();
            return Err(Conflict);
        }

        // changeset's base evicted → not enough history to resolve the conflict rule
        if changeset_version < self.oldest_retained {
            self.stats.conflict();
            return Err(Conflict);
        }

        // check the conflict rule
        for (version, written_vars) in self
            .write_history
            .range(changeset_version..=current_version)
        {
            if *version == changeset_version {
                continue;
            }

            self.stats.validation_scanned_versions += 1;
            if changeset.conflicts_with(written_vars) {
                self.stats.conflict();
                return Err(Conflict);
            }
        }

        // A merge applies to a value the attempt never observed, so its
        // type is checked here; a mismatch rejects the whole changeset
        // before anything applies.
        for (var_id, ops) in changeset.merges() {
            let mut current = self.snapshot.peek(*var_id).cloned();
            for op in ops {
                match op.apply_to(current.as_ref()) {
                    Ok(value) => current = Some(value),
                    Err(_) => {
                        self.stats.conflict();
                        return Err(Conflict);
                    }
                }
            }
        }

        let written_vars = changeset.touched_vars();
        if written_vars.is_empty() {
            // Conflict-free read-only changeset: no new version, no history.
            self.stats.read_only_commit();
            return Ok(());
        }

        let new_version = Version::new();
        let new_snapshot = self.snapshot.apply(new_version, changeset);
        self.snapshot = new_snapshot;

        // Keep history insert after the snapshot apply, else a problem in apply leads to
        // all transactions conflicting in the future.
        self.write_history.insert(new_version, written_vars);
        self.stats.commit();
        self.commit_watch.send_replace(new_version);

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

    /// The committed value of a var (`NULL` when absent), read in-thread.
    #[cfg(test)]
    pub(crate) fn committed(&self, var_id: VarId) -> WorldValue {
        self.snapshot.read(var_id).unwrap_or_else(WorldValue::null)
    }

    /// A clone of the current world snapshot (sibling test modules can't
    /// reach the field directly).
    pub(crate) fn snapshot_clone(&self) -> Snapshot {
        self.snapshot.clone()
    }

    /// Run the committer loop, rejecting the first `n` `Commit`s it
    /// receives. The rejection is a synthetic abort with the same
    /// `Err(Conflict)` reply the conflict rule would send.
    pub(crate) fn run_with_rejections(
        mut self,
        tx: flume::Sender<CommitProtocol>,
        rx: flume::Receiver<CommitProtocol>,
        rejections: usize,
    ) -> Snapshot {
        let mut rejections_left = rejections;
        while let Ok(msg) = rx.recv() {
            if rejections_left > 0
                && let CommitProtocol::Commit {
                    changeset,
                    releases_base,
                    reply,
                } = msg
            {
                rejections_left -= 1;
                // A rejection releases the pin exactly as the real path does.
                if releases_base {
                    self.release(changeset.base_version());
                }
                let _ = reply.send(Err(Conflict));
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
    use thin_vec::thin_vec;
    use tokio::{sync::Barrier, task::JoinSet};
    use ustr::ustr;

    use super::*;
    use crate::interpreter::{
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        stm::{SVar, Transaction, WorldValue, tests::*},
    };

    #[test]
    fn a_second_store_mutates_the_attempts_own_copy_and_the_world_only_at_commit() {
        let mut committer = Committer::new();
        let mut txn = Transaction::new(committer.snapshot_clone());

        let mut seeded = LpcArray::default();
        seeded.array.push(LpcRef::from(1_i64));
        seeded.array.push(LpcRef::from(2_i64));
        let cell = txn.mint_array(seeded);
        txn.with_array_cow(cell.id, |a| {
            a[0] = LpcRef::from(10_i64);
            Ok(())
        })
        .expect("first store");
        txn.with_array_cow(cell.id, |a| {
            a[1] = LpcRef::from(20_i64);
            Ok(())
        })
        .expect("second store");

        // Both edits landed in the attempt's copy...
        let owned = txn.read_array(cell.id).expect("attempt sees its array");
        assert_eq!(
            &owned.array[..],
            [LpcRef::from(10_i64), LpcRef::from(20_i64)]
        );
        // ...and the world has no such var until the commit applies it.
        assert_eq!(committer.snapshot_clone().read(cell.id), None);

        let (_, changeset) = txn.into_parts();
        committer.commit(changeset).expect("commit succeeds");
        let committed = committer
            .snapshot_clone()
            .read(cell.id)
            .expect("cell committed");
        let WorldValue::Array(arr) = committed else {
            panic!("expected an array world value");
        };
        assert_eq!(&arr.array[..], [LpcRef::from(10_i64), LpcRef::from(20_i64)]);
    }

    #[test]
    fn a_read_of_the_attempts_own_blind_write_does_not_conflict() {
        let mut committer = Committer::new();
        let x = VarId::new();
        let mut seed = Changeset::new(committer.current_version());
        seed.write(x, WorldValue::ref_of(LpcRef::from(1)));
        committer.commit(seed).expect("seed should commit");

        // A bases on the current world, then B commits a write to x.
        let mut a = Transaction::new(committer.snapshot_clone());
        let mut b = Transaction::new(committer.snapshot_clone());
        b.write(x, LpcRef::from(2));
        let (_, b_changeset) = b.into_parts();
        committer.commit(b_changeset).expect("b should commit");

        // A blind-writes x and reads its own write back.
        a.write(x, LpcRef::from(5));
        assert_eq!(a.read(x), Some(LpcRef::from(5)));

        // The read observed no committed state, so B's write is no conflict.
        let (_, a_changeset) = a.into_parts();
        assert!(
            committer.commit(a_changeset).is_ok(),
            "an own-write read must not enter the conflict rule"
        );
    }

    #[test]
    fn a_commit_releases_its_base_version_without_a_drop_message() {
        let (tx, rx) = flume::unbounded();
        let committer = Committer::new();
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        // Start pins the base version.
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: reply_tx })
            .expect("committer channel closed");
        let mut live = reply_rx.recv().expect("no reply from committer");

        // Disarmed: dropping this handle sends no release.
        live.disarm();
        let base = live.version();
        let var = VarId::new();
        drop(live);

        let mut changeset = Changeset::new(base);
        changeset.write(var, WorldValue::ref_of(LpcRef::from(1)));
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Commit {
            changeset,
            releases_base: true,
            reply: reply_tx,
        })
        .expect("committer channel closed");
        reply_rx
            .recv()
            .expect("no reply from committer")
            .expect("commit should succeed");

        // The commit released the pin: a GC pass finds nothing in flight.
        let (reply_tx, reply_rx) = flume::bounded(1);
        tx.send(CommitProtocol::GcPass {
            roots: vec![WorldRoot::Var(var)],
            reply: reply_tx,
        })
        .expect("committer channel closed");
        let reply = reply_rx.recv().expect("no reply from committer");
        assert!(reply.is_ok(), "gc must not be refused: {reply:?}");

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        handle.join().expect("committer panicked");
    }

    #[test]
    fn a_read_only_commit_is_counted_apart_from_writing_commits() {
        let mut committer = Committer::new();
        let x = VarId::new();

        let mut seeder = Transaction::new(committer.snapshot_clone());
        seeder.write(x, LpcRef::from(1));
        let (_, changeset) = seeder.into_parts();
        committer.commit(changeset).expect("seed commit");

        let mut reader = Transaction::new(committer.snapshot_clone());
        assert_eq!(reader.read(x), Some(LpcRef::from(1)));
        let (_, changeset) = reader.into_parts();
        committer.commit(changeset).expect("read-only commit");

        assert_eq!(committer.stats.commits, 1);
        assert_eq!(committer.stats.read_only_commits, 1);
    }

    #[test]
    fn validation_scans_sum_the_history_the_conflict_rule_walks() {
        let mut committer = Committer::new();
        let (x, y, z) = (VarId::new(), VarId::new(), VarId::new());

        let mut a = Transaction::new(committer.snapshot_clone());
        let mut b = Transaction::new(committer.snapshot_clone());
        let mut c = Transaction::new(committer.snapshot_clone());

        a.write(x, LpcRef::from(1));
        let (_, changeset) = a.into_parts();
        committer.commit(changeset).expect("first commit");
        assert_eq!(committer.stats.validation_scanned_versions, 0);

        b.write(y, LpcRef::from(2));
        let (_, changeset) = b.into_parts();
        committer.commit(changeset).expect("second commit");
        assert_eq!(committer.stats.validation_scanned_versions, 1);

        c.write(z, LpcRef::from(3));
        let (_, changeset) = c.into_parts();
        committer.commit(changeset).expect("third commit");
        assert_eq!(committer.stats.validation_scanned_versions, 3);
    }

    #[test]
    fn service_time_accumulates_through_process() {
        let (tx, _rx) = flume::unbounded();
        let mut committer = Committer::new();

        let mut t = Transaction::new(committer.snapshot_clone());
        t.write(VarId::new(), LpcRef::from(1));
        let (_, changeset) = t.into_parts();
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::Commit {
                changeset,
                releases_base: false,
                reply: reply_tx,
            },
            &tx,
        );
        reply_rx.recv().expect("no reply").expect("commit");

        assert!(committer.stats.busy_ns > 0);
        assert!(committer.stats.commit_service_ns > 0);
        assert!(committer.stats.busy_ns >= committer.stats.commit_service_ns);
    }

    #[test]
    fn evictions_count_the_history_trimmed_at_the_watermark() {
        let (tx, _rx) = flume::unbounded();
        let mut committer = Committer::new();

        for i in 0..2_i64 {
            let mut t = Transaction::new(committer.snapshot_clone());
            t.write(VarId::new(), LpcRef::from(i));
            let (_, changeset) = t.into_parts();
            let (reply_tx, reply_rx) = flume::bounded(1);
            committer.process(
                CommitProtocol::Commit {
                    changeset,
                    releases_base: false,
                    reply: reply_tx,
                },
                &tx,
            );
            reply_rx.recv().expect("no reply").expect("commit");
        }

        // The first pass finds only the current version; the second trims the first's.
        assert_eq!(committer.stats.evictions, 1);
    }

    #[test]
    fn the_run_loop_records_queue_peak_and_gauges() {
        let (tx, rx) = flume::unbounded();
        let committer = Committer::new();

        // Three messages queued before the loop starts: after Start is pulled, two remain.
        let (start_tx, start_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Start { reply: start_tx }).unwrap();
        let (stats_tx, stats_rx) = flume::bounded(1);
        tx.send(CommitProtocol::Stats { reply: stats_tx }).unwrap();
        tx.send(CommitProtocol::Close).unwrap();

        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || committer.run(committer_tx, rx));

        let live = start_rx.recv().expect("no start reply");
        let stats = stats_rx.recv().expect("no stats reply");
        assert_eq!(stats.queue_peak, 2);
        assert_eq!(stats.live_snapshots, 1);
        assert_eq!(stats.versions_retained, 0);

        drop(live);
        drop(tx);
        handle.join().expect("committer panicked");
    }

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
        t.write(counter, LpcRef::from(n.wrapping_add(1)));
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
                    releases_base: false,
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
                    releases_base: false,
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
                            releases_base: false,
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
            releases_base: false,
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
        assert_eq!(
            committer.committed(var),
            WorldValue::ref_of(LpcRef::from(0))
        );

        // A committed value is visible
        let mut seed = Changeset::new(committer.snapshot.version());
        seed.write(var, WorldValue::ref_of(LpcRef::from(7)));
        committer.commit(seed).unwrap();
        assert_eq!(
            committer.committed(var),
            WorldValue::ref_of(LpcRef::from(7))
        );

        // A pass that cannot reach `var` removes it from the world.
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                roots: Vec::new(),
                reply: reply_tx,
            },
            &tx,
        );
        assert!(reply_rx.recv().unwrap().is_ok());
        assert_eq!(
            committer.committed(var),
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
                roots: Vec::new(),
                reply: reply_tx,
            },
            &tx,
        );
        assert!(reply_rx.recv().unwrap().is_err());

        // Refused: the var survives the pass untouched.
        drop(live);
        pump(&mut committer, &tx, &drop_rx);
        assert_eq!(
            committer.committed(var),
            WorldValue::ref_of(LpcRef::from(7))
        );
    }

    #[test]
    fn gc_pass_reclaims_every_unreached_var() {
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

        // Root only the live payload: the dead payload and the cell go together.
        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                roots: vec![WorldRoot::Var(live_payload.id)],
                reply: reply_tx,
            },
            &tx,
        );
        let report = reply_rx.recv().unwrap().unwrap();
        assert_eq!(report.reclaimed, 2);

        let is_null = |committer: &mut Committer, var: VarId| -> bool {
            committer.committed(var) == WorldValue::null()
        };
        // The live payload survives; the dead payload and the cell read back NULL.
        assert!(!is_null(&mut committer, live_payload.id));
        assert!(is_null(&mut committer, dead_payload.id));
        assert!(is_null(&mut committer, cell));
    }

    #[test]
    fn gc_pass_keeps_the_cells_of_a_reachable_closure() {
        let (tx, _rx) = flume::unbounded();
        let mut committer = Committer::new();

        let cell = VarId::new();
        let holder = VarId::new();
        let ptr = FunctionPtrBuilder::default()
            .address(FunctionAddress::Efun(ustr("dump")))
            .upvalue_ptrs(thin_vec![cell])
            .build()
            .unwrap();
        let mut seed = Changeset::new(committer.snapshot.version());
        seed.write(cell, WorldValue::ref_of(LpcRef::from(9)));
        seed.write(holder, WorldValue::ref_of(LpcRef::Function(Arc::new(ptr))));
        committer.commit(seed).unwrap();

        let (reply_tx, reply_rx) = flume::bounded(1);
        committer.process(
            CommitProtocol::GcPass {
                roots: vec![WorldRoot::Var(holder)],
                reply: reply_tx,
            },
            &tx,
        );
        let report = reply_rx.recv().unwrap().unwrap();
        assert_eq!(report.reclaimed, 0);
        assert_eq!(
            committer.committed(cell),
            WorldValue::ref_of(LpcRef::from(9))
        );
    }
}
