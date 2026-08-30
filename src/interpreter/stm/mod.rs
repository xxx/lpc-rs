//! Software transactional memory implementation

use std::{
    collections::HashSet,
    marker::PhantomData,
    sync::{Arc, atomic::AtomicU64},
};

use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::Result;
use parking_lot::RwLock;

use crate::{
    command::registry::RuleList,
    interpreter::{
        lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, object_space::ObjectSpace,
        process::Process, task_context::ObjectLookup, vm::global_state::GlobalState,
    },
    telnet::connection::Connection,
    util::process_builder::compile_process_from_path,
};

mod backoff;
mod changeset;
mod committer;
mod effects;
mod merge;
mod retry;
mod snapshot;
mod world_value;

pub(crate) use changeset::Changeset;
/// Public API surface re-exports (read-only, for benches/tooling/tests).
pub use committer::CommitterStats;
pub(crate) use committer::{
    CommitProtocol, Committer, Conflict, GcPassReply, LiveSnapshot, WorldRoot,
};
pub use committer::{GcRefused, GcReport};
pub(crate) use effects::{CallOutSchedule, Effect, flush_effects};
pub(crate) use merge::MergeOp;
pub use retry::CommittedReader;
#[cfg(test)]
pub(crate) use retry::RetryStats;
pub(crate) use retry::{
    AttemptBody, commit_changeset, committer_stats, gc_pass, run_attempts, start_txn,
};
pub use retry::{AttemptTelemetry, AttemptTelemetrySnapshot};
pub(crate) use snapshot::Snapshot;
pub(crate) use world_value::WorldValue;

static VAR_ID_COUNT: AtomicU64 = AtomicU64::new(0);
// Stable ID for transactional cells
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(u64);

impl VarId {
    pub(crate) fn new() -> Self {
        Self(VAR_ID_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    pub(crate) const fn as_u64(self) -> u64 {
        self.0
    }
}

// _Global_ version counter because this is single-committer.
static VERSION_COUNT: AtomicU64 = AtomicU64::new(0);
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Version(u64);

impl Version {
    pub(crate) fn new() -> Self {
        Self(VERSION_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Transaction {
    snapshot: Snapshot,
    changeset: Changeset,
    /// Physical output recorded by this attempt, delivered once after a
    /// successful commit. Dropped with the attempt on conflict, so a
    /// re-run re-records it; never resolved from a cell at flush time.
    effects: Vec<Effect>,
    /// Call outs recorded by this attempt but not yet materialized. A
    /// separate list (not an `Effect`) because materialization needs the
    /// physical `CallOuts` queue at flush time, and cancellation must see
    /// both a recorded-but-unmaterialized call out and one that already
    /// fired-into the queue. Dropped with the attempt on conflict: an
    /// aborted attempt schedules nothing.
    pending_call_outs: Vec<CallOutSchedule>,
    /// Committed call outs this attempt canceled. The queries subtract this
    /// shadow; same lifetime as `pending_call_outs`.
    cancelled_call_outs: HashSet<u64>,
    /// True when this transaction was opened by the committer (a live
    /// attempt) and may be joined by a nested sub-task; false for the fresh
    /// empty one minted for top-level contexts, whose holder must open its
    /// own attempt.
    joinable: bool,
}

impl Transaction {
    pub(crate) fn new(snapshot: Snapshot) -> Self {
        let version = snapshot.version();
        Self {
            snapshot,
            changeset: Changeset::new(version),
            effects: Vec::new(),
            pending_call_outs: Vec::new(),
            cancelled_call_outs: HashSet::new(),
            joinable: true,
        }
    }

    /// Read a slot value (globals, upvalues) — the changeset first, so an
    /// attempt sees its own writes, then the committed world.
    pub(crate) fn read(&mut self, var_id: VarId) -> Option<LpcRef> {
        self.read_value(var_id).map(WorldValue::lpc_ref)
    }

    /// Read the world value of a var: `Ref` for slots, payload contents for
    /// payload vars. A var this attempt removed reads back as absent: the
    /// committed world still holds its old value until commit, so falling
    /// through to the snapshot would resurrect a removed var. A read
    /// satisfied by the attempt's own write or removal observes no committed
    /// state, so it is not tracked; world reads are tracked and memoized.
    pub(crate) fn read_value(&mut self, var_id: VarId) -> Option<WorldValue> {
        if self.changeset.is_removed(var_id) {
            return None;
        }
        if let Some(own) = self.changeset.read(var_id) {
            return Some(own);
        }
        let snapshot = &self.snapshot;
        let world = self
            .changeset
            .read_through(var_id, || snapshot.read(var_id));
        // A pending merge folds onto the world value. The read above is
        // tracked and guards the base changing, so the merge stays a merge.
        let mut value = world;
        for op in self.changeset.pending_merges(var_id) {
            value = Some(
                op.apply_to(value.as_ref())
                    .expect("the caller peeks the type before merging"),
            );
        }
        value
    }

    /// Write a slot value to the changeset.
    pub(crate) fn write(&mut self, var_id: VarId, value: LpcRef) {
        self.changeset.write(var_id, WorldValue::ref_of(value));
    }

    /// Write an object-space cell into the changeset. The `Process` is held
    /// strongly so the object stays alive between this write and the commit
    /// that makes it resolvable (the physical map is applied after commit).
    pub(crate) fn write_process(&mut self, var_id: VarId, process: Arc<Process>) {
        self.changeset.write(var_id, WorldValue::Process(process));
    }

    /// Record that this attempt removes a var from the world. For an object
    /// cell this is a transactional `destruct`: the cell reads back as absent
    /// to this attempt, and the committer removes it on commit (a concurrent
    /// reader of the cell conflicts and re-runs). A later `write` of the same
    /// var in this attempt cancels the removal.
    pub(crate) fn drop_var(&mut self, var_id: VarId) {
        self.changeset.drop_var(var_id);
    }

    /// Assert that `var_id` is unchanged since this attempt's base version:
    /// a later write to it rejects the commit.
    pub(crate) fn track_read(&mut self, var_id: VarId) {
        self.changeset.track_read(var_id);
    }

    /// Record a merge write: the committer applies `op` to the committed
    /// value at commit time. No read is tracked.
    pub(crate) fn merge(&mut self, var_id: VarId, op: MergeOp) {
        self.changeset.merge(var_id, op);
    }

    /// Whether the cell can take an int merge: it holds an int (or nothing)
    /// as seen by this attempt. The world probe is untracked — tracking it
    /// would re-buy the read the merge exists to avoid; a stale answer is
    /// caught by the committer's type check.
    pub(crate) fn peek_int(&self, var_id: VarId) -> bool {
        if self.changeset.is_removed(var_id) {
            return true; // absent: the op applies onto its identity
        }
        if let Some(own) = self.changeset.written(var_id) {
            return matches!(own, WorldValue::Ref(LpcRef::Int(_)));
        }
        if !self.changeset.pending_merges(var_id).is_empty() {
            return true; // an earlier peek already accepted the cell
        }
        !matches!(
            self.snapshot.peek(var_id),
            Some(value) if !matches!(value, WorldValue::Ref(LpcRef::Int(_)))
        )
    }

    /// Whether this attempt removes the var; a removed cell must not be read
    /// back from the world or the physical map until a re-write cancels it.
    pub(crate) fn is_removed(&self, var_id: VarId) -> bool {
        self.changeset.is_removed(var_id)
    }

    /// Mint a fresh array cell: a new `VarId` with its contents written into
    /// the changeset. A fresh id is never in the world, so the write can't
    /// conflict; the returned handle is the cell's identity.
    ///
    /// Once committed, the cell keeps its world entry even after the last
    /// `LpcRef` to it is dropped; reclaiming such `VarId`s is the quiescent
    /// pass's job (same class as the destructed-object retention).
    pub(crate) fn mint_array(&mut self, array: LpcArray) -> SVar<LpcArray> {
        let id = SVar::<LpcArray>::new();
        self.changeset
            .write(id.id, WorldValue::Array(Arc::new(array)));
        id
    }

    /// Mint a fresh mapping cell, as in [`mint_array`](Self::mint_array).
    pub(crate) fn mint_mapping(&mut self, mapping: LpcMapping) -> SVar<LpcMapping> {
        let id = SVar::<LpcMapping>::new();
        self.changeset
            .write(id.id, WorldValue::Mapping(Arc::new(mapping)));
        id
    }

    /// The committed array contents for a cell var, or `None` if the var is
    /// absent from both the changeset and the world.
    pub(crate) fn read_array(&mut self, var_id: VarId) -> Option<Arc<LpcArray>> {
        self.read_value(var_id)?.into_array()
    }

    /// The committed mapping contents for a cell var, or `None` if the var is
    /// absent from both the changeset and the world.
    pub(crate) fn read_mapping(&mut self, var_id: VarId) -> Option<Arc<LpcMapping>> {
        self.read_value(var_id)?.into_mapping()
    }

    /// The committed object for a cell var, or `None` if the var is absent
    /// from both the changeset and the world, or the cell holds a non-object
    /// (e.g. a slot that was written a plain ref). Reading the cell here is
    /// what a transactional `find_object` uses, and it is the read that makes
    /// a concurrent create of this cell conflict and re-run.
    pub(crate) fn read_object(&mut self, var_id: VarId) -> Option<Arc<Process>> {
        self.read_value(var_id)?.into_process()
    }

    /// Write the connection-binding cell: the `Connection` attached to a
    /// `Process`, or `None` to clear it. The `Connection` is held strongly so
    /// it stays alive between this write and the commit that makes it
    /// visible, as in [`write_process`](Self::write_process).
    pub(crate) fn write_connection(&mut self, var_id: VarId, connection: Option<Arc<Connection>>) {
        self.changeset
            .write(var_id, WorldValue::Connection(connection));
    }

    /// The committed connection in a cell var, or `None` if the var is absent
    /// from both the changeset and the world, or the cell holds a non-connection
    /// value. An in-transaction `write_connection` is visible to this attempt
    /// (changeset first), which is what lets `interactive()` and the like see
    /// an `exec` that has not yet committed.
    pub(crate) fn read_connection(&mut self, var_id: VarId) -> Option<Arc<Connection>> {
        self.read_value(var_id)?.into_connection()
    }

    /// The rule list in a cell var, empty when absent; a tracked read.
    pub(crate) fn read_rules(&mut self, var_id: VarId) -> RuleList {
        self.read_value(var_id)
            .and_then(WorldValue::into_rules)
            .unwrap_or_else(|| Arc::from(Vec::new()))
    }

    /// Copy-on-write the array cell `var_id`. The first write in an attempt
    /// clones the world's contents once into the changeset (that world read
    /// is tracked; an absent cell starts empty); later writes mutate the
    /// attempt's own copy in place (`Arc::make_mut` re-clones only if a
    /// read handed the `Arc` out). The committed world is never mutated in
    /// place. A failing closure leaves no new write, but on the own-copy
    /// path it may already have mutated — callers check before mutating.
    pub(crate) fn with_array_cow(
        &mut self,
        var_id: VarId,
        f: impl FnOnce(&mut LpcArray) -> Result<()>,
    ) -> Result<()> {
        if !self.changeset.is_removed(var_id)
            && let Some(own) = self.changeset.written_array_mut(var_id)
        {
            return f(Arc::make_mut(own));
        }
        let current = self
            .read_value(var_id)
            .and_then(WorldValue::into_array)
            .unwrap_or_else(|| Arc::new(LpcArray::default()));
        let mut clone = (*current).clone();
        f(&mut clone)?;
        self.changeset
            .write(var_id, WorldValue::Array(Arc::new(clone)));
        Ok(())
    }

    /// Copy-on-write the mapping cell `var_id`, as in [`with_array_cow`].
    pub(crate) fn with_mapping_cow(
        &mut self,
        var_id: VarId,
        f: impl FnOnce(&mut LpcMapping) -> Result<()>,
    ) -> Result<()> {
        if !self.changeset.is_removed(var_id)
            && let Some(own) = self.changeset.written_mapping_mut(var_id)
        {
            return f(Arc::make_mut(own));
        }
        let current = self
            .read_value(var_id)
            .and_then(WorldValue::into_mapping)
            .unwrap_or_else(|| Arc::new(LpcMapping::default()));
        let mut clone = (*current).clone();
        f(&mut clone)?;
        self.changeset
            .write(var_id, WorldValue::Mapping(Arc::new(clone)));
        Ok(())
    }

    /// Record a physical side effect for delivery after this attempt commits.
    pub(crate) fn record_effect(&mut self, effect: Effect) {
        self.effects.push(effect);
    }

    /// Record a call out for materialization after this attempt commits. The
    /// physical timer task and queue entry are created only at flush, so an
    /// aborted attempt schedules nothing.
    pub(crate) fn record_call_out(&mut self, schedule: CallOutSchedule) {
        self.pending_call_outs.push(schedule);
    }

    /// The call outs this attempt has recorded (for the transactional query
    /// view).
    pub(crate) fn pending_call_outs(&self) -> &[CallOutSchedule] {
        &self.pending_call_outs
    }

    /// Cancel a call out this attempt recorded, if any. Returns the
    /// milliseconds it had left (its full delay: it has not run yet). A
    /// `None` means the ID is not one of this attempt's pending call outs;
    /// the caller then looks for it among the committed ones.
    pub(crate) fn cancel_pending_call_out(&mut self, id: u64) -> Option<i64> {
        let pos = self
            .pending_call_outs
            .iter()
            .position(|schedule| schedule.id == id)?;
        let schedule = self.pending_call_outs.remove(pos);
        Some(schedule.delay.num_milliseconds())
    }

    /// Cancel a committed call out: record the deferred physical removal
    /// and add the ID to the shadow.
    pub(crate) fn cancel_committed_call_out(&mut self, id: u64) {
        self.cancelled_call_outs.insert(id);
        self.record_effect(Effect::CancelCallOut { id });
    }

    /// Whether this attempt canceled the committed call out with `id`.
    pub(crate) fn is_cancelled_call_out(&self, id: u64) -> bool {
        self.cancelled_call_outs.contains(&id)
    }

    /// Take out the attempt's recorded side effects for delivery. The
    /// pending call outs are folded in as scheduling effects first. Called
    /// by the retry loop after a successful commit; a rejected attempt's log
    /// is dropped with the attempt instead.
    pub(crate) fn take_effects(&mut self) -> Vec<Effect> {
        for schedule in self.pending_call_outs.drain(..) {
            self.effects.push(Effect::ScheduleCallOut(schedule));
        }
        std::mem::take(&mut self.effects)
    }

    /// Take the changeset out for commit; an empty one at the same version
    /// replaces it.
    pub(crate) fn take_changeset(&mut self) -> Changeset {
        std::mem::replace(&mut self.changeset, Changeset::new(self.snapshot.version()))
    }

    /// Clone the changeset for commit, leaving it in place for reads that
    /// follow the commit through this handle.
    pub(crate) fn clone_changeset(&self) -> Changeset {
        self.changeset.clone()
    }

    /// Dismantle the transaction into its snapshot and changeset.
    #[cfg(test)]
    pub(crate) fn into_parts(self) -> (Snapshot, Changeset) {
        (self.snapshot, self.changeset)
    }
}

/// One top-level task = one transaction. Nested sub-tasks join it by
/// cloning this handle, so a joiner's reads, writes, effects and call outs
/// are the parent's attempt's and ride the parent's single commit.
#[derive(Debug, Clone)]
pub(crate) struct TxnHandle(Arc<RwLock<Transaction>>);

impl TxnHandle {
    pub(crate) fn new(txn: Transaction) -> Self {
        Self(Arc::new(RwLock::new(txn)))
    }

    /// Empty, uncommitted transaction (top-level defaults, fresh
    /// contexts). Not joinable: its holder is top-level and must open its
    /// own attempt.
    pub(crate) fn empty() -> Self {
        let mut txn = Transaction::new(Snapshot::new(Version::new(), imbl::HashMap::new()));
        txn.joinable = false;
        Self::new(txn)
    }

    /// Run `f` over the transaction, holding the lock.
    pub(crate) fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Transaction) -> R,
    {
        let mut guard = self.0.write();
        f(&mut guard)
    }

    /// Whether this handle wraps a live attempt that can be joined by a nested task.
    pub(crate) fn joinable(&self) -> bool {
        self.0.read().joinable
    }
}

impl Default for TxnHandle {
    fn default() -> Self {
        Self::empty()
    }
}

/// Find the object at `path` in the committed world seen by `txn`. An
/// object with no cell (bootstrap, created outside any transaction) is found
/// in the physical map; once a cell exists, only the cell counts, so a
/// committed-but-unflushed destruct reads as a miss.
pub(crate) fn txn_find_object(
    txn: &TxnHandle,
    object_space: &ObjectSpace,
    path: &LpcPath,
) -> ObjectLookup {
    let key = object_space.path_key(path.as_ref());
    match object_space.get_cell_id(&key) {
        Some(var_id) if txn.with(|t| t.is_removed(var_id)) => ObjectLookup::Removed,
        Some(var_id) => match txn.with(|t| t.read_object(var_id)) {
            Some(process) => ObjectLookup::Found(process),
            None => ObjectLookup::NotCreated,
        },
        None => match object_space.lookup(&key) {
            Some(process) => ObjectLookup::Found(process),
            None => ObjectLookup::NotCreated,
        },
    }
}

/// Insert `process` into the world seen by `txn`: the cell write makes it
/// usable within this transaction, the effect places it in the physical map
/// at commit, and the read-track means two attempts creating one path cannot
/// both commit.
pub(crate) fn txn_insert_process(
    txn: &TxnHandle,
    object_space: &ObjectSpace,
    process: &Arc<Process>,
) {
    let key = object_space.process_key(process);
    let var_id = *process.cell.get_or_init(|| object_space.cell_id(&key));
    txn.with(|t| {
        t.track_read(var_id);
        t.write_process(var_id, process.clone());
        t.record_effect(Effect::InsertObject {
            key,
            process: process.clone(),
        })
    });
}

/// Resolve an object path in its own short-lived transaction: find, or on a
/// miss compile and insert, committed (with the physical insert flushed)
/// before this returns. A rejected commit means a concurrent writer to the
/// same cell committed first; the next round finds the winner.
pub(crate) async fn resolve_or_create_object(
    gs: &GlobalState,
    path: &LpcPath,
) -> Result<Arc<Process>> {
    let mut body = ResolveObjectBody {
        gs,
        path,
        txn: None,
        process: None,
    };
    let (res, _) = run_attempts(
        &gs.committer_tx,
        &gs.attempt_telemetry,
        Some(gs.commit_watch.clone()),
        &mut body,
    )
    .await;
    res?;
    Ok(body
        .process
        .expect("a committed or found attempt leaves the process"))
}

/// One attempt of [`resolve_or_create_object`]: a find hit has nothing to
/// commit; a miss compiles, inserts, and commits.
struct ResolveObjectBody<'a> {
    gs: &'a GlobalState,
    path: &'a LpcPath,
    txn: Option<TxnHandle>,
    process: Option<Arc<Process>>,
}

#[async_trait::async_trait]
impl AttemptBody for ResolveObjectBody<'_> {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let object_space = self.gs.object_space.as_ref();
        let live = start_txn(tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));

        if let ObjectLookup::Found(process) = txn_find_object(&txn, object_space, self.path) {
            self.process = Some(process);
            return Ok(None);
        }

        let process = compile_process_from_path(object_space, self.path).await?;
        txn_insert_process(&txn, object_space, &process);
        self.process = Some(process);
        self.txn = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self.txn.take().expect("attempt present until committed");
        let commit = commit_changeset(tx, txn.with(|t| t.take_changeset())).await?;
        Ok((commit, txn.with(|t| t.take_effects())))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        flush_effects(self.gs, effects).await;
        Ok(())
    }
}

/// Identity cell for one transactional slot (a global or an upvalue cell).
/// The slot owns only its [`VarId`]; the *committed value* lives only in
/// the committer's world.
#[derive(Debug, Clone, Copy)]
pub struct SVar<T> {
    /// The slot's stable identity in the committer's world.
    pub id: VarId,
    _phantom: PhantomData<T>,
}

impl<T> SVar<T> {
    /// Mint a fresh slot identity.
    pub(crate) fn new() -> Self {
        Self {
            id: VarId::new(),
            _phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests;
