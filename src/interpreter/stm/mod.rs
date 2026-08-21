//! Software transactional memory implementation

use std::{
    marker::PhantomData,
    sync::{Arc, atomic::AtomicU64},
};

use parking_lot::RwLock;

use crate::{
    interpreter::{
        lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, process::Process,
    },
    telnet::connection::Connection,
};

mod changeset;
mod committer;
mod effects;
mod retry;
mod snapshot;
mod world_value;

pub(crate) use changeset::Changeset;
pub(crate) use committer::{CommitProtocol, Committer, LiveSnapshot};
pub(crate) use effects::{CallOutSchedule, Effect, EffectLog, flush_effects};
pub use retry::CommittedReader;
pub(crate) use retry::{RetryStats, commit_changeset, drop_var, start_txn};
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
    effects: EffectLog,
    /// Call outs recorded by this attempt but not yet materialized. A
    /// separate list (not an `Effect`) because materialization needs the
    /// physical `CallOuts` queue at flush time, and cancellation must see
    /// both a recorded-but-unmaterialized call out and one that already
    /// fired-into the queue. Dropped with the attempt on conflict: an
    /// aborted attempt schedules nothing.
    pending_call_outs: Vec<CallOutSchedule>,
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
            effects: EffectLog::new(),
            pending_call_outs: Vec::new(),
            joinable: true,
        }
    }

    /// Build a transaction from its parts.
    fn from_parts(snapshot: Snapshot, joinable: bool) -> Self {
        let version = snapshot.version();
        Self {
            snapshot,
            changeset: Changeset::new(version),
            effects: EffectLog::new(),
            pending_call_outs: Vec::new(),
            joinable,
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
    /// through to the snapshot would resurrect a removed var.
    pub(crate) fn read_value(&mut self, var_id: VarId) -> Option<WorldValue> {
        self.changeset.track_read(var_id);

        if self.changeset.is_removed(var_id) {
            return None;
        }
        self.changeset
            .read(var_id)
            .or_else(|| self.snapshot.read(var_id))
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
        match self.read_value(var_id)? {
            WorldValue::Array(a) => Some(a),
            _ => None,
        }
    }

    /// The committed mapping contents for a cell var, or `None` if the var is
    /// absent from both the changeset and the world.
    pub(crate) fn read_mapping(&mut self, var_id: VarId) -> Option<Arc<LpcMapping>> {
        match self.read_value(var_id)? {
            WorldValue::Mapping(m) => Some(m),
            _ => None,
        }
    }

    /// The committed object for a cell var, or `None` if the var is absent
    /// from both the changeset and the world, or the cell holds a non-object
    /// (e.g. a slot that was written a plain ref). Reading the cell here is
    /// what a transactional `find_object` uses, and it is the read that makes
    /// a concurrent create of this cell conflict and re-run.
    pub(crate) fn read_object(&mut self, var_id: VarId) -> Option<Arc<Process>> {
        match self.read_value(var_id)? {
            WorldValue::Process(p) => Some(p),
            _ => None,
        }
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
        match self.read_value(var_id)? {
            WorldValue::Connection(Some(connection)) => Some(connection),
            WorldValue::Connection(None) => None,
            _ => None,
        }
    }

    /// Copy-on-write the array cell `var_id`: read its contents, clone into a
    /// new `Arc`, mutate the clone, and write it back under the same var.
    /// One read tracked in the changeset (conflict-checked) plus one blind
    /// write; the contents are never mutated in place in the committed world.
    pub(crate) fn with_array_cow(&mut self, var_id: VarId, f: impl FnOnce(&mut LpcArray)) {
        let current = self
            .read_value(var_id)
            .and_then(|v| match v {
                WorldValue::Array(a) => Some(a),
                _ => None,
            })
            .unwrap_or_else(|| Arc::new(LpcArray::default()));
        let mut clone = (*current).clone();
        f(&mut clone);
        self.changeset
            .write(var_id, WorldValue::Array(Arc::new(clone)));
    }

    /// Copy-on-write the mapping cell `var_id`, as in [`with_array_cow`].
    pub(crate) fn with_mapping_cow(&mut self, var_id: VarId, f: impl FnOnce(&mut LpcMapping)) {
        let current = self
            .read_value(var_id)
            .and_then(|v| match v {
                WorldValue::Mapping(m) => Some(m),
                _ => None,
            })
            .unwrap_or_else(|| Arc::new(LpcMapping::default()));
        let mut clone = (*current).clone();
        f(&mut clone);
        self.changeset
            .write(var_id, WorldValue::Mapping(Arc::new(clone)));
    }

    /// The values this attempt has written (GC roots until commit).
    pub(crate) fn written_values(&self) -> impl Iterator<Item = &WorldValue> {
        self.changeset.written_values()
    }

    /// Record a physical side effect for delivery after this attempt commits.
    pub(crate) fn record_effect(&mut self, effect: Effect) {
        self.effects.record(effect);
    }

    /// Record a call out for materialization after this attempt commits. The
    /// physical timer task and queue entry are created only at flush, so an
    /// aborted attempt schedules nothing.
    pub(crate) fn record_call_out(&mut self, schedule: CallOutSchedule) {
        self.pending_call_outs.push(schedule);
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

    /// Take out the attempt's recorded side effects for delivery. The
    /// pending call outs are folded in as scheduling effects first. Called
    /// by the retry loop after a successful commit; a rejected attempt's log
    /// is dropped with the attempt instead.
    pub(crate) fn take_effects(&mut self) -> Vec<Effect> {
        for schedule in self.pending_call_outs.drain(..) {
            self.effects.record(Effect::ScheduleCallOut(schedule));
        }
        self.effects.take()
    }

    /// Dismantle the transaction into its snapshot and changeset for retries, etc.
    pub(crate) fn into_parts(self) -> (Snapshot, Changeset) {
        (self.snapshot, self.changeset)
    }
}

/// One top-level task = one transaction. Nested sub-tasks join it by
/// cloning this handle.
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
        let snapshot = Snapshot::new(Version::new(), imbl::OrdMap::new());
        Self::new(Transaction::from_parts(snapshot, false))
    }

    /// Run `f` over the transaction, holding the lock.
    pub(crate) fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Transaction) -> R,
    {
        let mut guard = self.0.write();
        f(&mut guard)
    }

    /// Read the object in a cell var (changeset first, then the committed
    /// world), or `None` if the cell is absent or holds a non-object. A
    /// joiner's handle is the parent's, so this reads the parent's attempt.
    pub(crate) fn read_object(&self, var_id: VarId) -> Option<Arc<Process>> {
        self.with(|t| t.read_object(var_id))
    }

    /// Read the connection in a cell var, as in
    /// [`Transaction::read_connection`](crate::interpreter::stm::Transaction).
    /// A joiner's handle is the parent's, so this reads the parent's attempt,
    /// which is what lets an `exec`'s effect see the binding its transaction
    /// just wrote.
    pub(crate) fn read_connection(&self, var_id: VarId) -> Option<Arc<Connection>> {
        self.with(|t| t.read_connection(var_id))
    }

    /// Write the connection cell, as in
    /// [`Transaction::write_connection`](crate::interpreter::stm::Transaction).
    /// A joiner's handle is the parent's, so the write folds into the
    /// parent's attempt and rides the parent's single commit.
    pub(crate) fn write_connection(&self, var_id: VarId, connection: Option<Arc<Connection>>) {
        self.with(|t| t.write_connection(var_id, connection))
    }

    /// Whether this attempt removes the var. A removed object cell must not
    /// be read back from the committed world or from the physical object map
    /// (the deferred insert/remove effects haven't landed yet): the removal
    /// is authoritative for this attempt until a re-write cancels it.
    pub(crate) fn is_removed(&self, var_id: VarId) -> bool {
        self.with(|t| t.changeset.is_removed(var_id))
    }

    /// Record a physical side effect on this attempt. A joiner's handle is
    /// the parent's, so its output folds into the parent's log and rides the
    /// parent's single commit.
    pub(crate) fn record_effect(&self, effect: Effect) {
        self.with(|t| t.record_effect(effect));
    }

    /// Record a call out for materialization after this attempt commits.
    /// A joiner's handle is the parent's, so it folds into the parent's
    /// pending list and rides the parent's single flush.
    pub(crate) fn record_call_out(&self, schedule: CallOutSchedule) {
        self.with(|t| t.record_call_out(schedule));
    }

    /// Cancel a call out this attempt recorded, as in
    /// [`Transaction::cancel_pending_call_out`](crate::interpreter::stm::Transaction).
    pub(crate) fn cancel_pending_call_out(&self, id: u64) -> Option<i64> {
        self.with(|t| t.cancel_pending_call_out(id))
    }

    /// Take out the attempt's recorded side effects for delivery after commit.
    pub(crate) fn take_effects(&self) -> Vec<Effect> {
        self.with(|t| t.take_effects())
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
