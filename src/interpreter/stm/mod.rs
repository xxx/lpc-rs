//! Software transactional memory implementation

use std::{
    marker::PhantomData,
    sync::{Arc, atomic::AtomicU64},
};

use parking_lot::RwLock;

use crate::interpreter::lpc_ref::LpcRef;

mod changeset;
mod committer;
mod retry;
mod snapshot;
mod world_value;

pub(crate) use changeset::Changeset;
pub(crate) use committer::{CommitProtocol, Committer, LiveSnapshot};
pub use retry::CommittedReader;
pub(crate) use retry::{RetryStats, commit_changeset, drop_var, start_txn};
pub(crate) use snapshot::Snapshot;
pub(crate) use world_value::WorldValue;

static VAR_ID_COUNT: AtomicU64 = AtomicU64::new(0);
// Stable ID for transactional cells
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct VarId(u64);

impl VarId {
    pub(crate) fn new() -> Self {
        Self(VAR_ID_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
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
            joinable: true,
        }
    }

    /// Build a transaction from its parts.
    fn from_parts(snapshot: Snapshot, joinable: bool) -> Self {
        let version = snapshot.version();
        Self {
            snapshot,
            changeset: Changeset::new(version),
            joinable,
        }
    }

    /// Read a slot value (globals, upvalues) — the changeset first, so an
    /// attempt sees its own writes, then the committed world.
    pub(crate) fn read(&mut self, var_id: VarId) -> Option<LpcRef> {
        self.read_value(var_id).map(WorldValue::lpc_ref)
    }

    /// Read the world value of a var: `Ref` for slots, payload contents for
    /// payload vars.
    pub(crate) fn read_value(&mut self, var_id: VarId) -> Option<WorldValue> {
        self.changeset.track_read(var_id);

        self.changeset
            .read(var_id)
            .or_else(|| self.snapshot.read(var_id))
    }

    /// Write a slot value to the changeset.
    pub(crate) fn write(&mut self, var_id: VarId, value: LpcRef) {
        self.changeset.write(var_id, WorldValue::ref_of(value));
    }

    /// The values this attempt has written (GC roots until commit).
    pub(crate) fn written_values(&self) -> impl Iterator<Item = &WorldValue> {
        self.changeset.written_values()
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
pub(crate) struct SVar<T> {
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
