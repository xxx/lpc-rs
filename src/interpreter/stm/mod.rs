//! Software transactional memory implementation

use std::sync::atomic::AtomicU64;

use crate::interpreter::lpc_ref::LpcRef;

mod changeset;
mod committer;
mod retry;
mod snapshot;

pub(crate) use changeset::Changeset;
pub(crate) use committer::{CommitProtocol, Committer, LiveSnapshot};
pub(crate) use retry::{RetryStats, commit_changeset, retry_async, start_txn};
pub(crate) use snapshot::Snapshot;

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

#[derive(Debug)]
pub(crate) struct Transaction {
    snapshot: Snapshot,
    changeset: Changeset,
}

impl Transaction {
    pub(crate) fn new(snapshot: Snapshot) -> Self {
        let version = snapshot.version();
        Self {
            snapshot,
            changeset: Changeset::new(version),
        }
    }

    pub(crate) fn read(&mut self, var_id: VarId) -> Option<LpcRef> {
        self.changeset.track_read(var_id);

        self.changeset
            .read(var_id)
            .or_else(|| self.snapshot.read(var_id))
    }

    pub(crate) fn write(&mut self, var_id: VarId, value: LpcRef) {
        self.changeset.write(var_id, value);
    }

    /// Dismantle the transaction into its snapshot and changeset for retries, etc.
    pub(crate) fn into_parts(self) -> (Snapshot, Changeset) {
        (self.snapshot, self.changeset)
    }
}

#[cfg(test)]
mod tests;
