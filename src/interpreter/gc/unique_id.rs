use std::{
    collections::HashSet,
    sync::atomic::{AtomicUsize, Ordering},
};

use bit_set::BitSet;
use lpc_rs_errors::Result;
use qcell::QCellOwner;
use serde::{Deserialize, Serialize};

/// A unique ID, suitable for uniquely identifying an object that contains
/// references that could be garbage-collected.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct UniqueId(usize);

impl UniqueId {
    /// Create a new [`UniqueId`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for UniqueId {
    fn default() -> Self {
        next_id()
    }
}

/// Get the next ID.
/// This ID is used so we can avoid track objects that have been marked,
/// to avoid infinitely processing a looping graph.
fn next_id() -> UniqueId {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    UniqueId(COUNTER.fetch_add(1, Ordering::Relaxed))
}

/// A trait for marking objects that contain references that could be
/// garbage-collected.
pub trait GcMark {
    /// Mark indices in the [`Process`]' `upvalues` list that are live.
    ///
    /// # Arguments
    /// * `marked` - A set of indices in the `upvalues` list that are live.
    /// * `processed` - A set of [`UniqueId`]s representing nodes that have
    ///   already been processed. Used to avoid infinite loops.
    /// * `cell_key` - A [`QCellOwner`] key to use for unlocking [`QCell`]ed
    ///   data.
    ///
    /// # Returns
    /// * `Ok(())` if the marking was successful.
    /// * `Err` if the marking failed.
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut HashSet<UniqueId>,
        cell_key: &QCellOwner,
    ) -> Result<()>;
}

/// A trait for sweeping objects that contain references that could be
/// garbage-collected.
pub trait GcSweep {
    /// Sweep the passed indices from the [`Process`]' `upvalues`.
    fn sweep(&mut self, marked: &BitSet) -> Result<()>;
}
