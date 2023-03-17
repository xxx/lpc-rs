use qcell::QCellOwner;
use bit_set::BitSet;
use std::collections::HashSet;
use crate::interpreter::gc::unique_id::UniqueId;

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
    ) -> lpc_rs_errors::Result<()>;
}
