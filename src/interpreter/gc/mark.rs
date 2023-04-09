use bit_set::BitSet;

/// A trait for marking objects that contain references that could be
/// garbage-collected.
pub trait Mark {
    /// Mark indices in the [`Vm`](crate::interpreter::vm::Vm)'s `upvalues` list that are live.
    ///
    /// # Arguments
    /// * `marked` - A set of indices in the `upvalues` list that are live.
    /// * `processed` - A set of [`UniqueId`](crate::interpreter::gc::unique_id)s representing nodes that have
    ///   already been processed. Used to avoid infinite loops.
    /// * `cell_key` - A [`QCellOwner`] key to use for unlocking [`QCell`](qcell::QCell)ed
    ///   data.
    ///
    /// # Returns
    /// * `Ok(())` if the marking was successful.
    /// * `Err(LpcError)` if the marking failed.
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
    ) -> lpc_rs_errors::Result<()>;
}
