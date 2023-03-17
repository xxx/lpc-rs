use bit_set::BitSet;
use qcell::QCellOwner;

/// A trait for sweeping objects that contain references that could be
/// garbage-collected.
pub trait GcSweep {
    /// Sweep the passed indices from the [`Vm`]'s `upvalues`.
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> lpc_rs_errors::Result<()>;
}
