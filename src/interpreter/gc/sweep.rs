use bit_set::BitSet;
use qcell::QCellOwner;
use lpc_rs_errors::Result;

/// A trait for sweeping objects that contain references that could be
/// garbage-collected.
pub trait GcSweep {
    /// Sweep the passed indices from the [`Vm`]'s `upvalues`.
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> Result<()>;
}

/// A trait for types that don't need a [`QCellOwner`] to sweep.
/// [`GcSweep`] is automatically implemented for types that implement this trait.
pub trait KeylessGcSweep {
    /// Sweep the passed indices from the [`Vm`]'s `upvalues`.
    fn keyless_sweep(&mut self, marked: &BitSet) -> Result<()>;
}

impl<T> GcSweep for T
where
    T: KeylessGcSweep,
{
    fn sweep(&mut self, marked: &BitSet, _cell_key: &mut QCellOwner) -> Result<()> {
        self.keyless_sweep(marked)
    }
}