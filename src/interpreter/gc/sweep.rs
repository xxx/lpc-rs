use bit_set::BitSet;
use lpc_rs_errors::Result;
use qcell::QCellOwner;

/// A trait for sweeping objects that contain references that could be
/// garbage-collected.
pub trait Sweep {
    /// Sweep the passed indices from the [`Vm`](crate::interpreter::vm::Vm)'s `upvalues`.
    fn sweep(&mut self, marked: &BitSet, cell_key: &mut QCellOwner) -> Result<()>;
}

/// A trait for types that don't need a [`QCellOwner`] to sweep.
/// [`Sweep`] is automatically implemented for types that implement this trait.
pub trait KeylessSweep {
    /// Sweep the passed indices from the [`Vm`](crate::interpreter::vm::Vm)'s `upvalues`.
    fn keyless_sweep(&mut self, marked: &BitSet) -> Result<()>;
}

impl<T> Sweep for T
where
    T: KeylessSweep,
{
    fn sweep(&mut self, marked: &BitSet, _cell_key: &mut QCellOwner) -> Result<()> {
        self.keyless_sweep(marked)
    }
}
