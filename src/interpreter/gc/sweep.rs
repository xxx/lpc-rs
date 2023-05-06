use bit_set::BitSet;
use lpc_rs_errors::Result;

/// A trait for sweeping objects that contain references that could be
/// garbage-collected.
pub trait Sweep {
    /// Sweep the passed indices from the [`Vm`](crate::interpreter::vm::Vm)'s `upvalues`.
    fn sweep(&mut self, marked: &BitSet) -> Result<()>;
}
