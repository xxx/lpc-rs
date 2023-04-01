/// Operations that can be communicated to the VM remotely.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VmOp {
    /// Run a task, identified by its index in the [`CallOuts`] heap
    RunTask(usize),
}