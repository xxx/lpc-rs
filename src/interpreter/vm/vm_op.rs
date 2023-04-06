/// Operations that can be communicated to the VM remotely.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VmOp {
    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    PrioritizeCallOut(usize),

    /// Clean up the Task with the passed ID
    FinishTask(usize),
}
