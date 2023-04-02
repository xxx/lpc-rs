/// Operations that can be communicated to the VM remotely.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VmOp {
    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    RunCallOut(usize),

    /// Let the currently-running `Task` yield, which pauses its execution and
    /// releases the `cell_key`, allowing us to check if others things need to
    /// be addressed (other scheduled tasks, new connections, etc.)
    Yield,
}
