use lpc_rs_errors::LpcError;
use crate::interpreter::task::task_id::TaskId;

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VmOp {
    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    PrioritizeCallOut(usize),

    /// The Task with the passed ID has completed successfully.
    TaskComplete(TaskId),

    /// The Task with the passed ID has failed.
    TaskError(TaskId, LpcError),
}
