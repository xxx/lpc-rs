use lpc_rs_errors::LpcError;

use crate::{interpreter::task::task_id::TaskId, telnet::connection::Connection};

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug)]
pub enum VmOp {
    /// Start the login process for a connection.
    InitiateLogin(Connection),

    /// A user has successfully logged-in.
    Connected(Connection),

    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    PrioritizeCallOut(usize),

    /// The Task with the passed ID has completed successfully.
    TaskComplete(TaskId),

    /// The Task with the passed ID has failed.
    TaskError(TaskId, LpcError),
}

impl PartialEq for VmOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VmOp::Connected(a), VmOp::Connected(b)) => a == b,
            (VmOp::InitiateLogin(a), VmOp::InitiateLogin(b)) => a == b,
            (VmOp::PrioritizeCallOut(a), VmOp::PrioritizeCallOut(b)) => a == b,
            (VmOp::TaskComplete(a), VmOp::TaskComplete(b)) => a == b,
            (VmOp::TaskError(a, ae), VmOp::TaskError(b, be)) => a == b && ae == be,
            _ => false,
        }
    }
}
