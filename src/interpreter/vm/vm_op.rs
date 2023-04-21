use lpc_rs_errors::LpcError;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::task::task_id::TaskId,
    telnet::{connection_broker::Connection, ops::ConnectionOp},
};

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug)]
pub enum VmOp {
    /// A user has successfully logged-in.
    Connected(Connection, Sender<ConnectionOp>),

    /// Start the login process for a connection.
    InitiateLogin(Connection, Sender<ConnectionOp>),

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
            (VmOp::Connected(a, _), VmOp::Connected(b, _)) => a == b,
            (VmOp::InitiateLogin(a, _), VmOp::InitiateLogin(b, _)) => a == b,
            (VmOp::PrioritizeCallOut(a), VmOp::PrioritizeCallOut(b)) => a == b,
            (VmOp::TaskComplete(a), VmOp::TaskComplete(b)) => a == b,
            (VmOp::TaskError(a, ae), VmOp::TaskError(b, be)) => a == b && ae == be,
            _ => false,
        }
    }
}
