use std::sync::Arc;

use lpc_rs_errors::LpcError;

use crate::{interpreter::task::task_id::TaskId, telnet::connection::Connection};
use crate::interpreter::process::Process;

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug)]
pub enum VmOp {
    /// Start the login process for a connection.
    InitiateLogin(Arc<Connection>),

    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    PrioritizeCallOut(usize),

    /// Connect a user to a Process. Include a channel to call the response back,
    /// since the entire point of this op is to enforce sequential handling.
    Exec(Arc<Connection>, Arc<Process>, tokio::sync::oneshot::Sender<Option<Arc<Connection>>>),

    /// The Task with the passed ID has failed.
    TaskError(TaskId, LpcError),

    /// A subsystem has run into a problem that cannot be recovered from, so we need to shut down.
    FatalError(String),
}

impl PartialEq for VmOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InitiateLogin(a), Self::InitiateLogin(b)) => a.address == b.address,
            (Self::PrioritizeCallOut(a), Self::PrioritizeCallOut(b)) => a == b,
            (Self::Exec(a, _, _), Self::Exec(b, _, _)) => a.address == b.address,
            (Self::TaskError(a, _), Self::TaskError(b, _)) => a == b,
            (Self::FatalError(a), Self::FatalError(b)) => a == b,
            _ => false,
        }
    }
}