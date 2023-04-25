use std::sync::Arc;

use lpc_rs_errors::LpcError;

use crate::{interpreter::task::task_id::TaskId, telnet::connection::Connection};

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug, PartialEq)]
pub enum VmOp {
    /// Start the login process for a connection.
    InitiateLogin(Arc<Connection>),

    /// Run a CallOut function, identified by its index in the [`CallOuts`](crate::interpreter::call_outs::CallOuts) list
    PrioritizeCallOut(usize),

    /// The Task with the passed ID has failed.
    TaskError(TaskId, LpcError),

    /// A subsystem has run into a problem that cannot be recovered from, so we need to shut down.
    FatalError(String),
}
