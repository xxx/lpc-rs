use std::sync::Arc;

use crate::telnet::connection::Connection;

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug)]
pub enum VmOp {
    /// Start the login process for a connection.
    InitiateLogin(Arc<Connection>),

    /// Run a scheduled [`CallOut`](crate::interpreter::call_outs::CallOut), identified by its ID.
    PrioritizeCallOut(u64),

    /// A subsystem has run into a problem that cannot be recovered from, so we need to shut down.
    FatalError(String),
}

impl PartialEq for VmOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InitiateLogin(a), Self::InitiateLogin(b)) => a.address == b.address,
            (Self::PrioritizeCallOut(a), Self::PrioritizeCallOut(b)) => a == b,
            (Self::FatalError(a), Self::FatalError(b)) => a == b,
            _ => false,
        }
    }
}
