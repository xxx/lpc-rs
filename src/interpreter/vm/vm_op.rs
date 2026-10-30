use std::sync::Arc;

use crate::telnet::connection::Connection;

/// Operations that can be communicated to the [`Vm`](crate::interpreter::vm::Vm) remotely.
#[derive(Debug)]
pub enum VmOp {
    /// Start the login process for a connection.
    InitiateLogin(Arc<Connection>),

    /// Run a scheduled [`CallOut`](crate::interpreter::call_outs::CallOut), identified by its ID.
    PrioritizeCallOut(u64),

    /// `shutdown(code)` committed: leave the main loop with this exit code.
    Shutdown(i32),

    /// An object update whose requesting transaction committed.
    ObjectUpdate(Arc<super::object_update::UpdateRequest>),
}

impl PartialEq for VmOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InitiateLogin(a), Self::InitiateLogin(b)) => a.address == b.address,
            (Self::PrioritizeCallOut(a), Self::PrioritizeCallOut(b)) => a == b,
            (Self::Shutdown(a), Self::Shutdown(b)) => a == b,
            (Self::ObjectUpdate(a), Self::ObjectUpdate(b)) => a.id == b.id,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::vm::{object_recompile::RecompileTarget, object_update::UpdateRequest};
    use std::sync::Weak;

    #[test]
    fn equality_requires_the_same_operation_and_identity() {
        let login = |port| {
            let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
            VmOp::InitiateLogin(Arc::new(Connection::new(([127, 0, 0, 1], port).into(), tx)))
        };
        let update = |id, program: &str| {
            VmOp::ObjectUpdate(Arc::new(UpdateRequest {
                id,
                target: RecompileTarget::Object(Weak::new()),
                caller: Weak::new(),
                player: None,
                program: Some(program.to_owned()),
            }))
        };
        let operations = [
            login(1000),
            login(1001),
            VmOp::PrioritizeCallOut(17),
            VmOp::PrioritizeCallOut(18),
            VmOp::Shutdown(17),
            VmOp::Shutdown(18),
            update(17, "/admin.c"),
            update(18, "/admin.c"),
        ];
        for (i, left) in operations.iter().enumerate() {
            for (j, right) in operations.iter().enumerate() {
                assert_eq!(left == right, i == j, "message indices {i} and {j}");
            }
        }
        assert_eq!(login(1000), login(1000));
        assert_eq!(update(17, "/first.c"), update(17, "/second.c"));
    }
}
