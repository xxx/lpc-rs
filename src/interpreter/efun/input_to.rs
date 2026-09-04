use lpc_rs_errors::Result;
use tracing::trace;

use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
    telnet::{connection::InputTo, ops::ConnectionOp},
};

/// `input_to`, an efun for registering a function to be called when the user
/// types something into the game.
pub fn input_to<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Function(ptr) = context.arg(0) else {
        return Err(context.runtime_error("non-function sent as first argument to `input_to`"));
    };

    if !ptr.receiver_bound() {
        return Err(context
            .runtime_error("`input_to` needs the receiver of a dynamic function pointer bound"));
    }

    let LpcRef::Int(no_echo) = context.arg(1) else {
        return Err(context.runtime_error("non-integer sent as second argument to `input_to`"));
    };

    let input_to = InputTo {
        ptr: ptr.clone(),
        no_echo: (*no_echo).into(),
    };

    let process = context.this_player().load();

    if let Some(process) = &*process
        && let Some(connection) = context
            .txn()
            .with(|t| t.read_connection(process.connection.id))
    {
        let _ = connection.send(ConnectionOp::InputTo(input_to));
        context.return_efun_result(LpcRef::from(1));
        return Ok(());
    } else {
        // No connection to receive the message, so do nothing.
        trace!("input_to on non-interactive process");
        // 0 is already returned by default
    }

    Ok(())
}
