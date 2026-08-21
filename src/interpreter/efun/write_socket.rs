use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    interpreter::{
        efun::{efun_context::EfunContext, write::record_output_effect},
        lpc_ref::LpcRef,
        stm::Effect,
    },
    telnet::ops::ConnectionOp,
};

/// `write_socket`, an efun for writing to the interactive inhabiting the object.
pub async fn write_socket<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let result = match arg_ref {
        LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::String(_) => arg_ref.to_string(),
        LpcRef::Array(_) | LpcRef::Mapping(_) | LpcRef::Function(_) | LpcRef::Object(_) => {
            return Err(context.runtime_error("invalid argument to `write_socket`"));
        }
    };

    let process = context.process();

    match context.txn().read_connection(process.connection.id) {
        Some(connection) => {
            // Record the socket send against this transaction's effect log,
            // carrying its own channel so the retry loop can deliver it
            // after commit without needing the connection again.
            context.txn().record_effect(Effect::Socket {
                op: ConnectionOp::SendMessage(result),
                tx: connection.tx.clone(),
            });
            context.return_efun_result(LpcRef::from(1));
        }
        None => {
            // No connection to receive the message, so dump it to the debug log.
            record_output_effect(context, result);

            // 0 is already returned by default
        }
    }

    Ok(())
}
