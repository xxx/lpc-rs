use lpc_rs_errors::Result;

use crate::{
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef},
    telnet::ops::ConnectionOp,
};

/// `write_socket`, an efun for writing to the interactive inhabiting the object.
pub async fn write_socket<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1_usize);

    let result = match arg_ref {
        LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::String(_) => arg_ref.to_string(),
        LpcRef::Array(_) | LpcRef::Mapping(_) | LpcRef::Function(_) | LpcRef::Object(_) => {
            return Err(context.runtime_error("invalid argument to `write_socket`"));
        }
    };

    let process = context.process();

    match &*process.connection.load() {
        Some(connection) => {
            let _ = connection.tx.send(ConnectionOp::SendMessage(result)).await;
            context.return_efun_result(LpcRef::from(1));
        }
        None => {
            // No connection to receive the message, so dump it to the debug log.
            context.config().debug_log(result).await;

            // 0 is already returned by default
        }
    }

    Ok(())
}
