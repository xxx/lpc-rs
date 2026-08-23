use std::sync::Arc;

use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::{LpcError, lpc_error};
use tracing::{debug, error, instrument};

use crate::{
    interpreter::{
        CONNECT, LOGON,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        task::{
            apply_function::{apply_function_by_name, apply_runtime_error},
            task_template::TaskTemplate,
        },
        vm::Vm,
    },
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
    },
};

impl Vm {
    /// Start the login process for a [`Connection`]. This assumes the connection is not
    /// already logged in and attached to an object.
    #[instrument(skip_all)]
    pub async fn initiate_login(&self, connection: Arc<Connection>) {
        let global_state = self.global_state.clone();
        let broker_tx = self.broker_tx.clone();
        let task_template = TaskTemplate::from(self.global_state.clone());

        let address = connection.address;
        let (ip, port) = (address.ip().to_string(), address.port());

        // call 'connect' in the master object
        let ip_ref = LpcRef::from(LpcString::from(ip));
        let port_ref = LpcRef::from(port as LpcIntInner);

        tokio::spawn(async move {
            debug!("initiating login for {}", connection.address);

            // Abort the login; no object to blame means the master object
            // itself is bad, so `runtime_error` is not applied.
            let fail = async |error: Box<LpcError>, object: Option<Arc<Process>>| {
                let _ = connection
                    .tx
                    .send(ConnectionOp::SendMessage(error.to_string()))
                    .await;
                let _ = broker_tx
                    .send_async(BrokerOp::Disconnect(connection.address))
                    .await;

                if object.is_some() {
                    let template = TaskTemplate::from(global_state.clone());
                    match apply_runtime_error(&error, object, template).await {
                        Some(Ok(_)) => {}
                        None => {
                            error!("runtime_error() is not defined in the master object.");
                        }
                        Some(Err(e)) => {
                            error!("Error applying runtime error: {}", e.diagnostic_string());
                        }
                    }
                }
            };

            // get the master object
            let Some(master) = global_state.object_space.master_object() else {
                fail(
                    lpc_error!("Fatal server error - Failed to get master object."),
                    None,
                )
                .await;
                return;
            };

            let maybe_login_ob = match apply_function_by_name(
                CONNECT,
                &[ip_ref.clone(), port_ref.clone()],
                master.clone(),
                task_template.clone(),
                Some(task_template.global_state.config.max_execution_time),
            )
            .await
            {
                Some(Ok(LpcRef::Object(ob))) => ob,
                Some(Ok(r)) => {
                    if let LpcRef::String(_) = r {
                        let message = r
                            .with_string(|s| s.to_string())
                            .unwrap_or_else(|_| "No message received?".to_string());
                        let _ = connection.tx.send(ConnectionOp::SendMessage(message)).await;
                        let _ = broker_tx
                            .send_async(BrokerOp::Disconnect(connection.address))
                            .await;
                        return;
                    }

                    fail(lpc_error!("Fatal server error - We didn't receive an object back when calling connect(). Received {}", r.type_name()), Some(master)).await;
                    return;
                }
                Some(Err(e)) => {
                    fail(e, Some(master)).await;
                    return;
                }
                None => {
                    fail(lpc_error!("Fatal server error - Unable to find the `connect` function in the master object."), Some(master)).await;
                    return;
                }
            };

            let Some(login_ob) = maybe_login_ob.upgrade() else {
                debug_assert!(
                    false,
                    "We received a destructed object back when calling connect(). This should never happen."
                );
                fail(lpc_error!("Fatal server error - We received a destructed object back when calling connect()."), Some(master)).await;
                return;
            };

            // This is the initial exec() of the player into a body.
            global_state
                .takeover(connection.clone(), login_ob.clone())
                .await;

            let template = task_template.clone();
            template.set_this_player(Some(login_ob.clone()));

            // call 'logon' in the login object
            let max_execution_time = task_template.global_state.config.max_execution_time;
            match apply_function_by_name(
                LOGON,
                &[ip_ref, port_ref],
                login_ob.clone(),
                template,
                Some(max_execution_time),
            )
            .await
            {
                Some(Ok(LpcRef::Int(i))) => {
                    if i == 0 {
                        // We don't send an error in this case, as we assume that logon() has sent them messages.
                        let _ = broker_tx
                            .send_async(BrokerOp::Disconnect(connection.address))
                            .await;
                        return;
                    }
                }
                Some(Ok(_)) => {
                    fail(lpc_error!("Fatal server error - We didn't receive an int back when calling logon()."), Some(login_ob)).await;
                    return;
                }
                Some(Err(e)) => {
                    fail(e, Some(login_ob)).await;
                    return;
                }
                None => {
                    fail(lpc_error!("Fatal server error - Unable to find the `logon` function in the object."), Some(login_ob)).await;
                    return;
                }
            }

            let _ = broker_tx.send_async(BrokerOp::Connected(connection)).await;
        });
    }
}
