use std::sync::Arc;

use flume::Sender as FlumeSender;
use tokio::sync::mpsc::Sender;
use lpc_rs_core::LpcIntInner;
use tracing::{debug, instrument};
use lpc_rs_errors::{lpc_error, LpcError};

use crate::{
    interpreter::{
        into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef, lpc_string::LpcString,
        task::apply_function::apply_function_by_name, vm::Vm, CONNECT, LOGON,
    },
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
    },
};
use crate::interpreter::process::Process;
use crate::interpreter::vm::vm_op::VmOp;

impl Vm {
    /// Start the login process for a [`Connection`]. This assumes the connection is not
    /// already logged in and attached to an object.
    #[instrument(skip_all)]
    pub async fn initiate_login(&self, connection: Arc<Connection>) {
        let object_space = self.object_space.clone();
        let broker_tx = self.broker_tx.clone();
        let task_template = self.new_task_template();
        let vm_tx = self.tx.clone();

        let address = connection.address;
        let (ip, port) = (address.ip().to_string(), address.port());

        // call 'connect' in the master object
        let ip_ref = LpcString::from(ip).into_lpc_ref(&self.memory);
        let port_ref = LpcRef::from(port as LpcIntInner);

        tokio::spawn(async move {
            debug!("initiating login for {}", connection.address);

            // get the master object
            let Some(master) = object_space.master_object() else {
                Self::fatal_error(
                    &connection,
                    lpc_error!("Fatal server error - Failed to get master object."),
                    None,
                    vm_tx.clone(),
                    broker_tx.clone()
                ).await;
                return;
            };

            let maybe_login_ob = match apply_function_by_name(
                CONNECT,
                &[ip_ref.clone(), port_ref.clone()],
                master.clone(),
                task_template.clone(),
                Some(task_template.config.max_execution_time),
            )
            .await
            {
                Some(Ok(LpcRef::Object(ob))) => ob,
                Some(Ok(LpcRef::String(string_arc))) => {
                    // Not a runtime error in this case. Just a custom error message.
                    let message = string_arc.read().to_string();
                    let _ = connection.tx.send(ConnectionOp::SendMessage(message)).await;
                    let _ = broker_tx
                        .send_async(BrokerOp::Disconnect(connection.address))
                        .await;
                    return;
                }
                Some(Ok(_)) => {
                    Self::fatal_error(
                        &connection,
                        lpc_error!("Fatal server error - We didn't receive an object back when calling connect()."),
                        Some(master),
                        vm_tx.clone(),
                        broker_tx.clone()
                    ).await;
                    return;
                }
                Some(Err(e)) => {
                    Self::fatal_error(
                        &connection,
                        e,
                        Some(master),
                        vm_tx.clone(),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                None => {
                    Self::fatal_error(
                        &connection,
                        lpc_error!("Fatal server error - Unable to find the `connect` function in the master object."),
                        Some(master),
                        vm_tx.clone(),
                        broker_tx.clone()
                    ).await;
                    return;
                }
            };

            let Some(login_ob) = maybe_login_ob.upgrade() else {
                debug_assert!(false, "We received a destructed object back when calling connect(). This should never happen.");
                Self::fatal_error(
                    &connection,
                    lpc_error!("Fatal server error - We received a destructed object back when calling connect()."),
                    Some(master),
                    vm_tx.clone(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // This is the initial exec() of the player into a body.
            Vm::exec(connection.clone(), login_ob.clone(), vm_tx.clone()).await;

            let template = task_template.clone();
            template.set_this_player(Some(login_ob.clone()));

            // call 'logon' in the login object
            let max_execution_time = task_template.config.max_execution_time;
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
                    Self::fatal_error(
                        &connection,
                        lpc_error!("Fatal server error - We didn't receive an int back when calling logon()."),
                        Some(login_ob),
                        vm_tx.clone(),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                Some(Err(e)) => {
                    Self::fatal_error(
                        &connection,
                        e,
                        Some(login_ob),
                        vm_tx.clone(),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                None => {
                    Self::fatal_error(
                        &connection,
                        lpc_error!("Fatal server error - Unable to find the `logon` function in the object."),
                        Some(login_ob),
                        vm_tx.clone(),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
            }

            let _ = broker_tx.send_async(BrokerOp::Connected(connection)).await;
        });
    }

    async fn fatal_error(
        connection: &Connection,
        error: Box<LpcError>,
        object: Option<Arc<Process>>,
        vm_tx: Sender<VmOp>,
        broker_tx: FlumeSender<BrokerOp>,
    ) {
        let _ = connection.tx.send(ConnectionOp::SendMessage(error.to_string())).await;
        let _ = broker_tx
            .send_async(BrokerOp::Disconnect(connection.address))
            .await;

        if object.is_some() {
            // if object is None here, it means we have a bad master object, so we can't send a runtime error.
            let _ = vm_tx.send(VmOp::RuntimeError(error, object)).await;
        }
    }
}
