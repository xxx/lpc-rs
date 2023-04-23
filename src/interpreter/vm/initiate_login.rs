use std::sync::Arc;

use flume::Sender as FlumeSender;
use tracing::{debug, error};

use crate::{
    interpreter::{
        lpc_ref::LpcRef, task::apply_function::apply_function_by_name, vm::Vm, CONNECT, LOGON,
    },
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
    },
};

impl Vm {
    /// Start the login process for a [`Connection`]. This assumes the connection is not
    /// already logged in and attached to an object.
    pub async fn initiate_login(&self, connection: Arc<Connection>) {
        let object_space = self.object_space.clone();
        let broker_tx = self.broker_tx.clone();
        let task_template = self.new_task_template();

        tokio::spawn(async move {
            debug!("initiating login for {}", connection.address);

            // get the master object
            let Some(master) = object_space.master_object() else {
                Self::fatal_error(
                    &connection,
                    "Fatal server error - Failed to get master object.".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // call 'connect' in the master object
            let maybe_login_ob = match apply_function_by_name(
                CONNECT,
                &[],
                master.clone(),
                task_template.clone(),
            )
            .await
            {
                Some(Ok(LpcRef::Object(ob))) => ob,
                Some(Ok(_)) => {
                    Self::fatal_error(
                        &connection,
                        "Fatal server error - We didn't receive an object back when calling connect().".to_string(),
                        broker_tx.clone()
                    ).await;
                    return;
                }
                Some(Err(e)) => {
                    Self::fatal_error(
                        &connection,
                        format!("Fatal server error - Failed during connect(): {}", e),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                None => {
                    Self::fatal_error(
                        &connection,
                        "Fatal server error - Unable to find the `connect` function in the master object.".to_string(),
                        broker_tx.clone()
                    ).await;
                    return;
                }
            };

            let Some(login_ob) = maybe_login_ob.upgrade() else {
                debug_assert!(false, "We received a destructed object back when calling connect(). This should never happen.");
                Self::fatal_error(
                    &connection,
                    "Fatal server error - We received a destructed object back when calling connect().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // This is the initial exec() of the player into a body.
            Connection::takeover_process(connection.clone(), login_ob.clone()).await;

            let mut template = task_template.clone();
            template.set_this_player(Some(login_ob.clone()));

            // call 'logon' in the login object
            match apply_function_by_name(LOGON, &[], login_ob, template).await {
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
                        "Fatal server error - We didn't receive an int back when calling logon()."
                            .to_string(),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                Some(Err(e)) => {
                    Self::fatal_error(
                        &connection,
                        format!(
                            "Fatal server error - Something failed while executing logon(). {}",
                            e
                        ),
                        broker_tx.clone(),
                    )
                    .await;
                    return;
                }
                None => {
                    Self::fatal_error(
                        &connection,
                        "Fatal server error - Unable to find the `logon` function in the login object.".to_string(),
                        broker_tx.clone()
                    ).await;
                    return;
                }
            }

            let _ = broker_tx.send_async(BrokerOp::Connected(connection)).await;
        });
    }

    async fn fatal_error(
        connection: &Connection,
        message: String,
        broker_tx: FlumeSender<BrokerOp>,
    ) {
        error!("{}", &message);
        let _ = connection.tx.send(ConnectionOp::SendMessage(message)).await;
        let _ = broker_tx
            .send_async(BrokerOp::Disconnect(connection.address))
            .await;
    }
}
