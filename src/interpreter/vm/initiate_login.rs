use flume::Sender as FlumeSender;
use tracing::{debug, error};

use crate::{
    interpreter::{lpc_ref::LpcRef, task::apply_function::apply_function, vm::Vm, CONNECT, LOGON},
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
    },
};

impl Vm {
    /// Start the login process for a [`Connection`]. This assumes the connection is not
    /// already logged in and attached to an object.
    pub async fn initiate_login(&self, mut connection: Connection) {
        let object_space = self.object_space.clone();
        let broker_tx = self.broker_tx.clone();
        let task_template = self.new_task_template();

        tokio::spawn(async move {
            debug!("initiating login for {}", connection.address);

            // get the master object
            let Some(master) = object_space.master_object() else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - Failed to get master object.".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // get the 'connect' function
            let Some(connect) = master.program.unmangled_functions.get(CONNECT) else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - Unable to find the `connect` function in the master object.".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // call connect()
            let connect_result =
                apply_function(connect.clone(), &[], master.clone(), task_template.clone()).await;

            // check the result
            let Ok(LpcRef::Object(maybe_login_ob)) = connect_result else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - We didn't receive an object back when calling connect().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            let Some(login_ob) = maybe_login_ob.upgrade() else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - We received a destructed object back when calling connect().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // This is the initial exec() of the player into a body.
            connection.takeover_process(&login_ob).await;

            // get the 'logon' function
            let Some(logon) = login_ob.program.unmangled_functions.get(LOGON) else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - Unable to find the `logon` function in the login object.".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // call logon()
            let Ok(logon_result) = apply_function(logon.clone(), &[], login_ob.clone(), task_template.clone()).await else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - Something failed while executing logon().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // check the result
            let LpcRef::Object(maybe_player_ob) = logon_result else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - We didn't receive an object back when calling logon().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            let Some(player_ob) = maybe_player_ob.upgrade() else {
                Self::fatal_error(
                    connection,
                    "Fatal server error - We received a destructed object back when calling logon().".to_string(),
                    broker_tx.clone()
                ).await;
                return;
            };

            // At this point, the player is assumed to be fully authenticated.
            connection.takeover_process(&player_ob).await;

            let _ = broker_tx.send_async(BrokerOp::Connected(connection)).await;
        });
    }

    async fn fatal_error(
        connection: Connection,
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
