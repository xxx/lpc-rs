use std::{net::SocketAddr, sync::Arc};

use flume::Sender as FlumeSender;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::process::Process,
    telnet::ops::{BrokerOp, ConnectionOp},
};

/// A connection from a user
#[derive(Debug, Clone)]
pub struct Connection {
    /// The address of the client.
    pub address: SocketAddr,

    /// The process that this connection is attached to.
    /// This is basically the player's in-game body object.
    pub process: Option<Arc<Process>>,

    /// The channel we use to send messages to the socket connection's thread.
    pub tx: Sender<ConnectionOp>,

    /// The channel we use to send messages to the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker).
    pub broker_tx: FlumeSender<BrokerOp>,
}

impl Connection {
    /// Creates a new [`Connection`].
    pub fn new(
        address: SocketAddr,
        connection_tx: Sender<ConnectionOp>,
        broker_tx: FlumeSender<BrokerOp>,
    ) -> Self {
        Self {
            address,
            process: None,
            tx: connection_tx,
            broker_tx,
        }
    }

    /// Set the [`Process`] that this [`Connection`] is connected to, and tag the
    /// [`Process`] with the [`Connection`].
    /// Drops the previous [`Connection`] that was attached to the [`Process`] if there was one.
    pub async fn takeover_process(&mut self, process: &Arc<Process>) -> Option<Connection> {
        let previous = {
            let mut lock = process.connection.write();
            // These assignments happen while both are mutable, which should protect from race conditions.
            let prev = std::mem::replace(&mut *lock, Some(self.clone()));
            self.process = Some(process.clone());

            prev
        };

        if let Some(prev) = &previous {
            let _ = prev
                .tx
                .send(ConnectionOp::SendMessage(
                    "You are being disconnected because someone else logged in as you.".to_string(),
                ))
                .await;
            let _ = prev
                .broker_tx
                .send_async(BrokerOp::Disconnect(prev.address))
                .await;
        }

        previous
    }
}

impl PartialEq for Connection {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
    }
}
