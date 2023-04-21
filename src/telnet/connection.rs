use std::net::SocketAddr;
use std::sync::Arc;
use flume::Sender as FlumeSender;
use tokio::sync::mpsc::Sender;
use crate::interpreter::process::Process;
use crate::telnet::ops::{BrokerOp, ConnectionOp};

/// A connection from a user
#[derive(Debug, Clone)]
pub struct Connection {
    /// The address of the client.
    pub address: SocketAddr,

    /// The process that this connection is attached to.
    /// This is basically the player's in-game body object.
    pub process: Option<Arc<Process>>,

    pub connection_tx: Sender<ConnectionOp>,

    pub broker_tx: FlumeSender<BrokerOp>,
}

impl Connection {
    /// Creates a new [`Connection`].
    pub fn new(address: SocketAddr, connection_tx: Sender<ConnectionOp>, broker_tx: FlumeSender<BrokerOp>) -> Self {
        Self {
            address,
            process: None,
            connection_tx,
            broker_tx
        }
    }

    /// Set the process that this connection is connected to, and tag the
    /// process with the connection.
    /// Drops the previous connection if there was one.
    pub async fn takeover_process(&mut self, process: Arc<Process>) -> Option<Connection> {
        let previous = {
            let mut lock = process.connection.write();
            std::mem::replace(&mut *lock, Some(self.clone()))
        };
        // TODO: this smells like a race condition
        self.process = Some(process);

        if let Some(prev) = &previous {
            let _ = prev
                .connection_tx
                .send(ConnectionOp::SendMessage(
                    "You are being disconnected because someone else logged in as you."
                        .to_string(),
                ))
                .await;
            let _ = prev.broker_tx.send_async(BrokerOp::Disconnect(self.address)).await;
        }

        previous
    }
}

impl PartialEq for Connection {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
    }
}
