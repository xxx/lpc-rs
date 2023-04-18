use tokio::{sync::mpsc::Sender, task::JoinHandle};

use crate::telnet::connection_broker::{Connection, ConnectionId};

/// Operations that are handled by the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
#[derive(Debug)]
pub enum BrokerOp {
    /// We have received a new, authenticated connection from a user.
    NewConnection(Connection, Sender<ConnectionOp>),

    /// Keep track of the handle for a connection, so we can drop it
    /// if necessary.
    NewHandle(ConnectionId, JoinHandle<()>),

    /// Disconnect the specified connection
    Disconnect(ConnectionId),

    /// Send a message to the specified connection
    SendMessage(String, ConnectionId),
}

/// Operations that can be performed on outgoing connections
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionOp {
    /// Send a message to the user
    SendMessage(String),
}
