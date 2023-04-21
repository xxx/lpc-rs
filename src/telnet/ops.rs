use std::net::SocketAddr;

use tokio::{sync::mpsc::Sender, task::JoinHandle};
use crate::telnet::connection::Connection;

/// Operations that are handled by the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
#[derive(Debug)]
pub enum BrokerOp {
    /// Start the login process for a connection.
    NewConnection(Connection, Sender<ConnectionOp>),

    /// We have received a new, authenticated connection from a user.
    Connected(Connection, Sender<ConnectionOp>),

    /// Keep track of the handle for a connection, so we can drop it
    /// if necessary.
    NewHandle(SocketAddr, JoinHandle<()>),

    /// Disconnect the specified connection
    Disconnect(SocketAddr),

    /// Send a message to the specified connection
    SendMessage(String, SocketAddr),
}

/// Operations that can be performed on outgoing connections
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionOp {
    /// Send a message to the user
    SendMessage(String),
}
