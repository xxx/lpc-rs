use std::{net::SocketAddr, sync::Arc};

use tokio::task::JoinHandle;

use crate::telnet::connection::{Connection, InputTo};

/// Operations that are handled by the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker)
#[derive(Debug)]
pub enum BrokerOp {
    /// Start the login process for a connection.
    NewConnection(Arc<Connection>),

    /// We have received a new, authenticated connection from a user.
    Connected(Arc<Connection>),

    /// Keep track of the handle for a connection, so we can drop it
    /// if necessary.
    NewHandle(SocketAddr, JoinHandle<()>),

    /// Disconnect the specified connection
    Disconnect(SocketAddr),

    /// Send a message to the specified connection
    SendMessage(String, SocketAddr),

    /// Shut down the broker, and all connections.
    Shutdown,
}

/// Operations that can be performed on outgoing connections
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionOp {
    /// Send a message to the user
    SendMessage(String),

    /// Set a function to receive the next line of input
    InputTo(InputTo),

    /// Signal to the connection that the server is shutting down.
    Shutdown,
}
