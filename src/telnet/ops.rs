use std::{net::SocketAddr, sync::Arc};

use lpc_rs_errors::LpcError;
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

    /// A subsystem has run into a problem that cannot be recovered from, so we need to shut down.
    /// This op is for sending messages up the chain to the VM only.
    /// The VM will handle actual shutdown.
    FatalError(LpcError),
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

    /// Close the connection once everything queued before this has been sent.
    Close,

    /// A GMCP message; the session drops it while GMCP is off.
    Gmcp {
        /// `Char.Vitals`, `Core.Ping`, …
        package: String,
        /// The body, usually JSON; empty sends the package alone.
        payload: String,
    },

    /// MXP markup sent as written; the session drops it while MXP is off.
    Mxp(String),

    /// Prompt text, then the mark the client negotiated (EOR, GA, or none).
    Prompt(String),

    /// A body was bound to this connection — at login, and at every `exec`.
    Attached,
}
