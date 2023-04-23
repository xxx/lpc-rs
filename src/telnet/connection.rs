use std::{net::SocketAddr, sync::Arc};

use arc_swap::{ArcSwapAny, ArcSwapOption};
use flume::Sender as FlumeSender;
use parking_lot::RwLock;
use shared_arena::ArenaArc;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::{function_type::function_ptr::FunctionPtr, process::Process},
    telnet::ops::{BrokerOp, ConnectionOp},
};

/// A struct to encapsulate the state of awaiting a line of input from the user.
#[derive(Debug, Clone)]
pub struct InputTo {
    /// The function to call when we receive input.
    pub ptr: ArenaArc<RwLock<FunctionPtr>>,

    /// Whether `no_echo` was set when `input_to` was called, so we know
    /// that we need to re-enable it.
    pub no_echo: bool,
}

impl PartialEq for InputTo {
    fn eq(&self, other: &Self) -> bool {
        self.ptr.data_ptr() == other.ptr.data_ptr() && self.no_echo == other.no_echo
    }
}

/// A connection from a user
#[derive(Debug)]
pub struct Connection {
    /// The address of the client.
    pub address: SocketAddr,

    /// The process that this connection is attached to.
    /// This is basically the player's in-game body object.
    pub process: ArcSwapAny<Option<Arc<Process>>>,

    /// The channel we use to send messages to the socket connection's thread.
    pub tx: Sender<ConnectionOp>,

    /// The channel we use to send messages to the [`ConnectionBroker`](crate::telnet::connection_broker::ConnectionBroker).
    pub broker_tx: FlumeSender<BrokerOp>,

    /// The function to call when we receive input.
    pub input_to: ArcSwapOption<InputTo>,
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
            process: ArcSwapAny::from(None),
            tx: connection_tx,
            broker_tx,
            input_to: ArcSwapOption::from(None),
        }
    }

    /// Set the [`Process`] that this [`Connection`] is connected to, and tag the
    /// [`Process`] with the [`Connection`].
    /// Drops the previous [`Connection`] that was attached to the [`Process`] if there was one.'
    /// This is the underlying mechanism of the `exec` efun.
    // TODO: hide this behind a channel, so calls to it can be serialized to avoid races.
    pub async fn takeover_process(
        connection: Arc<Connection>,
        process: Arc<Process>,
    ) -> Option<Arc<Connection>> {
        let previous = process.connection.rcu(|_c| {
            connection.process.rcu(|_p| Some(process.clone()));

            Some(connection.clone())
        });

        if let Some(conn) = &previous {
            let _ = conn
                .tx
                .send(ConnectionOp::SendMessage(
                    "You are being disconnected because someone else logged in as you.".to_string(),
                ))
                .await;
            let _ = conn
                .broker_tx
                .send_async(BrokerOp::Disconnect(conn.address))
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
