use std::net::SocketAddr;
use std::sync::Arc;
use dashmap::DashMap;
use tokio::sync::mpsc::{Receiver, Sender};
use flume::Receiver as FlumeReceiver;
use tracing::{error, info};
use crate::interpreter::vm::vm_op::VmOp;
use crate::telnet::ops::{BrokerOp, ConnectionOp};
use crate::telnet::Telnet;

/// The ID of a connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConnectionId(pub u32);

/// A connection from a user
#[derive(Debug)]
pub struct Connection {
    /// The ID of the connection.
    pub id: ConnectionId,

    /// The address of the client.
    pub address: SocketAddr
}

/// Manages all the outgoing connections to users.
#[derive(Debug)]
pub struct ConnectionBroker {
    /// Map of connection ID with the tx channel to the connection itself
    connections: Arc<DashMap<ConnectionId, Sender<ConnectionOp>>>,

    /// The channel we receive messages on.
    rx: FlumeReceiver<BrokerOp>,

    /// The channel we use to send messages to the [`Vm`](crate::interpreter::vm::Vm).
    vm_tx: Sender<VmOp>,

    /// The telnet server, which handles the socket operations.
    telnet: Telnet,
}

impl ConnectionBroker {
    /// Creates a new [`ConnectionBroker`].
    pub fn new(vm_tx: Sender<VmOp>, rx: FlumeReceiver<BrokerOp>, telnet: Telnet) -> Self {
        Self { connections: Arc::new(DashMap::new()), telnet, vm_tx, rx }
    }

    /// Starts the connection broker.
    /// This will also start the telnet server.
    pub async fn run(&mut self) {
        info!("Starting connection broker");
        self.telnet.run().await;

        self.main_loop();
    }

    fn main_loop(&self) {
        let vm_tx = self.vm_tx.clone();
        let broker_rx = self.rx.clone();
        let connections = self.connections.clone();

        tokio::spawn(async move {
            loop {
                while let Ok(op) = broker_rx.recv_async().await {
                    match op {
                        BrokerOp::NewConnection(connection, tx) => {
                            connections.insert(connection.id, tx);
                            let Ok(_) = vm_tx.send(VmOp::Connected(connection)).await else {
                                error!("Failed to send NewConnection to VM");
                                continue;
                            };
                        }
                        BrokerOp::SendMessage(msg, connection_id) => {
                            connections.get(&connection_id).map(|tx| {
                                let tx = tx.clone();

                                async move {
                                    let Ok(_) = tx.send(ConnectionOp::SendMessage(msg)).await else {
                                        error!("Failed to send ConnectionOp::SendMessage");
                                        return;
                                    };
                                }
                            });
                        }
                    }
                }
            }
        });
    }

    /// Removes a connection from the manager.
    #[inline]
    pub fn remove_connection(&self, id: ConnectionId) -> Option<(ConnectionId, Sender<ConnectionOp>)> {
        self.connections.remove(&id)
    }
}