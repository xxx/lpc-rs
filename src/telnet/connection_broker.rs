use std::net::{SocketAddr};
use std::sync::Arc;
use dashmap::DashMap;
use tokio::sync::mpsc::Sender;
use flume::Receiver as FlumeReceiver;
use tokio::net::ToSocketAddrs;
use tracing::{error, info};
use crate::interpreter::vm::vm_op::VmOp;
use crate::telnet::ops::{BrokerOp, ConnectionOp};
use crate::telnet::Telnet;

/// The ID of a connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConnectionId(pub u32);

/// A connection from a user
#[derive(Debug, Clone, PartialEq)]
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
    pub async fn run<A>(&mut self, listen_address: A)
    where
        A: ToSocketAddrs + Send + 'static,
    {
        info!("Starting connection broker");
        self.telnet.run(listen_address).await;

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
                                error!("Failed to send VmOp::NewConnection");
                                continue;
                            };
                        }
                        BrokerOp::SendMessage(msg, connection_id) => {
                            let Some(connection_tx) = connections.get(&connection_id) else {
                                error!("Failed to find connection with ID {}", connection_id.0);
                                continue;
                            };

                            let tx = connection_tx.clone();

                            let Ok(_) = tx.send(ConnectionOp::SendMessage(msg)).await else {
                                error!("Failed to send ConnectionOp::SendMessage");
                                return;
                            };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_connection_broker() {
        let (tx, rx) = flume::bounded(10);
        let (vm_tx, mut vm_rx) = tokio::sync::mpsc::channel(10);
        let (connection_tx, mut connection_rx) = tokio::sync::mpsc::channel(10);
        let telnet = Telnet::new(tx.clone());
        let mut broker = ConnectionBroker::new(vm_tx, rx, telnet);

        broker.run("127.0.0.1:6666").await;

        //
        // BrokerOp::NewConnection
        //
        let connection = Connection { id: ConnectionId(1), address: SocketAddr::from(([127, 0, 0, 1], 1234)) };
        let op = BrokerOp::NewConnection(connection.clone(), connection_tx);
        tx.send_async(op).await.unwrap();

        let Some(vm_op) = vm_rx.recv().await else {
            panic!("Failed to receive message");
        };

        assert_eq!(vm_op, VmOp::Connected(connection));
        assert!(broker.connections.contains_key(&ConnectionId(1)));

        //
        // BrokerOp::SendMessage
        //
        let op = BrokerOp::SendMessage("Welcome to the MUD!".to_string(), ConnectionId(1));
        tx.send_async(op).await.unwrap();

        let Some(connection_op) = connection_rx.recv().await else {
            panic!("Failed to receive message");
        };

        assert_eq!(connection_op, ConnectionOp::SendMessage("Welcome to the MUD!".to_string()));
    }
}