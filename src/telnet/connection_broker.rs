use std::{
    fmt::{Display, Formatter},
    net::SocketAddr,
    sync::Arc,
};

use dashmap::DashMap;
use flume::Receiver as FlumeReceiver;
use tokio::{net::ToSocketAddrs, sync::mpsc::Sender, task::JoinHandle};
use tracing::{error, info, instrument, trace};

use crate::{
    interpreter::vm::vm_op::VmOp,
    telnet::{
        ops::{BrokerOp, ConnectionOp},
        Telnet,
    },
};

/// The ID of a connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConnectionId(pub u32);

impl Display for ConnectionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A connection from a user
#[derive(Debug, Clone, PartialEq)]
pub struct Connection {
    /// The ID of the connection.
    pub id: ConnectionId,

    /// The address of the client.
    pub address: SocketAddr,
}

/// Manages all the outgoing connections to users.
#[derive(Debug)]
pub struct ConnectionBroker {
    /// Map of connection ID with the tx channel to the connection itself
    connections: Arc<DashMap<ConnectionId, Sender<ConnectionOp>>>,

    /// Map of ConnectionIds to join handles, which can be dropped to disconnect the user.
    handles: Arc<DashMap<ConnectionId, JoinHandle<()>>>,

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
        Self {
            connections: Arc::new(DashMap::new()),
            handles: Arc::new(DashMap::new()),
            telnet,
            vm_tx,
            rx,
        }
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

    /// The main loop of the connection broker.
    /// This will listen for incoming messages from the telnet server and the VM.
    #[instrument(skip(self))]
    fn main_loop(&self) {
        let vm_tx = self.vm_tx.clone();
        let broker_rx = self.rx.clone();
        let connections = self.connections.clone();
        let handles = self.handles.clone();
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
                        BrokerOp::NewHandle(connection_id, handle) => {
                            handles.insert(connection_id, handle);
                            trace!("Added handle for connection {}", connection_id.0);
                        }
                        BrokerOp::Disconnect(connection_id) => {
                            match handles.remove(&connection_id) {
                                Some((_, handle)) => {
                                    info!("Disconnecting connection {}", connection_id.0);
                                    handle.abort();
                                }
                                None => {
                                    error!(
                                        "Failed to find handle for connection {}",
                                        connection_id.0
                                    );
                                }
                            }

                            // let Ok(_) = vm_tx.send(VmOp::Disconnected(connection_id)).await else {
                            //     error!("Failed to send VmOp::Disconnected");
                            //     continue;
                            // };

                            if connections.remove(&connection_id).is_none() {
                                error!("Failed to find connection with ID {}", connection_id);
                            }
                            //
                            // return;
                        }
                        BrokerOp::SendMessage(msg, connection_id) => {
                            let Some(connection_tx) = connections.get(&connection_id) else {
                                error!("Failed to find connection with ID {}", connection_id);
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
    pub fn remove_connection(
        &self,
        id: ConnectionId,
    ) -> Option<(ConnectionId, Sender<ConnectionOp>)> {
        self.connections.remove(&id)
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    #[tokio::test]
    async fn test_connection_broker() {
        let (broker_tx, broker_rx) = flume::bounded(10);
        let (vm_tx, mut vm_rx) = tokio::sync::mpsc::channel(10);
        let (connection_tx, mut connection_rx) = tokio::sync::mpsc::channel(10);
        let telnet = Telnet::new(broker_tx.clone());
        let mut broker = ConnectionBroker::new(vm_tx, broker_rx.clone(), telnet);

        broker.run("127.0.0.1:6666").await;

        //
        // BrokerOp::NewConnection
        //
        let connection = Connection {
            id: ConnectionId(1),
            address: SocketAddr::from(([127, 0, 0, 1], 1234)),
        };
        let op = BrokerOp::NewConnection(connection.clone(), connection_tx);
        broker_tx.send_async(op).await.unwrap();

        let Some(vm_op) = vm_rx.recv().await else {
            panic!("Failed to receive message");
        };

        assert_eq!(vm_op, VmOp::Connected(connection));
        assert!(broker.connections.contains_key(&ConnectionId(1)));

        //
        // BrokerOp::NewHandle
        //
        let handle = tokio::spawn(async {});
        let op = BrokerOp::NewHandle(ConnectionId(1), handle);
        broker_tx.send_async(op).await.unwrap();

        // we need to wait for the broker to add the handle to the map
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert!(broker.handles.contains_key(&ConnectionId(1)));

        //
        // BrokerOp::SendMessage
        //
        let op = BrokerOp::SendMessage("Welcome to the MUD!".to_string(), ConnectionId(1));
        broker_tx.send_async(op).await.unwrap();

        let Some(connection_op) = connection_rx.recv().await else {
            panic!("Failed to receive message");
        };

        assert_eq!(
            connection_op,
            ConnectionOp::SendMessage("Welcome to the MUD!".to_string())
        );

        //
        // BrokerOp::Disconnect
        //
        let op = BrokerOp::Disconnect(ConnectionId(1));
        broker_tx.send_async(op).await.unwrap();

        // wait for broker
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert!(!broker.handles.contains_key(&ConnectionId(1)));
        assert!(!broker.connections.contains_key(&ConnectionId(1)));
    }
}
