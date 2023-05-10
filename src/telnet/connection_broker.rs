use std::{net::SocketAddr, sync::Arc};

use dashmap::DashMap;
use flume::Receiver as FlumeReceiver;
use tokio::{net::ToSocketAddrs, sync::mpsc::Sender, task::JoinHandle};
use tracing::{error, info, instrument, trace};

use crate::{
    interpreter::{task::task_template::TaskTemplate, vm::vm_op::VmOp},
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
        Telnet,
    },
};

/// Manages all the outgoing connections to users.
#[derive(Debug)]
pub struct ConnectionBroker {
    /// Map of remote IP address to the connection itself
    connections: Arc<DashMap<SocketAddr, Arc<Connection>>>,

    /// Map of remote IP addresses to join handles, which can be dropped to disconnect the user.
    handles: Arc<DashMap<SocketAddr, JoinHandle<()>>>,

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
    pub async fn run<A>(&mut self, listen_address: A, template: TaskTemplate)
    where
        A: ToSocketAddrs + Send + 'static,
    {
        info!("Starting connection broker");
        self.telnet.run(listen_address, template).await;

        self.main_loop();
    }

    /// The main loop of the connection broker.
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
                        BrokerOp::NewConnection(connection) => {
                            let address = connection.address;
                            let Ok(_) = vm_tx.send(VmOp::InitiateLogin(connection)).await else {
                                error!("Failed to send VmOp::InitiateLogin for {}. Disconnecting.", address);
                                continue;
                            };
                        }
                        BrokerOp::Connected(connection) => {
                            let address = connection.address;
                            connections.insert(address, connection);
                            trace!("Connected to {}", address);
                        }
                        BrokerOp::NewHandle(address, handle) => {
                            handles.insert(address, handle);
                            trace!("Added handle for {}", address);
                        }
                        BrokerOp::Disconnect(ref address) => {
                            match handles.remove(address) {
                                Some((_, handle)) => {
                                    info!(?op, "Disconnecting {}", address);
                                    handle.abort();
                                }
                                None => {
                                    error!("Failed to find handle for {}", address);
                                }
                            }

                            if connections.remove(address).is_none() {
                                error!("Failed to find connection with ID {}", address);
                            }
                        }
                        BrokerOp::SendMessage(msg, address) => {
                            let tx = {
                                let Some(connection) = connections.get(&address) else {
                                    error!("Failed to find connection for {}", address);
                                    continue;
                                };

                                connection.tx.clone()
                            };

                            let Ok(_) = tx.send(ConnectionOp::SendMessage(msg)).await else {
                                error!("Failed to send ConnectionOp::SendMessage");
                                return;
                            };
                        }
                        BrokerOp::Shutdown => {
                            info!("Shutting down broker main loop");
                            return;
                        }
                        BrokerOp::FatalError(s) => {
                            error!("Broker received fatal error: {}", s);
                            let _ = vm_tx.send(VmOp::FatalError(s)).await;
                            return;
                        }
                    }
                }
            }
        });
    }

    /// Disable new connections from being made, in preparation for shutdown.
    pub fn disable_incoming_connections(&mut self) {
        self.telnet.shutdown();
    }

    /// Disconnect all users, immediately.
    pub fn disconnect_users(&mut self) {
        info!("Disconnecting users");
        self.connections.clear();
    }

    /// Removes a connection from the manager.
    #[inline]
    pub fn remove_connection(&self, address: SocketAddr) -> Option<(SocketAddr, Arc<Connection>)> {
        self.connections.remove(&address)
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use arc_swap::ArcSwapAny;

    use super::*;
    use crate::{
        interpreter::{object_space::ObjectSpace, vm::global_state::GlobalState},
        telnet::connection::Connection,
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_connection_broker() {
        let (broker_tx, broker_rx) = flume::bounded(10);
        let (vm_tx, mut vm_rx) = tokio::sync::mpsc::channel(10);
        let (connection_tx, mut connection_rx) = tokio::sync::mpsc::channel(10);
        let telnet = Telnet::new(broker_tx.clone());
        let config = Arc::new(test_config());
        let _object_space = Arc::new(ObjectSpace::new(config.clone()));
        let mut broker = ConnectionBroker::new(vm_tx.clone(), broker_rx.clone(), telnet);
        let global_state = Arc::new(GlobalState::new(config, vm_tx.clone()));
        let template = TaskTemplate {
            global_state,
            this_player: ArcSwapAny::from(None),
            upvalue_ptrs: None,
        };

        broker.run("127.0.0.1:6666", template).await;

        //
        // BrokerOp::NewConnection
        //
        let address = SocketAddr::from(([127, 0, 0, 1], 1234));
        let connection = Arc::new(Connection::new(
            address,
            connection_tx.clone(),
            broker_tx.clone(),
        ));
        let op = BrokerOp::NewConnection(connection.clone());
        broker_tx.send_async(op).await.unwrap();

        let Some(vm_op) = vm_rx.recv().await else {
            panic!("Failed to receive message");
        };

        assert_eq!(vm_op, VmOp::InitiateLogin(connection.clone()));

        //
        // BrokerOp::Connected
        //
        let op = BrokerOp::Connected(connection.clone());
        broker_tx.send_async(op).await.unwrap();
        // allow the broker to handle the message
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert!(broker.connections.contains_key(&address));

        //
        // BrokerOp::NewHandle
        //
        let handle = tokio::spawn(async {});
        let op = BrokerOp::NewHandle(address, handle);
        broker_tx.send_async(op).await.unwrap();

        // we need to wait for the broker to add the handle to the map
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert!(broker.handles.contains_key(&address));

        //
        // BrokerOp::SendMessage
        //
        let op = BrokerOp::SendMessage("Welcome to the MUD!".to_string(), address);
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
        let op = BrokerOp::Disconnect(address);
        broker_tx.send_async(op).await.unwrap();

        // wait for broker
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert!(!broker.handles.contains_key(&address));
        assert!(!broker.connections.contains_key(&address));
    }
}
