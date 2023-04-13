pub mod connection_broker;
pub mod ops;

use std::net::SocketAddr;
use futures::{SinkExt, StreamExt};
use futures::stream::SplitSink;
use nectar::TelnetCodec;
use nectar::event::TelnetEvent;
use once_cell::sync::OnceCell;
use tokio::net::{TcpListener, TcpStream};
use flume::Sender as FlumeSender;
use tokio::task::JoinHandle;
use tokio_util::codec::{Decoder, Framed};
use tracing::{error, info, warn};
use crate::telnet::connection_broker::{Connection, ConnectionId};
use crate::telnet::ops::{BrokerOp, ConnectionOp};

/// The incoming connection handler. Once established, connections are handled by [`ConnectionManager`].
#[derive(Debug)]
pub struct Telnet {
    /// The handle to the main connection handler task. Dropping it will stop listening
    /// for new connections.
    handle: OnceCell<JoinHandle<()>>,

    /// The channel to send operations to the [`ConnectionBroker`](connection_broker::ConnectionBroker).
    broker_tx: FlumeSender<BrokerOp>,
}

impl Telnet {
    /// Creates a new [`Telnet`] instance.
    pub fn new(broker_tx: FlumeSender<BrokerOp>) -> Self {
        Self {
            handle: OnceCell::new(),
            broker_tx,
        }
    }

    /// Starts the telnet server.
    pub async fn run(&self) {
        if self.handle.get().is_some() {
            return;
        }

        info!("Starting telnet server");

        let broker_tx = self.broker_tx.clone();

        let handle = tokio::spawn(async move {
            let listener = match TcpListener::bind("127.0.0.1:6969").await {
                Ok(listener) => listener,
                Err(e) => {
                    error!("Failed to bind to port: {}", e);
                    std::process::exit(1); // TODO: this should send a message to the vm. VmOp::FatalError?
                    // return;
                }
            };

            loop {
                while let Ok((stream, remote_ip)) = listener.accept().await {
                    info!("New connection from {}", &remote_ip);

                    Self::new_connection(stream, remote_ip, broker_tx.clone());
                }
            }
        });

        let _ = self.handle.set(handle);
    }

    /// Start the main loop for a single user's connection. Handles send and receives.
    fn new_connection(stream: TcpStream, remote_ip: SocketAddr, broker_tx: FlumeSender<BrokerOp>) {
        // connection ID 0 is reserved for system use
        static CONNECTION_ID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1);

        tokio::spawn(async move {
            let codec = TelnetCodec::new(4096);
            let (mut sink, mut input) = codec.framed(stream).split();

            // negotiations

            // TODO: login / auth

            let (tx, mut rx) = tokio::sync::mpsc::channel::<ConnectionOp>(128);

            let connection_id = ConnectionId(CONNECTION_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
            let connection = Connection {
                id: connection_id,
                address: remote_ip,
            };

            let Ok(_) = broker_tx.send_async(BrokerOp::NewConnection(connection, tx)).await else {
                error!(?connection_id, "Failed to send BrokerOp::NewConnection. Dropping connection.");
                return;
            };

            loop {
                tokio::select! {
                    send_to_user = rx.recv() => {
                        info!("Received message from VM: {:?}", send_to_user);
                        match send_to_user {
                            Some(ConnectionOp::SendMessage(msg)) => {
                                sink.send(TelnetEvent::Message(msg)).await.unwrap();
                            },
                            None => {
                                info!("Broker closed the channel for {}. Closing connection.", &remote_ip);
                                break;
                            }
                        }
                    }
                    received_from_user = input.next() => {
                        match received_from_user {
                            Some(Ok(msg)) => {
                                Self::handle_input_event(msg, &mut sink).await;
                            }
                            Some(Err(e)) => {
                                warn!("Error: {:?}", e);
                            }
                            None => {
                                info!("Connection closed from {}", &remote_ip);
                                break;
                            }
                        }
                    },
                }
            }
        });
    }

    async fn handle_input_event(msg: TelnetEvent, sink: &mut SplitSink<Framed<TcpStream, TelnetCodec>, TelnetEvent>) {
        match msg {
            TelnetEvent::Character(char) => {
                println!("Received character: {}", char);
            }
            TelnetEvent::Message(msg) => {
                println!("Received message: {}", msg);
                sink.send(TelnetEvent::Message("Hello, world!".to_string())).await.unwrap();
            }
            TelnetEvent::Do(option) => {
                println!("Received DO: {:?}", option);
            }
            TelnetEvent::Dont(option) => {
                println!("Received DONT: {:?}", option);
            }
            TelnetEvent::Will(option) => {
                println!("Received WILL: {:?}", option);
            }
            TelnetEvent::Wont(option) => {
                println!("Received WONT: {:?}", option);
            }
            TelnetEvent::Subnegotiate(subneg) => {
                println!("Received subnegotiation: {:?}", subneg);
            }
            TelnetEvent::GoAhead => {
                println!("Received GA");
            }
            TelnetEvent::Nop => {
                println!("Received NOP");
            }
        }
    }

    /// Stops the telnet server.
    pub fn shutdown(&mut self) {
        self.handle.take();
    }
}
