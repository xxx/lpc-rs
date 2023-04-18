pub mod connection_broker;
pub mod ops;

use std::net::SocketAddr;

use flume::Sender as FlumeSender;
use futures::{stream::SplitSink, SinkExt, StreamExt};
use nectar::{event::TelnetEvent, TelnetCodec};
use once_cell::sync::OnceCell;
use tokio::{
    net::{TcpListener, TcpStream, ToSocketAddrs},
    sync::mpsc,
    task::JoinHandle,
};
use tokio_util::codec::{Decoder, Framed};
use tracing::{error, info, instrument, trace, warn};

use crate::telnet::{
    connection_broker::{Connection, ConnectionId},
    ops::{BrokerOp, ConnectionOp},
};

/// The incoming connection handler. Once established, connections are handled by [`ConnectionBroker`](connection_broker::ConnectionBroker).
#[derive(Debug)]
pub struct Telnet {
    /// The handle to the main connection handler task.
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
    pub async fn run<A>(&self, address: A)
    where
        A: ToSocketAddrs + Send + 'static,
    {
        if self.handle.get().is_some() {
            return;
        }

        let broker_tx = self.broker_tx.clone();

        let handle = tokio::spawn(async move {
            let listener = match TcpListener::bind(address).await {
                Ok(listener) => listener,
                Err(e) => {
                    error!("Failed to bind to port: {}", e);
                    std::process::exit(1); // TODO: this should send a message to the vm. VmOp::FatalError?
                                           // return;
                }
            };

            info!(
                "Listening for connections on {}",
                listener.local_addr().unwrap()
            );

            // connection ID 0 is reserved for system use
            let mut connection_id: u32 = 1;

            // incoming telnet connection handling loop
            loop {
                while let Ok((stream, remote_ip)) = listener.accept().await {
                    let spawn_broker_tx = broker_tx.clone();
                    let cid = ConnectionId(connection_id);

                    connection_id += 1;

                    let handle = tokio::spawn(async move {
                        info!("New connection from {}", &remote_ip);

                        Self::set_up_connection(cid, stream, remote_ip, spawn_broker_tx).await;
                    });

                    let Ok(_) = broker_tx.send_async(BrokerOp::NewHandle(cid, handle)).await else {
                        error!("Failed to send BrokerOp::NewHandle. Dropping connection as we otherwise can't track it.");
                        handle.abort();
                        continue;
                    };
                }
            }
        });

        let _ = self.handle.set(handle);
    }

    /// Start the main loop for a single user's connection. Handles sends and receives.
    #[instrument(skip(stream, broker_tx))]
    async fn set_up_connection(
        connection_id: ConnectionId,
        stream: TcpStream,
        remote_ip: SocketAddr,
        broker_tx: FlumeSender<BrokerOp>,
    ) {
        let codec = TelnetCodec::new(4096);
        let (mut sink, mut input) = codec.framed(stream).split();

        // negotiations

        // TODO: login / auth
        // apply `connect` to the master
        // if an object result, apply `logon` to it
        // if an object result, it's a successful connection

        let (tx, mut rx) = mpsc::channel::<ConnectionOp>(128);

        let connection = Connection {
            id: connection_id,
            address: remote_ip,
        };

        let Ok(_) = broker_tx.send_async(BrokerOp::NewConnection(connection, tx)).await else {
            let msg = TelnetEvent::Message("The server is currently unable to accept new connections. Please try again shortly.".to_string());
            let _ = sink.send(msg).await;
            let _ = broker_tx.send_async(BrokerOp::Disconnect(connection_id)).await;
            error!("Failed to send BrokerOp::NewConnection. Dropping connection.");
            return;
        };

        loop {
            tokio::select! {
                send_to_user = rx.recv() => {
                    trace!("Received message from VM: {:?}", send_to_user);

                    match send_to_user {
                        Some(ConnectionOp::SendMessage(msg)) => {
                            if let Err(e) = sink.send(TelnetEvent::Message(msg)).await {
                                error!("Failed to send message to user: {}", e);
                            }
                        },
                        None => {
                            info!("Broker closed the channel for {}. Closing connection.", &remote_ip);
                            let _ = broker_tx.send_async(BrokerOp::Disconnect(connection_id)).await;
                            break;
                        }
                    }
                }
                received_from_user = input.next() => {
                    match received_from_user {
                        Some(Ok(msg)) => {
                            Self::handle_input_event(msg, &mut sink, connection_id, &broker_tx).await;
                        }
                        Some(Err(e)) => {
                            warn!("User input error: {:?}", e);
                        }
                        None => {
                            info!("Connection closed.");
                            break;
                        }
                    }
                },
            }
        }
    }

    async fn handle_input_event(
        msg: TelnetEvent,
        _sink: &mut SplitSink<Framed<TcpStream, TelnetCodec>, TelnetEvent>,
        _connection_id: ConnectionId,
        _broker_tx: &FlumeSender<BrokerOp>,
    ) {
        match msg {
            TelnetEvent::Character(char) => {
                println!("Received character: {}", char);
            }
            TelnetEvent::Message(msg) => {
                println!("Received message: {}", msg);
                // let _ = sink
                //     .send(TelnetEvent::Message("Goodbye, world!".to_string()))
                //     .await;
                // let _ = broker_tx.send_async(BrokerOp::Disconnect(connection_id)).await;
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

    /// Stops the telnet server. This will disable new connections.
    pub fn shutdown(&mut self) {
        info!("Shutting down telnet server");
        if let Some(h) = self.handle.take() {
            h.abort()
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[tokio::test]
//     async fn test_connection_loop() {
//         let (tx, _rx) = flume::unbounded();
//
//
//
//     }
// }
