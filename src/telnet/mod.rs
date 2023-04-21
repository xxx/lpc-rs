pub mod connection;
pub mod connection_broker;
pub mod ops;

use std::net::SocketAddr;

use bytes::Bytes;
use flume::Sender as FlumeSender;
use futures::{stream::SplitSink, SinkExt, StreamExt};
use nectar::{
    event::TelnetEvent, option::TelnetOption, subnegotiation::SubnegotiationType, TelnetCodec,
};
use once_cell::sync::OnceCell;
use tokio::{
    net::{TcpListener, TcpStream, ToSocketAddrs},
    sync::mpsc,
    task::JoinHandle,
};
use tokio_util::codec::{Decoder, Framed};
use tracing::{error, info, instrument, trace, warn};

use crate::telnet::{
    ops::{BrokerOp, ConnectionOp},
};
use crate::telnet::connection::Connection;

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

            // incoming telnet connection handling loop
            loop {
                while let Ok((stream, remote_ip)) = listener.accept().await {
                    let spawn_broker_tx = broker_tx.clone();

                    let handle = tokio::spawn(async move {
                        info!("New connection from {}", &remote_ip);

                        Self::connection_loop(stream, remote_ip, spawn_broker_tx).await;
                    });

                    let Ok(_) = broker_tx.send_async(BrokerOp::NewHandle(remote_ip, handle)).await else {
                        error!("Failed to send BrokerOp::NewHandle. Please consider restarting the server.");
                        continue;
                    };
                }
            }
        });

        let _ = self.handle.set(handle);
    }

    /// Start the main loop for a single user's connection. Handles sends and receives.
    #[instrument(skip(stream, broker_tx))]
    async fn connection_loop(
        stream: TcpStream,
        remote_ip: SocketAddr,
        broker_tx: FlumeSender<BrokerOp>,
    ) {
        let codec = TelnetCodec::new(4096);
        let mut framed = codec.framed(stream);

        Self::negotiations(&mut framed, remote_ip, &broker_tx).await;

        // Disable echo for password entry. It may seem counterintuitive that we're
        // saying we WILL echo here, but it's really telling their client to stop its
        // own echoing. We're lying to the client - we're not going to echo either.
        // let _ = framed.send(TelnetEvent::Will(TelnetOption::Echo)).await;

        // TODO: login / auth
        // apply `connect` to the master
        // if an object result, apply `logon` to it
        // if an object result, it's a successful connection

        let (mut sink, mut input) = framed.split();

        let (connection_tx, mut connection_rx) = mpsc::channel::<ConnectionOp>(128);

        let connection = Connection::new(remote_ip, connection_tx.clone(), broker_tx.clone());

        let Ok(_) = broker_tx.send_async(BrokerOp::NewConnection(connection)).await else {
            error!("Failed to send BrokerOp::NewConnection. Dropping connection.");
            let msg = TelnetEvent::Message("The server is currently unable to accept new connections. Please try again shortly.".to_string());
            let _ = sink.send(msg).await;
            let _ = broker_tx.send_async(BrokerOp::Disconnect(remote_ip)).await;
            return;
        };

        loop {
            tokio::select! {
                send_to_user = connection_rx.recv() => {
                    trace!("Received message from VM: {:?}", send_to_user);

                    match send_to_user {
                        Some(ConnectionOp::SendMessage(msg)) => {
                            if let Err(e) = sink.send(TelnetEvent::Message(msg)).await {
                                error!("Failed to send message to user: {}", e);
                            }
                        },
                        None => {
                            info!("Broker closed the channel for {}. Closing connection.", &remote_ip);
                            let _ = broker_tx.send_async(BrokerOp::Disconnect(remote_ip)).await;
                            break;
                        }
                    }
                }
                received_from_user = input.next() => {
                    match received_from_user {
                        Some(Ok(msg)) => {
                            println!("Received message from user: {:?}", msg);
                            Self::handle_input_event(msg, &mut sink, remote_ip, &broker_tx).await;
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

    async fn negotiations(
        framed: &mut Framed<TcpStream, TelnetCodec>,
        _remote_ip: SocketAddr,
        _broker_tx: &FlumeSender<BrokerOp>,
    ) {
        // CHARSET negotiation
        let _ = framed.send(TelnetEvent::Will(TelnetOption::Charset)).await;
        loop {
            match framed.next().await {
                // *We* send the requests, so we reject their charset suggestion.
                // This is technically not to spec (we should only do this after we send the
                // request ourselves, and they respond with a charset request).
                Some(Ok(TelnetEvent::Subnegotiate(SubnegotiationType::CharsetRequest(_data)))) => {
                    let _ = framed
                        .send(TelnetEvent::Subnegotiate(
                            SubnegotiationType::CharsetRejected,
                        ))
                        .await;
                }
                Some(Ok(TelnetEvent::Do(TelnetOption::Charset))) => {
                    info!(
                        "matching CHARSET negotiation result: {:?}",
                        TelnetEvent::Do(TelnetOption::Charset)
                    );

                    let _ = framed
                        .send(TelnetEvent::Subnegotiate(
                            SubnegotiationType::CharsetRequest(vec![Bytes::from("UTF-8")]),
                        ))
                        .await;
                    // let _ = broker_tx.send_async(BrokerOp::SetCharset(remote_ip, data)).await;
                }
                Some(Ok(TelnetEvent::Subnegotiate(SubnegotiationType::CharsetAccepted(data)))) => {
                    info!("Charset accepted: {:?}", data);
                    break;
                    // let _ = broker_tx.send_async(BrokerOp::SetCharset(remote_ip, data)).await;
                }
                Some(Ok(TelnetEvent::Subnegotiate(SubnegotiationType::CharsetRejected))) => {
                    info!("CHARSET rejected");
                    break;
                    // let _ = broker_tx.send_async(BrokerOp::SetCharset(remote_ip, data)).await;
                }
                x => {
                    trace!("CHARSET negotiation result: {:?}", x);
                    break;
                }
            }
        }

        // let _ = framed.send(TelnetEvent::Will(TelnetOption::MSSP)).await;
        // let result = framed.next().await;
        // println!("MSSP negotiation result: {:?}", result);
        //
        // let _ = framed.send(TelnetEvent::Will(GMCP)).await;
        // let result = framed.next().await;
        // println!("GMCP negotiation result: {:?}", result);
    }

    async fn handle_input_event(
        msg: TelnetEvent,
        _sink: &mut SplitSink<Framed<TcpStream, TelnetCodec>, TelnetEvent>,
        _remote_ip: SocketAddr,
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
