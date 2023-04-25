pub mod connection;
pub mod connection_broker;
pub mod ops;

use std::{net::SocketAddr, sync::Arc};

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

use crate::{
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        into_lpc_ref::IntoLpcRef,
        lpc_string::LpcString,
        task::{apply_function::apply_function, task_template::TaskTemplate},
    },
    telnet::{
        connection::{Connection, InputTo},
        ops::{BrokerOp, ConnectionOp},
    },
};

/// The incoming connection handler. Once established, individual connections are managed by [`ConnectionBroker`](connection_broker::ConnectionBroker).
#[derive(Debug)]
pub struct Telnet {
    /// The handle to the main connection handler task. Dropping it will shut
    /// down incoming connections, but will not disconnect existing ones.
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
    pub async fn run<A>(&self, address: A, template: TaskTemplate)
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
                    let template = template.clone();

                    let handle = tokio::spawn(async move {
                        info!("New connection from {}", &remote_ip);

                        Self::connection_loop(stream, remote_ip, spawn_broker_tx, template).await;
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
    #[instrument(skip(stream, broker_tx, template))]
    async fn connection_loop(
        stream: TcpStream,
        remote_ip: SocketAddr,
        broker_tx: FlumeSender<BrokerOp>,
        template: TaskTemplate,
    ) {
        let codec = TelnetCodec::new(8192);
        let mut framed = codec.framed(stream);

        Self::negotiations(&mut framed, remote_ip, &broker_tx).await;

        let (mut sink, mut input) = framed.split();

        let (connection_tx, mut connection_rx) = mpsc::channel::<ConnectionOp>(128);

        let connection = Arc::new(Connection::new(
            remote_ip,
            connection_tx.clone(),
            broker_tx.clone(),
        ));

        let Ok(_) = broker_tx.send_async(BrokerOp::NewConnection(connection.clone())).await else {
            error!("Failed to send BrokerOp::NewConnection. Dropping connection.");
            let msg = TelnetEvent::Message("The server is currently unable to accept new connections. Please try again shortly.".to_string());
            let _ = sink.send(msg).await;
            let _ = broker_tx.send_async(BrokerOp::Disconnect(remote_ip)).await;
            return;
        };

        loop {
            let mut shutting_down = false;

            tokio::select! {
                send_to_user = connection_rx.recv() => {
                    trace!("Received message from VM: {:?}", send_to_user);

                    match send_to_user {
                        Some(ConnectionOp::SendMessage(msg)) => {
                            if let Err(e) = sink.send(TelnetEvent::RawMessage(msg)).await {
                                error!("Failed to send message to user: {}", e);
                            }
                        },
                        Some(ConnectionOp::InputTo(input_to)) => {
                            if input_to.no_echo {
                                // It may seem counterintuitive that we're saying we WILL echo here,
                                // but it's really telling their client to stop its own echoing.
                                // We're lying to the client - we're not going to echo, either.

                                sink.send(TelnetEvent::Will(TelnetOption::Echo)).await.unwrap();
                            }
                            connection.input_to.store(Some(Arc::new(input_to)));
                        }
                        Some(ConnectionOp::Shutdown) => {
                            shutting_down = true;
                            trace!("Shutting down connection for {}", &remote_ip);
                            // sink.send(TelnetEvent::Message("The server is shutting down. Please try again shortly.".to_string())).await;
                        }
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
                            if shutting_down {
                                // TODO: apply something in the player here?
                                // let _ = sink.send(TelnetEvent::Message("The server is shutting down. Please try again shortly.".to_string())).await;
                            } else {
                                Self::handle_input_event(msg, &mut sink, &connection, &template).await;
                            }
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

    /// Handle the Telnet negotiations when a user first connects.
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
                Some(Ok(TelnetEvent::Subnegotiate(SubnegotiationType::CharsetRequest(data)))) => {
                    trace!("Charset request: {:?}", data);
                    let _ = framed
                        .send(TelnetEvent::Subnegotiate(
                            SubnegotiationType::CharsetRejected,
                        ))
                        .await;
                }
                Some(Ok(TelnetEvent::Do(TelnetOption::Charset))) => {
                    trace!(
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
                    trace!("Charset accepted: {:?}", data);
                    break;
                    // let _ = broker_tx.send_async(BrokerOp::SetCharset(remote_ip, data)).await;
                }
                Some(Ok(TelnetEvent::Subnegotiate(SubnegotiationType::CharsetRejected))) => {
                    trace!("CHARSET rejected");
                    break;
                    // let _ = broker_tx.send_async(BrokerOp::SetCharset(remote_ip, data)).await;
                }
                x => {
                    trace!("Unknown CHARSET negotiation result: {:?}", x);
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
        sink: &mut SplitSink<Framed<TcpStream, TelnetCodec>, TelnetEvent>,
        connection: &Connection,
        template: &TaskTemplate,
    ) {
        match msg {
            TelnetEvent::Character(char) => {
                trace!("Received character: {}", char);
            }
            TelnetEvent::Message(msg) => {
                // TODO: don't make this swap if it's already None.
                if let Some(input_to) = connection.input_to.swap(None) {
                    Self::resolve_input_to(&input_to, &msg, sink, connection, template).await;

                    return;
                }
                info!("Received message: {}", msg);
            }
            TelnetEvent::RawMessage(msg) => {
                trace!("Received raw message: {}", msg);
            }
            TelnetEvent::Do(option) => {
                trace!("Received DO: {:?}", option);
            }
            TelnetEvent::Dont(option) => {
                trace!("Received DONT: {:?}", option);
            }
            TelnetEvent::Will(option) => {
                trace!("Received WILL: {:?}", option);
            }
            TelnetEvent::Wont(option) => {
                trace!("Received WONT: {:?}", option);
            }
            TelnetEvent::Subnegotiate(subneg) => {
                trace!("Received subnegotiation: {:?}", subneg);
            }
            TelnetEvent::GoAhead => {
                trace!("Received GA");
            }
            TelnetEvent::Nop => {
                trace!("Received NOP");
            }
        }
    }

    async fn resolve_input_to(
        input_to: &InputTo,
        msg: &String,
        sink: &mut SplitSink<Framed<TcpStream, TelnetCodec>, TelnetEvent>,
        connection: &Connection,
        template: &TaskTemplate,
    ) {
        if input_to.no_echo {
            // Tell the client to start echoing again (by saying that we won't be).
            let _ = sink.send(TelnetEvent::Wont(TelnetOption::Echo)).await;
        }

        let triple = FunctionPtr::triple(&input_to.ptr, &template.config, &template.object_space);
        let Ok((process, function, mut args)) = triple else {
            let _ = sink.send(TelnetEvent::Message("Canceled.".to_string())).await;
            return;
        };

        let arg_index: Option<usize> = input_to
            .ptr
            .read()
            .partial_args
            .iter()
            .position(|x| x.is_none());
        let input_arg = LpcString::from(msg).into_lpc_ref(&template.memory);
        if let Some(idx) = arg_index {
            args[idx] = input_arg;
        } else {
            args.push(input_arg);
        }

        let template = template.clone();
        template.set_this_player(connection.process.load_full());

        let max_execution_time = template.config.max_execution_time;
        let result = apply_function(function, &args, process, template, Some(max_execution_time)).await;
        if let Err(e) = result {
            // TODO: this should apply runtime_error() on the master.
            error!("{}", e);
        };
    }

    /// Stops the telnet server. This will disable new connections, but will _not_
    /// drop any of the existing connections.
    pub fn shutdown(&mut self) {
        info!("Shutting down telnet server & disabling new connections");
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
