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
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        into_lpc_ref::IntoLpcRef,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        object_flags::ObjectFlags,
        task::{
            apply_function::{apply_function, apply_function_by_name, apply_runtime_error},
            into_task_context::IntoTaskContext,
            task_template::TaskTemplate,
            Task,
        },
        PROCESS_INPUT,
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
                    error!("Telnet failed to bind to port: {}", &e);
                    let _ = broker_tx
                        .send_async(BrokerOp::FatalError(e.to_string()))
                        .await;
                    return;
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

        let mut shutting_down = false;

        loop {
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
                if connection.input_to.load().is_some() {
                    if let Some(input_to) = connection.input_to.swap(None) {
                        Self::resolve_input_to(&input_to, &msg, sink, connection, template).await;

                        return;
                    }
                }

                let Some(proc) = connection.process.load_full() else {
                    warn!("No process for connection. Closing.");
                    return;
                };

                let arg = LpcString::from(msg).into_lpc_ref(&template.global_state.memory);
                let timeout = Some(template.global_state.config.max_execution_time);

                let template = template.clone();
                template.this_player.store(Some(proc.clone()));

                match apply_function_by_name(PROCESS_INPUT, &[arg], proc, template.clone(), timeout)
                    .await
                {
                    Some(Ok(LpcRef::Int(LpcInt(0)))) => {
                        // TODO: notify_fail is handled here, but will require updating the apply functions to return the Task itself,
                        //       rather than just the result.
                        let _ = sink.send(TelnetEvent::Message("What?".to_string())).await;

                        // nothing else to do here unless / until add_action support is added
                    }
                    Some(Ok(_)) => {
                        // nothing to do here unless / until add_action support is added
                    }
                    Some(Err(x)) => {
                        apply_runtime_error(&x, connection.process.load_full(), template.clone())
                            .await;
                    }
                    None => {
                        let err = concat!(
                            "Error: *I* received your command, but the game hasn't implemented any way to handle it. ",
                            "Please tell the game's owner to implement `process_input` in your body."
                        );
                        let _ = sink.send(TelnetEvent::Message(err.to_string())).await;
                    }
                }
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

    async fn resolve_input_to<S>(
        input_to: &InputTo,
        msg: &String,
        sink: &mut S,
        connection: &Connection,
        template: &TaskTemplate,
    ) where
        S: SinkExt<TelnetEvent> + Unpin,
    {
        if input_to.no_echo {
            // Tell the client to start echoing again (by saying that we won't be).
            let _ = sink.send(TelnetEvent::Wont(TelnetOption::Echo)).await;
        }

        let triple = FunctionPtr::triple(
            &input_to.ptr,
            &template.global_state.config,
            &template.global_state.object_space,
        )
        .await;
        let Ok((process, function, mut args)) = triple else {
            let _ = sink.send(TelnetEvent::Message("Canceled.".to_string())).await;
            return;
        };

        if !process.flags.test(ObjectFlags::Initialized) {
            let init_template = template.clone();
            init_template.set_this_player(connection.process.load_full());
            let ctx = init_template.into_task_context(process.clone());
            if let Err(e) = Task::<MAX_CALL_STACK_SIZE>::initialize_process(ctx).await {
                let template = template.clone();
                let config = template.global_state.config.clone();

                let Some(Ok(_)) = apply_runtime_error(&e, Some(process), template).await else {
                    config.debug_log(e.diagnostic_string()).await;
                    return;
                };

                return;
            }
        }

        let arg_index: Option<usize> = input_to
            .ptr
            .partial_args
            .read()
            .iter()
            .position(|x| x.is_none());
        let input_arg = LpcString::from(msg).into_lpc_ref(&template.global_state.memory);
        if let Some(idx) = arg_index {
            args[idx] = input_arg;
        } else {
            args.push(input_arg);
        }

        let apply_template = template.clone();
        apply_template.set_this_player(connection.process.load_full());

        let max_execution_time = apply_template.global_state.config.max_execution_time;
        let result = apply_function(
            function,
            &args,
            process.clone(),
            apply_template,
            Some(max_execution_time),
        )
        .await;

        if let Err(e) = result {
            let Some(Ok(_)) = apply_runtime_error(&e, Some(process), template.clone()).await else {
                template.global_state.config.debug_log(e.diagnostic_string()).await;
                return;
            };
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

#[cfg(test)]
mod tests {
    use std::{
        net::ToSocketAddrs,
        pin::Pin,
        task::{Context, Poll},
    };

    use indoc::indoc;
    use parking_lot::RwLock;
    use thin_vec::thin_vec;

    use super::*;
    use crate::{
        interpreter::{
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            lpc_ref::LpcRef,
            object_flags::ObjectFlags,
            vm::Vm,
        },
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
    };

    struct FakeSink;
    impl futures::Sink<TelnetEvent> for FakeSink {
        type Error = ();

        fn poll_ready(
            self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
        ) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }

        fn start_send(self: Pin<&mut Self>, _item: TelnetEvent) -> Result<(), Self::Error> {
            Ok(())
        }

        fn poll_flush(
            self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
        ) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }

        fn poll_close(
            self: Pin<&mut Self>,
            _cx: &mut Context<'_>,
        ) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }
    }

    #[tokio::test]
    async fn test_resolve_input_to() {
        let code = indoc! { r#"
            int i = 123;

            void foo() {
                i += 42;
            }
        "# };

        let vm = Vm::new(test_config());

        let r = vm.initialize_process_from_code("/foo/bar.c", code).await;
        let proc = r.unwrap().context.process;
        let func = proc.program.lookup_function("foo").unwrap().clone();

        let ptr = FunctionPtrBuilder::default()
            .address(FunctionAddress::Local(Arc::downgrade(&proc), func.clone()))
            .build()
            .unwrap();

        let (broker_tx, _broker_rx) = flume::unbounded();
        let (connection_tx, _connection_rx) = mpsc::channel(1);

        let mut sink = FakeSink;

        let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
        let connection = Connection::new(addr, connection_tx, broker_tx.clone());
        let input_to = InputTo {
            ptr: vm.global_state.memory.alloc_function_arc(ptr),
            no_echo: false,
        };

        Telnet::resolve_input_to(
            &input_to,
            &"hello".to_string(),
            &mut sink,
            &connection,
            &vm.new_task_template(),
        )
        .await;

        assert_eq!(proc.globals.read().get(0).unwrap(), &LpcRef::from(165));
    }

    mod test_string_receivers {
        use super::*;
        use crate::interpreter::process::Process;

        async fn check(vm: &Vm, proc: Arc<Process>) {
            let ptr = FunctionPtrBuilder::default()
                .address(FunctionAddress::Dynamic("foo".into()))
                .partial_args(RwLock::new(thin_vec![Some(
                    "/foo/bar".into_lpc_ref(&vm.global_state.memory)
                )]))
                .build()
                .unwrap();

            let (broker_tx, _broker_rx) = flume::unbounded();
            let (connection_tx, _connection_rx) = mpsc::channel(1);

            let mut sink = FakeSink;

            let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
            let connection = Connection::new(addr, connection_tx, broker_tx.clone());
            let input_to = InputTo {
                ptr: vm.global_state.memory.alloc_function_arc(ptr),
                no_echo: false,
            };

            Telnet::resolve_input_to(
                &input_to,
                &"hello".to_string(),
                &mut sink,
                &connection,
                &vm.new_task_template(),
            )
            .await;

            assert_eq!(proc.globals.read().get(0).unwrap(), &LpcRef::from(165));
            assert!(proc.flags.test(ObjectFlags::Initialized));
        }

        #[tokio::test]
        async fn test_preinitialized_string_receiver() {
            let code = indoc! { r#"
            int i = 123;

            void foo() {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let r = vm.initialize_process_from_code("/foo/bar.c", code).await;
            let proc = r.unwrap().context.process;

            check(&vm, proc).await;
        }

        #[tokio::test]
        async fn test_noninitialized_string_receiver() {
            let code = indoc! { r#"
            int i = 123;

            void foo() {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let proc = vm
                .create_process_from_code("/foo/bar.c", code)
                .await
                .unwrap();

            check(&vm, proc).await;
        }
    }
}
