pub mod connection;
pub mod connection_broker;
pub mod ops;

use std::{net::SocketAddr, sync::Arc};

use bytes::Bytes;
use flume::Sender as FlumeSender;
use futures::{SinkExt, StreamExt, stream::SplitSink};
use lpc_rs_errors::lpc_error;
use nectar::{
    TelnetCodec, event::TelnetEvent, option::TelnetOption, subnegotiation::SubnegotiationType,
};
use once_cell::sync::OnceCell;
use tokio::{
    io::{AsyncRead, AsyncWrite},
    net::{TcpListener, ToSocketAddrs},
    sync::mpsc,
    task::JoinHandle,
};
use tokio_util::codec::{Decoder, Framed};
use tracing::{error, info, instrument, trace, warn};

use crate::{
    command::command_task::run_command_line,
    interpreter::{
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        task::{
            apply_function::{apply_function, apply_runtime_error},
            task_template::TaskTemplate,
        },
        vm::global_state::PreparedCall,
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
                    let _ = broker_tx
                        .send_async(BrokerOp::FatalError(lpc_error!(
                            "telnet failed to bind its listener: {e}"
                        )))
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

                    let Ok(_) = broker_tx
                        .send_async(BrokerOp::NewHandle(remote_ip, handle))
                        .await
                    else {
                        error!(
                            "Failed to send BrokerOp::NewHandle. Please consider restarting the server."
                        );
                        continue;
                    };
                }
            }
        });

        let _ = self.handle.set(handle);
    }

    /// Start the main loop for a single user's connection. Handles sends and receives.
    #[instrument(skip(stream, broker_tx, template))]
    async fn connection_loop<S>(
        stream: S,
        remote_ip: SocketAddr,
        broker_tx: FlumeSender<BrokerOp>,
        template: TaskTemplate,
    ) where
        S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    {
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

        let Ok(_) = broker_tx
            .send_async(BrokerOp::NewConnection(connection.clone()))
            .await
        else {
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
                        Some(ConnectionOp::Close) => {
                            info!("Closing connection for {}.", &remote_ip);
                            let _ = broker_tx.send_async(BrokerOp::Disconnect(remote_ip)).await;
                            break;
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
                            info!("Connection closed by {}.", &remote_ip);
                            let _ = broker_tx.send_async(BrokerOp::Disconnect(remote_ip)).await;
                            break;
                        }
                    }
                },
            }
        }
    }

    /// Handle the Telnet negotiations when a user first connects.
    async fn negotiations<S>(
        framed: &mut Framed<S, TelnetCodec>,
        _remote_ip: SocketAddr,
        _broker_tx: &FlumeSender<BrokerOp>,
    ) where
        S: AsyncRead + AsyncWrite + Unpin,
    {
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

    async fn handle_input_event<S>(
        msg: TelnetEvent,
        sink: &mut SplitSink<Framed<S, TelnetCodec>, TelnetEvent>,
        connection: &Connection,
        template: &TaskTemplate,
    ) where
        S: AsyncRead + AsyncWrite + Unpin,
    {
        match msg {
            TelnetEvent::Character(char) => {
                trace!("Received character: {}", char);
            }
            TelnetEvent::Message(msg) => {
                if connection.input_to.load().is_some()
                    && let Some(input_to) = connection.input_to.swap(None)
                {
                    Self::resolve_input_to(&input_to, &msg, sink, connection, template).await;

                    return;
                }

                let Some(proc) = connection.process.load_full() else {
                    warn!("No process for connection. Closing.");
                    return;
                };

                let template = template.clone();
                template.set_this_player(Some(proc.clone()));
                if let Err(e) = run_command_line(&template, proc.clone(), msg).await {
                    apply_runtime_error(&e, connection.process.load_full(), template.clone()).await;
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

        let input: LpcRef = LpcString::from(msg).into();
        let prepared = template
            .global_state
            .prepare_function_ptr(
                &input_to.ptr,
                std::slice::from_ref(&input),
                connection.process.load_full(),
            )
            .await;
        let PreparedCall {
            context,
            function,
            args,
        } = match prepared {
            Ok(Some(prepared)) => prepared,
            Ok(None) => return,
            Err(_) => {
                let _ = sink
                    .send(TelnetEvent::Message("Canceled.".to_string()))
                    .await;
                return;
            }
        };

        let process = context.process.clone();
        let max_execution_time = template.global_state.config.max_execution_time;
        let result = apply_function(function, &args, context, Some(max_execution_time)).await;

        if let Err(e) = result {
            let Some(Ok(_)) = apply_runtime_error(&e, Some(process), template.clone()).await else {
                template
                    .global_state
                    .config
                    .debug_log(e.diagnostic_string())
                    .await;
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
    use thin_vec::thin_vec;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            lpc_ref::LpcRef,
            vm::Vm,
        },
        test_support::test_config,
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
            ptr: Arc::new(ptr),
            no_echo: false,
        };

        Telnet::resolve_input_to(
            &input_to,
            &"hello".to_string(),
            &mut sink,
            &connection,
            &TaskTemplate::from(vm.global_state.clone()),
        )
        .await;

        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(165)
        );
    }

    mod test_string_receivers {
        use super::*;
        use crate::interpreter::process::Process;

        async fn check(vm: &Vm, proc: Arc<Process>) {
            let ptr = FunctionPtrBuilder::default()
                .owner(Arc::downgrade(&proc))
                .address(FunctionAddress::Dynamic("foo".into()))
                .partial_args(thin_vec![Some("/foo/bar".into())])
                .build()
                .unwrap();

            let (broker_tx, _broker_rx) = flume::unbounded();
            let (connection_tx, _connection_rx) = mpsc::channel(1);

            let mut sink = FakeSink;

            let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
            let connection = Connection::new(addr, connection_tx, broker_tx.clone());
            let input_to = InputTo {
                ptr: Arc::new(ptr),
                no_echo: false,
            };

            Telnet::resolve_input_to(
                &input_to,
                &"hello".to_string(),
                &mut sink,
                &connection,
                &TaskTemplate::from(vm.global_state.clone()),
            )
            .await;

            assert_eq!(
                vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(165)
            );
            assert!(vm.global_state.is_initialized(&proc));
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

    mod upvalues {
        use super::*;

        async fn fire_the_stored_pointer(code: &str) -> LpcRef {
            let vm = Vm::new(test_config());
            let r = vm.initialize_process_from_code("/foo/bar.c", code).await;
            let proc = r.unwrap().context.process;
            let LpcRef::Function(ptr) = vm.global_state.committed_global(&proc, 1u16) else {
                panic!("global 1 holds the pointer");
            };
            let (broker_tx, _broker_rx) = flume::unbounded();
            let (connection_tx, _connection_rx) = mpsc::channel(1);
            let mut sink = FakeSink;
            let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
            let connection = Connection::new(addr, connection_tx, broker_tx.clone());
            let input_to = InputTo {
                ptr,
                no_echo: false,
            };
            Telnet::resolve_input_to(
                &input_to,
                &"hello".to_string(),
                &mut sink,
                &connection,
                &TaskTemplate::from(vm.global_state.clone()),
            )
            .await;
            vm.global_state.committed_global(&proc, 0u16)
        }

        #[tokio::test]
        async fn a_closure_fires_with_its_captures() {
            let code = r##"
                int result;
                function f;
                void create() { int j = 5; f = (: result = j + 1 :); }
            "##;
            assert_eq!(fire_the_stored_pointer(code).await, LpcRef::from(6));
        }

        #[tokio::test]
        async fn a_bound_dynamic_receiver_takes_the_input_after_it() {
            let code = r##"
                string result;
                function f;
                void create() { f = papplyv(&->name(), ({ this_object() })); }
                void name(string s) { result = s + "!"; }
            "##;
            assert_eq!(fire_the_stored_pointer(code).await.to_string(), "hello!");
        }

        #[tokio::test]
        async fn a_static_function_fires_without_the_creators_captures() {
            let code = r##"
                int result;
                function f;
                function g;
                void foo() { int k = 7; function h = (: k :); result = g(); }
                void create() { int j = 5; g = (: j :); f = &foo(); }
            "##;
            assert_eq!(fire_the_stored_pointer(code).await, LpcRef::from(5));
        }
    }

    mod over_a_duplex {
        use std::time::Duration;

        use nectar::constants::{CHARSET, DONT, ECHO, IAC, WILL, WONT};
        use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

        use super::*;
        use crate::interpreter::{
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            vm::Vm,
        };

        const ADDR: &str = "10.0.0.1:4000";

        /// The loop running on one end of a duplex; the test holds the other.
        struct Wired {
            client: DuplexStream,
            broker_rx: flume::Receiver<BrokerOp>,
            connection: Arc<Connection>,
            vm: Vm,
        }

        /// Spawn the loop, answer its CHARSET offer, and return once it has announced itself.
        async fn wire() -> Wired {
            let vm = Vm::new(test_config());
            let (mut client, server) = tokio::io::duplex(4096);
            let (broker_tx, broker_rx) = flume::unbounded();
            let addr: SocketAddr = ADDR.parse().expect("a literal address");
            tokio::spawn(Telnet::connection_loop(
                server,
                addr,
                broker_tx,
                TaskTemplate::from(vm.global_state.clone()),
            ));
            assert_eq!(read_n(&mut client, 3).await, [IAC, WILL, CHARSET]);
            client.write_all(&[IAC, DONT, CHARSET]).await.unwrap();
            let BrokerOp::NewConnection(connection) = within(broker_rx.recv_async()).await.unwrap()
            else {
                panic!("the loop announces itself first");
            };
            Wired {
                client,
                broker_rx,
                connection,
                vm,
            }
        }

        async fn read_n(client: &mut DuplexStream, n: usize) -> Vec<u8> {
            let mut buf = vec![0; n];
            within(client.read_exact(&mut buf)).await.unwrap();
            buf
        }

        async fn within<F: std::future::Future>(f: F) -> F::Output {
            tokio::time::timeout(Duration::from_secs(2), f)
                .await
                .expect("the loop answers within two seconds")
        }

        #[tokio::test]
        async fn a_new_connection_reaches_the_broker() {
            let w = wire().await;
            assert_eq!(w.connection.address, ADDR.parse::<SocketAddr>().unwrap());
        }

        #[tokio::test]
        async fn a_send_message_reaches_the_client() {
            let mut w = wire().await;
            w.connection
                .tx
                .send(ConnectionOp::SendMessage("hi\n".into()))
                .await
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"hi\n");
        }

        #[tokio::test]
        async fn input_to_turns_echo_off_and_on_around_the_line() {
            let mut w = wire().await;
            let code = "int i = 123;\nvoid foo() { i += 42; }";
            let r = w.vm.initialize_process_from_code("/foo/bar.c", code).await;
            let proc = r.unwrap().context.process;
            let func = proc.program.lookup_function("foo").unwrap().clone();
            let ptr = FunctionPtrBuilder::default()
                .address(FunctionAddress::Local(Arc::downgrade(&proc), func))
                .build()
                .unwrap();
            w.connection.process.store(Some(proc.clone()));

            w.connection
                .tx
                .send(ConnectionOp::InputTo(InputTo {
                    ptr: Arc::new(ptr),
                    no_echo: true,
                }))
                .await
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WILL, ECHO]);

            w.client.write_all(b"hello\r\n").await.unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WONT, ECHO]);

            // The loop handles one op at a time, so this message arrives only
            // after the pointer has fired and committed.
            w.connection
                .tx
                .send(ConnectionOp::SendMessage("done\n".into()))
                .await
                .unwrap();
            assert_eq!(read_n(&mut w.client, 5).await, b"done\n");
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(165)
            );
        }

        #[tokio::test]
        async fn close_sends_what_was_queued_first() {
            let mut w = wire().await;
            w.connection
                .tx
                .send(ConnectionOp::SendMessage("bye\n".into()))
                .await
                .unwrap();
            w.connection.tx.send(ConnectionOp::Close).await.unwrap();
            assert_eq!(read_n(&mut w.client, 4).await, b"bye\n");
            let mut rest = [0u8; 8];
            assert_eq!(
                within(w.client.read(&mut rest)).await.unwrap(),
                0,
                "EOF after Close"
            );
            let BrokerOp::Disconnect(addr) = within(w.broker_rx.recv_async()).await.unwrap() else {
                panic!("Close reports a disconnect");
            };
            assert_eq!(addr, ADDR.parse::<SocketAddr>().unwrap());
        }

        #[tokio::test]
        async fn a_client_that_hangs_up_is_reported() {
            let w = wire().await;
            drop(w.client);
            let BrokerOp::Disconnect(addr) = within(w.broker_rx.recv_async()).await.unwrap() else {
                panic!("a hang-up reports a disconnect");
            };
            assert_eq!(addr, ADDR.parse::<SocketAddr>().unwrap());
        }
    }
}
