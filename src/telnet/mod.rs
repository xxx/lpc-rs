pub mod connection;
pub mod ops;

use std::{net::SocketAddr, sync::Arc, time::Duration};

use bytes::BytesMut;
use indexmap::IndexMap;
use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::lpc_error;
use lpc_rs_telnet::{Event, MAX_LINE, Op, Session};
use once_cell::sync::OnceCell;
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    net::{TcpListener, ToSocketAddrs},
    sync::mpsc,
    task::JoinHandle,
};
use tracing::{error, info, instrument, trace, warn};

use crate::{
    command::command_task::run_command_line,
    interpreter::{
        CommittedReader, GET_MUD_STATS, GMCP, NET_DEAD, WINDOW_SIZE, WRITE_PROMPT,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        task::{
            apply_function::{
                apply_function, apply_function_by_name, apply_function_in_master,
                apply_runtime_error,
            },
            task_template::TaskTemplate,
        },
        vm::{global_state::PreparedCall, vm_op::VmOp},
    },
    telnet::{
        connection::{Connection, InputTo},
        ops::ConnectionOp,
    },
};

/// The listener: accepts clients and runs one loop per connection.
#[derive(Debug, Default)]
pub struct Telnet {
    /// The acceptor task; dropping it stops new connections, not existing ones.
    handle: OnceCell<JoinHandle<()>>,
}

/// Which side ended a connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Departure {
    /// The driver closed it: a `Close`, or a login that could not start.
    Server,
    /// The client's socket ended or failed.
    Client,
}

/// What an op told the loop to do next.
enum Flow {
    Continue,
    Leave(Departure),
}

impl Telnet {
    /// Creates a new [`Telnet`] instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Bind `address` and start accepting; `Err` when the bind fails. A
    /// second call is a no-op.
    pub async fn run<A>(&self, address: A, template: TaskTemplate) -> lpc_rs_errors::Result<()>
    where
        A: ToSocketAddrs + Send + 'static,
    {
        if self.handle.get().is_some() {
            return Ok(());
        }

        let listener = TcpListener::bind(address)
            .await
            .map_err(|e| lpc_error!("telnet failed to bind its listener: {e}"))?;
        info!(
            "Listening for connections on {}",
            listener
                .local_addr()
                .map_or_else(|e| e.to_string(), |a| a.to_string())
        );

        let handle = tokio::spawn(async move {
            loop {
                match listener.accept().await {
                    Ok((stream, remote_ip)) => {
                        let template = template.clone();
                        tokio::spawn(async move {
                            info!("New connection from {}", &remote_ip);
                            Self::connection_loop(stream, remote_ip, template).await;
                        });
                    }
                    Err(e) => {
                        // A sticky error (EMFILE/ENFILE) would otherwise spin
                        // this loop and flood the log.
                        warn!("accept failed: {e}");
                        tokio::time::sleep(Duration::from_millis(100)).await;
                    }
                }
            }
        });

        let _ = self.handle.set(handle);
        Ok(())
    }

    /// Start the main loop for a single user's connection. Handles sends and receives.
    #[instrument(skip(stream, template))]
    async fn connection_loop<S>(mut stream: S, remote_ip: SocketAddr, template: TaskTemplate)
    where
        S: AsyncRead + AsyncWrite + Unpin + Send + 'static,
    {
        let mut session = Session::new();
        let mut out = BytesMut::with_capacity(4096);
        let (connection_tx, mut connection_rx) = mpsc::unbounded_channel::<ConnectionOp>();
        let connection = Arc::new(Connection::new(remote_ip, connection_tx));
        let global_state = template.global_state.clone();
        global_state.registry.insert(connection.clone());

        if global_state
            .tx
            .send(VmOp::InitiateLogin(connection.clone()))
            .await
            .is_err()
        {
            error!("Failed to send VmOp::InitiateLogin. Dropping connection.");
            session.send(Op::Text(
                "The server is currently unable to accept new connections. Please try again shortly.\n",
            ));
            let _ = flush(&mut session, &mut out, &mut stream).await;
            Self::leave(&connection, &template, Departure::Server, false).await;
            return;
        }

        // The offers go out before anything else; a client that never
        // answers them still gets its login.
        if let Err(e) = flush(&mut session, &mut out, &mut stream).await {
            warn!("Failed to send to {}: {}", &remote_ip, e);
            Self::leave(&connection, &template, Departure::Client, false).await;
            return;
        }

        let mut shutting_down = false;
        let mut buf = [0u8; 4096];
        // The master's MSSP contribution, run at most once per connection.
        let mut mud_stats: Option<IndexMap<String, Vec<String>>> = None;

        let departure = loop {
            let flow = tokio::select! {
                op = connection_rx.recv() => {
                    Self::handle_op(op, &mut session, &connection, &template, &mut shutting_down).await
                }
                read = stream.read(&mut buf) => {
                    match read {
                        Ok(0) => {
                            info!("Connection closed by {}.", &remote_ip);
                            Flow::Leave(Departure::Client)
                        }
                        Ok(n) => {
                            session.feed(&buf[..n]);
                            connection.refresh(&session);
                            let mut flow = Flow::Continue;
                            while let Some(event) = session.next_event() {
                                Self::handle_event(event, &mut session, &connection, &template, shutting_down, &mut mud_stats).await;
                                // What the event queued lands before the next
                                // event: a second line in the same read must
                                // see the first line's `input_to`.
                                while let Ok(op) = connection_rx.try_recv() {
                                    flow = Self::handle_op(Some(op), &mut session, &connection, &template, &mut shutting_down).await;
                                    if matches!(flow, Flow::Leave(_)) {
                                        break;
                                    }
                                }
                                if matches!(flow, Flow::Leave(_)) {
                                    break;
                                }
                            }
                            flow
                        }
                        Err(e) => {
                            warn!("User input error for {}: {:?}", &remote_ip, e);
                            Flow::Leave(Departure::Client)
                        }
                    }
                }
            };

            if let Flow::Leave(departure) = flow {
                // The driver's close sends what was queued first; a client
                // that left cannot hear it.
                if departure == Departure::Server {
                    let _ = flush(&mut session, &mut out, &mut stream).await;
                }
                break departure;
            }

            if let Err(e) = flush(&mut session, &mut out, &mut stream).await {
                warn!("Failed to send to {}: {}", &remote_ip, e);
                break Departure::Client;
            }
        };

        Self::leave(&connection, &template, departure, shutting_down).await;
    }

    /// One op from the VM side; `None` is the channel closing.
    async fn handle_op(
        op: Option<ConnectionOp>,
        session: &mut Session,
        connection: &Connection,
        template: &TaskTemplate,
        shutting_down: &mut bool,
    ) -> Flow {
        trace!("Received message from VM: {:?}", op);
        match op {
            Some(ConnectionOp::SendMessage(msg)) => session.send(Op::Text(&msg)),
            Some(ConnectionOp::InputTo(input_to)) => {
                if input_to.no_echo {
                    session.send(Op::EchoOff);
                }
                connection.set_input_to(Some(input_to));
            }
            Some(ConnectionOp::Gmcp { package, payload }) => session.send(Op::Gmcp {
                package: &package,
                payload: &payload,
            }),
            Some(ConnectionOp::Mxp(markup)) => session.send(Op::Mxp(&markup)),
            Some(ConnectionOp::Prompt(text)) => session.send(Op::Prompt(&text)),
            Some(ConnectionOp::Attached) => {
                if let Some((cols, rows)) = session.naws()
                    && !*shutting_down
                {
                    Self::window_size(cols, rows, connection, template).await;
                }
            }
            Some(ConnectionOp::PromptCycle) => {
                if !*shutting_down {
                    Self::prompt_cycle(session, connection, template).await;
                }
            }
            Some(ConnectionOp::Shutdown) => {
                *shutting_down = true;
                trace!("Shutting down connection for {}", connection.address);
            }
            Some(ConnectionOp::Close) => {
                info!("Closing connection for {}.", connection.address);
                return Flow::Leave(Departure::Server);
            }
            None => {
                info!(
                    "Channel closed for {}. Closing connection.",
                    connection.address
                );
                return Flow::Leave(Departure::Server);
            }
        }
        Flow::Continue
    }

    /// Every exit path: unbind whatever body the connection had, tell it
    /// if the client left, then leave the registry.
    async fn leave(
        connection: &Arc<Connection>,
        template: &TaskTemplate,
        departure: Departure,
        shutting_down: bool,
    ) {
        let global_state = &template.global_state;
        let body = global_state.detach(connection, None).await;
        if let Some(body) = body
            && departure == Departure::Client
            && !shutting_down
        {
            Self::net_dead(body, template).await;
        }
        global_state.registry.remove(connection);
    }

    /// `net_dead()` on the body the client left; it is no longer
    /// interactive by the time it runs.
    async fn net_dead(body: Arc<Process>, template: &TaskTemplate) {
        Self::apply_on(body, NET_DEAD, &[], template).await;
    }

    /// Apply `name` on `body` as `this_player`; no apply is nothing, an
    /// error goes to `error_handler`.
    async fn apply_on(body: Arc<Process>, name: &str, args: &[LpcRef], template: &TaskTemplate) {
        let template = template.clone();
        template.set_this_player(Some(body.clone()));
        let timeout = template.global_state.config.max_execution_time;
        if let Some(Err(e)) =
            apply_function_by_name(name, args, body.clone(), template.clone(), Some(timeout)).await
        {
            apply_runtime_error(&e, Some(body), template).await;
        }
    }

    /// One thing the client did.
    async fn handle_event(
        event: Event,
        session: &mut Session,
        connection: &Connection,
        template: &TaskTemplate,
        shutting_down: bool,
        mud_stats: &mut Option<IndexMap<String, Vec<String>>>,
    ) {
        match event {
            Event::Line(line) => {
                if shutting_down {
                    return;
                }
                if let Some(input_to) = connection.take_input_to() {
                    Self::resolve_input_to(&input_to, &line, session, connection, template).await;
                    Self::request_prompt(connection);
                    return;
                }
                let Some(proc) = connection.body() else {
                    warn!("No process for connection. Ignoring input.");
                    return;
                };
                let template = template.clone();
                template.set_this_player(Some(proc.clone()));
                if let Err(e) = run_command_line(&template, proc, line).await {
                    apply_runtime_error(&e, connection.body(), template.clone()).await;
                }
                Self::request_prompt(connection);
            }
            Event::LineTruncated => warn!(
                "Input from {} exceeded {} bytes; the rest was dropped",
                connection.address, MAX_LINE
            ),
            Event::Naws { cols, rows } => {
                if !shutting_down {
                    Self::window_size(cols, rows, connection, template).await;
                }
            }
            Event::Charset(name) => trace!("{} charset is {}", connection.address, name),
            Event::Gmcp { package, payload } => {
                if shutting_down {
                    return;
                }
                let args = [
                    LpcString::from(package).into(),
                    LpcString::from(payload).into(),
                ];
                Self::apply_on_body(GMCP, &args, connection, template).await;
            }
            Event::MsspRequested => {
                Self::mssp(
                    session,
                    template,
                    template.global_state.registry.logged_in(),
                    shutting_down,
                    mud_stats,
                )
                .await
            }
        }
    }

    /// Answer MSSP: the driver's defaults under whatever the master's
    /// `get_mud_stats()` says, or the defaults alone while shutting down.
    /// `mud_stats` carries the master's answer across requests — it runs
    /// once per connection, so a toggling client cannot re-run the apply.
    async fn mssp(
        session: &mut Session,
        template: &TaskTemplate,
        players: usize,
        shutting_down: bool,
        mud_stats: &mut Option<IndexMap<String, Vec<String>>>,
    ) {
        let global_state = &template.global_state;
        let uptime = global_state
            .booted_at
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let mut vars: IndexMap<String, Vec<String>> = IndexMap::new();
        vars.insert("NAME".into(), vec!["lpc-rs".into()]);
        vars.insert("PLAYERS".into(), vec![players.to_string()]);
        vars.insert("UPTIME".into(), vec![uptime.to_string()]);
        vars.insert("PORT".into(), vec![global_state.config.port.to_string()]);
        vars.insert(
            "CODEBASE".into(),
            vec![format!("lpc-rs {}", env!("CARGO_PKG_VERSION"))],
        );

        if mud_stats.is_none() && !shutting_down {
            let mut overrides = IndexMap::new();
            Self::mud_stats_into(&mut overrides, template).await;
            *mud_stats = Some(overrides);
        }
        if let Some(overrides) = mud_stats.as_ref() {
            for (key, values) in overrides {
                vars.insert(key.clone(), values.clone());
            }
        }

        let borrowed: Vec<(&str, Vec<&str>)> = vars
            .iter()
            .map(|(k, v)| (k.as_str(), v.iter().map(String::as_str).collect()))
            .collect();
        let pairs: Vec<(&str, &[&str])> =
            borrowed.iter().map(|(k, v)| (*k, v.as_slice())).collect();
        session.send(Op::Mssp(&pairs));
    }

    /// The master's `get_mud_stats()` mapping over `vars`. A string or int
    /// value is one MSSP value; an array value is many, one per string/int
    /// element. Any other value, and any other array element, is logged and
    /// skipped; a master with no mapping to give leaves `vars` untouched.
    async fn mud_stats_into(vars: &mut IndexMap<String, Vec<String>>, template: &TaskTemplate) {
        let global_state = &template.global_state;
        let timeout = global_state.config.max_execution_time;
        let mapping =
            match apply_function_in_master(GET_MUD_STATS, &[], template.clone(), Some(timeout))
                .await
            {
                Some(Ok(LpcRef::Mapping(cell))) => global_state.committed_mapping(cell.id),
                Some(Ok(other)) => {
                    global_state
                        .config
                        .debug_log(format!(
                            "get_mud_stats returned a {}; defaults only",
                            other.type_name()
                        ))
                        .await;
                    None
                }
                Some(Err(e)) => {
                    global_state
                        .config
                        .debug_log(format!("get_mud_stats failed: {}", e.diagnostic_string()))
                        .await;
                    None
                }
                None => None,
            };
        let Some(mapping) = mapping else {
            return;
        };
        for (key, value) in mapping.iter() {
            let values = match value {
                LpcRef::String(_) | LpcRef::Int(_) => vec![value.to_string()],
                LpcRef::Array(cell) => match global_state.committed_array(cell.id) {
                    Some(array) => {
                        let mut elements = Vec::new();
                        for element in array.iter() {
                            match element {
                                LpcRef::String(_) | LpcRef::Int(_) => {
                                    elements.push(element.to_string())
                                }
                                other => {
                                    global_state
                                        .config
                                        .debug_log(format!(
                                            "get_mud_stats: {key} has a {} element; skipped",
                                            other.type_name()
                                        ))
                                        .await;
                                }
                            }
                        }
                        elements
                    }
                    None => continue,
                },
                other => {
                    global_state
                        .config
                        .debug_log(format!(
                            "get_mud_stats: {key} is a {}; skipped",
                            other.type_name()
                        ))
                        .await;
                    continue;
                }
            };
            vars.insert(key.to_string(), values);
        }
    }

    /// `apply_on` the bound body; no body is a trace.
    async fn apply_on_body(
        name: &str,
        args: &[LpcRef],
        connection: &Connection,
        template: &TaskTemplate,
    ) {
        let Some(body) = connection.body() else {
            trace!("{} has no body for {name}", connection.address);
            return;
        };
        Self::apply_on(body, name, args, template).await;
    }

    /// `window_size(cols, rows)` on the bound body.
    async fn window_size(cols: u16, rows: u16, connection: &Connection, template: &TaskTemplate) {
        let args = [
            LpcRef::from(cols as LpcIntInner),
            LpcRef::from(rows as LpcIntInner),
        ];
        Self::apply_on_body(WINDOW_SIZE, &args, connection, template).await;
    }

    /// Queue the prompt cycle behind everything the command just sent.
    fn request_prompt(connection: &Connection) {
        let _ = connection.send(ConnectionOp::PromptCycle);
    }

    /// The mark alone behind a pending `input_to`; else the body's
    /// `write_prompt`, whose text is queued so its own output goes first;
    /// a body without the apply gets nothing.
    async fn prompt_cycle(session: &mut Session, connection: &Connection, template: &TaskTemplate) {
        if connection.awaits_input() {
            session.send(Op::Prompt(""));
            return;
        }
        let Some(body) = connection.body() else {
            return;
        };
        if !body.program.unmangled_functions.contains_key(WRITE_PROMPT) {
            return;
        }
        let template = template.clone();
        template.set_this_player(Some(body.clone()));
        let timeout = template.global_state.config.max_execution_time;
        let text = match apply_function_by_name(
            WRITE_PROMPT,
            &[],
            body.clone(),
            template.clone(),
            Some(timeout),
        )
        .await
        {
            Some(Ok(LpcRef::String(s))) => s.to_str().to_owned(),
            Some(Ok(_)) | None => String::new(),
            Some(Err(e)) => {
                apply_runtime_error(&e, Some(body), template).await;
                String::new()
            }
        };
        let _ = connection.send(ConnectionOp::Prompt(text));
    }

    async fn resolve_input_to(
        input_to: &InputTo,
        msg: &str,
        session: &mut Session,
        connection: &Connection,
        template: &TaskTemplate,
    ) {
        if input_to.no_echo {
            session.send(Op::EchoOn);
        }

        let input: LpcRef = LpcString::from(msg).into();
        let prepared = template
            .global_state
            .prepare_function_ptr(
                &input_to.ptr,
                std::slice::from_ref(&input),
                connection.body(),
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
                session.send(Op::Text("Canceled.\n"));
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

/// Write whatever the session has queued.
async fn flush<S: AsyncWrite + Unpin>(
    session: &mut Session,
    out: &mut BytesMut,
    stream: &mut S,
) -> std::io::Result<()> {
    session.drain_output(out);
    if out.is_empty() {
        return Ok(());
    }
    let written = stream.write_all(&out[..]).await;
    out.clear();
    written
}

#[cfg(test)]
mod tests {
    use std::net::ToSocketAddrs;

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

        let (connection_tx, _connection_rx) = mpsc::unbounded_channel();

        let mut session = Session::new();

        let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
        let connection = Connection::new(addr, connection_tx);
        let input_to = InputTo {
            ptr: Arc::new(ptr),
            no_echo: false,
        };

        Telnet::resolve_input_to(
            &input_to,
            "hello",
            &mut session,
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

            let (connection_tx, _connection_rx) = mpsc::unbounded_channel();

            let mut session = Session::new();

            let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
            let connection = Connection::new(addr, connection_tx);
            let input_to = InputTo {
                ptr: Arc::new(ptr),
                no_echo: false,
            };

            Telnet::resolve_input_to(
                &input_to,
                "hello",
                &mut session,
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
            let (connection_tx, _connection_rx) = mpsc::unbounded_channel();
            let mut session = Session::new();
            let addr = "127.0.0.1:12343".to_socket_addrs().unwrap().next().unwrap();
            let connection = Connection::new(addr, connection_tx);
            let input_to = InputTo {
                ptr,
                no_echo: false,
            };
            Telnet::resolve_input_to(
                &input_to,
                "hello",
                &mut session,
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

        use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

        use super::*;
        use crate::{
            interpreter::{
                function_type::{
                    function_address::FunctionAddress, function_ptr::FunctionPtrBuilder,
                },
                vm::Vm,
            },
            telnet::connection::Snapshot,
        };

        const ADDR: &str = "10.0.0.1:4000";

        const IAC: u8 = 255;
        const WILL: u8 = 251;
        const WONT: u8 = 252;
        const DO: u8 = 253;
        const DONT: u8 = 254;
        const ECHO: u8 = 1;
        const EOR: u8 = 25;
        const NAWS: u8 = 31;
        const CHARSET: u8 = 42;
        const MSSP: u8 = 70;
        const MXP: u8 = 91;
        const GMCP: u8 = 201;
        const SB: u8 = 250;
        const SE: u8 = 240;
        const GA: u8 = 249;
        const EOR_CMD: u8 = 239;

        /// The loop running on one end of a duplex; the test holds the other.
        struct Wired {
            client: DuplexStream,
            connection: Arc<Connection>,
            vm: Vm,
        }

        /// Spawn the loop and return once it has registered itself and sent
        /// its offers. The client says nothing: login is not gated on it.
        async fn wire() -> Wired {
            let vm = Vm::new(test_config());
            let (mut client, server) = tokio::io::duplex(4096);
            let addr: SocketAddr = ADDR.parse().expect("a literal address");
            tokio::spawn(Telnet::connection_loop(
                server,
                addr,
                TaskTemplate::from(vm.global_state.clone()),
            ));
            let registry = &vm.global_state.registry;
            let connection = within(async {
                loop {
                    if let Some(connection) = registry.get(addr) {
                        break connection;
                    }
                    tokio::time::sleep(Duration::from_millis(5)).await;
                }
            })
            .await;
            assert_eq!(
                read_n(&mut client, 18).await,
                [
                    IAC, DO, NAWS, IAC, WILL, CHARSET, IAC, WILL, GMCP, IAC, WILL, MXP, IAC, WILL,
                    EOR, IAC, WILL, MSSP
                ]
            );
            Wired {
                client,
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

        /// Poll `probe` until it holds, within the timeout.
        async fn eventually<F: Fn() -> bool>(probe: F) {
            within(async {
                while !probe() {
                    tokio::time::sleep(Duration::from_millis(5)).await;
                }
            })
            .await
        }

        #[tokio::test]
        async fn a_new_connection_registers_and_asks_for_its_login() {
            let mut w = wire().await;
            let addr: SocketAddr = ADDR.parse().unwrap();
            assert_eq!(w.connection.address, addr);
            assert_eq!(w.vm.global_state.registry.len(), 1);
            assert!(
                matches!(w.vm.next_op(), Some(VmOp::InitiateLogin(c)) if c.address == addr),
                "the loop asks the VM for its login"
            );
        }

        #[tokio::test]
        async fn the_snapshot_follows_the_wire() {
            let mut w = wire().await;
            assert_eq!(*w.connection.snapshot(), Snapshot::default());
            w.client
                .write_all(&[IAC, DO, GMCP, IAC, 250, NAWS, 0, 100, 0, 40, IAC, 240])
                .await
                .unwrap();
            eventually(|| w.connection.snapshot().cols == 100).await;
            let snapshot = w.connection.snapshot();
            assert_eq!(
                (snapshot.rows, snapshot.gmcp, snapshot.mxp),
                (40, true, false)
            );
        }

        #[tokio::test]
        async fn a_send_message_reaches_the_client() {
            let mut w = wire().await;
            w.connection
                .send(ConnectionOp::SendMessage("hi\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 4).await, b"hi\r\n");
        }

        #[tokio::test]
        async fn a_charset_do_is_answered_with_our_request() {
            let mut w = wire().await;
            w.client.write_all(&[IAC, DO, CHARSET]).await.unwrap();
            let mut expected = vec![IAC, 250, CHARSET, 1, b' '];
            expected.extend(b"UTF-8");
            expected.extend([IAC, 240]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
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
            w.connection.set_body(Some(proc.clone()));

            w.connection
                .send(ConnectionOp::InputTo(InputTo {
                    ptr: Arc::new(ptr),
                    no_echo: true,
                }))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WILL, ECHO]);
            // A real client acks WILL ECHO with DO ECHO; without it EchoOn
            // queues its WONT and sends nothing.
            w.client.write_all(&[IAC, DO, ECHO]).await.unwrap();

            w.client.write_all(b"hello\r\n").await.unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WONT, ECHO]);

            // The loop handles one op at a time, so this message arrives only
            // after the pointer has fired and committed.
            w.connection
                .send(ConnectionOp::SendMessage("done\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 6).await, b"done\r\n");
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(165)
            );
        }

        #[tokio::test]
        async fn close_sends_what_was_queued_first() {
            let mut w = wire().await;
            w.connection
                .send(ConnectionOp::SendMessage("bye\n".into()))
                .unwrap();
            w.connection.send(ConnectionOp::Close).unwrap();
            assert_eq!(read_n(&mut w.client, 5).await, b"bye\r\n");
            let mut rest = [0u8; 8];
            assert_eq!(
                within(w.client.read(&mut rest)).await.unwrap(),
                0,
                "EOF after Close"
            );
            eventually(|| w.vm.global_state.registry.is_empty()).await;
        }

        #[tokio::test]
        async fn a_client_that_hangs_up_leaves_the_registry() {
            let w = wire().await;
            drop(w.client);
            eventually(|| w.vm.global_state.registry.is_empty()).await;
            assert!(w.connection.is_dead());
        }

        #[tokio::test]
        async fn a_gmcp_op_goes_out_once_gmcp_is_on() {
            let mut w = wire().await;
            w.client.write_all(&[IAC, DO, GMCP]).await.unwrap();
            eventually(|| w.connection.snapshot().gmcp).await;
            w.connection
                .send(ConnectionOp::Gmcp {
                    package: "Char.Vitals".into(),
                    payload: "{}".into(),
                })
                .unwrap();
            let mut expected = vec![IAC, SB, GMCP];
            expected.extend(b"Char.Vitals {}");
            expected.extend([IAC, SE]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }

        #[tokio::test]
        async fn a_gmcp_op_is_dropped_while_gmcp_is_off() {
            let mut w = wire().await;
            w.connection
                .send(ConnectionOp::Gmcp {
                    package: "Char.Vitals".into(),
                    payload: "{}".into(),
                })
                .unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
        }

        #[tokio::test]
        async fn mxp_markup_goes_out_raw_while_text_is_escaped() {
            let mut w = wire().await;
            w.client.write_all(&[IAC, DO, MXP]).await.unwrap();
            assert_eq!(read_n(&mut w.client, 5).await, [IAC, SB, MXP, IAC, SE]);
            w.connection
                .send(ConnectionOp::SendMessage("<b>\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 11).await, b"&lt;b&gt;\r\n");
            w.connection
                .send(ConnectionOp::Mxp("<b>x</b>".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 8).await, b"<b>x</b>");
        }

        #[tokio::test]
        async fn a_prompt_ends_with_the_negotiated_mark() {
            let mut w = wire().await;
            w.connection
                .send(ConnectionOp::Prompt("> ".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 4).await, [b'>', b' ', IAC, GA]);
            w.client.write_all(&[IAC, DO, EOR]).await.unwrap();
            eventually(|| w.connection.snapshot().eor).await;
            w.connection
                .send(ConnectionOp::Prompt("> ".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 4).await, [b'>', b' ', IAC, EOR_CMD]);
        }

        #[tokio::test]
        async fn attached_is_quiet_on_the_wire() {
            let mut w = wire().await;
            w.connection.send(ConnectionOp::Attached).unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
        }

        /// A body bound to the loop's connection, initialized from `code`.
        async fn bind(w: &Wired, code: &str) -> Arc<crate::interpreter::process::Process> {
            let proc =
                w.vm.initialize_process_from_code("/body.c", code)
                    .await
                    .unwrap()
                    .context
                    .process;
            w.connection.set_body(Some(proc.clone()));
            proc
        }

        fn gmcp_frame(text: &str) -> Vec<u8> {
            let mut frame = vec![IAC, SB, GMCP];
            frame.extend(text.as_bytes());
            frame.extend([IAC, SE]);
            frame
        }

        const NAWS_100_40: [u8; 9] = [IAC, SB, NAWS, 0, 100, 0, 40, IAC, SE];

        #[tokio::test]
        async fn a_gmcp_message_is_applied_on_the_body() {
            let mut w = wire().await;
            let proc = bind(
                &w,
                "string package; string payload;\nvoid gmcp(string p, string j) { package = p; payload = j; }",
            )
            .await;
            w.client.write_all(&[IAC, DO, GMCP]).await.unwrap();
            w.client
                .write_all(&gmcp_frame("Core.Hello {\"client\":\"t\"}"))
                .await
                .unwrap();
            eventually(|| w.vm.global_state.committed_global(&proc, 0u16) != LpcRef::from(0)).await;
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16).to_string(),
                "Core.Hello"
            );
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 1u16).to_string(),
                "{\"client\":\"t\"}"
            );
        }

        #[tokio::test]
        async fn naws_is_applied_as_window_size() {
            let mut w = wire().await;
            let proc = bind(
                &w,
                "int c; int r;\nvoid window_size(int cols, int rows) { c = cols; r = rows; }",
            )
            .await;
            w.client.write_all(&NAWS_100_40).await.unwrap();
            eventually(|| w.vm.global_state.committed_global(&proc, 0u16) == LpcRef::from(100))
                .await;
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 1u16),
                LpcRef::from(40)
            );
        }

        #[tokio::test]
        async fn a_size_reported_before_the_body_is_replayed_at_attach() {
            let mut w = wire().await;
            w.client.write_all(&NAWS_100_40).await.unwrap();
            eventually(|| w.connection.snapshot().cols == 100).await;
            let proc = bind(
                &w,
                "int calls; int c;\nvoid window_size(int cols, int rows) { calls += 1; c = cols; }",
            )
            .await;
            w.connection.send(ConnectionOp::Attached).unwrap();
            eventually(|| w.vm.global_state.committed_global(&proc, 0u16) == LpcRef::from(1)).await;
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 1u16),
                LpcRef::from(100)
            );
        }

        #[tokio::test]
        async fn attach_without_a_size_applies_nothing() {
            let mut w = wire().await;
            let proc = bind(
                &w,
                "int calls;\nvoid window_size(int cols, int rows) { calls += 1; }",
            )
            .await;
            w.connection.send(ConnectionOp::Attached).unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(0)
            );
        }

        #[tokio::test]
        async fn gmcp_is_delivered_behind_a_pending_input_to() {
            let mut w = wire().await;
            let proc = bind(
                &w,
                "string package;\nvoid gmcp(string p, string j) { package = p; }\nvoid line(string s) {}",
            )
            .await;
            let func = proc.program.lookup_function("line").unwrap().clone();
            let ptr = FunctionPtrBuilder::default()
                .address(FunctionAddress::Local(Arc::downgrade(&proc), func))
                .build()
                .unwrap();
            w.connection
                .send(ConnectionOp::InputTo(InputTo {
                    ptr: Arc::new(ptr),
                    no_echo: false,
                }))
                .unwrap();
            w.client.write_all(&[IAC, DO, GMCP]).await.unwrap();
            w.client.write_all(&gmcp_frame("Core.Ping")).await.unwrap();
            eventually(|| w.vm.global_state.committed_global(&proc, 0u16) != LpcRef::from(0)).await;
            assert!(w.connection.awaits_input(), "the input_to still waits");
        }

        #[tokio::test]
        async fn nothing_is_applied_while_shutting_down() {
            let mut w = wire().await;
            let proc = bind(
                &w,
                "int calls;\nvoid gmcp(string p, string j) { calls += 1; }\nvoid window_size(int c, int r) { calls += 1; }",
            )
            .await;
            w.connection.send(ConnectionOp::Shutdown).unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
            w.client.write_all(&[IAC, DO, GMCP]).await.unwrap();
            w.client.write_all(&gmcp_frame("Core.Ping")).await.unwrap();
            w.client.write_all(&NAWS_100_40).await.unwrap();
            eventually(|| w.connection.snapshot().cols == 100).await;
            // The snapshot refreshes before the events drain, so only the
            // sentinel's round trip proves they finished.
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(0)
            );
        }

        #[tokio::test]
        async fn a_body_without_the_apply_is_left_alone() {
            let mut w = wire().await;
            let _proc = bind(&w, "int x;").await;
            w.client.write_all(&[IAC, DO, GMCP]).await.unwrap();
            w.client.write_all(&gmcp_frame("Core.Ping")).await.unwrap();
            w.client.write_all(&NAWS_100_40).await.unwrap();
            eventually(|| w.connection.snapshot().cols == 100).await;
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
        }

        /// One MSSP reply's variables, in wire order.
        async fn read_mssp(client: &mut DuplexStream) -> Vec<(String, Vec<String>)> {
            assert_eq!(read_n(client, 3).await, [IAC, SB, MSSP]);
            let mut body = Vec::new();
            loop {
                let byte = read_n(client, 1).await[0];
                if byte != IAC {
                    body.push(byte);
                    continue;
                }
                let next = read_n(client, 1).await[0];
                if next == SE {
                    break;
                }
                body.push(next);
            }
            body.split(|&b| b == 1)
                .skip(1)
                .map(|var| {
                    let mut parts = var.split(|&b| b == 2);
                    let name = String::from_utf8(parts.next().unwrap().to_vec()).unwrap();
                    let values = parts
                        .map(|v| String::from_utf8(v.to_vec()).unwrap())
                        .collect();
                    (name, values)
                })
                .collect()
        }

        fn var<'a>(vars: &'a [(String, Vec<String>)], name: &str) -> Option<&'a [String]> {
            vars.iter()
                .find(|(n, _)| n == name)
                .map(|(_, v)| v.as_slice())
        }

        #[tokio::test]
        async fn mssp_merges_the_masters_stats_over_the_defaults() {
            let mut w = wire().await;
            let master = indoc! { r#"
                mapping get_mud_stats() {
                    return ([
                        "NAME": "Test MUD",
                        "PORTS": ({ "4000", "4001" }),
                        "AGE": 12,
                        "BAD": 1.5,
                        "MIXED": ({ "a", 1.5, 2 }),
                    ]);
                }
            "# };
            w.vm.initialize_process_from_code("/secure/master.c", master)
                .await
                .unwrap();
            assert!(w.vm.global_state.object_space.master_object().is_some());
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            let vars = read_mssp(&mut w.client).await;
            let names: Vec<&str> = vars.iter().map(|(n, _)| n.as_str()).collect();
            assert_eq!(
                names,
                [
                    "NAME", "PLAYERS", "UPTIME", "PORT", "CODEBASE", "PORTS", "AGE", "MIXED"
                ],
                "an override keeps its position; new keys are appended in mapping order"
            );
            assert_eq!(var(&vars, "NAME"), Some(&["Test MUD".to_string()][..]));
            assert_eq!(
                var(&vars, "PORTS"),
                Some(&["4000".to_string(), "4001".to_string()][..])
            );
            assert_eq!(var(&vars, "AGE"), Some(&["12".to_string()][..]));
            assert_eq!(var(&vars, "BAD"), None, "a float is skipped");
            assert_eq!(
                var(&vars, "MIXED"),
                Some(&["a".to_string(), "2".to_string()][..]),
                "a float element is skipped, its neighbors kept"
            );
            assert_eq!(var(&vars, "PLAYERS"), Some(&["0".to_string()][..]));
            assert_eq!(
                var(&vars, "PORT"),
                Some(&[w.vm.global_state.config.port.to_string()][..])
            );
            let uptime: u64 = var(&vars, "UPTIME").unwrap()[0].parse().unwrap();
            assert!(uptime > 1_700_000_000, "unix seconds");
            assert!(var(&vars, "CODEBASE").unwrap()[0].starts_with("lpc-rs "));
        }

        #[tokio::test]
        async fn mssp_without_a_usable_master_is_the_defaults() {
            let mut w = wire().await;
            w.vm.initialize_process_from_code(
                "/secure/master.c",
                "int get_mud_stats() { return 1; }",
            )
            .await
            .unwrap();
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            let vars = read_mssp(&mut w.client).await;
            let names: Vec<&str> = vars.iter().map(|(n, _)| n.as_str()).collect();
            assert_eq!(names, ["NAME", "PLAYERS", "UPTIME", "PORT", "CODEBASE"]);
            assert_eq!(var(&vars, "NAME"), Some(&["lpc-rs".to_string()][..]));
        }

        #[tokio::test]
        async fn mssp_counts_the_logged_in() {
            let mut w = wire().await;
            w.connection.set_logged_in();
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            let vars = read_mssp(&mut w.client).await;
            assert_eq!(var(&vars, "PLAYERS"), Some(&["1".to_string()][..]));
        }

        #[tokio::test]
        async fn mssp_while_shutting_down_is_the_defaults() {
            let mut w = wire().await;
            let master = indoc! { r#"
                mapping get_mud_stats() {
                    return ([ "NAME": "Test MUD" ]);
                }
            "# };
            w.vm.initialize_process_from_code("/secure/master.c", master)
                .await
                .unwrap();
            w.connection.send(ConnectionOp::Shutdown).unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            let vars = read_mssp(&mut w.client).await;
            assert_eq!(var(&vars, "NAME"), Some(&["lpc-rs".to_string()][..]));
        }

        #[tokio::test]
        async fn mssp_runs_the_master_once_per_connection() {
            let mut w = wire().await;
            let master = indoc! { r#"
                int calls;
                mapping get_mud_stats() {
                    calls += 1;
                    return ([ "NAME": "Test MUD" ]);
                }
            "# };
            let proc =
                w.vm.initialize_process_from_code("/secure/master.c", master)
                    .await
                    .unwrap()
                    .context
                    .process;
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            let vars = read_mssp(&mut w.client).await;
            assert_eq!(var(&vars, "NAME"), Some(&["Test MUD".to_string()][..]));

            // The Q method answers each toggle, and only turning the option
            // back on asks for MSSP again.
            w.client.write_all(&[IAC, DONT, MSSP]).await.unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WONT, MSSP]);
            w.client.write_all(&[IAC, DO, MSSP]).await.unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, [IAC, WILL, MSSP]);
            let vars = read_mssp(&mut w.client).await;
            assert_eq!(var(&vars, "NAME"), Some(&["Test MUD".to_string()][..]));

            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(1),
                "the cached answer served the second request"
            );
        }

        const ROOM: &str = indoc! { r#"
            void init() {
                add_action("do_look", "look");
                add_action("do_ask", "ask");
                add_action("do_spam", "spam");
            }
            int do_look() { write("seen\n"); return 1; }
            int do_ask() { write("Name: "); input_to(got_name); return 1; }
            void got_name(string s) { write("got " + s + "\n"); }
            int do_spam() {
                int i;
                for (i = 0; i < 300; i++) write("x\n");
                return 1;
            }
        "# };

        /// A body in `ROOM` with verbs, plus `extra` (a `write_prompt`, or not).
        async fn commanding_body(
            w: &Wired,
            extra: &str,
        ) -> Arc<crate::interpreter::process::Process> {
            w.vm.create_process_from_code("/room.c", ROOM)
                .await
                .unwrap();
            let code =
                format!("void create() {{ enable_commands(); move_object(\"/room\"); }}\n{extra}");
            let proc = bind(w, &code).await;
            // `bind` only sets the loop's back-pointer; `write` reaches the
            // wire through the committed connection cell a real login sets.
            w.vm.global_state
                .attach(w.connection.clone(), proc.clone())
                .await;
            proc
        }

        #[tokio::test]
        async fn a_command_ends_with_the_prompt_and_its_mark() {
            let mut w = wire().await;
            commanding_body(&w, "string write_prompt() { return \"> \"; }").await;
            w.client.write_all(&[IAC, DO, EOR]).await.unwrap();
            eventually(|| w.connection.snapshot().eor).await;
            w.client.write_all(b"look\r\n").await.unwrap();
            let mut expected = b"seen\r\n> ".to_vec();
            expected.extend([IAC, EOR_CMD]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }

        #[tokio::test]
        async fn without_eor_the_mark_is_ga() {
            let mut w = wire().await;
            commanding_body(&w, "string write_prompt() { return \"> \"; }").await;
            w.client.write_all(b"look\r\n").await.unwrap();
            let mut expected = b"seen\r\n> ".to_vec();
            expected.extend([IAC, GA]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }

        #[tokio::test]
        async fn a_prompt_that_writes_and_returns_nothing_still_gets_its_mark() {
            let mut w = wire().await;
            commanding_body(&w, "void write_prompt() { write(\"$ \"); }").await;
            w.client.write_all(b"look\r\n").await.unwrap();
            let mut expected = b"seen\r\n$ ".to_vec();
            expected.extend([IAC, GA]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }

        #[tokio::test]
        async fn a_pending_input_to_gets_the_mark_alone() {
            let mut w = wire().await;
            commanding_body(&w, "string write_prompt() { return \"> \"; }").await;
            w.client.write_all(b"ask\r\n").await.unwrap();
            let mut expected = b"Name: ".to_vec();
            expected.extend([IAC, GA]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
            // The callback's line: its own cycle, and now the body's prompt.
            w.client.write_all(b"bob\r\n").await.unwrap();
            let mut expected = b"got bob\r\n> ".to_vec();
            expected.extend([IAC, GA]);
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }

        #[tokio::test]
        async fn a_body_without_write_prompt_gets_no_prompt_and_no_mark() {
            let mut w = wire().await;
            commanding_body(&w, "").await;
            w.client.write_all(b"look\r\n").await.unwrap();
            assert_eq!(read_n(&mut w.client, 6).await, b"seen\r\n");
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
        }

        #[tokio::test]
        async fn a_client_that_hangs_up_unbinds_its_body_and_tells_it() {
            let w = wire().await;
            let proc = commanding_body(
                &w,
                "int dead;\nvoid net_dead() { dead = interactive() ? 2 : 1; }",
            )
            .await;
            drop(w.client);
            eventually(|| w.vm.global_state.committed_global(&proc, 0u16) == LpcRef::from(1)).await;
            assert!(w.connection.body().is_none());
            assert!(w.vm.global_state.committed_connection(&proc).is_none());
            eventually(|| w.vm.global_state.registry.is_empty()).await;
        }

        #[tokio::test]
        async fn a_hang_up_while_shutting_down_is_silent() {
            let mut w = wire().await;
            let proc = commanding_body(&w, "int dead;\nvoid net_dead() { dead = 1; }").await;
            w.connection.send(ConnectionOp::Shutdown).unwrap();
            w.connection
                .send(ConnectionOp::SendMessage("z\n".into()))
                .unwrap();
            assert_eq!(read_n(&mut w.client, 3).await, b"z\r\n");
            drop(w.client);
            eventually(|| w.vm.global_state.registry.is_empty()).await;
            assert!(w.connection.body().is_none());
            assert_eq!(
                w.vm.global_state.committed_global(&proc, 0u16),
                LpcRef::from(0)
            );
        }

        #[tokio::test]
        async fn a_command_may_queue_more_than_the_old_channel_held() {
            let mut w = wire().await;
            commanding_body(&w, "").await;
            w.client.write_all(b"spam\r\n").await.unwrap();
            let bytes = read_n(&mut w.client, 900).await;
            assert!(bytes.chunks(3).all(|line| line == b"x\r\n"));
        }

        #[tokio::test]
        async fn two_lines_in_one_read_reach_the_input_to() {
            let mut w = wire().await;
            commanding_body(&w, "").await;
            w.client.write_all(b"ask\r\nbob\r\n").await.unwrap();
            let mut expected = b"Name: ".to_vec();
            expected.extend([IAC, GA]);
            expected.extend(b"got bob\r\n");
            assert_eq!(read_n(&mut w.client, expected.len()).await, expected);
        }
    }
}
