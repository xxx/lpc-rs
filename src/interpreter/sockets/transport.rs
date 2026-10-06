use std::{
    collections::HashMap,
    future::Future,
    io,
    net::SocketAddr,
    sync::{Arc, Weak},
    time::Duration,
};

use bytes::Bytes;
use lpc_rs_errors::Result;
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    sync::watch,
    task::{JoinHandle, JoinSet},
    time::{Instant, timeout},
};

use super::{
    SocketAcceptPermit, SocketAccess, SocketAdmission, SocketCommand, SocketCommands,
    SocketEventData, SocketHandle, SocketState, SocketTarget, error,
};
use crate::interpreter::{
    stm::{
        AttemptBody, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction, TxnHandle,
        commit_changeset, start_txn,
    },
    vm::global_state::GlobalState,
};

/// OS operations reported by the TCP transport, separate from LPC event names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketOperation {
    Connect,
    Listen,
    Write,
    Read,
    Accept,
    Close,
}

/// Why a physical TCP resource reached its terminal state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketCloseReason {
    Local,
    Error,
    Timeout,
}

/// Immutable native observations whose state changes have already committed.
///
/// Callback payloads also remain in a transactional inbox until acknowledged;
/// these observations do not themselves consume a callback event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SocketIoEvent {
    Ready {
        handle: SocketHandle,
        local: SocketAddr,
        remote: Option<SocketAddr>,
    },
    WriteComplete {
        handle: SocketHandle,
        sequence: u64,
    },
    Read {
        handle: SocketHandle,
        sequence: u64,
        data: Bytes,
    },
    Eof {
        handle: SocketHandle,
        sequence: u64,
    },
    Accept {
        handle: SocketHandle,
        child: SocketHandle,
    },
    Closed {
        handle: SocketHandle,
        reason: SocketCloseReason,
        operation: Option<SocketOperation>,
        error: Option<String>,
        local: Option<SocketAddr>,
        remote: Option<SocketAddr>,
    },
}

impl SocketIoEvent {
    fn handle(&self) -> SocketHandle {
        match *self {
            Self::Ready { handle, .. }
            | Self::WriteComplete { handle, .. }
            | Self::Closed { handle, .. }
            | Self::Read { handle, .. }
            | Self::Eof { handle, .. }
            | Self::Accept { handle, .. } => handle,
        }
    }
}

/// Explicitly attached native TCP workers and their bounded completion channel.
///
/// This does not enable LPC socket efuns or callback delivery. Dropping this
/// supervisor closes its physical resources without draining or callback promises.
#[derive(Debug)]
pub struct SocketTransport {
    events: flume::Receiver<SocketIoEvent>,
    stop: watch::Sender<bool>,
    task: Option<JoinHandle<()>>,
}

impl SocketTransport {
    /// Attach once to this VM's committed command stream on the current Tokio runtime.
    pub fn start(gs: &Arc<GlobalState>) -> Result<Self> {
        tokio::runtime::Handle::try_current()
            .map_err(|_| error("TCP transport requires a Tokio runtime"))?;
        let commands = gs
            .sockets
            .take_commands()
            .ok_or_else(|| error("TCP command receiver was already taken"))?;
        Ok(Self::spawn(gs, commands, TokioBackend))
    }

    fn spawn<B: Backend>(
        gs: &Arc<GlobalState>,
        commands: flume::Receiver<SocketCommands>,
        backend: B,
    ) -> Self {
        let (events_tx, events) = flume::bounded(gs.config.max_sockets);
        let (stop, stopping) = watch::channel(false);
        let publisher = Publisher::new(gs, events_tx);
        let (accepted_tx, accepted_rx) = flume::bounded(gs.config.max_sockets);
        let connect_timeout = Duration::from_millis(gs.config.socket_connect_timeout as u64);
        let close_timeout = Duration::from_millis(gs.config.socket_close_timeout as u64);
        let routing = Routing {
            io: Arc::new(NativeIo {
                backend: Arc::new(backend),
                publisher,
                connect_timeout,
                accepted: accepted_tx,
            }),
            close_timeout,
            workers: HashMap::new(),
            tasks: JoinSet::new(),
            disposals: JoinSet::new(),
        };
        let task = tokio::spawn(manager(commands, stopping, routing, accepted_rx));
        Self {
            events,
            stop,
            task: Some(task),
        }
    }

    /// Receive one committed native completion; cancellation retains queued events.
    pub async fn next_event(&self) -> Option<SocketIoEvent> {
        self.events.recv_async().await.ok()
    }

    /// Stop all workers and wait for their TCP resources to be dropped.
    pub async fn shutdown(mut self) -> Result<()> {
        let _ = self.stop.send(true);
        if let Some(task) = self.task.take() {
            task.await
                .map_err(|e| error(&format!("TCP supervisor failed: {e}")))?;
        }
        Ok(())
    }
}

impl Drop for SocketTransport {
    fn drop(&mut self) {
        let _ = self.stop.send(true);
        if let Some(task) = self.task.take() {
            task.abort();
        }
    }
}

trait Backend: Send + Sync + 'static {
    type Writer: AsyncRead + AsyncWrite + Unpin + Send + 'static;
    fn open(
        &self,
        target: SocketTarget,
    ) -> impl Future<Output = io::Result<Resource<Self::Writer>>> + Send;
    fn accept(
        &self,
        listener: &TcpListener,
    ) -> impl Future<Output = io::Result<Resource<Self::Writer>>> + Send;
}

struct TokioBackend;

enum Resource<W> {
    Stream {
        writer: W,
        local: SocketAddr,
        remote: SocketAddr,
    },
    Listener {
        listener: TcpListener,
        local: SocketAddr,
    },
}

impl Backend for TokioBackend {
    type Writer = TcpStream;
    async fn open(&self, target: SocketTarget) -> io::Result<Resource<TcpStream>> {
        match target {
            SocketTarget::Connect { host, port } => {
                let writer = TcpStream::connect((host.as_str(), port)).await?;
                Ok(Resource::Stream {
                    local: writer.local_addr()?,
                    remote: writer.peer_addr()?,
                    writer,
                })
            }
            SocketTarget::Listen(address) => {
                let listener = TcpListener::bind(address).await?;
                Ok(Resource::Listener {
                    local: listener.local_addr()?,
                    listener,
                })
            }
        }
    }
    async fn accept(&self, listener: &TcpListener) -> io::Result<Resource<TcpStream>> {
        let (writer, remote) = listener.accept().await?;
        Ok(Resource::Stream {
            local: writer.local_addr()?,
            remote,
            writer,
        })
    }
}

#[derive(Clone, Copy)]
struct Close {
    drain: bool,
    deadline: Instant,
}

struct Worker<W> {
    writes: flume::Sender<(u64, Bytes)>,
    close: watch::Sender<Option<Close>>,
    activated: watch::Sender<bool>,
    resource: Option<tokio::sync::oneshot::Sender<Resource<W>>>,
    finished: bool,
    retiring: bool,
}

struct NativeIo<B: Backend> {
    backend: Arc<B>,
    publisher: Publisher,
    connect_timeout: Duration,
    accepted: flume::Sender<(SocketHandle, Resource<B::Writer>)>,
}

struct Routing<B: Backend> {
    io: Arc<NativeIo<B>>,
    close_timeout: Duration,
    workers: HashMap<SocketHandle, Worker<B::Writer>>,
    tasks: JoinSet<SocketHandle>,
    disposals: JoinSet<()>,
}

impl<B: Backend> Routing<B> {
    fn batch(&mut self, batch: SocketCommands) {
        for command in batch {
            match command {
                SocketCommand::Open { handle, target } => {
                    let (writes_tx, writes) = flume::unbounded();
                    let (close_tx, close) = watch::channel(None);
                    let (activated, _) = watch::channel(true);
                    self.workers.insert(
                        handle,
                        Worker {
                            writes: writes_tx,
                            close: close_tx,
                            activated,
                            resource: None,
                            finished: false,
                            retiring: false,
                        },
                    );
                    let io = self.io.clone();
                    self.tasks.spawn(async move {
                        run_worker(handle, target, io, writes, close).await;
                        handle
                    });
                }
                SocketCommand::OpenAccepted { handle } => {
                    let (writes_tx, writes) = flume::unbounded();
                    let (close_tx, close) = watch::channel(None);
                    let (activated_tx, activated) = watch::channel(false);
                    let (resource_tx, resource) = tokio::sync::oneshot::channel();
                    self.workers.insert(
                        handle,
                        Worker {
                            writes: writes_tx,
                            close: close_tx,
                            activated: activated_tx,
                            resource: Some(resource_tx),
                            finished: false,
                            retiring: false,
                        },
                    );
                    let publisher = self.io.publisher.clone();
                    self.tasks.spawn(async move {
                        run_accepted(handle, publisher, writes, close, activated, resource).await;
                        handle
                    });
                }
                SocketCommand::Write {
                    handle,
                    sequence,
                    data,
                } => {
                    if let Some(worker) = self.workers.get(&handle) {
                        let _ = worker.writes.send((sequence, data));
                    }
                }
                SocketCommand::Close { handle, drain } => {
                    if let Some(worker) = self.workers.get(&handle) {
                        worker.close.send_modify(|closing| {
                            if closing.is_none() || !drain {
                                *closing = Some(Close {
                                    drain,
                                    deadline: Instant::now() + self.close_timeout,
                                });
                            }
                        });
                    }
                }
                SocketCommand::Retire { handle } => {
                    if let Some(worker) = self.workers.get_mut(&handle) {
                        worker.retiring = true;
                        worker.close.send_replace(Some(Close {
                            drain: false,
                            deadline: Instant::now(),
                        }));
                        self.settle(handle);
                    } else {
                        self.dispose(handle);
                    }
                }
                SocketCommand::Activate { handle } => {
                    if let Some(worker) = self.workers.get(&handle) {
                        let _ = worker.activated.send(true);
                    }
                }
            }
        }
    }

    fn settle(&mut self, handle: SocketHandle) {
        if self
            .workers
            .get(&handle)
            .is_some_and(|worker| worker.finished && worker.resource.is_none())
        {
            let worker = self
                .workers
                .remove(&handle)
                .expect("finished worker exists");
            let retiring = worker.retiring;
            drop(worker);
            if retiring {
                self.dispose(handle);
            }
        }
    }

    fn dispose(&mut self, handle: SocketHandle) {
        let publisher = self.io.publisher.clone();
        self.disposals.spawn(async move {
            if let Err(e) = publisher.publish(Update::Disposed(handle)).await {
                tracing::error!("TCP disposal publication failed for {}: {e}", handle.get());
            }
        });
    }
}

async fn manager<B: Backend>(
    commands: flume::Receiver<SocketCommands>,
    mut stopping: watch::Receiver<bool>,
    mut routing: Routing<B>,
    accepted_rx: flume::Receiver<(SocketHandle, Resource<B::Writer>)>,
) {
    loop {
        tokio::select! {
            _ = stopping.changed() => break,
            finished = routing.tasks.join_next(), if !routing.tasks.is_empty() => match finished {
                Some(Ok(handle)) => {
                    if let Some(worker) = routing.workers.get_mut(&handle) { worker.finished = true; }
                    routing.settle(handle);
                }
                Some(Err(e)) => { tracing::error!("TCP worker failed: {e}"); break; }
                None => {}
            },
            finished = routing.disposals.join_next(), if !routing.disposals.is_empty() => {
                if let Some(Err(e)) = finished { tracing::error!("TCP disposal worker failed: {e}"); break; }
            },
            batch = commands.recv_async() => match batch { Ok(batch) => routing.batch(batch), Err(_) => break },
            accepted = accepted_rx.recv_async() => if let Ok((handle, resource)) = accepted {
                // The attach command committed before this resource was sent through a separate channel.
                for _ in 0..commands.len() { if let Ok(batch) = commands.try_recv() { routing.batch(batch); } }
                if let Some(worker) = routing.workers.get_mut(&handle)
                    && let Some(sender) = worker.resource.take() {
                    let _ = sender.send(resource);
                }
                routing.settle(handle);
            }
        }
    }
    routing.tasks.abort_all();
    routing.disposals.abort_all();
    while routing.tasks.join_next().await.is_some() {}
    while routing.disposals.join_next().await.is_some() {}
}

async fn run_worker<B: Backend>(
    handle: SocketHandle,
    target: SocketTarget,
    io: Arc<NativeIo<B>>,
    writes: flume::Receiver<(u64, Bytes)>,
    mut close: watch::Receiver<Option<Close>>,
) {
    let publisher = &io.publisher;
    let expected = match target {
        SocketTarget::Connect { .. } => SocketState::Connecting,
        SocketTarget::Listen(_) => SocketState::Binding,
    };
    let allowed = tokio::select! {
        biased;
        _ = wait_close(&mut close) => false,
        result = publisher.publish(Update::Opening(handle, expected)) => result.unwrap_or(false),
    };
    let operation = if expected == SocketState::Connecting {
        SocketOperation::Connect
    } else {
        SocketOperation::Listen
    };
    let opened = if !allowed {
        Err(Terminal::local())
    } else {
        tokio::select! {
            biased;
            _ = wait_close(&mut close) => Err(Terminal::local()),
            result = timeout(io.connect_timeout, io.backend.open(target)) => match result {
                Ok(Ok(resource)) => Ok(resource),
                Ok(Err(e)) => Err(Terminal::failed(operation, e)),
                Err(_) => Err(Terminal::timed_out(operation)),
            },
        }
    };
    let (terminal, local, remote) = match opened {
        Err(terminal) => (terminal, None, None),
        Ok(resource) => {
            let (local, remote) = match &resource {
                Resource::Stream { local, remote, .. } => (*local, Some(*remote)),
                Resource::Listener { local, .. } => (*local, None),
            };
            let event = SocketIoEvent::Ready {
                handle,
                local,
                remote,
            };
            // Queued bytes cannot activate until readiness wins native conflict checking.
            let ready = publisher
                .publish(Update::Event(event.clone()))
                .await
                .unwrap_or(false);
            let terminal = if ready && publisher.emit(event, &mut close).await {
                match resource {
                    Resource::Stream { mut writer, .. } => {
                        stream(handle, &mut writer, publisher, writes, &mut close).await
                    }
                    Resource::Listener { listener, .. } => {
                        listen(
                            handle,
                            listener,
                            io.backend.clone(),
                            publisher,
                            &mut close,
                            io.accepted.clone(),
                        )
                        .await
                    }
                }
            } else {
                interruption(&close)
            };
            (terminal, Some(local), remote)
        }
    };
    finish_worker(handle, terminal, local, remote, publisher, &mut close).await;
}

async fn finish_worker(
    handle: SocketHandle,
    terminal: Terminal,
    local: Option<SocketAddr>,
    remote: Option<SocketAddr>,
    publisher: &Publisher,
    close: &mut watch::Receiver<Option<Close>>,
) {
    let event = SocketIoEvent::Closed {
        handle,
        reason: terminal.reason,
        operation: terminal.operation,
        error: terminal.error,
        local,
        remote,
    };
    match publisher.publish(Update::Event(event.clone())).await {
        Ok(true) => publisher.emit_terminal(event, close).await,
        Ok(false) => {}
        Err(e) => tracing::error!("TCP terminal publication failed for {}: {e}", handle.get()),
    }
}

async fn stream<W: AsyncRead + AsyncWrite + Unpin>(
    handle: SocketHandle,
    writer: &mut W,
    publisher: &Publisher,
    writes: flume::Receiver<(u64, Bytes)>,
    close: &mut watch::Receiver<Option<Close>>,
) -> Terminal {
    let (reader, mut writer) = tokio::io::split(writer);
    let mut read_close = close.clone();
    let reading = read_stream(handle, reader, publisher, &mut read_close);
    let writing = write_stream(handle, &mut writer, publisher, writes, close);
    tokio::pin!(writing);
    tokio::select! {
        biased;
        terminal = &mut writing => terminal,
        terminal = reading => match terminal { Some(terminal) => terminal, None => writing.await },
    }
}

async fn read_stream<R: AsyncRead + Unpin>(
    handle: SocketHandle,
    mut reader: R,
    publisher: &Publisher,
    close: &mut watch::Receiver<Option<Close>>,
) -> Option<Terminal> {
    let mut sequence = 0u64;
    let mut buffer = Vec::new();
    let mut commits = publisher.commits.clone();
    loop {
        if let Some(commits) = &mut commits {
            commits.borrow_and_update();
        }
        let capacity = match publisher.run(Update::InputCapacity(handle)).await {
            Ok(Outcome::Capacity(Some(capacity))) => capacity,
            Ok(_) => return None,
            Err(e) => {
                return Some(Terminal::failed(
                    SocketOperation::Read,
                    io::Error::other(e.to_string()),
                ));
            }
        };
        if capacity == 0 {
            tokio::select! { biased; _ = wait_close(close) => return None, _ = changed(&mut commits) => {} }
            continue;
        }
        buffer.resize(capacity, 0);
        let count = tokio::select! {
            biased;
            _ = wait_close(close) => return None,
            count = reader.read(&mut buffer) => match count { Ok(count) => count, Err(e) => return Some(Terminal::failed(SocketOperation::Read, e)) },
        };
        let Some(next) = sequence.checked_add(1) else {
            return Some(Terminal::failed(
                SocketOperation::Read,
                io::Error::other("read sequence exhausted"),
            ));
        };
        sequence = next;
        let event = if count == 0 {
            SocketIoEvent::Eof { handle, sequence }
        } else {
            SocketIoEvent::Read {
                handle,
                sequence,
                data: Bytes::copy_from_slice(&buffer[..count]),
            }
        };
        match publisher.publish(Update::Event(event.clone())).await {
            Ok(true) => {}
            Ok(false) => return None,
            Err(e) => {
                return Some(Terminal::failed(
                    SocketOperation::Read,
                    io::Error::other(e.to_string()),
                ));
            }
        }
        if !publisher.emit(event, close).await || count == 0 {
            return None;
        }
    }
}

async fn changed(commits: &mut Option<watch::Receiver<crate::interpreter::stm::Version>>) {
    match commits {
        Some(commits) => {
            let _ = commits.changed().await;
        }
        None => std::future::pending().await,
    }
}

async fn listen<B: Backend>(
    handle: SocketHandle,
    listener: TcpListener,
    backend: Arc<B>,
    publisher: &Publisher,
    close: &mut watch::Receiver<Option<Close>>,
    accepted: flume::Sender<(SocketHandle, Resource<B::Writer>)>,
) -> Terminal {
    let mut commits = publisher.commits.clone();
    loop {
        if let Some(commits) = &mut commits {
            commits.borrow_and_update();
        }
        let permit = tokio::select! {
            biased;
            _ = wait_close(close) => return Terminal::local(),
            permit = publisher.run(Update::ReserveAccept(handle)) => match permit {
                Ok(Outcome::Permit(SocketAdmission::Admitted(permit))) => permit,
                Ok(Outcome::Permit(SocketAdmission::Full)) => { tokio::select! { biased; _ = wait_close(close) => return Terminal::local(), _ = changed(&mut commits) => {} } continue; },
                Ok(_) => return Terminal::local(),
                Err(e) => return Terminal::failed(SocketOperation::Accept, io::Error::other(e.to_string())),
            }
        };
        let resource = tokio::select! {
            biased;
            _ = wait_close(close) => return Terminal::local(),
            resource = backend.accept(&listener) => match resource { Ok(resource) => resource, Err(e) => return Terminal::failed(SocketOperation::Accept, e) },
        };
        let (local, remote) = match &resource {
            Resource::Stream { local, remote, .. } => (*local, *remote),
            _ => {
                return Terminal::failed(
                    SocketOperation::Accept,
                    io::Error::other("accept returned a listener"),
                );
            }
        };
        let child = match publisher.run(Update::Accepted(permit, local, remote)).await {
            Ok(Outcome::Accepted(SocketAdmission::Admitted(child))) => child,
            Ok(_) => return Terminal::local(),
            Err(e) => {
                return Terminal::failed(SocketOperation::Accept, io::Error::other(e.to_string()));
            }
        };
        // Committed accept delivery may already have made the child independent of its listener.
        if accepted.send_async((child, resource)).await.is_err() {
            return Terminal::local();
        }
        if !publisher
            .emit(SocketIoEvent::Accept { handle, child }, close)
            .await
        {
            return Terminal::local();
        }
    }
}

async fn run_accepted<W: AsyncRead + AsyncWrite + Unpin>(
    handle: SocketHandle,
    publisher: Publisher,
    writes: flume::Receiver<(u64, Bytes)>,
    mut close: watch::Receiver<Option<Close>>,
    mut activated: watch::Receiver<bool>,
    resource: tokio::sync::oneshot::Receiver<Resource<W>>,
) {
    let resource = tokio::select! { biased; _ = wait_interrupt(&mut close) => None, resource = resource => resource.ok() };
    let (terminal, local, remote) = match resource {
        Some(Resource::Stream {
            mut writer,
            local,
            remote,
        }) => {
            let ready = loop {
                if *activated.borrow() {
                    break true;
                }
                tokio::select! {
                    biased;
                    _ = wait_interrupt(&mut close) => break false,
                    result = activated.changed() => if result.is_err() { break false; },
                }
            };
            let terminal = if ready {
                stream(handle, &mut writer, &publisher, writes, &mut close).await
            } else {
                interruption(&close)
            };
            (terminal, Some(local), Some(remote))
        }
        _ => (interruption(&close), None, None),
    };
    finish_worker(handle, terminal, local, remote, &publisher, &mut close).await;
}

async fn write_stream<W: AsyncWrite + Unpin>(
    handle: SocketHandle,
    writer: &mut W,
    publisher: &Publisher,
    writes: flume::Receiver<(u64, Bytes)>,
    close: &mut watch::Receiver<Option<Close>>,
) -> Terminal {
    loop {
        let closing = *close.borrow_and_update();
        if let Some(closing) = closing {
            if !closing.drain {
                return Terminal::local();
            }
            if Instant::now() >= closing.deadline {
                return Terminal::timed_out(SocketOperation::Close);
            }
            if writes.is_empty() {
                return tokio::select! {
                    biased;
                    terminal = wait_interrupt(close) => terminal,
                    result = writer.shutdown() => result.map_or_else(|e| Terminal::failed(SocketOperation::Close, e), |()| Terminal::local()),
                };
            }
        }
        let mut interrupt = close.clone();
        let next = tokio::select! {
            biased;
            changed = close.changed() => {
                if changed.is_err() { return Terminal::local(); }
                continue;
            },
            terminal = wait_interrupt(&mut interrupt) => return terminal,
            next = writes.recv_async() => next,
        };
        let Ok((sequence, data)) = next else {
            return Terminal::local();
        };
        let result = tokio::select! {
            biased;
            terminal = wait_interrupt(close) => return terminal,
            result = writer.write_all(&data) => result,
        };
        if let Err(e) = result {
            return Terminal::failed(SocketOperation::Write, e);
        }
        let event = SocketIoEvent::WriteComplete { handle, sequence };
        match publisher.publish(Update::Event(event.clone())).await {
            Ok(true) => {
                if !publisher.emit(event, close).await {
                    return interruption(close);
                }
            }
            Ok(false) => return Terminal::local(),
            Err(e) => {
                tracing::error!("TCP write publication failed for {}: {e}", handle.get());
                return Terminal::failed(SocketOperation::Write, io::Error::other(e.to_string()));
            }
        }
    }
}

async fn wait_close(close: &mut watch::Receiver<Option<Close>>) {
    loop {
        if close.borrow().is_some() {
            return;
        }
        if close.changed().await.is_err() {
            return;
        }
    }
}

async fn wait_interrupt(close: &mut watch::Receiver<Option<Close>>) -> Terminal {
    loop {
        let current = *close.borrow();
        match current {
            Some(closing) if !closing.drain => return Terminal::local(),
            Some(closing) => tokio::select! {
                biased;
                changed = close.changed() => if changed.is_err() { return Terminal::local(); },
                _ = tokio::time::sleep_until(closing.deadline) => return Terminal::timed_out(SocketOperation::Close),
            },
            None => {
                if close.changed().await.is_err() {
                    return Terminal::local();
                }
            }
        }
    }
}

fn interruption(close: &watch::Receiver<Option<Close>>) -> Terminal {
    if close
        .borrow()
        .is_some_and(|closing| closing.drain && Instant::now() >= closing.deadline)
    {
        Terminal::timed_out(SocketOperation::Close)
    } else {
        Terminal::local()
    }
}

struct Terminal {
    reason: SocketCloseReason,
    operation: Option<SocketOperation>,
    error: Option<String>,
}

impl Terminal {
    fn local() -> Self {
        Self {
            reason: SocketCloseReason::Local,
            operation: None,
            error: None,
        }
    }
    fn failed(operation: SocketOperation, e: io::Error) -> Self {
        Self {
            reason: SocketCloseReason::Error,
            operation: Some(operation),
            error: Some(e.to_string()),
        }
    }
    fn timed_out(operation: SocketOperation) -> Self {
        Self {
            reason: SocketCloseReason::Timeout,
            operation: Some(operation),
            error: Some("TCP operation timed out".into()),
        }
    }
}

#[derive(Clone)]
struct Publisher {
    gs: Weak<GlobalState>,
    events: flume::Sender<SocketIoEvent>,
    commits: Option<watch::Receiver<crate::interpreter::stm::Version>>,
}

#[derive(Clone)]
enum Update {
    Opening(SocketHandle, SocketState),
    Present(SocketHandle),
    Disposed(SocketHandle),
    Event(SocketIoEvent),
    InputCapacity(SocketHandle),
    ReserveAccept(SocketHandle),
    Accepted(SocketAcceptPermit, SocketAddr, SocketAddr),
}

#[derive(Clone)]
enum Outcome {
    Applied(bool),
    Capacity(Option<usize>),
    Permit(SocketAdmission<SocketAcceptPermit>),
    Accepted(SocketAdmission<SocketHandle>),
}

impl Publisher {
    fn new(gs: &Arc<GlobalState>, events: flume::Sender<SocketIoEvent>) -> Self {
        Self {
            gs: Arc::downgrade(gs),
            events,
            commits: gs.attempt_runner.commit_watch(),
        }
    }

    async fn run(&self, update: Update) -> Result<Outcome> {
        let Some(gs) = self.gs.upgrade() else {
            return Ok(Outcome::Applied(false));
        };
        let mut body = Publication {
            gs: gs.clone(),
            update,
            txn: None,
            outcome: Outcome::Applied(false),
        };
        let (result, _) = gs.attempt_runner.run(&mut body).await;
        result.inspect_err(|e| tracing::error!("native TCP publication failed: {e}"))?;
        Ok(body.outcome)
    }

    async fn publish(&self, update: Update) -> Result<bool> {
        match self.run(update).await? {
            Outcome::Applied(applied) => Ok(applied),
            _ => Err(error("wrong TCP publication result")),
        }
    }

    async fn emit_terminal(
        &self,
        event: SocketIoEvent,
        close: &mut watch::Receiver<Option<Close>>,
    ) {
        loop {
            close.borrow_and_update();
            if !self
                .publish(Update::Present(event.handle()))
                .await
                .unwrap_or(false)
            {
                return;
            }
            tokio::select! {
                biased;
                changed = close.changed() => if changed.is_err() { return; },
                _ = self.events.send_async(event.clone()) => return,
            }
        }
    }

    async fn emit(&self, event: SocketIoEvent, close: &mut watch::Receiver<Option<Close>>) -> bool {
        tokio::select! {
            biased;
            _ = wait_interrupt(close) => false,
            result = self.events.send_async(event) => result.is_ok(),
        }
    }
}

struct Publication {
    gs: Arc<GlobalState>,
    update: Update,
    txn: Option<TxnHandle>,
    outcome: Outcome,
}

#[async_trait::async_trait]
impl AttemptBody for Publication {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        let access = SocketAccess {
            sockets: &self.gs.sockets,
            txn: txn.clone(),
        };
        self.outcome = match &self.update {
            Update::InputCapacity(handle) => Outcome::Capacity(access.input_capacity(*handle)?),
            Update::ReserveAccept(handle) => Outcome::Permit(access.reserve_accept(*handle)?),
            Update::Accepted(permit, local, remote) => {
                Outcome::Accepted(access.accepted(*permit, *local, *remote)?)
            }
            update => Outcome::Applied(match update {
                Update::Opening(handle, state) => txn.with(|txn| {
                    self.gs
                        .sockets
                        .record(txn, *handle)
                        .is_ok_and(|(_, record)| record.info.state == *state)
                }),
                Update::Present(handle) => {
                    txn.with(|txn| self.gs.sockets.record(txn, *handle).is_ok())
                }
                Update::Disposed(handle) => access.disposed(*handle),
                Update::Event(SocketIoEvent::Ready {
                    handle,
                    local,
                    remote,
                }) => access.ready(*handle, *local, *remote)?,
                Update::Event(SocketIoEvent::WriteComplete { handle, sequence }) => {
                    access.complete_write(*handle, *sequence)?
                }
                Update::Event(SocketIoEvent::Read {
                    handle,
                    sequence,
                    data,
                }) => access.read(*handle, *sequence, data.clone())?,
                Update::Event(SocketIoEvent::Eof { handle, sequence }) => {
                    access.eof(*handle, *sequence)?
                }
                Update::Event(SocketIoEvent::Closed {
                    handle,
                    reason,
                    operation,
                    error,
                    local,
                    remote,
                }) => access.terminal(
                    *handle,
                    SocketEventData::Closed {
                        reason: *reason,
                        operation: *operation,
                        error: error.clone(),
                        local: *local,
                        remote: *remote,
                    },
                )?,
                _ => return Err(error("invalid native TCP update")),
            }),
        };
        self.txn = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self
            .txn
            .take()
            .ok_or_else(|| error("missing native socket attempt"))?;
        Ok((
            commit_changeset(tx, txn.with(Transaction::take_changeset)).await?,
            Vec::new(),
        ))
    }

    async fn deliver(&mut self, _effects: Vec<Effect>) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests;
