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
    io::{AsyncWrite, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    sync::watch,
    task::{JoinHandle, JoinSet},
    time::{Instant, timeout},
};

use super::{
    SocketAccess, SocketCommand, SocketCommands, SocketHandle, SocketState, SocketTarget, error,
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
/// These are transport completions, not retryable LPC callbacks. Input, accepts,
/// and retryable LPC callback delivery remain unimplemented.
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
            | Self::Closed { handle, .. } => handle,
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
        let publisher = Publisher {
            gs: Arc::downgrade(gs),
            events: events_tx,
        };
        let connect_timeout = Duration::from_millis(gs.config.socket_connect_timeout as u64);
        let close_timeout = Duration::from_millis(gs.config.socket_close_timeout as u64);
        let task = tokio::spawn(manager(
            commands,
            stopping,
            Arc::new(backend),
            publisher,
            connect_timeout,
            close_timeout,
        ));
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
    type Writer: AsyncWrite + Unpin + Send + 'static;
    fn open(
        &self,
        target: SocketTarget,
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
}

#[derive(Clone, Copy)]
struct Close {
    drain: bool,
    deadline: Instant,
}

struct Worker {
    writes: flume::Sender<(u64, Bytes)>,
    close: watch::Sender<Option<Close>>,
}

async fn manager<B: Backend>(
    commands: flume::Receiver<SocketCommands>,
    mut stopping: watch::Receiver<bool>,
    backend: Arc<B>,
    publisher: Publisher,
    connect_timeout: Duration,
    close_timeout: Duration,
) {
    let mut workers = HashMap::<SocketHandle, Worker>::new();
    let mut tasks = JoinSet::new();
    loop {
        tokio::select! {
            biased;
            _ = stopping.changed() => break,
            finished = tasks.join_next(), if !tasks.is_empty() => {
                match finished {
                    Some(Ok(handle)) => { workers.remove(&handle); }
                    Some(Err(e)) => { tracing::error!("TCP worker failed: {e}"); break; }
                    None => {}
                }
            }
            batch = commands.recv_async() => {
                let Ok(batch) = batch else { break; };
                for command in batch {
                    match command {
                        SocketCommand::Open { handle, target } => {
                            let (writes_tx, writes) = flume::unbounded();
                            let (close_tx, close) = watch::channel(None);
                            workers.insert(handle, Worker { writes: writes_tx, close: close_tx });
                            let backend = backend.clone();
                            let publisher = publisher.clone();
                            tasks.spawn(async move {
                                run_worker(handle, target, backend, publisher, writes, close, connect_timeout).await;
                                handle
                            });
                        }
                        SocketCommand::Write { handle, sequence, data } => {
                            if let Some(worker) = workers.get(&handle) { let _ = worker.writes.send((sequence, data)); }
                        }
                        SocketCommand::Close { handle, drain } => {
                            if let Some(worker) = workers.get(&handle) {
                                worker.close.send_modify(|closing| {
                                    if closing.is_none() || !drain { *closing = Some(Close { drain, deadline: Instant::now() + close_timeout }); }
                                });
                            }
                        }
                    }
                }
            }
        }
    }
    tasks.abort_all();
    while tasks.join_next().await.is_some() {}
}

async fn run_worker<B: Backend>(
    handle: SocketHandle,
    target: SocketTarget,
    backend: Arc<B>,
    publisher: Publisher,
    writes: flume::Receiver<(u64, Bytes)>,
    mut close: watch::Receiver<Option<Close>>,
    connect_timeout: Duration,
) {
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
            result = timeout(connect_timeout, backend.open(target)) => match result {
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
                        stream(handle, &mut writer, &publisher, writes, &mut close).await
                    }
                    Resource::Listener { listener, .. } => {
                        wait_close(&mut close).await;
                        drop(listener);
                        Terminal::local()
                    }
                }
            } else {
                interruption(&close)
            };
            (terminal, Some(local), remote)
        }
    };
    let event = SocketIoEvent::Closed {
        handle,
        reason: terminal.reason,
        operation: terminal.operation,
        error: terminal.error,
        local,
        remote,
    };
    match publisher.publish(Update::Event(event.clone())).await {
        Ok(true) => {
            publisher.emit_terminal(event, &mut close).await;
        }
        Ok(false) => {}
        Err(e) => tracing::error!("TCP terminal publication failed for {}: {e}", handle.get()),
    }
}

async fn stream<W: AsyncWrite + Unpin>(
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
}

#[derive(Clone)]
enum Update {
    Opening(SocketHandle, SocketState),
    Present(SocketHandle),
    Event(SocketIoEvent),
}

impl Publisher {
    async fn publish(&self, update: Update) -> Result<bool> {
        let Some(gs) = self.gs.upgrade() else {
            return Ok(false);
        };
        let mut body = Publication {
            gs: gs.clone(),
            update,
            txn: None,
            applied: false,
        };
        let (result, _) = gs.attempt_runner.run(&mut body).await;
        result.inspect_err(|e| tracing::error!("native TCP publication failed: {e}"))?;
        Ok(body.applied)
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
    applied: bool,
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
        self.applied = match &self.update {
            Update::Opening(handle, state) => txn.with(|txn| {
                self.gs
                    .sockets
                    .record(txn, *handle)
                    .is_ok_and(|(_, record)| record.info.state == *state)
            }),
            Update::Present(handle) => {
                txn.with(|txn| self.gs.sockets.cells(txn, *handle).is_some())
            }
            Update::Event(SocketIoEvent::Ready {
                handle,
                local,
                remote,
            }) => access.ready(*handle, *local, *remote)?,
            Update::Event(SocketIoEvent::WriteComplete { handle, sequence }) => {
                access.complete_write(*handle, *sequence)?
            }
            Update::Event(event @ SocketIoEvent::Closed { .. }) => {
                let handle = event.handle();
                if access
                    .info(handle)
                    .is_some_and(|info| info.state != SocketState::Closed)
                {
                    access.closed(handle);
                    true
                } else {
                    false
                }
            }
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
