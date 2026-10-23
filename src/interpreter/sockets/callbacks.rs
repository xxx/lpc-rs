use std::{
    collections::HashSet,
    sync::{Arc, Weak},
};

use lpc_rs_errors::{LpcError, Result};
use tokio::{
    sync::watch,
    task::{JoinHandle, JoinSet},
};

use super::{
    SocketAccess, SocketCloseReason, SocketEventData, SocketHandle, SocketLease, SocketOperation,
    SocketTarget, SocketTransport, error, index,
};
use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::LpcRef,
        process::Process,
        stm::{
            AttemptBody, CommitOrigin, CommitProtocol, Conflict, Effect, LiveSnapshot, Transaction,
            TxnHandle, VarId, Version, commit_changeset, flush_effects, start_txn,
        },
        task::{
            DriverCallbackError, Task, apply_function::report_runtime_error,
            task_template::TaskTemplate,
        },
        task_context::Caller,
        vm::global_state::GlobalState,
    },
};

/// Explicitly attached TCP transport and LPC callback dispatch for one VM.
///
/// Each handle runs serially; unrelated handles can run concurrently. Events and
/// registrations are read again on every STM retry, and successful callback writes
/// commit with their event acknowledgements. Dropping this runtime cancels its
/// callbacks and closes physical resources without promising drain or new callbacks.
///
/// Registered pointers receive `(int handle, string event, mixed data)` with driver
/// origin, no player or command context, and the scheduling owner as caller.
/// `ready` carries a mapping with `kind`, `local` and `remote`; endpoints are
/// `({ numeric_address, port })` or the VM's nil. `accept` carries the child handle,
/// `read` carries bytes, and `eof`/`drained` carry nil. `closed` carries `reason`,
/// `operation`, `error`, `local` and `remote` in a mapping. Runtime errors discard
/// the failed event after reporting; an unresolvable registration retires the socket.
#[derive(Debug)]
pub struct SocketRuntime {
    stop: watch::Sender<bool>,
    task: Option<JoinHandle<Result<()>>>,
}

impl SocketRuntime {
    /// Attach on the current Tokio runtime, taking the VM's sole TCP command receiver.
    pub fn start(gs: &Arc<GlobalState>) -> Result<Self> {
        let commits = gs
            .attempt_runner
            .commit_watch()
            .ok_or_else(|| error("callback dispatch requires the VM commit watch"))?;
        let transport = SocketTransport::start(gs)?;
        Ok(Self::spawn(gs, commits, Some(transport)))
    }

    fn spawn(
        gs: &Arc<GlobalState>,
        commits: watch::Receiver<Version>,
        transport: Option<SocketTransport>,
    ) -> Self {
        let (stop, stopping) = watch::channel(false);
        let weak = Arc::downgrade(gs);
        let task = tokio::spawn(supervise(weak, commits, stopping, transport));
        Self {
            stop,
            task: Some(task),
        }
    }

    /// Cancel callbacks and wait for their attempts and TCP resources to be dropped.
    pub async fn shutdown(mut self) -> Result<()> {
        let _ = self.stop.send(true);
        if let Some(task) = self.task.take() {
            task.await
                .map_err(|e| error(&format!("callback supervisor failed: {e}")))??;
        }
        Ok(())
    }
}

impl Drop for SocketRuntime {
    fn drop(&mut self) {
        let _ = self.stop.send(true);
        if let Some(task) = self.task.take() {
            task.abort();
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Pending {
    handle: SocketHandle,
    id: u64,
}

async fn supervise(
    gs: Weak<GlobalState>,
    mut commits: watch::Receiver<Version>,
    mut stopping: watch::Receiver<bool>,
    transport: Option<SocketTransport>,
) -> Result<()> {
    let mut workers = JoinSet::new();
    let mut active = HashSet::new();
    loop {
        if *stopping.borrow() {
            break;
        }
        // Scanning after marking the watch also sees publications whose raw observation was cancelled.
        commits.borrow_and_update();
        let Some(state) = gs.upgrade() else {
            break;
        };
        for event in pending(&state).await? {
            if active.insert(event.handle) {
                let state = state.clone();
                workers.spawn(async move { (event.handle, dispatch(&state, event).await) });
            }
        }
        drop(state);
        tokio::select! {
            changed = stopping.changed() => {
                if changed.is_err() || *stopping.borrow() { break; }
            }
            changed = commits.changed() => { if changed.is_err() { break; } }
            finished = workers.join_next(), if !workers.is_empty() => {
                let (handle, result) = finished.expect("nonempty worker set")
                    .map_err(|e| error(&format!("callback worker failed: {e}")))?;
                active.remove(&handle);
                result?;
            }
            observation = async {
                match &transport {
                    Some(transport) => transport.next_event().await,
                    None => std::future::pending().await,
                }
            } => { if observation.is_none() { break; } }
        }
    }
    workers.abort_all();
    while workers.join_next().await.is_some() {}
    if let Some(transport) = transport {
        transport.shutdown().await?;
    }
    Ok(())
}

async fn pending(gs: &Arc<GlobalState>) -> Result<Vec<Pending>> {
    let Some(registry) = gs.sockets.root() else {
        return Ok(Vec::new());
    };
    let live = start_txn(&gs.committer_tx).await?;
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let handles: Vec<_> = txn.with(|t| index(t, registry).keys().copied().collect());
    let access = SocketAccess {
        sockets: &gs.sockets,
        txn,
    };
    let mut pending = Vec::new();
    for handle in handles {
        if let Some(lease) = access.lease(handle)? {
            pending.push(Pending {
                handle,
                id: lease.event().id,
            });
        }
    }
    Ok(pending)
}

struct Invocation<'a> {
    gs: &'a Arc<GlobalState>,
    event: Pending,
    txn: Option<TxnHandle>,
    lease: Option<SocketLease>,
    task: Option<Task<MAX_CALL_STACK_SIZE>>,
    failure: Option<DriverCallbackError>,
}

impl Invocation<'_> {
    fn access(&self) -> SocketAccess<'_> {
        SocketAccess {
            sockets: &self.gs.sockets,
            txn: self.txn.as_ref().expect("attempt opened").clone(),
        }
    }
}

#[async_trait::async_trait]
impl AttemptBody for Invocation<'_> {
    fn origin(&self) -> Option<CommitOrigin> {
        self.lease
            .as_ref()
            .map(|lease| CommitOrigin::new(lease.owner(), "socket_callback"))
    }

    fn describe_cell(&self, cell: VarId) -> Option<String> {
        self.txn
            .as_ref()
            .and_then(|txn| txn.with(|t| t.describe_cell(&self.gs.object_space, cell)))
    }

    fn take_compilation_time(&mut self) -> std::time::Duration {
        self.txn.as_ref().map_or(std::time::Duration::ZERO, |txn| {
            txn.with(|t| t.take_compilation_time())
        })
    }

    fn timeout_ms(&self) -> u64 {
        self.gs.config.max_execution_time
    }

    fn timeout_error(&self) -> LpcError {
        let error = LpcError::runtime(format!(
            "evaluation limit of {}ms has been reached in socket callback",
            self.timeout_ms()
        ));
        self.txn
            .as_ref()
            .map_or_else(|| error.clone(), |txn| txn.with_interruption(error.clone()))
    }

    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        self.lease = None;
        self.failure = None;
        self.task = None;
        self.txn = None;
        let live = start_txn(tx).await?;
        let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        self.txn = Some(txn.clone());
        let Some(lease) = self.access().lease(self.event.handle)? else {
            return Ok(Some(live));
        };
        if lease.event().id != self.event.id {
            return Ok(Some(live));
        }
        txn.with(|t| t.set_origin(lease.owner(), "socket_callback"));
        let mut template = TaskTemplate::from(self.gs.clone());
        template.txn = txn.clone();
        let mut context = template.into_task_context(lease.owner().clone());
        context.callers = Some(Caller::link(lease.owner().clone(), None));
        let args = arguments(&lease, &self.access());
        self.task = Some(Task::<MAX_CALL_STACK_SIZE>::new(context));
        self.lease = Some(lease.clone());
        self.failure = self
            .task
            .as_mut()
            .expect("callback task prepared")
            .run_driver_callback(lease.callback(), &args)
            .await
            .err();
        if self.failure.is_none() {
            self.access().acknowledge(&lease, true)?;
        }
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self.txn.as_ref().expect("attempt opened");
        let changes = txn.with(|t| {
            if self.failure.is_some() {
                t.validation_changeset()
            } else {
                t.clone_changeset()
            }
        });
        let commit = commit_changeset(tx, changes).await?;
        let effects = if self.failure.is_none() {
            txn.with(|t| t.take_effects())
        } else {
            Vec::new()
        };
        Ok((commit, effects))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        flush_effects(self.gs, effects).await;
        Ok(())
    }
}

async fn dispatch(gs: &Arc<GlobalState>, event: Pending) -> Result<()> {
    let mut invocation = Invocation {
        gs,
        event,
        txn: None,
        lease: None,
        task: None,
        failure: None,
    };
    let (result, _) = gs.attempt_runner.run(&mut invocation).await;
    let failure = match result {
        Ok(()) => invocation.failure.take(),
        Err(error) => Some(DriverCallbackError {
            error,
            retire: false,
        }),
    };
    if let Some(failure) = failure {
        let Some(lease) = invocation.lease.take() else {
            return Err(failure.error);
        };
        let mut cleanup = Cleanup {
            gs,
            event,
            owner: Arc::downgrade(lease.owner()),
            callback: Arc::downgrade(lease.callback()),
            retire: failure.retire,
            txn: None,
        };
        let process = invocation
            .task
            .as_ref()
            .map(|task| task.context.process().clone())
            .or_else(|| Some(lease.owner().clone()));
        drop(lease);
        drop(invocation);
        report_runtime_error(&failure.error, process, TaskTemplate::from(gs.clone())).await;
        let (result, _) = gs.attempt_runner.run(&mut cleanup).await;
        result?;
    }
    Ok(())
}

struct Cleanup<'a> {
    gs: &'a Arc<GlobalState>,
    event: Pending,
    owner: Weak<Process>,
    callback: Weak<FunctionPtr>,
    retire: bool,
    txn: Option<TxnHandle>,
}

#[async_trait::async_trait]
impl AttemptBody for Cleanup<'_> {
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
        if let Some(lease) = access.lease(self.event.handle)?
            && lease.event().id == self.event.id
            && std::ptr::eq(self.owner.as_ptr(), Arc::as_ptr(lease.owner()))
            && std::ptr::eq(self.callback.as_ptr(), Arc::as_ptr(lease.callback()))
        {
            if self.retire {
                txn.with(|t| self.gs.sockets.remove(t, self.event.handle));
            } else {
                access.acknowledge(&lease, false)?;
            }
        }
        self.txn = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Conflict>, Vec<Effect>)> {
        let txn = self.txn.as_ref().expect("cleanup attempt opened");
        Ok((
            commit_changeset(tx, txn.with(|t| t.clone_changeset())).await?,
            Vec::new(),
        ))
    }

    async fn deliver(&mut self, _effects: Vec<Effect>) -> Result<()> {
        Ok(())
    }
}

fn arguments(lease: &SocketLease, access: &SocketAccess<'_>) -> Vec<LpcRef> {
    let nil = access.txn.nil();
    let address = |endpoint: Option<std::net::SocketAddr>| {
        endpoint.map_or_else(
            || nil.clone(),
            |a| {
                let array = LpcArray::new(vec![
                    LpcRef::from(a.ip().to_string()),
                    LpcRef::from(i64::from(a.port())),
                ]);
                LpcRef::Array(access.txn.with(|t| t.mint_array(array)))
            },
        )
    };
    let mapping = |values: &[(&str, LpcRef)]| {
        let mut map = LpcMapping::default();
        for (key, value) in values {
            map.insert(LpcRef::from(*key), value.clone());
        }
        LpcRef::Mapping(access.txn.with(|t| t.mint_mapping(map)))
    };
    let (name, data) = match &lease.event().data {
        SocketEventData::Ready { local, remote } => {
            let kind = match access
                .info(lease.event().handle)
                .expect("leased socket exists")
                .target
            {
                SocketTarget::Listen(_) => "listener",
                SocketTarget::Connect { .. } => "stream",
            };
            (
                "ready",
                mapping(&[
                    ("kind", LpcRef::from(kind)),
                    ("local", address(Some(*local))),
                    ("remote", address(*remote)),
                ]),
            )
        }
        SocketEventData::Accept(child) => ("accept", LpcRef::from(child.get())),
        SocketEventData::Read(bytes) => ("read", LpcRef::from(bytes.clone())),
        SocketEventData::Eof => ("eof", nil.clone()),
        SocketEventData::Drained => ("drained", nil.clone()),
        SocketEventData::Closed {
            reason,
            operation,
            error,
            local,
            remote,
        } => {
            let reason = match reason {
                SocketCloseReason::Local => "local",
                SocketCloseReason::Error => "error",
                SocketCloseReason::Timeout => "timeout",
            };
            let operation = operation.map_or_else(
                || nil.clone(),
                |op| {
                    LpcRef::from(match op {
                        SocketOperation::Connect => "connect",
                        SocketOperation::Listen => "listen",
                        SocketOperation::Write => "write",
                        SocketOperation::Read => "read",
                        SocketOperation::Accept => "accept",
                        SocketOperation::Close => "close",
                    })
                },
            );
            (
                "closed",
                mapping(&[
                    ("reason", LpcRef::from(reason)),
                    ("operation", operation),
                    (
                        "error",
                        error
                            .as_ref()
                            .map_or_else(|| nil.clone(), |s| LpcRef::from(s.as_str())),
                    ),
                    ("local", address(*local)),
                    ("remote", address(*remote)),
                ]),
            )
        }
    };
    vec![
        LpcRef::from(lease.event().handle.get()),
        LpcRef::from(name),
        data,
    ]
}

#[cfg(test)]
mod tests;
