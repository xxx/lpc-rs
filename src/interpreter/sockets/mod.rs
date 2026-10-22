//! Transactional TCP resource bookkeeping and commit-ordered transport commands.
//!
//! Efun adapters authorize operations and establish synchronous ownership-offer consent.

use std::{
    net::SocketAddr,
    sync::{
        Arc, OnceLock, Weak,
        atomic::{AtomicI64, Ordering},
    },
};

use bytes::Bytes;
use imbl::{HashMap, Vector};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;
use thin_vec::ThinVec;

use crate::interpreter::{
    function_type::function_ptr::FunctionPtr,
    process::Process,
    stm::{Transaction, TxnHandle, VarId, WorldValue},
    task_context::TaskContext,
};

mod callbacks;
mod events;
mod transport;

use events::Inbox;
pub use events::{SocketAcceptPermit, SocketAdmission, SocketEvent, SocketEventData, SocketLease};

pub use callbacks::SocketRuntime;
pub use transport::{SocketCloseReason, SocketIoEvent, SocketOperation, SocketTransport};

#[cfg(test)]
mod tests;

/// A positive, VM-local identity, never reused even when creation aborts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SocketHandle(i64);

impl SocketHandle {
    /// The integer exposed to LPC.
    pub fn get(self) -> i64 {
        self.0
    }
}

impl TryFrom<i64> for SocketHandle {
    type Error = LpcError;

    fn try_from(value: i64) -> Result<Self> {
        if value > 0 {
            Ok(Self(value))
        } else {
            Err(error("invalid handle"))
        }
    }
}

/// Setup performed by the transport after a successful commit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SocketTarget {
    /// DNS and TCP connect, with the requested hostname retained.
    Connect { host: String, port: u16 },
    /// Bind and listen on a numeric address; port zero is allowed.
    Listen(SocketAddr),
}

/// Immutable commands, containing no LPC heap values or callback pointers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SocketCommand {
    /// Begin asynchronous setup.
    Open {
        handle: SocketHandle,
        target: SocketTarget,
    },
    /// Complete all physical short writes internally before acknowledging this sequence.
    Write {
        handle: SocketHandle,
        sequence: u64,
        data: Bytes,
    },
    /// Drain admitted output for an established close, or cancel setup.
    Close { handle: SocketHandle, drain: bool },
    /// Discard remaining work and release quota only after transport disposal.
    Retire { handle: SocketHandle },
    /// Prepare a worker for an already admitted physical accepted stream.
    OpenAccepted { handle: SocketHandle },
    /// Release a delivered child from its listener notification gate.
    Activate { handle: SocketHandle },
}

/// Commands from one committed transaction, in source order.
pub type SocketCommands = ThinVec<SocketCommand>;

#[derive(Debug, Clone)]
pub(crate) struct Publication {
    pub sender: flume::Sender<SocketCommands>,
    pub commands: SocketCommands,
}

impl Publication {
    pub(crate) fn apply(self) {
        if self.sender.send(self.commands).is_err() {
            tracing::error!("socket transport command receiver disconnected after commit");
        }
    }
}

/// Transaction-visible lifecycle; peer EOF does not change an open stream's state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketState {
    Connecting,
    Binding,
    Open,
    Listening,
    Closing,
    Closed,
}

/// Socket metadata, read from this attempt rather than from the OS.
#[derive(Debug, Clone)]
pub struct SocketInfo {
    pub handle: SocketHandle,
    pub owner: Weak<Process>,
    pub state: SocketState,
    pub target: SocketTarget,
    pub local: Option<SocketAddr>,
    pub remote: Option<SocketAddr>,
    pub output_pending: usize,
    pub read_eof: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct Record {
    pub info: SocketInfo,
    pub callback: Arc<FunctionPtr>,
    pub accepted_by: Option<SocketHandle>,
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.info.handle == other.info.handle
            && Weak::ptr_eq(&self.info.owner, &other.info.owner)
            && self.info.state == other.info.state
            && self.info.target == other.info.target
            && self.info.local == other.info.local
            && self.info.remote == other.info.remote
            && self.info.output_pending == other.info.output_pending
            && self.info.read_eof == other.info.read_eof
            && self.accepted_by == other.accepted_by
            && Arc::ptr_eq(&self.callback, &other.callback)
    }
}
impl Eq for Record {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Cells {
    pub record: VarId,
    pub output: VarId,
    pub inbox: VarId,
}

pub(crate) type Index = HashMap<SocketHandle, Cells>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Reservation {
    sequence: u64,
    bytes: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Output {
    bytes: usize,
    sequence: u64,
    pending: Vector<Reservation>,
    closing: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Index(Index),
    Record(Record),
    Output(Output),
    Inbox(Inbox),
    Retired,
}

#[derive(Debug)]
struct Queue {
    sender: flume::Sender<SocketCommands>,
    receiver: parking_lot::Mutex<Option<flume::Receiver<SocketCommands>>>,
}

/// One VM's registry, limits, and lazy transport command channel.
#[derive(Debug)]
pub struct Sockets {
    registry: OnceLock<VarId>,
    queue: OnceLock<Queue>,
    next_handle: AtomicI64,
    max_sockets: usize,
    max_output: usize,
    max_writes: usize,
    max_input: usize,
    max_accepts: usize,
}

impl Sockets {
    /// Construct without allocating world cells or a command channel.
    pub fn new(config: &Config) -> Self {
        Self {
            registry: OnceLock::new(),
            queue: OnceLock::new(),
            next_handle: AtomicI64::new(0),
            max_sockets: config.max_sockets,
            max_output: config.max_pending_socket_output,
            max_writes: config.max_pending_socket_writes,
            max_input: config.max_pending_socket_input,
            max_accepts: config.max_pending_socket_accepts,
        }
    }

    /// Take the sole transport receiver; a second take returns `None`.
    pub fn take_commands(&self) -> Option<flume::Receiver<SocketCommands>> {
        self.queue().receiver.lock().take()
    }

    fn queue(&self) -> &Queue {
        self.queue.get_or_init(|| {
            let (sender, receiver) = flume::unbounded();
            Queue {
                sender,
                receiver: parking_lot::Mutex::new(Some(receiver)),
            }
        })
    }

    pub(crate) fn root(&self) -> Option<VarId> {
        self.registry.get().copied()
    }

    fn registry(&self) -> VarId {
        *self.registry.get_or_init(VarId::new)
    }

    fn sender(&self) -> Result<&flume::Sender<SocketCommands>> {
        let sender = &self.queue().sender;
        if sender.is_disconnected() {
            Err(error("transport is unavailable"))
        } else {
            Ok(sender)
        }
    }

    fn cells(&self, txn: &mut Transaction, handle: SocketHandle) -> Option<Cells> {
        index(txn, self.registry()).get(&handle).copied()
    }

    fn record(&self, txn: &mut Transaction, handle: SocketHandle) -> Result<(Cells, Record)> {
        let cells = self
            .cells(txn, handle)
            .ok_or_else(|| error("stale handle"))?;
        match txn.read_value(cells.record) {
            Some(WorldValue::Socket(value)) => match value.as_ref() {
                Value::Record(record) => Ok((cells, record.clone())),
                Value::Retired => Err(error("stale handle")),
                _ => unreachable!("socket record cell holds metadata"),
            },
            _ => Err(error("stale handle")),
        }
    }

    fn remove(&self, txn: &mut Transaction, handle: SocketHandle) {
        let Ok((cells, record)) = self.record(txn, handle) else {
            return;
        };
        self.cancel_accepts(txn, cells);
        if let Some(owner) = record.info.owner.upgrade() {
            let cell = owner_index(&owner);
            let mut owned = index(txn, cell);
            owned.remove(&handle);
            txn.write_socket(cell, Value::Index(owned));
        }
        txn.drop_var(cells.output);
        txn.drop_var(cells.inbox);
        // The hidden registry entry charges queued buffers and workers until disposal commits.
        txn.publish_socket(
            cells.record,
            Some(Value::Retired),
            &self.queue().sender,
            SocketCommand::Retire { handle },
        );
    }

    pub(crate) fn retire_object(&self, txn: &mut Transaction, owner: &Process) {
        let Some(cell) = owner.socket_index.get() else {
            return;
        };
        for handle in index(txn, *cell).keys().copied().collect::<Vec<_>>() {
            self.remove(txn, handle);
        }
        txn.drop_var(*cell);
    }
}

/// Low-level socket operations in a task's shared transaction.
pub struct SocketAccess<'a> {
    sockets: &'a Sockets,
    txn: TxnHandle,
}

impl<'a> SocketAccess<'a> {
    /// Join this task's VM and transaction; efun authorization remains the caller's job.
    pub fn new(context: &'a TaskContext) -> Self {
        Self {
            sockets: &context.global_state.sockets,
            txn: context.txn().clone(),
        }
    }
}

impl SocketAccess<'_> {
    /// Reserve a connecting stream; transport setup starts only after commit.
    pub fn connect(
        &self,
        owner: &Arc<Process>,
        host: String,
        port: u16,
        callback: Arc<FunctionPtr>,
    ) -> Result<SocketHandle> {
        if host.is_empty() || port == 0 {
            return Err(error("invalid remote endpoint"));
        }
        self.create(owner, SocketTarget::Connect { host, port }, callback)
    }

    /// Reserve a listener on a numeric address, allowing an OS-selected port.
    pub fn listen(
        &self,
        owner: &Arc<Process>,
        address: SocketAddr,
        callback: Arc<FunctionPtr>,
    ) -> Result<SocketHandle> {
        self.create(owner, SocketTarget::Listen(address), callback)
    }

    fn create(
        &self,
        owner: &Arc<Process>,
        target: SocketTarget,
        callback: Arc<FunctionPtr>,
    ) -> Result<SocketHandle> {
        self.txn.with(|txn| {
            live(txn, owner)?;
            let sender = self.sockets.sender()?;
            let registry = self.sockets.registry();
            let mut all = index(txn, registry);
            if all.len() >= self.sockets.max_sockets {
                return Err(error("socket limit exceeded"));
            }
            let previous = self
                .sockets
                .next_handle
                .try_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                    value.checked_add(1)
                })
                .map_err(|_| error("handle range exhausted"))?;
            let handle = SocketHandle(previous + 1);
            let cells = Cells {
                record: VarId::new(),
                output: VarId::new(),
                inbox: VarId::new(),
            };
            let state = match &target {
                SocketTarget::Connect { .. } => SocketState::Connecting,
                SocketTarget::Listen(_) => SocketState::Binding,
            };
            let record = Record {
                info: SocketInfo {
                    handle,
                    owner: Arc::downgrade(owner),
                    state,
                    target: target.clone(),
                    local: None,
                    remote: None,
                    output_pending: 0,
                    read_eof: false,
                },
                callback,
                accepted_by: None,
            };
            all.insert(handle, cells);
            txn.write_socket(registry, Value::Index(all));
            let owned_cell = owner_index(owner);
            let mut owned = index(txn, owned_cell);
            owned.insert(handle, cells);
            txn.write_socket(owned_cell, Value::Index(owned));
            txn.write_socket(cells.output, Value::Output(Output::default()));
            txn.write_socket(cells.inbox, Value::Inbox(Inbox::default()));
            txn.publish_socket(
                cells.record,
                Some(Value::Record(record)),
                sender,
                SocketCommand::Open { handle, target },
            );
            Ok(handle)
        })
    }

    /// Admit the whole buffer, returning false without taking a prefix on saturation.
    pub fn write(&self, owner: &Arc<Process>, handle: SocketHandle, data: Bytes) -> Result<bool> {
        self.txn.with(|txn| {
            let (cells, record) = self.sockets.record(txn, handle)?;
            owned(txn, &record, owner)?;
            if !matches!(
                record.info.state,
                SocketState::Connecting | SocketState::Open
            ) {
                return Err(error("stream is not writable"));
            }
            if data.len() > self.sockets.max_output {
                return Err(error("write exceeds output limit"));
            }
            if data.is_empty() {
                return Ok(true);
            }
            let sender = self.sockets.sender()?;
            let mut output = output(txn, cells.output);
            if output.pending.len() >= self.sockets.max_writes
                || data.len() > self.sockets.max_output - output.bytes
            {
                return Ok(false);
            }
            output.sequence = output
                .sequence
                .checked_add(1)
                .ok_or_else(|| error("write sequence exhausted"))?;
            let sequence = output.sequence;
            output.bytes += data.len();
            output.pending.push_back(Reservation {
                sequence,
                bytes: data.len(),
            });
            txn.publish_socket(
                cells.output,
                Some(Value::Output(output)),
                sender,
                SocketCommand::Write {
                    handle,
                    sequence,
                    data,
                },
            );
            Ok(true)
        })
    }

    /// Explicit close rejects subsequent writes immediately and publishes one drain command.
    pub fn close(&self, owner: &Arc<Process>, handle: SocketHandle) -> Result<()> {
        self.txn.with(|txn| {
            let (cells, mut record) = self.sockets.record(txn, handle)?;
            owned(txn, &record, owner)?;
            if matches!(
                record.info.state,
                SocketState::Closing | SocketState::Closed
            ) {
                return Ok(());
            }
            let sender = self.sockets.sender()?;
            let drain = record.info.state == SocketState::Open;
            self.sockets.cancel_accepts(txn, cells);
            self.sockets.cancel_drained(txn, cells);
            let mut output = output(txn, cells.output);
            output.closing = true;
            txn.write_socket(cells.output, Value::Output(output));
            record.info.state = SocketState::Closing;
            txn.publish_socket(
                cells.record,
                Some(Value::Record(record)),
                sender,
                SocketCommand::Close { handle, drain },
            );
            Ok(())
        })
    }

    /// Read registration metadata without observing output or input progress.
    pub(crate) fn registration(&self, handle: SocketHandle) -> Option<SocketInfo> {
        self.txn.with(|txn| {
            self.sockets
                .record(txn, handle)
                .ok()
                .map(|(_, record)| record.info)
        })
    }

    /// Read metadata; the efun adapter authorizes foreign inspection.
    pub fn info(&self, handle: SocketHandle) -> Option<SocketInfo> {
        self.txn.with(|txn| {
            let (cells, record) = self.sockets.record(txn, handle).ok()?;
            let mut info = record.info;
            info.output_pending = output(txn, cells.output).bytes;
            info.read_eof = events::inbox(txn, cells.inbox).eof;
            Some(info)
        })
    }

    /// Enumerate a selected owner's inventory or the full VM registry.
    pub fn query(&self, owner: Option<&Arc<Process>>) -> Result<Vec<SocketInfo>> {
        let handles = self.txn.with(|txn| -> Result<Vec<SocketHandle>> {
            let cell = match owner {
                Some(owner) => {
                    live(txn, owner)?;
                    owner_index(owner)
                }
                None => self.sockets.registry(),
            };
            Ok(index(txn, cell).keys().copied().collect())
        })?;
        Ok(handles
            .into_iter()
            .filter_map(|handle| self.info(handle))
            .collect())
    }

    /// Publish an OS-ready result; false means a concurrent close or retirement won.
    pub fn ready(
        &self,
        handle: SocketHandle,
        local: SocketAddr,
        remote: Option<SocketAddr>,
    ) -> Result<bool> {
        self.txn.with(|txn| {
            let Ok((cells, mut record)) = self.sockets.record(txn, handle) else {
                return Ok(false);
            };
            record.info.state = match record.info.state {
                SocketState::Connecting if remote.is_some() => SocketState::Open,
                SocketState::Binding if remote.is_none() => SocketState::Listening,
                SocketState::Connecting | SocketState::Binding => {
                    return Err(error("invalid ready endpoints"));
                }
                _ => return Ok(false),
            };
            record.info.local = Some(local);
            record.info.remote = remote;
            self.sockets.queue_event_for(
                txn,
                cells,
                handle,
                SocketEventData::Ready { local, remote },
            )?;
            txn.write_socket(cells.record, Value::Record(record));
            Ok(true)
        })
    }

    /// Release one fully written reservation; duplicate completion is a no-op.
    pub fn complete_write(&self, handle: SocketHandle, sequence: u64) -> Result<bool> {
        self.txn.with(|txn| {
            let Some(cells) = self.sockets.cells(txn, handle) else {
                return Ok(false);
            };
            let Some(mut output) = pending_output(txn, cells.output) else {
                return Ok(false);
            };
            let Some(head) = output.pending.front() else {
                return Ok(false);
            };
            if sequence < head.sequence {
                return Ok(false);
            }
            if sequence != head.sequence {
                return Err(error("out-of-order write completion"));
            }
            output.bytes -= head.bytes;
            output.pending.pop_front();
            if output.bytes == 0 && !output.closing {
                self.sockets
                    .queue_event_for(txn, cells, handle, SocketEventData::Drained)?;
            }
            txn.write_socket(cells.output, Value::Output(output));
            Ok(true)
        })
    }

    /// Change an established stream's registration after synchronous consent is checked.
    pub fn transfer(
        &self,
        handle: SocketHandle,
        previous: &Arc<Process>,
        recipient: &Arc<Process>,
        callback: Arc<FunctionPtr>,
    ) -> Result<()> {
        self.txn.with(|txn| {
            let (cells, mut record) = self.sockets.record(txn, handle)?;
            owned(txn, &record, previous)?;
            live(txn, recipient)?;
            if record.info.state != SocketState::Open {
                return Err(error("only established streams can be offered"));
            }
            let previous_cell = owner_index(previous);
            let mut previous_index = index(txn, previous_cell);
            previous_index.remove(&handle);
            txn.write_socket(previous_cell, Value::Index(previous_index));
            let recipient_cell = owner_index(recipient);
            let mut recipient_index = index(txn, recipient_cell);
            recipient_index.insert(handle, cells);
            txn.write_socket(recipient_cell, Value::Index(recipient_index));
            record.info.owner = Arc::downgrade(recipient);
            record.callback = callback;
            txn.write_socket(cells.record, Value::Record(record));
            Ok(())
        })
    }

    /// Retain terminal metadata and quota until the eventual closed callback finishes.
    pub fn closed(&self, handle: SocketHandle) {
        if let Some(info) = self.info(handle) {
            let data = SocketEventData::Closed {
                reason: SocketCloseReason::Local,
                operation: None,
                error: None,
                local: info.local,
                remote: info.remote,
            };
            if let Err(e) = self.terminal(handle, data) {
                tracing::error!("socket terminal publication failed: {e}");
            }
        }
    }

    /// Remove a terminal record after dispatch, even if its callback failed.
    pub fn finish_closed(&self, handle: SocketHandle) -> Result<()> {
        self.txn.with(|txn| {
            let (_, record) = self.sockets.record(txn, handle)?;
            if record.info.state != SocketState::Closed {
                return Err(error("socket is not closed"));
            }
            self.sockets.remove(txn, handle);
            Ok(())
        })
    }

    /// Release a hidden retirement charge after its queued commands and worker are disposed.
    pub fn disposed(&self, handle: SocketHandle) -> bool {
        self.txn.with(|txn| {
            let Some(cells) = self.sockets.cells(txn, handle) else {
                return false;
            };
            if !matches!(txn.read_value(cells.record), Some(WorldValue::Socket(value)) if matches!(value.as_ref(), Value::Retired)) {
                return false;
            }
            let registry = self.sockets.registry();
            let mut all = index(txn, registry);
            all.remove(&handle);
            txn.write_socket(registry, Value::Index(all));
            txn.drop_var(cells.record);
            true
        })
    }
}

fn owner_index(owner: &Process) -> VarId {
    *owner.socket_index.get_or_init(VarId::new)
}

fn index(txn: &mut Transaction, cell: VarId) -> Index {
    match txn.read_value(cell) {
        Some(WorldValue::Socket(value)) => match value.as_ref() {
            Value::Index(index) => index.clone(),
            _ => unreachable!("socket index cell holds an index"),
        },
        None => Index::new(),
        _ => unreachable!("socket index cell holds a socket value"),
    }
}

fn output(txn: &mut Transaction, cell: VarId) -> Output {
    pending_output(txn, cell).expect("a registered socket has an output cell")
}

fn pending_output(txn: &mut Transaction, cell: VarId) -> Option<Output> {
    match txn.read_value(cell) {
        Some(WorldValue::Socket(value)) => match value.as_ref() {
            Value::Output(output) => Some(output.clone()),
            _ => unreachable!("socket output cell holds admission state"),
        },
        None => None,
        _ => unreachable!("socket output cell holds a socket value"),
    }
}

fn live(txn: &mut Transaction, owner: &Process) -> Result<()> {
    if owner.is_live_in(txn) {
        Ok(())
    } else {
        Err(error("owner was destroyed"))
    }
}

fn owned(txn: &mut Transaction, record: &Record, owner: &Arc<Process>) -> Result<()> {
    live(txn, owner)?;
    if std::ptr::eq(record.info.owner.as_ptr(), Arc::as_ptr(owner)) {
        Ok(())
    } else {
        Err(error("wrong owner"))
    }
}

fn error(message: &str) -> LpcError {
    LpcError::runtime(format!("socket: {message}"))
}
