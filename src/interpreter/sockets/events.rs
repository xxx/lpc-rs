use super::*;

/// Immutable callback payloads retained until their event is settled.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SocketEventData {
    Ready {
        local: SocketAddr,
        remote: Option<SocketAddr>,
    },
    Accept(SocketHandle),
    Read(Bytes),
    Eof,
    /// Output became empty; repeated transitions share one pending wakeup.
    Drained,
    Closed {
        reason: SocketCloseReason,
        operation: Option<SocketOperation>,
        error: Option<String>,
        local: Option<SocketAddr>,
        remote: Option<SocketAddr>,
    },
}

/// A stable identity and immutable payload within one socket's FIFO inbox.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SocketEvent {
    pub handle: SocketHandle,
    pub id: u64,
    pub data: SocketEventData,
}

/// The head event and current registration, read anew in each dispatch attempt.
#[derive(Debug, Clone)]
pub struct SocketLease {
    event: SocketEvent,
    owner: Arc<Process>,
    callback: Arc<FunctionPtr>,
    txn: TxnHandle,
}

impl SocketLease {
    /// The immutable identity and payload of this head event.
    pub fn event(&self) -> &SocketEvent {
        &self.event
    }

    /// The scheduling owner read in this attempt.
    pub fn owner(&self) -> &Arc<Process> {
        &self.owner
    }

    /// The callback registration read in this attempt.
    pub fn callback(&self) -> &Arc<FunctionPtr> {
        &self.callback
    }
}

/// An accept slot held before the next physical accept, including unpublished input.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SocketAcceptPermit {
    pub listener: SocketHandle,
    pub sequence: u64,
    pub child: SocketHandle,
}

/// Native admission distinguishes backpressure from a retired or closing resource.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketAdmission<T> {
    Admitted(T),
    Full,
    Stopped,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Inbox {
    sequence: u64,
    events: Vector<SocketEvent>,
    input: usize,
    last_read: u64,
    drained: bool,
    pub(super) eof: bool,
    accepts: usize,
    accept_sequence: u64,
    accept_slot: Option<SocketAcceptPermit>,
}

impl Inbox {
    fn push(&mut self, handle: SocketHandle, data: SocketEventData) -> Result<()> {
        if data == SocketEventData::Drained && self.drained {
            return Ok(());
        }
        self.sequence = self
            .sequence
            .checked_add(1)
            .ok_or_else(|| error("event sequence exhausted"))?;
        self.drained |= data == SocketEventData::Drained;
        self.events.push_back(SocketEvent {
            handle,
            id: self.sequence,
            data,
        });
        Ok(())
    }
}

pub(super) fn inbox(txn: &mut Transaction, cell: VarId) -> Inbox {
    match txn.read_value(cell) {
        Some(WorldValue::Socket(value)) => match value.as_ref() {
            Value::Inbox(inbox) => inbox.clone(),
            _ => unreachable!("socket inbox cell holds events"),
        },
        _ => unreachable!("registered sockets have an inbox"),
    }
}

impl Sockets {
    pub(super) fn queue_event_for(
        &self,
        txn: &mut Transaction,
        cells: Cells,
        handle: SocketHandle,
        data: SocketEventData,
    ) -> Result<()> {
        let mut pending = inbox(txn, cells.inbox);
        pending.push(handle, data)?;
        txn.write_socket(cells.inbox, Value::Inbox(pending));
        Ok(())
    }

    pub(super) fn cancel_accepts(&self, txn: &mut Transaction, cells: Cells) {
        let mut pending = inbox(txn, cells.inbox);
        if pending.accepts == 0 {
            return;
        }
        let children: Vec<_> = pending
            .events
            .iter()
            .filter_map(|event| {
                if let SocketEventData::Accept(child) = event.data {
                    Some(child)
                } else {
                    None
                }
            })
            .collect();
        pending.events = pending
            .events
            .into_iter()
            .filter(|event| !matches!(event.data, SocketEventData::Accept(_)))
            .collect();
        if let Some(slot) = pending.accept_slot {
            let registry = self.registry();
            let mut all = index(txn, registry);
            if let Some(reserved) = all.remove(&slot.child) {
                txn.drop_var(reserved.record);
                txn.drop_var(reserved.output);
                txn.drop_var(reserved.inbox);
                txn.write_socket(registry, Value::Index(all));
            }
        }
        pending.accepts = 0;
        pending.accept_slot = None;
        txn.write_socket(cells.inbox, Value::Inbox(pending));
        for child in children {
            self.remove(txn, child, true);
        }
    }
}

impl SocketAccess<'_> {
    /// Lease the head in this transaction; retries must obtain a fresh registration.
    pub fn lease(&self, handle: SocketHandle) -> Result<Option<SocketLease>> {
        self.txn.with(|txn| {
            let Ok((cells, record)) = self.sockets.record(txn, handle) else {
                return Ok(None);
            };
            if record.accepted_by.is_some() {
                return Ok(None);
            }
            let Some(owner) = record.info.owner.upgrade() else {
                return Ok(None);
            };
            live(txn, &owner)?;
            Ok(inbox(txn, cells.inbox)
                .events
                .front()
                .cloned()
                .map(|event| SocketLease {
                    event,
                    owner,
                    callback: record.callback,
                    txn: self.txn.clone(),
                }))
        })
    }

    /// Settle the leased head atomically with callback writes; failure also retires an undelivered child.
    pub fn acknowledge(&self, lease: &SocketLease, succeeded: bool) -> Result<bool> {
        if !self.txn.same_transaction(&lease.txn) {
            return Err(error("event lease belongs to another transaction"));
        }
        self.txn.with(|txn| {
            let handle = lease.event.handle;
            let Ok((cells, _)) = self.sockets.record(txn, handle) else {
                return Ok(false);
            };
            let mut pending = inbox(txn, cells.inbox);
            if pending
                .events
                .front()
                .is_none_or(|head| head.id != lease.event.id)
            {
                return Ok(false);
            }
            let event = pending
                .events
                .pop_front()
                .ok_or_else(|| error("missing leased head"))?;
            match event.data {
                SocketEventData::Read(bytes) => pending.input -= bytes.len(),
                SocketEventData::Drained => pending.drained = false,
                SocketEventData::Accept(child) => {
                    pending.accepts -= 1;
                    if let Ok((child_cells, mut record)) = self.sockets.record(txn, child) {
                        let owner_live = record
                            .info
                            .owner
                            .upgrade()
                            .is_some_and(|owner| owner.is_live_in(txn));
                        if succeeded && owner_live {
                            record.accepted_by = None;
                            if record.info.state == SocketState::Closed {
                                txn.write_socket(child_cells.record, Value::Record(record));
                            } else {
                                txn.publish_socket(
                                    child_cells.record,
                                    Some(Value::Record(record)),
                                    &self.sockets.queue().sender,
                                    SocketCommand::Activate { handle: child },
                                );
                            }
                        } else {
                            self.sockets.remove(txn, child, true);
                        }
                    }
                }
                SocketEventData::Closed { .. } => {
                    self.sockets.remove(txn, handle, false);
                    return Ok(true);
                }
                _ => {}
            }
            txn.write_socket(cells.inbox, Value::Inbox(pending));
            Ok(true)
        })
    }

    /// Remaining input credits, or no readable stream after close, EOF or a pending accept.
    pub fn input_capacity(&self, handle: SocketHandle) -> Result<Option<usize>> {
        self.txn.with(|txn| {
            let Ok((cells, record)) = self.sockets.record(txn, handle) else {
                return Ok(None);
            };
            let pending = inbox(txn, cells.inbox);
            Ok((record.info.state == SocketState::Open
                && record.accepted_by.is_none()
                && !pending.eof)
                .then_some(self.sockets.max_input - pending.input))
        })
    }

    /// Publish one ordered nonempty read; retrying the same source sequence never duplicates it.
    pub fn read(&self, handle: SocketHandle, sequence: u64, bytes: Bytes) -> Result<bool> {
        self.txn.with(|txn| {
            let Ok((cells, record)) = self.sockets.record(txn, handle) else {
                return Ok(false);
            };
            let mut pending = inbox(txn, cells.inbox);
            if record.info.state != SocketState::Open
                || record.accepted_by.is_some()
                || pending.eof
                || sequence <= pending.last_read
            {
                return Ok(false);
            }
            if sequence != pending.last_read + 1 || bytes.is_empty() {
                return Err(error("invalid read sequence or empty read"));
            }
            if bytes.len() > self.sockets.max_input - pending.input {
                return Err(error("read exceeds available input credits"));
            }
            let count = bytes.len();
            pending.push(handle, SocketEventData::Read(bytes))?;
            pending.input += count;
            pending.last_read = sequence;
            txn.write_socket(cells.inbox, Value::Inbox(pending));
            Ok(true)
        })
    }

    /// Publish EOF after preceding reads without changing the writable stream state.
    pub fn eof(&self, handle: SocketHandle, sequence: u64) -> Result<bool> {
        self.txn.with(|txn| {
            let Ok((cells, record)) = self.sockets.record(txn, handle) else {
                return Ok(false);
            };
            let mut pending = inbox(txn, cells.inbox);
            if record.info.state != SocketState::Open
                || record.accepted_by.is_some()
                || pending.eof
                || sequence <= pending.last_read
            {
                return Ok(false);
            }
            if sequence != pending.last_read + 1 {
                return Err(error("invalid EOF sequence"));
            }
            pending.push(handle, SocketEventData::Eof)?;
            pending.eof = true;
            pending.last_read = sequence;
            txn.write_socket(cells.inbox, Value::Inbox(pending));
            Ok(true)
        })
    }

    /// Reserve one unpublished accept slot before polling the OS.
    pub fn reserve_accept(
        &self,
        handle: SocketHandle,
    ) -> Result<SocketAdmission<SocketAcceptPermit>> {
        self.txn.with(|txn| {
            let Ok((cells, record)) = self.sockets.record(txn, handle) else {
                return Ok(SocketAdmission::Stopped);
            };
            if record.info.state != SocketState::Listening {
                return Ok(SocketAdmission::Stopped);
            }
            let mut pending = inbox(txn, cells.inbox);
            if pending.accept_slot.is_some() || pending.accepts >= self.sockets.max_accepts {
                return Ok(SocketAdmission::Full);
            }
            pending.accept_sequence = pending
                .accept_sequence
                .checked_add(1)
                .ok_or_else(|| error("accept sequence exhausted"))?;
            let sequence = pending.accept_sequence;
            let registry = self.sockets.registry();
            let mut all = index(txn, registry);
            if all.len() >= self.sockets.max_sockets {
                return Ok(SocketAdmission::Full);
            }
            let previous = self
                .sockets
                .next_handle
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                    value.checked_add(1)
                })
                .map_err(|_| error("handle range exhausted"))?;
            let child = SocketHandle(previous + 1);
            let reserved = Cells {
                record: VarId::new(),
                output: VarId::new(),
                inbox: VarId::new(),
            };
            all.insert(child, reserved);
            txn.write_socket(registry, Value::Index(all));
            txn.write_socket(reserved.output, Value::Output(Output::default()));
            txn.write_socket(reserved.inbox, Value::Inbox(Inbox::default()));
            let permit = SocketAcceptPermit {
                listener: handle,
                sequence,
                child,
            };
            pending.accept_slot = Some(permit);
            pending.accepts += 1;
            txn.write_socket(cells.inbox, Value::Inbox(pending));
            Ok(SocketAdmission::Admitted(permit))
        })
    }

    /// Admit an accepted child against VM quota before activating or announcing it.
    pub fn accepted(
        &self,
        permit: SocketAcceptPermit,
        local: SocketAddr,
        remote: SocketAddr,
    ) -> Result<SocketAdmission<SocketHandle>> {
        self.txn.with(|txn| {
            let Ok((parent_cells, parent)) = self.sockets.record(txn, permit.listener) else {
                return Ok(SocketAdmission::Stopped);
            };
            let mut pending = inbox(txn, parent_cells.inbox);
            if parent.info.state != SocketState::Listening || pending.accept_slot != Some(permit) {
                return Ok(SocketAdmission::Stopped);
            }
            let Some(owner) = parent.info.owner.upgrade() else {
                return Ok(SocketAdmission::Stopped);
            };
            live(txn, &owner)?;
            let handle = permit.child;
            let Some(cells) = self.sockets.cells(txn, handle) else {
                return Ok(SocketAdmission::Stopped);
            };
            pending.accept_slot = None;
            pending.push(permit.listener, SocketEventData::Accept(handle))?;
            let record = Record {
                info: SocketInfo {
                    handle,
                    owner: Arc::downgrade(&owner),
                    state: SocketState::Open,
                    target: SocketTarget::Connect {
                        host: remote.ip().to_string(),
                        port: remote.port(),
                    },
                    local: Some(local),
                    remote: Some(remote),
                    output_pending: 0,
                    read_eof: false,
                },
                callback: parent.callback,
                accepted_by: Some(permit.listener),
            };
            let owned_cell = owner_index(&owner);
            let mut owned = index(txn, owned_cell);
            owned.insert(handle, cells);
            txn.write_socket(owned_cell, Value::Index(owned));
            txn.write_socket(cells.output, Value::Output(Output::default()));
            txn.write_socket(cells.inbox, Value::Inbox(Inbox::default()));
            txn.write_socket(parent_cells.inbox, Value::Inbox(pending));
            txn.publish_socket(
                cells.record,
                Some(Value::Record(record)),
                &self.sockets.queue().sender,
                SocketCommand::OpenAccepted { handle },
            );
            Ok(SocketAdmission::Admitted(handle))
        })
    }

    pub(super) fn terminal(&self, handle: SocketHandle, data: SocketEventData) -> Result<bool> {
        self.txn.with(|txn| {
            let Ok((cells, mut record)) = self.sockets.record(txn, handle) else {
                return Ok(false);
            };
            if record.info.state == SocketState::Closed {
                return Ok(false);
            }
            self.sockets.cancel_accepts(txn, cells);
            self.sockets.queue_event_for(txn, cells, handle, data)?;
            record.info.state = SocketState::Closed;
            let mut output = output(txn, cells.output);
            output.bytes = 0;
            output.pending.clear();
            output.closing = true;
            txn.write_socket(cells.record, Value::Record(record));
            txn.write_socket(cells.output, Value::Output(output));
            Ok(true)
        })
    }
}

#[cfg(test)]
mod tests;
