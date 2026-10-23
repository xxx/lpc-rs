//! Authorized connection observation, with transactional relationships and bounded delivery.

use std::sync::{Arc, Weak};

use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::{LpcError, Result};

use crate::{
    interpreter::{
        RECEIVE_SNOOP,
        process::Process,
        program::Function,
        stm::{Effect, Transaction, TxnHandle, VarId, WorldValue},
    },
    telnet::ops::ConnectionOp,
};

mod delivery;
pub use delivery::Registration;

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub(crate) struct Slots {
    pub incoming: VarId,
    pub outgoing: VarId,
    pub serial: Arc<tokio::sync::Mutex<()>>,
    budget: Arc<parking_lot::Mutex<delivery::Budget>>,
}

impl Default for Slots {
    fn default() -> Self {
        Self {
            incoming: VarId::new(),
            outgoing: VarId::new(),
            serial: Arc::default(),
            budget: Arc::default(),
        }
    }
}

pub(crate) fn slots(process: &Process) -> &Slots {
    process.snoop.get_or_init(Box::default)
}

fn read(txn: &mut Transaction, cell: VarId) -> Option<Arc<Registration>> {
    match txn.read_value(cell) {
        Some(WorldValue::Snoop(registration)) => Some(registration),
        _ => None,
    }
}

pub(crate) fn incoming(txn: &mut Transaction, process: &Process) -> Option<Arc<Registration>> {
    read(txn, slots(process).incoming)
}

pub(crate) fn outgoing(txn: &mut Transaction, process: &Process) -> Option<Arc<Registration>> {
    read(txn, slots(process).outgoing)
}

pub(crate) fn handler(txn: &TxnHandle, observer: &Process) -> Result<Function> {
    let function = observer
        .program(txn)
        .unmangled_functions
        .get(RECEIVE_SNOOP)
        .cloned();
    let Some(function) = function else {
        return Err(LpcError::runtime(
            "snoop: observer has no receive_snoop apply",
        ));
    };
    let expected = [
        LpcType::String(false),
        LpcType::Object(false),
        LpcType::String(false),
    ];
    if !function.prototype.accepts_arg_count(3)
        || function.prototype.first_ref_param().is_some()
        || function
            .prototype
            .arg_types
            .iter()
            .zip(expected)
            .any(|(declared, actual)| !declared.matches_type(actual))
    {
        return Err(LpcError::runtime("snoop: incompatible receive_snoop apply"));
    }
    Ok(function)
}

pub(crate) fn start(
    txn: &TxnHandle,
    observer: &Arc<Process>,
    target: &Arc<Process>,
    limit: usize,
) -> Result<()> {
    handler(txn, observer)?;
    txn.with(|t| {
        if !observer.is_live_in(t) || !target.is_live_in(t) {
            return Err(LpcError::runtime("snoop: endpoint was destroyed"));
        }
        let connection = t
            .read_connection(target.connection.id)
            .filter(|c| !c.is_dead())
            .ok_or_else(|| LpcError::runtime("snoop: target is not interactive"))?;
        if Arc::ptr_eq(observer, target) {
            return Err(LpcError::runtime("snoop: cycle"));
        }
        if let Some(existing) = outgoing(t, observer) {
            if existing
                .target
                .upgrade()
                .is_some_and(|p| Arc::ptr_eq(&p, target))
                && existing.valid(t)
            {
                return Ok(());
            }
            return Err(LpcError::runtime("snoop: observer is already attached"));
        }
        if incoming(t, target).is_some() {
            return Err(LpcError::runtime("snoop: target already has an observer"));
        }
        let mut cursor = target.clone();
        while let Some(next) = outgoing(t, &cursor).and_then(|r| r.target.upgrade()) {
            if Arc::ptr_eq(&next, observer) {
                return Err(LpcError::runtime("snoop: cycle"));
            }
            cursor = next;
        }
        let registration = Arc::new(Registration::new(observer, target, &connection, limit));
        t.write_snoop(slots(observer).outgoing, registration.clone());
        t.write_snoop(slots(target).incoming, registration.clone());
        t.record_effect(Effect::Socket {
            op: ConnectionOp::StartSnoop(registration),
            tx: connection.sender(),
        });
        Ok(())
    })
}

pub(crate) fn retire(txn: &mut Transaction, registration: &Arc<Registration>) {
    for (endpoint, incoming) in [
        (&registration.observer, false),
        (&registration.target, true),
    ] {
        if let Some(process) = endpoint.upgrade() {
            let s = slots(&process);
            let cell = if incoming { s.incoming } else { s.outgoing };
            if read(txn, cell).is_some_and(|current| current.id == registration.id) {
                txn.drop_var(cell);
            }
        }
    }
    txn.record_effect(Effect::StopSnoop(registration.clone()));
    if let Some(connection) = registration.connection.upgrade() {
        txn.record_effect(Effect::Socket {
            op: ConnectionOp::StopSnoop(registration.id),
            tx: connection.sender(),
        });
    }
}

/// A connection change ends incoming observation but preserves this object's outgoing relay.
pub(crate) fn retire_target(txn: &mut Transaction, target: &Process) {
    if let Some(s) = target.snoop.get()
        && let Some(registration) = read(txn, s.incoming)
    {
        retire(txn, &registration);
    }
}

pub(crate) fn retire_object(txn: &mut Transaction, process: &Process) {
    retire_target(txn, process);
    if let Some(s) = process.snoop.get()
        && let Some(registration) = read(txn, s.outgoing)
    {
        retire(txn, &registration);
    }
}

impl Registration {
    pub(crate) fn valid(&self, txn: &mut Transaction) -> bool {
        let (Some(observer), Some(target), Some(connection)) = (
            self.observer.upgrade(),
            self.target.upgrade(),
            self.connection.upgrade(),
        ) else {
            return false;
        };
        observer.is_live_in(txn)
            && target.is_live_in(txn)
            && !connection.is_dead()
            && txn
                .read_connection(target.connection.id)
                .is_some_and(|c| Arc::ptr_eq(&c, &connection))
            && outgoing(txn, &observer).is_some_and(|r| r.id == self.id)
            && incoming(txn, &target).is_some_and(|r| r.id == self.id)
    }
}

fn weak_object(process: &Weak<Process>) -> crate::interpreter::lpc_ref::LpcRef {
    crate::interpreter::lpc_ref::LpcRef::Object(process.clone())
}
