//! Physical side effects that a task must deliver exactly once, after its
//! transaction commits.
//!
//! A task that writes output mid-attempt records an [`Effect`] on its
//! transaction instead of delivering it: the delivery would be physical and
//! irreversible, but the attempt itself may still be rejected. On a rejected
//! attempt the log is dropped with the attempt, and the re-run records the
//! effects fresh; on a successful commit the retry loop takes and flushes the
//! log. Either way each committed attempt's effects fire exactly once, and an
//! aborted attempt emits nothing.
//!
//! Every variant carries a fully materialized payload captured at record
//! time: a debug-log message is already a formatted `String`, a socket op
//! carries its own send channel. Flushing never re-resolves a transactional
//! cell, so an effect can never observe end-of-transaction state.

use std::sync::Arc;

use lpc_rs_utils::config::Config;
use tokio::sync::mpsc::Sender;

use crate::{
    interpreter::{
        call_outs::CallOuts, lpc_ref::LpcRef, object_space::ObjectSpace, process::Process,
    },
    telnet::{
        connection::Connection,
        ops::{BrokerOp, ConnectionOp},
    },
};

/// A fully materialized, not-yet-scheduled call out. Everything the physical
/// materialization needs is captured at record time: the owner process, the
/// function, the timing, and the explicit ID. The flush never re-resolves a
/// transactional value.
#[derive(Clone)]
pub struct CallOutSchedule {
    /// The call out's explicit ID, minted at record time.
    pub id: u64,
    /// The process the `call_out` was called from. Held weakly (as in the
    /// physical [`CallOut`]); the owner is kept alive by its commit into the
    /// object space, so no strong ref is needed across the flush.
    pub process: std::sync::Weak<Process>,
    /// The function to run.
    pub func_ref: LpcRef,
    /// The delay before the first run.
    pub delay: chrono::Duration,
    /// The repeat interval, if this is a repeating call out.
    pub repeat: Option<chrono::Duration>,
}

impl std::fmt::Debug for CallOutSchedule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallOutSchedule")
            .field("id", &self.id)
            .field("func_ref", &self.func_ref)
            .field("delay", &self.delay)
            .field("repeat", &self.repeat)
            .finish()
    }
}

/// One physical side effect pending delivery.
#[derive(Clone)]
pub(crate) enum Effect {
    /// A line for the in-game debug log (where unreceived `write`s go).
    DebugLog(String),

    /// A socket operation, with the channel it must be sent on. The op is
    /// `Clone`, so this is a copy of what the efun would have sent directly.
    Socket {
        op: ConnectionOp,
        tx: Sender<ConnectionOp>,
    },

    /// A deferred object-space insert: the `Process` and its physical key,
    /// materialized at record time. Applied to the `ObjectSpace` at commit.
    InsertObject {
        key: String,
        process: std::sync::Arc<Process>,
    },

    /// A deferred object-space removal of `process` under its key, applied at
    /// commit; a newer object under the key stays.
    RemoveObject {
        key: String,
        process: std::sync::Arc<Process>,
    },

    /// A deferred call-out scheduling: the timer task is spawned and the
    /// entry pushed into the queue only when the attempt commits. An
    /// aborted attempt's record is dropped with the attempt, so no timer
    /// is spawned and nothing is enqueued for work that never happened.
    ScheduleCallOut(CallOutSchedule),

    /// A deferred call-out cancellation: a committed call out whose ID
    /// matches this one is removed from the queue at flush. A no-op if it
    /// is already gone (e.g. it already fired and removed itself).
    CancelCallOut { id: u64 },

    /// A deferred socket-level `exec` handover. The transactional part (the
    /// connection cell on `new_process`) is committed as part of the owning
    /// task; this effect is the physical part that must not run until that
    /// commit lands. `connection` is the connection the cell now holds; the
    /// flush points its back-reference at `new_process`. `previous` is the
    /// connection that was bound before the handover (if any): the telnet
    /// input it belongs to now goes to `new_process` instead, so it is
    /// disconnected. All fields are captured at record time, so the flush
    /// never re-resolves a cell.
    Exec {
        new_process: Arc<Process>,
        connection: Arc<Connection>,
        previous: Option<Arc<Connection>>,
    },
}

impl Effect {
    /// Deliver this effect physically. Object effects go to the passed
    /// `ObjectSpace` (the committer's physical map); the others to config /
    /// their own channel. The call-out lock is held only for the synchronous
    /// materialize/remove, never across an await.
    pub(crate) async fn flush(
        self,
        config: &Config,
        object_space: &ObjectSpace,
        call_outs: &parking_lot::RwLock<CallOuts>,
    ) {
        match self {
            Self::DebugLog(msg) => config.debug_log(msg).await,
            Self::Socket { op, tx } => {
                let _ = tx.send(op).await;
            }
            Self::InsertObject { key, process } => {
                object_space.apply_insert(&key, process);
            }
            Self::RemoveObject { key, process } => {
                object_space.apply_remove(&key, &process);
            }
            Self::ScheduleCallOut(schedule) => {
                call_outs.write().materialize(schedule);
            }
            Self::CancelCallOut { id } => {
                call_outs.write().remove_by_id(id);
            }
            Self::Exec {
                new_process,
                connection,
                previous,
            } => {
                // The cell write already committed with the owning task, so
                // the back-reference is the only physical handover left.
                connection.process.store(Some(new_process.clone()));

                // The connection moved to `new_process`; its old holder is
                // disconnected, mirroring the previous `takeover_process`.
                if let Some(prev_conn) = &previous {
                    let _ = prev_conn
                        .tx
                        .send(ConnectionOp::SendMessage(
                            "You are being disconnected because someone else logged in as you."
                                .to_string(),
                        ))
                        .await;
                    let _ = prev_conn
                        .broker_tx
                        .send_async(BrokerOp::Disconnect(prev_conn.address))
                        .await;
                }
            }
        }
    }
}

/// Deliver a batch of effects in order. The retry loop calls this after a
/// successful commit; a rejected attempt's batch is never delivered.
pub(crate) async fn flush_effects(
    config: &Config,
    object_space: &ObjectSpace,
    call_outs: &parking_lot::RwLock<CallOuts>,
    effects: Vec<Effect>,
) {
    for effect in effects {
        effect.flush(config, object_space, call_outs).await;
    }
}

// `Effect` can't derive `Debug`: `Sender<ConnectionOp>` isn't `Debug`.
impl std::fmt::Debug for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DebugLog(msg) => f.debug_tuple("DebugLog").field(msg).finish(),
            Self::Socket { op, .. } => f.debug_tuple("Socket").field(op).finish(),
            Self::InsertObject { key, .. } => f.debug_tuple("InsertObject").field(key).finish(),
            Self::RemoveObject { key, .. } => f.debug_tuple("RemoveObject").field(key).finish(),
            Self::ScheduleCallOut(schedule) => {
                f.debug_tuple("ScheduleCallOut").field(schedule).finish()
            }
            Self::CancelCallOut { id } => f.debug_tuple("CancelCallOut").field(id).finish(),
            Self::Exec { .. } => f.debug_tuple("Exec").finish(),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::telnet::ops::ConnectionOp;

    /// A recorded socket op must arrive on its own channel when the batch is
    /// flushed, and a second `Effect` recorded against a second channel must
    /// go to that second channel: the op travels with the channel it was
    /// recorded on, never with the flushed batch's owner.
    #[tokio::test]
    async fn flush_delivers_socket_ops_on_their_recorded_channels() {
        let (tx_a, mut rx_a) = tokio::sync::mpsc::channel(16);
        let (tx_b, mut rx_b) = tokio::sync::mpsc::channel(16);
        let op_a = ConnectionOp::SendMessage("a".to_string());
        let op_b = ConnectionOp::SendMessage("b".to_string());

        let log = vec![
            Effect::Socket {
                op: op_a.clone(),
                tx: tx_a,
            },
            Effect::Socket {
                op: op_b.clone(),
                tx: tx_b,
            },
        ];

        let object_space = ObjectSpace::default();
        let (tx_d, _rx_d) = tokio::sync::mpsc::channel(16);
        let call_outs = parking_lot::RwLock::new(CallOuts::new(tx_d));
        flush_effects(&Config::default(), &object_space, &call_outs, log).await;

        assert_eq!(rx_a.recv().await, Some(op_a));
        assert_eq!(rx_b.recv().await, Some(op_b));
    }
}
