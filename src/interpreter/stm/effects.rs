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

use lpc_rs_utils::config::Config;
use tokio::sync::mpsc::Sender;

use crate::telnet::ops::ConnectionOp;

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
}

impl Effect {
    /// Deliver this effect physically.
    pub(crate) async fn flush(self, config: &Config) {
        match self {
            Self::DebugLog(msg) => config.debug_log(msg).await,
            Self::Socket { op, tx } => {
                let _ = tx.send(op).await;
            }
        }
    }
}

/// Deliver a batch of effects in order. The retry loop calls this after a
/// successful commit; a rejected attempt's batch is never delivered.
pub(crate) async fn flush_effects(config: &Config, effects: Vec<Effect>) {
    for effect in effects {
        effect.flush(config).await;
    }
}

/// A batch of pending effects on one attempt.
#[derive(Clone)]
pub(crate) struct EffectLog(Vec<Effect>);

impl EffectLog {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    /// Add an effect recorded during this attempt.
    pub(crate) fn record(&mut self, effect: Effect) {
        self.0.push(effect);
    }

    /// Take all effects out, leaving an empty log. The caller delivers them.
    pub(crate) fn take(&mut self) -> Vec<Effect> {
        std::mem::take(&mut self.0)
    }
}

impl Default for EffectLog {
    fn default() -> Self {
        Self::new()
    }
}

// `Effect` can't derive `Debug`: `Sender<ConnectionOp>` isn't `Debug`.
impl std::fmt::Debug for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DebugLog(msg) => f.debug_tuple("DebugLog").field(msg).finish(),
            Self::Socket { op, .. } => f.debug_tuple("Socket").field(op).finish(),
        }
    }
}

impl std::fmt::Debug for EffectLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
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

        let mut log = EffectLog::new();
        log.record(Effect::Socket {
            op: op_a.clone(),
            tx: tx_a,
        });
        log.record(Effect::Socket {
            op: op_b.clone(),
            tx: tx_b,
        });

        flush_effects(&Config::default(), log.take()).await;

        assert_eq!(rx_a.recv().await, Some(op_a));
        assert_eq!(rx_b.recv().await, Some(op_b));
    }
}
