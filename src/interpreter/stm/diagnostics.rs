//! Conflict provenance and compilation timing, without retaining game objects.

use std::{fmt, sync::Arc, time::Duration};

use tokio::time::Instant;

use super::{Transaction, TxnHandle, VarId, Version, WorldValue};
use crate::interpreter::{lpc_ref::LpcRef, object_space::ObjectSpace, process::Process};

/// The owning entry point; arguments and command text are never captured.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CommitOrigin {
    object: String,
    entry: String,
}

impl CommitOrigin {
    pub(crate) fn new(process: &Process, entry: &str) -> Self {
        Self {
            object: process.filename().into_owned(),
            entry: entry.to_owned(),
        }
    }
}

impl fmt::Display for CommitOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.object, self.entry)
    }
}

/// The first validation failure encountered, rather than every conflicting cell.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Conflict {
    ReadInvalidated {
        cell: VarId,
        base: Version,
        current: Version,
        written_at: Version,
        writer: Option<Arc<CommitOrigin>>,
    },
    MergeMismatch {
        cell: VarId,
        base: Version,
        current: Version,
    },
    HistoryUnavailable {
        base: Version,
        oldest: Version,
        current: Version,
    },
    FutureVersion {
        base: Version,
        current: Version,
    },
    #[cfg(test)]
    Forced,
}

impl Conflict {
    pub(crate) fn cell(&self) -> Option<VarId> {
        match self {
            Self::ReadInvalidated { cell, .. } | Self::MergeMismatch { cell, .. } => Some(*cell),
            _ => None,
        }
    }
}

impl fmt::Display for Conflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReadInvalidated {
                cell,
                base,
                current,
                written_at,
                writer,
            } => {
                write!(
                    f,
                    "read invalidated: cell={}, base={}, current={}, written_at={}, writer=",
                    cell.0, base.0, current.0, written_at.0
                )?;
                match writer {
                    Some(writer) => writer.fmt(f),
                    None => f.write_str("unknown"),
                }
            }
            Self::MergeMismatch {
                cell,
                base,
                current,
            } => write!(
                f,
                "merge type mismatch: cell={}, base={}, current={}",
                cell.0, base.0, current.0
            ),
            Self::HistoryUnavailable {
                base,
                oldest,
                current,
            } => write!(
                f,
                "history unavailable: base={}, oldest={}, current={}",
                base.0, oldest.0, current.0
            ),
            Self::FutureVersion { base, current } => write!(
                f,
                "future base version: base={}, current={}",
                base.0, current.0
            ),
            #[cfg(test)]
            Self::Forced => f.write_str("forced test rejection"),
        }
    }
}

impl Transaction {
    /// Payload aliases come from this snapshot and never enter the read set.
    pub(crate) fn describe_cell(&self, space: &ObjectSpace, cell: VarId) -> Option<String> {
        if let Some(label) = space.describe_cell(cell) {
            return Some(label);
        }
        let kind = match self.snapshot.peek(cell) {
            Some(WorldValue::Array(_)) => "array payload",
            Some(WorldValue::Mapping(_)) => "mapping payload",
            _ => return None,
        };
        for (slot, value) in self.snapshot.state() {
            let matches = match value {
                WorldValue::Ref(LpcRef::Array(array)) => array.id == cell,
                WorldValue::Ref(LpcRef::Mapping(mapping)) => mapping.id == cell,
                _ => false,
            };
            if matches && let Some(label) = space.describe_cell(*slot) {
                return Some(format!("{kind} via {label}"));
            }
        }
        Some(kind.to_owned())
    }
}

/// Nested compiler invocations count once, including interrupted compilation.
#[derive(Debug, Clone, Default)]
pub(crate) struct CompilationTiming {
    depth: usize,
    started: Option<Instant>,
    pub(crate) elapsed: Duration,
}

pub(crate) struct CompilationTimer(TxnHandle);

impl TxnHandle {
    pub(crate) fn time_compilation(&self) -> CompilationTimer {
        self.with(|txn| {
            if txn.compilation.depth == 0 {
                txn.compilation.started = Some(Instant::now());
            }
            txn.compilation.depth += 1;
        });
        CompilationTimer(self.clone())
    }
}

impl Drop for CompilationTimer {
    fn drop(&mut self) {
        self.0.with(|txn| {
            txn.compilation.depth -= 1;
            if txn.compilation.depth == 0
                && let Some(started) = txn.compilation.started.take()
            {
                txn.compilation.elapsed += started.elapsed();
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test(start_paused = true)]
    async fn nested_compilation_is_counted_once_when_cancelled() {
        let txn = TxnHandle::empty();
        let work = async {
            let _outer = txn.time_compilation();
            tokio::time::sleep(Duration::from_millis(10)).await;
            let _inner = txn.time_compilation();
            tokio::time::sleep(Duration::from_secs(1)).await;
        };
        assert!(
            tokio::time::timeout(Duration::from_millis(30), work)
                .await
                .is_err()
        );
        assert_eq!(
            txn.with(|txn| txn.take_compilation_time()),
            Duration::from_millis(30)
        );
        assert_eq!(txn.with(|txn| txn.take_compilation_time()), Duration::ZERO);
        let next = txn.time_compilation();
        tokio::time::sleep(Duration::from_millis(5)).await;
        drop(next);
        assert_eq!(
            txn.with(|txn| txn.take_compilation_time()),
            Duration::from_millis(5)
        );
    }

    #[tokio::test]
    async fn payload_diagnostics_name_an_alias_without_tracking_reads() {
        use crate::{
            interpreter::{stm::start_txn, vm::Vm},
            test_support::test_config,
        };

        let vm = Vm::new(test_config());
        let process = vm
            .initialize_process_from_code("/daemon.c", "mapping members = ([]);")
            .await
            .unwrap()
            .context
            .process;
        let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let txn = Transaction::new(live.inner.clone());
        let global = process.var_id(0);
        let Some(WorldValue::Ref(LpcRef::Mapping(payload))) = txn.snapshot.peek(global) else {
            panic!("global should hold a mapping");
        };
        assert_eq!(
            txn.describe_cell(&vm.global_state.object_space, payload.id)
                .as_deref(),
            Some("mapping payload via /daemon.global.members")
        );
        assert!(
            txn.changeset
                .conflicting_read(&[global, payload.id].into_iter().collect())
                .is_none()
        );
    }
}
