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

use std::{path::PathBuf, sync::Arc};

use tokio::{io::AsyncWriteExt, sync::mpsc::UnboundedSender};

use crate::{
    interpreter::{lpc_ref::LpcRef, process::Process, vm::global_state::GlobalState},
    telnet::{connection::Connection, ops::ConnectionOp},
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
        tx: UnboundedSender<ConnectionOp>,
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

    /// The physical half of a handover: the connection cell on `new_process`
    /// committed with the owning task; the flush points `connection`'s
    /// back-reference at `new_process` and announces `Attached` on it.
    Exec {
        new_process: Arc<Process>,
        connection: Arc<Connection>,
    },

    /// A connection's end — its holder destructed, or displaced by `exec`:
    /// the back-reference is cleared and the connection is told to close
    /// behind everything queued to it, `message` included. The close rides
    /// the connection's own channel so its task finishes the flush it may be
    /// running before it exits.
    Disconnect {
        connection: Arc<Connection>,
        message: Option<String>,
    },

    /// `write_file`'s append of `contents` to the file at `server`, once the
    /// attempt commits; `in_game` names it in the log when the append fails.
    AppendFile {
        in_game: String,
        server: PathBuf,
        contents: String,
    },

    /// `rm`'s unlink of the file at `server`, once the attempt commits;
    /// `in_game` names it in the log when the unlink fails.
    RemoveFile { in_game: String, server: PathBuf },

    /// `mkdir`'s directory at `server`, once the attempt commits.
    CreateDir { in_game: String, server: PathBuf },

    /// `rmdir`'s removal of the empty directory at `server`, once the
    /// attempt commits.
    RemoveDir { in_game: String, server: PathBuf },

    /// `rename`'s move of `from` to `to`, once the attempt commits;
    /// `in_game` names the source in the log when the move fails.
    Rename {
        in_game: String,
        from: PathBuf,
        to: PathBuf,
    },
}

impl Effect {
    /// Deliver this effect physically. Object effects go to the passed
    /// state's `ObjectSpace` (the committer's physical map); the others to
    /// config / their own channel. The call-out lock is held only for the
    /// synchronous materialize/remove, never across an await.
    pub(crate) async fn flush(self, global_state: &GlobalState) {
        match self {
            Self::DebugLog(msg) => global_state.config.debug_log(msg).await,
            Self::Socket { op, tx } => {
                let _ = tx.send(op);
            }
            Self::InsertObject { key, process } => {
                global_state.object_space.apply_insert(&key, process);
            }
            Self::RemoveObject { key, process } => {
                global_state.object_space.apply_remove(&key, &process);
            }
            Self::ScheduleCallOut(schedule) => {
                global_state.call_outs().write().materialize(schedule);
            }
            Self::CancelCallOut { id } => {
                global_state.call_outs().write().remove_by_id(id);
            }
            Self::Exec {
                new_process,
                connection,
            } => {
                connection.set_body(Some(new_process));
                let _ = connection.send(ConnectionOp::Attached);
                // The client dropped between this exec's commit and now:
                // unbind the body the connection just reached.
                if connection.is_dead() {
                    global_state.detach(&connection, None).await;
                }
            }
            Self::Disconnect {
                connection,
                message,
            } => global_state.release(&connection, message),
            Self::AppendFile {
                in_game,
                server,
                contents,
            } => {
                let appended = async {
                    let mut file = tokio::fs::OpenOptions::new()
                        .append(true)
                        .create(true)
                        .open(&server)
                        .await?;
                    file.write_all(contents.as_bytes()).await?;
                    file.flush().await
                }
                .await;
                if let Err(e) = appended {
                    global_state
                        .config
                        .debug_log(format!("write_file: {in_game}: {e}"))
                        .await;
                }
            }
            Self::RemoveFile { in_game, server } => {
                if let Err(e) = tokio::fs::remove_file(&server).await {
                    global_state
                        .config
                        .debug_log(format!("rm: {in_game}: {e}"))
                        .await;
                }
            }
            Self::CreateDir { in_game, server } => {
                if let Err(e) = tokio::fs::create_dir(&server).await {
                    global_state
                        .config
                        .debug_log(format!("mkdir: {in_game}: {e}"))
                        .await;
                }
            }
            Self::RemoveDir { in_game, server } => {
                if let Err(e) = tokio::fs::remove_dir(&server).await {
                    global_state
                        .config
                        .debug_log(format!("rmdir: {in_game}: {e}"))
                        .await;
                }
            }
            Self::Rename { in_game, from, to } => {
                if let Err(e) = tokio::fs::rename(&from, &to).await {
                    global_state
                        .config
                        .debug_log(format!("rename: {in_game}: {e}"))
                        .await;
                }
            }
        }
    }
}

/// Deliver a batch of effects in order. The retry loop calls this after a
/// successful commit; a rejected attempt's batch is never delivered.
pub(crate) async fn flush_effects(global_state: &GlobalState, effects: Vec<Effect>) {
    for effect in effects {
        effect.flush(global_state).await;
    }
}

// Manual so the channel and the process payloads stay out of the output.
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
            Self::Disconnect { message, .. } => f.debug_tuple("Disconnect").field(message).finish(),
            Self::AppendFile { in_game, .. } => f.debug_tuple("AppendFile").field(in_game).finish(),
            Self::RemoveFile { in_game, .. } => f.debug_tuple("RemoveFile").field(in_game).finish(),
            Self::CreateDir { in_game, .. } => f.debug_tuple("CreateDir").field(in_game).finish(),
            Self::RemoveDir { in_game, .. } => f.debug_tuple("RemoveDir").field(in_game).finish(),
            Self::Rename { in_game, to, .. } => {
                f.debug_tuple("Rename").field(in_game).field(to).finish()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::telnet::ops::ConnectionOp;

    fn global_state() -> GlobalState {
        let (vm_tx, _vm_rx) = tokio::sync::mpsc::channel(16);
        GlobalState::new(crate::test_support::test_config(), vm_tx)
    }

    /// A recorded socket op must arrive on its own channel when the batch is
    /// flushed, and a second `Effect` recorded against a second channel must
    /// go to that second channel: the op travels with the channel it was
    /// recorded on, never with the flushed batch's owner.
    #[tokio::test]
    async fn flush_delivers_socket_ops_on_their_recorded_channels() {
        let (tx_a, mut rx_a) = tokio::sync::mpsc::unbounded_channel();
        let (tx_b, mut rx_b) = tokio::sync::mpsc::unbounded_channel();
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

        let global_state = global_state();
        flush_effects(&global_state, log).await;

        assert_eq!(rx_a.recv().await, Some(op_a));
        assert_eq!(rx_b.recv().await, Some(op_b));
    }

    #[tokio::test]
    async fn exec_points_the_connection_at_the_body_then_announces_it() {
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let addr = "127.0.0.1:1".parse().unwrap();
        let connection = Arc::new(Connection::new(addr, tx));
        let body = Arc::new(Process::default());

        let global_state = global_state();
        Effect::Exec {
            new_process: body.clone(),
            connection: connection.clone(),
        }
        .flush(&global_state)
        .await;

        assert!(Arc::ptr_eq(&connection.body().unwrap(), &body));
        assert_eq!(rx.recv().await, Some(ConnectionOp::Attached));
    }

    #[tokio::test]
    async fn append_file_creates_then_appends() {
        let root = crate::test_support::TempLib::new("append-effect");
        let server = root.join("out.txt");
        let gs = global_state();
        for contents in ["one\n", "two\n"] {
            Effect::AppendFile {
                in_game: "/out.txt".to_owned(),
                server: server.clone(),
                contents: contents.to_owned(),
            }
            .flush(&gs)
            .await;
        }
        assert_eq!(std::fs::read_to_string(&server).unwrap(), "one\ntwo\n");
    }

    #[tokio::test]
    async fn remove_file_unlinks() {
        let root = crate::test_support::TempLib::new("remove-effect");
        let server = root.join("gone.txt");
        std::fs::write(&server, "x").unwrap();
        Effect::RemoveFile {
            in_game: "/gone.txt".to_owned(),
            server: server.clone(),
        }
        .flush(&global_state())
        .await;
        assert!(!server.exists());
    }

    /// A commit-time failure has no caller to error into; it goes to the
    /// debug log, naming the in-game path.
    #[tokio::test]
    async fn a_failed_file_effect_is_logged_not_raised() {
        use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};
        use tokio::io::AsyncReadExt;

        let root = crate::test_support::TempLib::new("failed-effect");
        let (writer, mut reader) = tokio::io::duplex(1024);
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .debug_log(DebugLog::new(writer))
            .build()
            .unwrap();
        let (vm_tx, _vm_rx) = tokio::sync::mpsc::channel(16);
        let gs = GlobalState::new(config, vm_tx);
        Effect::RemoveFile {
            in_game: "/missing.txt".to_owned(),
            server: root.join("missing.txt"),
        }
        .flush(&gs)
        .await;
        let mut buf = vec![0u8; 256];
        let n = tokio::time::timeout(std::time::Duration::from_secs(1), reader.read(&mut buf))
            .await
            .expect("the log line arrives")
            .unwrap();
        let logged = String::from_utf8_lossy(&buf[..n]);
        assert!(logged.starts_with("rm: /missing.txt:"), "{logged}");
    }
}
