//! The binding: one connection, one body, three stores kept together —
//! the body's transactional cell, the connection's back-reference, and
//! the registry of live connections.

use std::{net::SocketAddr, sync::Arc};

use dashmap::DashMap;
use lpc_rs_errors::Result;
use tracing::error;

use crate::{
    interpreter::{
        efun::exec::DISPLACED,
        process::Process,
        stm::{
            AttemptBody, CommitProtocol, Effect, LiveSnapshot, Transaction, commit_changeset,
            flush_effects, run_attempts, start_txn,
        },
        vm::global_state::GlobalState,
    },
    telnet::{connection::Connection, ops::ConnectionOp},
};

/// Every live connection, by client address; the loop inserts itself at
/// start and removes itself as it exits.
#[derive(Debug, Default)]
pub struct Registry {
    live: DashMap<SocketAddr, Arc<Connection>>,
}

impl Registry {
    /// Add a connection whose loop is running.
    pub(crate) fn insert(&self, connection: Arc<Connection>) {
        self.live.insert(connection.address, connection);
    }

    /// Remove `connection`'s own entry; a newer connection at the same
    /// address keeps its place.
    pub(crate) fn remove(&self, connection: &Arc<Connection>) -> bool {
        self.live
            .remove_if(&connection.address, |_, live| Arc::ptr_eq(live, connection))
            .is_some()
    }

    /// The live connection at `address`.
    pub fn get(&self, address: SocketAddr) -> Option<Arc<Connection>> {
        self.live.get(&address).map(|entry| entry.clone())
    }

    /// How many loops are running.
    pub fn len(&self) -> usize {
        self.live.len()
    }

    /// No loop is running.
    pub fn is_empty(&self) -> bool {
        self.live.is_empty()
    }

    /// How many connections finished `logon()`.
    pub fn logged_in(&self) -> usize {
        self.live
            .iter()
            .filter(|entry| entry.is_logged_in())
            .count()
    }

    /// A copy of the live set, for a walk that must not hold the map.
    pub fn connections(&self) -> Vec<Arc<Connection>> {
        self.live.iter().map(|entry| entry.clone()).collect()
    }
}

impl GlobalState {
    /// Bind `connection` to `process` in its own transaction. The
    /// back-reference is a deferred `Effect::Exec`, a displaced holder's
    /// close an `Effect::Disconnect`, both flushed after the commit lands.
    pub async fn attach(self: &Arc<Self>, connection: Arc<Connection>, process: Arc<Process>) {
        let mut body = AttachBody {
            global_state: self.clone(),
            connection,
            process,
            attempt: None,
        };
        let (res, _) = run_attempts(
            &self.committer_tx,
            &self.attempt_telemetry,
            Some(self.commit_watch.clone()),
            &mut body,
        )
        .await;
        if let Err(e) = res {
            error!("attach: committer failed: {e}");
        }
    }

    /// Unbind `connection` from the body its back-reference names — that
    /// body's cell in a transaction, when the cell still holds this
    /// connection — then [`release`](Self::release). Returns the body it
    /// unbound; `None` when none held it. Marks the connection dead first,
    /// so an `Effect::Exec` flushed after this undoes itself.
    pub async fn detach(
        &self,
        connection: &Arc<Connection>,
        message: Option<String>,
    ) -> Option<Arc<Process>> {
        connection.mark_dead();
        let unbound = match connection.body() {
            Some(body) => {
                let mut attempt = DetachBody {
                    connection: connection.clone(),
                    process: body.clone(),
                    attempt: None,
                    held: false,
                };
                let (res, _) = run_attempts(
                    &self.committer_tx,
                    &self.attempt_telemetry,
                    Some(self.commit_watch.clone()),
                    &mut attempt,
                )
                .await;
                if let Err(e) = res {
                    error!("detach: committer failed: {e}");
                }
                attempt.held.then_some(body)
            }
            None => None,
        };
        self.release(connection, message);
        unbound
    }

    /// The physical half of a detach: the back-reference cleared, then
    /// `message` and `Close` queued. The registry entry is the loop's own
    /// to remove as it exits, so shutdown's wait means what it says.
    pub fn release(&self, connection: &Connection, message: Option<String>) {
        connection.set_body(None);
        if let Some(message) = message {
            let _ = connection.send(ConnectionOp::SendMessage(message));
        }
        let _ = connection.send(ConnectionOp::Close);
    }
}

/// One attempt of [`GlobalState::attach`]: the connection-cell write plus
/// the deferred socket handover.
struct AttachBody {
    global_state: Arc<GlobalState>,
    connection: Arc<Connection>,
    process: Arc<Process>,
    attempt: Option<Transaction>,
}

#[async_trait::async_trait]
impl AttemptBody for AttachBody {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let mut txn = Transaction::new(live.inner.clone());

        // The connection currently bound to `process`; the handover
        // displaces it.
        let previous = txn.read_connection(self.process.connection.id);
        txn.write_connection(self.process.connection.id, Some(self.connection.clone()));

        txn.record_effect(Effect::Exec {
            new_process: self.process.clone(),
            connection: self.connection.clone(),
        });
        if let Some(previous) = previous {
            txn.record_effect(Effect::Disconnect {
                connection: previous,
                message: Some(DISPLACED.to_owned()),
            });
        }

        self.attempt = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), crate::interpreter::stm::Conflict>,
        Vec<Effect>,
    )> {
        let mut txn = self
            .attempt
            .take()
            .expect("attempt present until committed");
        let commit = commit_changeset(tx, txn.take_changeset()).await?;
        Ok((commit, txn.take_effects()))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        flush_effects(&self.global_state, effects).await;
        Ok(())
    }
}

/// One attempt of [`GlobalState::detach`]: clear the body's cell if it
/// holds this connection. `held` reports whether it did.
struct DetachBody {
    connection: Arc<Connection>,
    process: Arc<Process>,
    attempt: Option<Transaction>,
    held: bool,
}

#[async_trait::async_trait]
impl AttemptBody for DetachBody {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = start_txn(tx).await?;
        let mut txn = Transaction::new(live.inner.clone());
        self.held = txn
            .read_connection(self.process.connection.id)
            .is_some_and(|held| Arc::ptr_eq(&held, &self.connection));
        if self.held {
            txn.write_connection(self.process.connection.id, None);
        }
        self.attempt = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), crate::interpreter::stm::Conflict>,
        Vec<Effect>,
    )> {
        let mut txn = self
            .attempt
            .take()
            .expect("attempt present until committed");
        let commit = commit_changeset(tx, txn.take_changeset()).await?;
        Ok((commit, txn.take_effects()))
    }

    async fn deliver(&mut self, _effects: Vec<Effect>) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::net::ToSocketAddrs;

    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            program::ProgramBuilder,
            stm::{Committer, WorldValue},
            vm::Vm,
        },
        test_support::{connect, test_config},
    };

    /// A `Connection` whose own channel is dropped after the test.
    fn make_connection() -> Arc<Connection> {
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        Arc::new(Connection::new(
            "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            tx,
        ))
    }

    /// A rejected attach attempt re-runs: the second attempt commits the
    /// connection cell and only then flushes the back-reference.
    #[tokio::test]
    async fn attach_reruns_after_rejection() {
        let (vm_tx, _vm_rx) = tokio::sync::mpsc::channel(128);
        let global_state = Arc::new(GlobalState::new(test_config(), vm_tx));
        let process = Arc::new(Process::new(
            ProgramBuilder::default()
                .filename(LpcPath::InGame(std::path::PathBuf::from("/body")))
                .build()
                .unwrap(),
        ));
        let connection = make_connection();

        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone();
        let handle =
            std::thread::spawn(move || Committer::new().run_with_rejections(committer_tx, rx, 1));

        let mut body = AttachBody {
            global_state: global_state.clone(),
            connection: connection.clone(),
            process: process.clone(),
            attempt: None,
        };
        let (res, stats) = run_attempts(
            &tx,
            &crate::interpreter::stm::AttemptTelemetry::default(),
            None,
            &mut body,
        )
        .await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 2, "one forced rejection, then a commit");
        assert_eq!(stats.conflicts, 1);
        assert!(
            connection
                .body()
                .is_some_and(|bound| Arc::ptr_eq(&bound, &process)),
            "the back-reference is flushed after the commit"
        );

        tx.send(CommitProtocol::Close).unwrap();
        drop(tx);
        let snapshot = handle.join().unwrap();
        assert!(matches!(
            snapshot.read(process.connection.id),
            Some(WorldValue::Connection(Some(_)))
        ));
    }

    #[tokio::test]
    async fn detach_clears_the_cell_and_the_back_reference() {
        let vm = Vm::new(test_config());
        let body = vm.create_process_from_code("/body.c", "").await.unwrap();
        let mut on = connect(&vm, &body).await;

        let unbound = vm
            .global_state
            .detach(&on.connection, Some("bye\n".into()))
            .await;

        assert!(unbound.is_some_and(|p| Arc::ptr_eq(&p, &body)));
        assert!(vm.global_state.committed_connection(&body).is_none());
        assert!(on.connection.body().is_none());
        assert!(on.connection.is_dead());
        assert_eq!(
            on.rx.try_recv(),
            Ok(ConnectionOp::SendMessage("bye\n".into()))
        );
        assert_eq!(on.rx.try_recv(), Ok(ConnectionOp::Close));
    }

    #[tokio::test]
    async fn detach_of_an_unbound_connection_releases_only() {
        let vm = Vm::new(test_config());
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
        let connection = Arc::new(Connection::new("127.0.0.1:23124".parse().unwrap(), tx));

        assert!(vm.global_state.detach(&connection, None).await.is_none());
        assert!(connection.is_dead());
        assert_eq!(rx.try_recv(), Ok(ConnectionOp::Close));
        assert!(rx.try_recv().is_err(), "no message was asked for");
    }

    #[tokio::test]
    async fn detach_leaves_a_cell_another_connection_holds() {
        let vm = Vm::new(test_config());
        let body = vm.create_process_from_code("/body.c", "").await.unwrap();
        let on = connect(&vm, &body).await;
        // A stale back-reference: this connection thinks it has the body.
        let stale = make_connection();
        stale.set_body(Some(body.clone()));

        assert!(vm.global_state.detach(&stale, None).await.is_none());
        assert!(
            vm.global_state
                .committed_connection(&body)
                .is_some_and(|held| Arc::ptr_eq(&held, &on.connection)),
            "the cell still holds the connection that owns it"
        );
        assert!(stale.body().is_none());
    }

    /// Spec D4: a connection detached while an `exec` to it is in flight is
    /// unbound from the body the exec's flush points it at.
    #[tokio::test]
    async fn an_attach_of_a_dead_connection_is_undone_at_its_flush() {
        let vm = Vm::new(test_config());
        let a = vm.create_process_from_code("/a.c", "").await.unwrap();
        let b = vm.create_process_from_code("/b.c", "").await.unwrap();
        let mut on = connect(&vm, &a).await;

        assert!(vm.global_state.detach(&on.connection, None).await.is_some());
        assert_eq!(on.rx.try_recv(), Ok(ConnectionOp::Close));
        vm.global_state
            .attach(on.connection.clone(), b.clone())
            .await;

        assert!(vm.global_state.committed_connection(&b).is_none());
        assert!(on.connection.body().is_none());
        assert_eq!(on.rx.try_recv(), Ok(ConnectionOp::Attached));
        assert_eq!(on.rx.try_recv(), Ok(ConnectionOp::Close));
    }

    #[test]
    fn the_registry_counts_the_logged_in() {
        let registry = Registry::default();
        let a = make_connection();
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        let b = Arc::new(Connection::new("127.0.0.1:23125".parse().unwrap(), tx));
        registry.insert(a.clone());
        registry.insert(b.clone());
        assert_eq!((registry.len(), registry.logged_in()), (2, 0));
        b.set_logged_in();
        assert_eq!(registry.logged_in(), 1);
        assert!(registry.get(b.address).is_some());
        assert_eq!(registry.connections().len(), 2);
        assert!(registry.remove(&a));
        assert!(!registry.remove(&a));
        assert!(!registry.is_empty());

        // A reconnect at `a`'s address before its old loop's `leave` runs
        // replaces the entry; the departing loop's remove must not evict it.
        registry.insert(a.clone());
        let (tx2, _rx2) = tokio::sync::mpsc::unbounded_channel();
        let a2 = Arc::new(Connection::new(a.address, tx2));
        registry.insert(a2.clone());
        assert!(!registry.remove(&a));
        assert!(registry.get(a.address).is_some());
    }
}
