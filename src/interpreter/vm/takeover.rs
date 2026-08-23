use std::sync::Arc;

use lpc_rs_errors::Result;
use tracing::{error, trace};

use crate::{
    interpreter::{
        process::Process,
        stm::{
            AttemptBody, Changeset, CommitProtocol, Effect, LiveSnapshot, Transaction,
            commit_changeset, flush_effects, run_attempts, start_txn,
        },
        vm::global_state::GlobalState,
    },
    telnet::connection::Connection,
};

impl GlobalState {
    /// Bind a [`Connection`] to a [`Process`] in its own transaction. The
    /// socket-level handover (back-reference, disconnect of the displaced
    /// holder) is a deferred `Effect::Exec`, flushed after the commit lands.
    pub async fn takeover(self: &Arc<Self>, connection: Arc<Connection>, process: Arc<Process>) {
        let mut body = TakeoverBody {
            global_state: self.clone(),
            connection,
            process,
            attempt: None,
        };
        let (res, stats) = run_attempts(&self.committer_tx, &mut body).await;
        trace!(
            attempts = stats.attempts,
            conflicts = stats.conflicts,
            ?stats.duration,
            "takeover finished"
        );
        if let Err(e) = res {
            error!("takeover: committer failed: {e}");
        }
    }
}

/// One attempt of [`GlobalState::takeover`]: the connection-cell write plus
/// the deferred socket handover.
struct TakeoverBody {
    global_state: Arc<GlobalState>,
    connection: Arc<Connection>,
    process: Arc<Process>,
    attempt: Option<Transaction>,
}

#[async_trait::async_trait]
impl AttemptBody for TakeoverBody {
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
            previous,
        });

        self.attempt = Some(txn);
        Ok(Some(live))
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(std::result::Result<(), Changeset>, Vec<Effect>)> {
        let mut txn = self
            .attempt
            .take()
            .expect("attempt present until committed");
        let commit = commit_changeset(tx, txn.take_changeset()).await?;
        Ok((commit, txn.take_effects()))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        let gs = &self.global_state;
        flush_effects(&gs.config, &gs.object_space, gs.call_outs(), effects).await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::net::ToSocketAddrs;

    use arc_swap::ArcSwapAny;
    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;
    use crate::{
        interpreter::{
            program::ProgramBuilder,
            stm::{Committer, WorldValue},
        },
        test_support::test_config,
    };

    /// A `Connection` whose own channels are dropped after the test.
    fn make_connection() -> Arc<Connection> {
        let (tx, _rx) = tokio::sync::mpsc::channel(1);
        let (broker_tx, _broker_rx) = flume::unbounded();
        Arc::new(Connection {
            address: "127.0.0.1:23123".to_socket_addrs().unwrap().next().unwrap(),
            process: ArcSwapAny::from(None),
            tx,
            broker_tx,
            input_to: Default::default(),
        })
    }

    /// A rejected takeover attempt re-runs: the second attempt commits the
    /// connection cell and only then flushes the back-reference.
    #[tokio::test]
    async fn takeover_reruns_after_rejection() {
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

        let mut body = TakeoverBody {
            global_state: global_state.clone(),
            connection: connection.clone(),
            process: process.clone(),
            attempt: None,
        };
        let (res, stats) = run_attempts(&tx, &mut body).await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 2, "one forced rejection, then a commit");
        assert_eq!(stats.conflicts, 1);
        assert!(
            connection
                .process
                .load()
                .as_ref()
                .is_some_and(|bound| Arc::ptr_eq(bound, &process)),
            "the back-reference is flushed after the commit"
        );

        tx.send(CommitProtocol::Close).unwrap();
        drop(tx);
        let world = handle.join().expect("committer panicked");
        assert!(
            matches!(
                world.read(process.connection.id),
                Some(WorldValue::Connection(Some(bound))) if Arc::ptr_eq(&bound, &connection)
            ),
            "the connection cell holds the committed binding"
        );
    }
}
