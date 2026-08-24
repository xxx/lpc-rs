//! Shared test drivers for the commit protocol, used by `committer::tests`,
//! `retry::tests`, and `soak`. Kept in one gated module so the production
//! surface stays protocol-only.

use crate::interpreter::stm::{
    Transaction, Version,
    changeset::Changeset,
    committer::{CommitProtocol, Committer, LiveSnapshot},
    snapshot::Snapshot,
};

/// Spawn the committer thread on a bounded channel. Returns the
/// protocol sender, the initial world version, and the committer handle.
pub(crate) fn start_committer() -> (
    flume::Sender<CommitProtocol>,
    Version,
    std::thread::JoinHandle<Snapshot>,
) {
    let (tx, rx) = flume::bounded(4);
    let committer = Committer::new();
    let version = committer.current_version();
    let committer_tx = tx.clone();
    let handle = std::thread::spawn(move || committer.run(committer_tx, rx));
    (tx, version, handle)
}

/// Settle the channel and take the committer's final snapshot.
pub(crate) fn close_committer(
    tx: flume::Sender<CommitProtocol>,
    handle: std::thread::JoinHandle<Snapshot>,
) -> Snapshot {
    tx.send(CommitProtocol::Close)
        .expect("committer channel closed");
    drop(tx);
    handle.join().expect("committer panicked")
}

/// Process every message currently queued on `rx`, in FIFO order. The
/// committer run loop's role, played back synchronously for tests.
pub(crate) fn pump(
    committer: &mut Committer,
    tx: &flume::Sender<CommitProtocol>,
    rx: &flume::Receiver<CommitProtocol>,
) {
    while let Ok(msg) = rx.try_recv() {
        assert!(committer.process(msg, tx), "committer stopped unexpectedly");
    }
}

/// Synchronous Start: hand out a live snapshot handle without a thread.
pub(crate) fn start_live(
    committer: &mut Committer,
    tx: &flume::Sender<CommitProtocol>,
) -> LiveSnapshot {
    let (reply_tx, reply_rx) = flume::bounded(1);
    assert!(
        committer.process(CommitProtocol::Start { reply: reply_tx }, tx),
        "committer stopped unexpectedly"
    );
    reply_rx.recv().expect("no reply from committer")
}

/// Drive one Start/Commit through `process` on a live committer, without a
/// thread or runtime, then process the release the drop enqueued - the run
/// loop's role, in FIFO order. Returns the base version and the commit result.
pub(crate) fn drive_txn(
    committer: &mut Committer,
    tx: &flume::Sender<CommitProtocol>,
    rx: &flume::Receiver<CommitProtocol>,
    f: impl FnOnce(&mut Transaction),
) -> (Version, Result<(), Changeset>) {
    let live = start_live(committer, tx);
    let mut transaction = Transaction::new(live.inner.clone());
    f(&mut transaction);
    let (world, changeset) = transaction.into_parts();
    let base = world.version();
    let (reply_tx, reply_rx) = flume::bounded(1);
    committer.process(
        CommitProtocol::Commit {
            changeset,
            releases_base: false,
            reply: reply_tx,
        },
        tx,
    );
    let result = reply_rx.recv().expect("no reply from committer");
    drop(live);
    pump(committer, tx, rx);
    (base, result)
}
