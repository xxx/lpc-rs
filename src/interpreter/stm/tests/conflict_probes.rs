//! Pin the contention numbers against the task's real commit path
//! (`start_txn` / `commit_changeset` on a threaded `Committer`), so an
//! anomaly is attributable to the mechanism rather than the bench.

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{
        CommitProtocol, Committer, CommitterStats, Snapshot, Transaction, VarId, WorldValue,
        changeset::Changeset, commit_changeset, committer_stats, start_txn,
    },
};

/// One `counter = counter + 1` as a transactional RMW; `Ok` commits, `Err` is a rejected (conflicting) commit.
async fn increment_once(tx: &flume::Sender<CommitProtocol>, counter: VarId) -> Result<(), ()> {
    let live = start_txn(tx).await.expect("start failed");
    let mut txn = Transaction::new(live.inner.clone());
    let LpcRef::Int(n) = txn.read(counter).expect("counter cell missing") else {
        panic!("counter cell is not an int");
    };
    txn.write(counter, LpcRef::from(n.wrapping_add(1)));
    let (_snap, changeset) = txn.clone().into_parts();
    let res = commit_changeset(tx, changeset)
        .await
        .expect("commit send failed");
    drop(live);
    res.map_err(|_| ())
}

/// Retry the RMW until it commits, counting attempts/conflicts like `run_attempts`.
async fn increment_with_retry(tx: &flume::Sender<CommitProtocol>, counter: VarId) -> (u64, u64) {
    let mut attempts = 0u64;
    let mut conflicts = 0u64;
    loop {
        attempts += 1;
        match increment_once(tx, counter).await {
            Ok(()) => return (attempts, conflicts),
            Err(()) => conflicts += 1,
        }
    }
}

/// Threaded committer with `counter` seeded to 0, on a wide channel so backpressure never serializes the probes.
fn spawn_seeded_committer(
    counter: VarId,
) -> (
    flume::Sender<CommitProtocol>,
    std::thread::JoinHandle<Snapshot>,
) {
    let (tx, rx) = flume::bounded(1 << 10);
    let mut committer = Committer::new();
    let v0 = committer.current_version();
    let mut seed = Changeset::new(v0);
    seed.write(counter, WorldValue::ref_of(LpcRef::from(0)));
    committer.commit(seed).expect("seed should commit");

    let committer_tx = tx.clone();
    let handle = std::thread::spawn(move || committer.run(committer_tx, rx));
    (tx, handle)
}

/// The committed value of `counter`, read from a fresh transaction's snapshot.
fn committed_counter(tx: &flume::Sender<CommitProtocol>, counter: VarId) -> i64 {
    let (reply_tx, reply_rx) = flume::bounded(1);
    tx.send(CommitProtocol::Start { reply: reply_tx })
        .expect("channel closed");
    let live = reply_rx
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("start timed out");
    match live.inner.read(counter) {
        Some(WorldValue::Ref(LpcRef::Int(n))) => n.0,
        other => panic!("counter read back as {other:?}"),
    }
}

/// Sequential RMW (the bench's 1-worker shape): 0 conflicts, one commit each, no lost updates.
#[tokio::test(flavor = "multi_thread", worker_threads = 1)]
async fn sequential_rmw_is_conflict_free_and_lossless() {
    const N: usize = 1000;
    let counter = VarId::new();
    let (tx, _handle) = spawn_seeded_committer(counter);

    let before: CommitterStats = committer_stats(&tx).await.unwrap();
    for _ in 0..N {
        increment_once(&tx, counter)
            .await
            .expect("a sequential RMW must commit");
    }
    let after: CommitterStats = committer_stats(&tx).await.unwrap();
    let final_val = committed_counter(&tx, counter);

    let commits = after.commits - before.commits;
    let conflicts = after.conflicts - before.conflicts;
    println!(
        "[probe:seq] commits={commits} conflicts={conflicts} final_counter={final_val} (expect 1000/0/1000)"
    );

    assert_eq!(conflicts, 0, "sequential RMW must not conflict");
    assert_eq!(commits, N, "each RMW is one commit");
    assert_eq!(final_val as usize, N, "no lost updates");
}

/// Eight retry-loop RMWs on one shared counter: conflicts must happen (each RMW's read is
/// invalidated by the next commit) and no updates may be lost. Task attempts/conflicts are
/// summed and cross-checked against the committer's lifetime totals.
#[tokio::test(flavor = "multi_thread", worker_threads = 8)]
async fn concurrent_rmw_conflicts_and_loses_no_updates() {
    const N: usize = 1000;
    const WORKERS: usize = 8;
    let counter = VarId::new();
    let (tx, _handle) = spawn_seeded_committer(counter);

    let before: CommitterStats = committer_stats(&tx).await.unwrap();

    let mut set = tokio::task::JoinSet::new();
    for _ in 0..WORKERS {
        let tx = tx.clone();
        set.spawn(async move {
            let mut attempts = 0u64;
            let mut conflicts = 0u64;
            for _ in 0..N {
                let (a, c) = increment_with_retry(&tx, counter).await;
                attempts += a;
                conflicts += c;
            }
            (attempts, conflicts)
        });
    }
    let mut total_attempts = 0u64;
    let mut total_conflicts = 0u64;
    while let Some(res) = set.join_next().await {
        let (a, c) = res.expect("worker panicked");
        total_attempts += a;
        total_conflicts += c;
    }

    let after: CommitterStats = committer_stats(&tx).await.unwrap();
    let final_val = committed_counter(&tx, counter);

    let commits = after.commits - before.commits;
    let committer_conflicts = after.conflicts - before.conflicts;
    let total = WORKERS * N;
    println!(
        "[probe:conc] task_attempts={total_attempts} task_conflicts={total_conflicts} committer_commits={commits} committer_conflicts={committer_conflicts} final_counter={final_val} (expect commits=8000, conflicts>0, final=8000)",
    );

    assert_eq!(commits as usize, total, "every RMW eventually commits");
    assert!(
        total_conflicts > 0,
        "8 concurrent RMWs on one var MUST conflict; 0 means the measurement is blind"
    );
    assert_eq!(
        total_attempts,
        total as u64 + total_conflicts,
        "attempts = commits + conflicts"
    );
    assert_eq!(
        total_conflicts as usize, committer_conflicts,
        "each task retry is exactly one committer-conflicted commit (the rate we report IS the retry count)"
    );
    assert_eq!(
        final_val as usize, total,
        "no lost updates under concurrency"
    );
}

/// The bench diffs a `CommitterStats` snapshot per iteration, so the accessor must be monotonic (commits/conflicts only grow).
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn stats_accessor_is_monotonic_and_consistent() {
    let counter = VarId::new();
    let (tx, _handle) = spawn_seeded_committer(counter);

    let s0 = committer_stats(&tx).await.unwrap();
    let s1 = committer_stats(&tx).await.unwrap();
    assert_eq!(s0, s1, "no work -> stats unchanged");

    increment_once(&tx, counter).await.expect("RMW must commit");
    let s2 = committer_stats(&tx).await.unwrap();
    assert!(
        s1.commits < s2.commits,
        "commits only grow: {s1:?} -> {s2:?}"
    );
    assert!(
        s2.conflicts >= s1.conflicts,
        "conflicts only grow: {s1:?} -> {s2:?}"
    );
    assert_eq!(s2.errors, s1.errors, "no errors expected");
}
