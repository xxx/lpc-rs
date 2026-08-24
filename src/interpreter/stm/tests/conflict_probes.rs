//! Pin the contention numbers against the task's real attempt loop
//! (`run_attempts` driving `start_txn`/`commit_changeset` on a threaded
//! `Committer`), so an anomaly is attributable to the mechanism rather than
//! the bench.

use std::sync::Arc;

use super::helpers::IncBody;
use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{
        AttemptTelemetry, CommitProtocol, Committer, CommitterStats, RetryStats, Snapshot, VarId,
        Version, WorldValue, changeset::Changeset, committer_stats, run_attempts,
    },
};

/// One `counter = counter + 1` through the production attempt loop; panics
/// if the loop errors.
async fn rmw_once(
    tx: &flume::Sender<CommitProtocol>,
    telemetry: &AttemptTelemetry,
    watch: &tokio::sync::watch::Receiver<Version>,
    counter: VarId,
) -> RetryStats {
    let mut body = IncBody::new(counter);
    let (res, stats) = run_attempts(tx, telemetry, Some(watch.clone()), &mut body).await;
    res.expect("the attempt loop retries until commit");
    stats
}

/// Threaded committer with `counter` seeded to 0, on a wide channel so backpressure never serializes the probes.
fn spawn_seeded_committer(
    counter: VarId,
) -> (
    flume::Sender<CommitProtocol>,
    tokio::sync::watch::Receiver<Version>,
    std::thread::JoinHandle<Snapshot>,
) {
    let (tx, rx) = flume::bounded(1 << 10);
    let mut committer = Committer::new();
    let v0 = committer.current_version();
    let mut seed = Changeset::new(v0);
    seed.write(counter, WorldValue::ref_of(LpcRef::from(0)));
    committer.commit(seed).expect("seed should commit");
    let watch = committer.commit_watch();

    let committer_tx = tx.clone();
    let handle = std::thread::spawn(move || committer.run(committer_tx, rx));
    (tx, watch, handle)
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
    let (tx, watch, _handle) = spawn_seeded_committer(counter);
    let telemetry = AttemptTelemetry::default();

    let before: CommitterStats = committer_stats(&tx).await.unwrap();
    for _ in 0..N {
        let stats = rmw_once(&tx, &telemetry, &watch, counter).await;
        assert_eq!(stats.attempts, 1, "a sequential RMW commits first try");
    }
    let after: CommitterStats = committer_stats(&tx).await.unwrap();
    let final_val = committed_counter(&tx, counter);

    let commits = after.commits - before.commits;
    let conflicts = after.conflicts - before.conflicts;
    let snap = telemetry.snapshot();
    println!(
        "[probe:seq] commits={commits} conflicts={conflicts} applies={} final_counter={final_val} (expect 1000/0/1000/1000)",
        snap.applies
    );

    assert_eq!(conflicts, 0, "sequential RMW must not conflict");
    assert_eq!(commits, N, "each RMW is one commit");
    assert_eq!(snap.applies as usize, N);
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
    let (tx, watch, _handle) = spawn_seeded_committer(counter);
    let telemetry = Arc::new(AttemptTelemetry::default());

    let before: CommitterStats = committer_stats(&tx).await.unwrap();

    let mut set = tokio::task::JoinSet::new();
    for _ in 0..WORKERS {
        let tx = tx.clone();
        let watch = watch.clone();
        let telemetry = telemetry.clone();
        set.spawn(async move {
            let mut attempts = 0u64;
            let mut conflicts = 0u64;
            for _ in 0..N {
                let stats = rmw_once(&tx, &telemetry, &watch, counter).await;
                attempts += stats.attempts;
                conflicts += stats.conflicts;
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
    let snap = telemetry.snapshot();
    println!(
        "[probe:conc] task_attempts={total_attempts} task_conflicts={total_conflicts} committer_commits={commits} committer_conflicts={committer_conflicts} wakes={} expiries={} backoff_yield_ms={:.1} backoff_sleep_ms={:.1} final_counter={final_val} (expect commits=8000, conflicts>0, final=8000)",
        snap.backoff_commit_wakes,
        snap.backoff_cap_expiries,
        snap.backoff_yield.as_secs_f64() * 1e3,
        snap.backoff_sleep.as_secs_f64() * 1e3,
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
    assert_eq!(
        snap.applies as usize, total,
        "every probe RMW runs through run_attempts"
    );
    assert_eq!(snap.attempts, total_attempts, "one telemetry, one loop");
}

/// The bench diffs a `CommitterStats` snapshot per iteration, so the accessor must be monotonic (commits/conflicts only grow).
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn stats_accessor_is_monotonic_and_consistent() {
    let counter = VarId::new();
    let (tx, watch, _handle) = spawn_seeded_committer(counter);
    let telemetry = AttemptTelemetry::default();

    let s0 = committer_stats(&tx).await.unwrap();
    let s1 = committer_stats(&tx).await.unwrap();
    assert_eq!(s0, s1, "no work -> stats unchanged");

    rmw_once(&tx, &telemetry, &watch, counter).await;
    let s2 = committer_stats(&tx).await.unwrap();
    assert!(
        s1.commits < s2.commits,
        "commits only grow: {s1:?} -> {s2:?}"
    );
    assert!(
        s2.conflicts >= s1.conflicts,
        "conflicts only grow: {s1:?} -> {s2:?}"
    );
    assert_eq!(
        s2.reply_failures, s1.reply_failures,
        "no reply failures expected"
    );
}
