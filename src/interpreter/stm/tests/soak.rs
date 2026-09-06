//! Long-run soak tests: sustained commits must plateau in the bytes the
//! committing thread holds, and retention must be bounded by the oldest
//! live snapshot and released when it drops.

use jemalloc_ctl::thread::{ThreadLocal, allocatedp, deallocatedp};

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{VarId, committer::Committer, tests::*},
};

/// The calling thread's jemalloc counters: bytes ever allocated and ever
/// freed by it, which no other thread's allocations move.
struct ThreadBytes {
    allocated: ThreadLocal<u64>,
    deallocated: ThreadLocal<u64>,
}

impl ThreadBytes {
    fn current() -> Self {
        Self {
            allocated: allocatedp::read().expect("jemalloc thread.allocatedp"),
            deallocated: deallocatedp::read().expect("jemalloc thread.deallocatedp"),
        }
    }

    /// Bytes this thread holds right now.
    fn live(&self) -> u64 {
        self.allocated.get() - self.deallocated.get()
    }
}

/// Upward slope the failure mode.
fn assert_plateau(samples: &[u64], baseline: u64, headroom: u64) {
    assert!(
        samples.iter().all(|&s| s <= baseline + headroom),
        "live bytes grew past headroom: baseline {baseline} B, max {} B",
        samples.iter().max().copied().unwrap_or(0)
    );

    let middle: Vec<u64> = samples[1..samples.len() - 1].to_vec();
    let mut sorted = middle.clone();
    sorted.sort();
    let median = sorted[middle.len() / 2];
    let last = samples[samples.len() - 1];
    assert!(
        last <= median + headroom / 4,
        "live bytes trending upward: tail {last} B, median {median} B\nsamples: {samples:?}"
    );
}

#[test]
fn soak_live_bytes_plateau_under_sustained_commits() {
    const COMMITS: usize = 100_000;
    const WARMUP: usize = 2_000;
    const SAMPLE_EVERY: usize = 5_000;
    const HEADROOM: u64 = 1024 * 1024;

    let (tx, rx) = flume::unbounded();
    let mut committer = Committer::new();
    let cell = VarId::new();

    // Warm up so allocator arenas settle before the baseline.
    for _ in 0..WARMUP {
        let (_, result) = drive_txn(&mut committer, &tx, &rx, |t| {
            t.write(cell, LpcRef::from(1));
        });
        assert!(result.is_ok());
    }

    let bytes = ThreadBytes::current();
    let baseline = bytes.live();

    // A neighbour on another thread holds twice the headroom for the
    // whole sampling window: the measurement must not see it.
    let (release_tx, release_rx) = flume::bounded::<()>(1);
    let (ready_tx, ready_rx) = flume::bounded::<()>(1);
    let neighbour = std::thread::spawn(move || {
        let held = vec![1u8; 2 * HEADROOM as usize];
        ready_tx.send(()).unwrap();
        release_rx.recv().unwrap();
        drop(held);
    });
    ready_rx.recv().unwrap();

    let started = std::time::Instant::now();
    let mut samples = vec![baseline];
    for i in 1..=COMMITS {
        let (_, result) = drive_txn(&mut committer, &tx, &rx, |t| {
            t.write(cell, LpcRef::from(1));
        });
        assert!(result.is_ok());
        if i % SAMPLE_EVERY == 0 {
            samples.push(bytes.live());
        }
    }
    let elapsed = started.elapsed();
    release_tx.send(()).unwrap();
    neighbour.join().unwrap();
    let ns_per_commit = elapsed.as_nanos() as u64 / COMMITS as u64;
    let final_live = samples[samples.len() - 1];
    eprintln!(
        "soak: {COMMITS} commits in {elapsed:?} ({ns_per_commit} ns/commit); baseline {baseline} B, final {final_live} B"
    );

    assert_plateau(&samples, baseline, HEADROOM);

    // Retention is bounded: with nothing live, only the newest
    // version's write set is kept, no matter how many versions passed.
    assert_eq!(committer.retained_versions(), 1);
}

/// Ensure that long-lived snapshots bound the write history.
#[test]
fn retention_is_bounded_by_the_oldest_live_snapshot_and_freed_on_drop() {
    const COMMITS: usize = 2_000;

    let (tx, rx) = flume::unbounded();
    let mut committer = Committer::new();
    let cell = VarId::new();

    let commits = |c: &mut Committer| {
        for _ in 0..COMMITS {
            let (_, result) = drive_txn(c, &tx, &rx, |t| t.write(cell, LpcRef::from(1)));
            assert!(result.is_ok());
        }
    };

    // History before any pin: nothing live, so it evicts to the
    // current write set. Retention must not count process uptime.
    commits(&mut committer);
    assert_eq!(committer.retained_versions(), 1);

    // Pin A, then advance. Retention spans from A's own version to
    // current - A's age, not the total commits since the process
    // started.
    let pin_a = start_live(&mut committer, &tx);
    let a_version = pin_a.version();
    commits(&mut committer);
    assert_eq!(
        committer.retained_versions(),
        COMMITS + 1,
        "retention is bounded by pin A's age, not the commit count"
    );
    assert!(committer.retains_version(a_version));
    assert_eq!(committer.oldest_retained_version(), a_version);

    // Pin B above A and advance again. The watermark does not move -
    // A is older, and it is the oldest that counts.
    let pin_b = start_live(&mut committer, &tx);
    let b_version = pin_b.version();
    assert!(a_version < b_version);
    commits(&mut committer);
    assert_eq!(committer.retained_versions(), 2 * COMMITS + 1);
    assert!(committer.retains_version(b_version));
    assert_eq!(
        committer.oldest_retained_version(),
        a_version,
        "the watermark follows the oldest live snapshot"
    );

    // Drop the newer pin: the watermark is A's, so nothing is
    // evicted. A per-pin release here would corrupt A's world.
    drop(pin_b);
    pump(&mut committer, &tx, &rx);
    assert_eq!(
        committer.retained_versions(),
        2 * COMMITS + 1,
        "dropping the newer pin frees nothing while the older holds"
    );

    // Drop the oldest pin: everything above the current version's
    // write set is released.
    drop(pin_a);
    pump(&mut committer, &tx, &rx);
    assert_eq!(committer.retained_versions(), 1);
    assert_eq!(
        committer.oldest_retained_version(),
        committer.current_version()
    );
}
