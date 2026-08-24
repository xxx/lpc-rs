//! Contention backoff: how a conflicted attempt staggers before re-running.
//!
//! One [`Backoff`] per attempt loop owns the loss count, the jitter RNG, and
//! the realized-time totals; the loop only calls [`Backoff::stagger`]. A
//! watching ladder's sleep tier awaits the committer's watermark under the
//! jittered cap.

use std::time::Duration;

use tokio::sync::watch;

use super::Version;

/// Losses that re-run immediately: the rebase usually wins on its own.
const FREE_LOSSES: u64 = 1;
/// Highest loss count staggered by yielding; past it the loop sleeps.
const MAX_YIELD_LOSSES: u64 = 6;
/// Sleep bounds for persistent losers; tokio's timer fires sub-millisecond
/// sleeps at the next ~1ms tick.
const SLEEP_FLOOR: Duration = Duration::from_millis(1);
const SLEEP_CAP: Duration = Duration::from_millis(8);

/// How the loop staggers a conflicted attempt before re-running it.
#[derive(Debug, PartialEq)]
enum BackoffStep {
    /// Re-run at once.
    None,
    /// Yield to the scheduler this many times.
    Yields(u32),
    /// Sleep this long.
    Sleep(Duration),
}

/// The stagger for the attempt after loss number `losses`: the first loss is
/// free, low losses yield, persistent losses sleep, both on doubling caps
/// ([`SLEEP_CAP`] tops the ladder). `roll(n)` supplies the jitter as a draw
/// from `1..=n`.
fn backoff_step(losses: u64, roll: impl FnOnce(u64) -> u64) -> BackoffStep {
    if losses <= FREE_LOSSES {
        return BackoffStep::None;
    }
    if losses <= MAX_YIELD_LOSSES {
        let cap = 1u64 << (losses - FREE_LOSSES - 1);
        return BackoffStep::Yields(roll(cap) as u32);
    }
    let doublings = (losses - MAX_YIELD_LOSSES).min(3) as u32;
    let cap = SLEEP_CAP.min(SLEEP_FLOOR.saturating_mul(1 << doublings));
    let span = (cap - SLEEP_FLOOR).as_micros() as u64;
    BackoffStep::Sleep(SLEEP_FLOOR + Duration::from_micros(roll(span)))
}

/// One step of splitmix64 over `state`; not randomness anyone may rely on.
fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Realized backoff totals for one attempt loop.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BackoffSpent {
    /// Sleep as the ladder requested it; realized runs above on timer
    /// rounding, below when a commit wake cuts the wait short.
    pub(crate) sleep_requested: Duration,
    /// Wall time in the sleep tier.
    pub(crate) slept: Duration,
    /// Wall time in the yield tier.
    pub(crate) yielded: Duration,
    /// Sleep-tier waits ended by a commit bump.
    pub(crate) commit_wakes: u64,
    /// Sleep-tier waits that ran the full cap; every plain timer sleep counts.
    pub(crate) cap_expiries: u64,
}

/// One attempt loop's contention backoff: counts losses, draws jitter,
/// executes the ladder, accumulates realized time.
#[derive(Debug)]
pub(crate) struct Backoff {
    losses: u64,
    rng: u64,
    spent: BackoffSpent,
    commit_watch: Option<watch::Receiver<Version>>,
}

impl Default for Backoff {
    fn default() -> Self {
        Self::new()
    }
}

impl Backoff {
    /// A fresh ladder with zero losses and self-seeded jitter.
    pub(crate) fn new() -> Self {
        // Seeded per loop so concurrent losers draw different jitter; the
        // stack address varies per worker, the clock per run.
        let mut rng = 0u64;
        rng = std::ptr::from_ref(&rng) as u64
            ^ std::time::UNIX_EPOCH
                .elapsed()
                .map_or(0, |d| u64::from(d.subsec_nanos()));
        Self {
            losses: 0,
            rng,
            spent: BackoffSpent::default(),
            commit_watch: None,
        }
    }

    /// A ladder whose sleep tier waits on `commit_watch` — the committer's
    /// per-commit watermark — capped by the jittered ladder.
    pub(crate) fn watching(commit_watch: watch::Receiver<Version>) -> Self {
        Self {
            commit_watch: Some(commit_watch),
            ..Self::new()
        }
    }

    /// Losses recorded so far, one per [`stagger`](Backoff::stagger) call.
    pub(crate) fn losses(&self) -> u64 {
        self.losses
    }

    /// Realized totals so far.
    pub(crate) fn spent(&self) -> BackoffSpent {
        self.spent
    }

    /// Record one loss and stagger the re-run accordingly. Realized time
    /// reads tokio's clock, so paused-time tests observe the sleeps.
    pub(crate) async fn stagger(&mut self) {
        self.losses += 1;
        match backoff_step(self.losses, |n| splitmix64(&mut self.rng) % n + 1) {
            BackoffStep::None => {}
            BackoffStep::Yields(n) => {
                let started = tokio::time::Instant::now();
                for _ in 0..n {
                    tokio::task::yield_now().await;
                }
                self.spent.yielded += started.elapsed();
            }
            BackoffStep::Sleep(duration) => {
                let started = tokio::time::Instant::now();
                self.spent.sleep_requested += duration;
                match &mut self.commit_watch {
                    Some(watch) => {
                        // The wait is for further progress, not the commit
                        // that caused this loss.
                        watch.borrow_and_update();
                        match tokio::time::timeout(duration, watch.changed()).await {
                            Ok(_) => self.spent.commit_wakes += 1,
                            Err(_) => self.spent.cap_expiries += 1,
                        }
                    }
                    None => {
                        tokio::time::sleep(duration).await;
                        self.spent.cap_expiries += 1;
                    }
                }
                self.spent.slept += started.elapsed();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::{
        Backoff, BackoffSpent, BackoffStep, MAX_YIELD_LOSSES, SLEEP_CAP, SLEEP_FLOOR, backoff_step,
    };

    /// A roll that always takes the maximum of its range.
    fn max_roll(n: u64) -> u64 {
        n
    }

    #[test]
    fn the_first_loss_retries_immediately() {
        assert_eq!(backoff_step(1, max_roll), BackoffStep::None);
    }

    #[test]
    fn early_losses_yield_with_doubling_jittered_caps() {
        for losses in 2..=MAX_YIELD_LOSSES {
            let BackoffStep::Yields(cap) = backoff_step(losses, max_roll) else {
                panic!("losses={losses} should yield");
            };
            assert_eq!(u64::from(cap), 1 << (losses - 2));

            let BackoffStep::Yields(low) = backoff_step(losses, |_| 1) else {
                panic!("losses={losses} should yield");
            };
            assert_eq!(low, 1);
        }
    }

    #[test]
    fn persistent_losses_sleep_between_floor_and_a_doubling_cap() {
        let mut previous = Duration::ZERO;
        for losses in (MAX_YIELD_LOSSES + 1)..=(MAX_YIELD_LOSSES + 5) {
            let BackoffStep::Sleep(longest) = backoff_step(losses, max_roll) else {
                panic!("losses={losses} should sleep");
            };
            assert!(longest > SLEEP_FLOOR);
            assert!(longest <= SLEEP_CAP);
            assert!(longest >= previous, "caps never shrink");
            previous = longest;

            let BackoffStep::Sleep(shortest) = backoff_step(losses, |_| 1) else {
                panic!("losses={losses} should sleep");
            };
            assert!(shortest > SLEEP_FLOOR);
            assert!(shortest < longest || longest == shortest);
        }
        // the ladder tops out at the cap
        assert_eq!(previous, SLEEP_CAP);
    }

    #[tokio::test(start_paused = true)]
    async fn a_commit_bump_wakes_the_sleep_tier_early() {
        let (bump, rx) = tokio::sync::watch::channel(crate::interpreter::stm::Version::new());
        let mut backoff = Backoff::watching(rx);
        for _ in 0..MAX_YIELD_LOSSES {
            backoff.stagger().await;
        }
        assert_eq!(
            backoff.spent().commit_wakes,
            0,
            "yield tier never waits on the watch"
        );

        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_micros(100)).await;
            bump.send_replace(crate::interpreter::stm::Version::new());
        });
        backoff.stagger().await;

        let spent = backoff.spent();
        assert_eq!(spent.commit_wakes, 1);
        assert_eq!(spent.cap_expiries, 0);
        // Woken at 100µs against a >1ms cap: realized stays under the ask.
        assert!(spent.slept < spent.sleep_requested);
    }

    #[tokio::test(start_paused = true)]
    async fn a_quiet_watch_runs_the_full_cap() {
        let (_bump, rx) = tokio::sync::watch::channel(crate::interpreter::stm::Version::new());
        let mut backoff = Backoff::watching(rx);
        for _ in 0..7 {
            backoff.stagger().await;
        }

        let spent = backoff.spent();
        assert_eq!(spent.commit_wakes, 0);
        assert_eq!(spent.cap_expiries, 1);
        assert!(spent.slept >= spent.sleep_requested);
    }

    #[tokio::test(start_paused = true)]
    async fn the_first_loss_spends_nothing() {
        let mut backoff = Backoff::new();
        backoff.stagger().await;
        assert_eq!(backoff.losses(), 1);
        assert_eq!(backoff.spent(), BackoffSpent::default());
    }

    #[tokio::test(start_paused = true)]
    async fn eight_losses_cross_into_the_sleep_tier_without_wall_time() {
        let mut backoff = Backoff::new();
        for _ in 0..8 {
            backoff.stagger().await;
        }
        assert_eq!(backoff.losses(), 8);

        let spent = backoff.spent();
        // Losses 7 and 8 sleep under caps of 2ms and 4ms, each above the floor.
        assert!(spent.sleep_requested > Duration::from_millis(2));
        assert!(spent.sleep_requested <= Duration::from_millis(6));
        // The timer never fires early, paused or not.
        assert!(spent.slept >= spent.sleep_requested);
    }
}
