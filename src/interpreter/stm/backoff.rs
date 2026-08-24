//! Contention backoff: how a conflicted attempt staggers before re-running.
//!
//! One [`Backoff`] per attempt loop owns the loss count, the jitter RNG, and
//! the realized-time totals; the loop only calls [`Backoff::stagger`].

use std::time::Duration;

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
    /// Sleep as the ladder requested it, before timer rounding.
    pub(crate) sleep_requested: Duration,
    /// Wall time in the sleep tier.
    pub(crate) slept: Duration,
    /// Wall time in the yield tier.
    pub(crate) yielded: Duration,
}

/// One attempt loop's contention backoff: counts losses, draws jitter,
/// executes the ladder, accumulates realized time.
#[derive(Debug)]
pub(crate) struct Backoff {
    losses: u64,
    rng: u64,
    spent: BackoffSpent,
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
                tokio::time::sleep(duration).await;
                self.spent.sleep_requested += duration;
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
