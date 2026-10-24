use std::sync::{
    Weak,
    atomic::{AtomicU64, Ordering::Relaxed},
};

use chrono::{DateTime, Duration, Utc};
use delegate::delegate;
use derive_builder::Builder;
use educe::Educe;
use stable_vec::StableVec;
use tokio::{sync::mpsc::Sender, task::JoinHandle, time::Instant};

use crate::interpreter::{
    lpc_ref::LpcRef, process::Process, stm::CallOutSchedule, vm::vm_op::VmOp,
};

/// A single call out to a function, to be run at a later time, potentially on an interval.
#[derive(Educe, Builder)]
#[educe(Debug)]
#[builder(pattern = "owned")]
pub struct CallOut {
    /// The call out ID, minted when the `call_out` was recorded so it is
    /// addressable before the deferred physical insert.
    pub id: u64,

    /// The process where `call_out` was called from.
    process: Weak<Process>,

    /// The scheduling command giver, restored independently for each firing.
    #[builder(default)]
    pub player: Option<Weak<Process>>,

    /// The reference to the function that will be run.
    pub func_ref: LpcRef,

    /// How often does this call out repeat?
    #[builder(default, setter(strip_option))]
    repeat_duration: Option<Duration>,

    /// When does this call out run next?
    #[builder(default)]
    next_run: DateTime<Utc>,

    /// The timer task, aborted when this call out is dropped.
    #[educe(Debug(ignore))]
    _handle: JoinHandle<()>,
}

impl Drop for CallOut {
    fn drop(&mut self) {
        self._handle.abort();
    }
}

impl CallOut {
    /// Is this a repeating call out?
    #[inline]
    pub fn is_repeating(&self) -> bool {
        self.repeat_duration.is_some()
    }

    /// Update the next run time, if relevant
    pub fn refresh(&mut self) {
        if let Some(repeat) = self.repeat_duration {
            if let Some(next_run) = Utc::now().checked_add_signed(repeat) {
                self.next_run = next_run;
            } else {
                self.repeat_duration = None;
                tracing::warn!("call_out: repeat exceeds the supported calendar range");
            }
        }
    }

    /// Get the duration of the repeat interval, if any
    #[inline]
    pub fn repeat_duration(&self) -> Option<Duration> {
        self.repeat_duration
    }

    /// How much time is left until this call out runs?
    /// If the call out has already run and will not repeat, this will return `None`.
    pub fn time_remaining(&self) -> Option<Duration> {
        let now = Utc::now();
        if now > self.next_run {
            None
        } else {
            Some(self.next_run - now)
        }
    }

    /// Get the process that owns this call out
    #[inline]
    pub fn process(&self) -> &Weak<Process> {
        &self.process
    }
}

/// The handlers for scheduled [`CallOut`]s
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOuts {
    /// The collection of call out data
    // TODO: stable vec isn't great for this.
    queue: StableVec<CallOut>,

    /// A channel to talk to the [`Vm`](crate::interpreter::vm::Vm)
    tx: Sender<VmOp>,

    /// The next ID in this VM; zero is reserved for unset mudlib handles.
    next_call_out_id: AtomicU64,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: StableVec::with_capacity(512),
            tx,
            next_call_out_id: AtomicU64::new(1),
        }
    }

    /// Mint the next explicit call out ID. `&self` + atomic: the `call_out`
    /// efun records its schedule before the flush, when it only holds a
    /// shared view of the queue.
    pub fn mint_id(&self) -> u64 {
        self.next_call_out_id.fetch_add(1, Relaxed)
    }

    /// Clear all [`CallOut`]s from the queue
    pub fn clear(&mut self) {
        self.queue.clear();
    }

    delegate! {
        to self.queue {
            /// Get the number of scheduled [`CallOut`]s
            #[call(num_elements)]
            pub fn len(&self) -> usize;

            /// Is the queue empty?
            pub fn is_empty(&self) -> bool;
        }
    }

    /// Push a ready-made [`CallOut`] (no timer task) for fire-path fixtures.
    #[cfg(test)]
    pub fn push(&mut self, value: CallOut) {
        self.queue.push(value);
    }

    /// Get a reference to the underlying [`StableVec`]
    /// This is only used for iterating, as StableVec's
    /// Iter type is private, so we can't even declare it
    /// as a return type.
    #[inline]
    pub fn queue(&self) -> &StableVec<CallOut> {
        &self.queue
    }

    /// Get a reference to a [`CallOut`] by its ID.
    pub fn get_by_id(&self, id: u64) -> Option<&CallOut> {
        self.queue
            .iter()
            .find(|(_idx, call_out)| call_out.id == id)
            .map(|(_idx, call_out)| call_out)
    }

    /// Get a mutable reference to a [`CallOut`] by its ID.
    pub fn get_mut_by_id(&mut self, id: u64) -> Option<&mut CallOut> {
        self.queue
            .iter_mut()
            .find(|(_idx, call_out)| call_out.id == id)
            .map(|(_idx, call_out)| call_out)
    }

    /// Remove a [`CallOut`] by its ID, returning it.
    pub fn remove_by_id(&mut self, id: u64) -> Option<CallOut> {
        let idx = self
            .queue
            .iter()
            .find_map(|(idx, call_out)| (call_out.id == id).then_some(idx))?;
        self.queue.remove(idx)
    }

    /// Check the first firing and repeat against both timer clocks.
    pub(crate) fn checked_deadlines(
        delay: Duration,
        repeat: Option<Duration>,
    ) -> Option<(DateTime<Utc>, Instant)> {
        let delay = delay.max(Duration::zero());
        let next_run = Utc::now().checked_add_signed(delay)?;
        let start = Instant::now().checked_add(delay.to_std().ok()?)?;
        if let Some(repeat) = repeat {
            next_run.checked_add_signed(repeat)?;
            start.checked_add(repeat.to_std().ok()?)?;
        }
        Some((next_run, start))
    }

    /// Physically materialize a recorded call out: spawn its timer task and
    /// push itself into the queue. The flush calls this after a successful
    /// commit, so an aborted attempt spawns no timer and enqueues nothing.
    ///
    /// TODO: Unclear if there's a race condition for the 0 delay case.
    ///       Hand testing doesn't show one, but it's possible that
    ///       something shows up under load.
    pub fn materialize(&mut self, schedule: CallOutSchedule) {
        let CallOutSchedule {
            id,
            process,
            player,
            func_ref,
            delay,
            repeat,
        } = schedule;
        let delay = delay.max(Duration::zero());
        let repeat = repeat.filter(|duration| duration.num_milliseconds() > 0);
        let Some((next_run, start)) = Self::checked_deadlines(delay, repeat) else {
            tracing::warn!("call_out: schedule exceeds the supported clock range");
            return;
        };
        let tx = self.tx.clone();

        let handle = if let Some(repeat) = repeat {
            tokio::spawn(async move {
                let mut i = tokio::time::interval_at(start, repeat.to_std().unwrap());

                loop {
                    i.tick().await;
                    let _ = tx.send(VmOp::PrioritizeCallOut(id)).await;
                }
            })
        } else {
            tokio::spawn(async move {
                tokio::time::sleep_until(start).await;
                let _ = tx.send(VmOp::PrioritizeCallOut(id)).await;
            })
        };

        self.queue.push(CallOut {
            id,
            process,
            player,
            func_ref,
            repeat_duration: repeat,
            next_run,
            _handle: handle,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn unsupported_schedules_are_not_materialized() {
        for (delay, repeat) in [
            (Duration::MAX, None),
            (Duration::zero(), Some(Duration::MAX)),
        ] {
            let (tx, mut rx) = tokio::sync::mpsc::channel(4);
            let mut calls = CallOuts::new(tx);
            calls.materialize(CallOutSchedule {
                id: 1,
                process: Weak::new(),
                player: None,
                func_ref: 0.into(),
                delay,
                repeat,
            });
            assert!(calls.is_empty());
            assert!(rx.try_recv().is_err());
        }
    }

    #[tokio::test]
    async fn nonpositive_repeat_intervals_schedule_one_firing() {
        for repeat in [
            Duration::zero(),
            Duration::seconds(-1),
            Duration::microseconds(1),
        ] {
            let (tx, _rx) = tokio::sync::mpsc::channel(4);
            let mut calls = CallOuts::new(tx);
            calls.materialize(CallOutSchedule {
                id: 1,
                process: Weak::new(),
                player: None,
                func_ref: 0.into(),
                delay: Duration::zero(),
                repeat: Some(repeat),
            });
            assert_eq!(calls.get_by_id(1).unwrap().repeat_duration(), None);
        }
    }

    #[tokio::test]
    async fn a_repeat_is_disabled_when_its_calendar_deadline_overflows() {
        let (tx, _rx) = tokio::sync::mpsc::channel(4);
        let mut calls = CallOuts::new(tx);
        calls.materialize(CallOutSchedule {
            id: 1,
            process: Weak::new(),
            player: None,
            func_ref: 0.into(),
            delay: Duration::seconds(3600),
            repeat: Some(Duration::seconds(1)),
        });
        let call = calls.get_mut_by_id(1).unwrap();
        call.repeat_duration = Some(Duration::MAX);
        call.refresh();
        assert!(!call.is_repeating());
    }

    #[tokio::test(start_paused = true)]
    async fn nonpositive_delays_fire_immediately_and_preserve_repetition() {
        for delay in [
            Duration::seconds(-1),
            Duration::zero(),
            Duration::seconds(2),
        ] {
            for repeat in [None, Some(Duration::seconds(1))] {
                let (tx, mut rx) = tokio::sync::mpsc::channel(16);
                let mut calls = CallOuts::new(tx);
                calls.materialize(CallOutSchedule {
                    id: 17,
                    process: Weak::new(),
                    player: None,
                    func_ref: 0.into(),
                    delay,
                    repeat,
                });
                tokio::task::yield_now().await;
                if delay > Duration::zero() {
                    assert!(rx.try_recv().is_err(), "positive delay fired early");
                    tokio::time::advance(std::time::Duration::from_secs(2)).await;
                    tokio::task::yield_now().await;
                }
                assert_eq!(rx.try_recv().unwrap(), VmOp::PrioritizeCallOut(17));
                assert!(rx.try_recv().is_err(), "timer fired twice initially");
                tokio::time::advance(std::time::Duration::from_secs(1)).await;
                tokio::task::yield_now().await;
                if repeat.is_some() {
                    assert_eq!(rx.try_recv().unwrap(), VmOp::PrioritizeCallOut(17));
                }
                assert!(rx.try_recv().is_err(), "unexpected extra timer firing");
            }
        }
    }

    #[tokio::test(start_paused = true)]
    async fn removing_or_clearing_call_outs_stops_their_timers() {
        for clear in [false, true] {
            for repeat in [None, Some(Duration::seconds(100))] {
                let (tx, mut rx) = tokio::sync::mpsc::channel(16);
                let mut calls = CallOuts::new(tx);
                calls.materialize(CallOutSchedule {
                    id: 0,
                    process: Weak::new(),
                    player: None,
                    func_ref: 0.into(),
                    delay: Duration::seconds(100),
                    repeat,
                });
                tokio::task::yield_now().await;
                if clear {
                    calls.clear();
                } else {
                    calls.remove_by_id(0);
                }
                tokio::time::advance(std::time::Duration::from_secs(101)).await;
                tokio::task::yield_now().await;
                assert!(rx.try_recv().is_err(), "a canceled timer still fired");
            }
        }
    }
}
