use std::sync::{
    Weak,
    atomic::{AtomicU64, Ordering::Relaxed},
};

use chrono::{DateTime, Duration, Utc};
use delegate::delegate;
use derive_builder::Builder;
use educe::Educe;
use if_chain::if_chain;
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

    /// The reference to the function that will be run.
    pub func_ref: LpcRef,

    /// How often does this call out repeat?
    #[builder(default, setter(strip_option))]
    repeat_duration: Option<Duration>,

    /// When does this call out run next?
    #[builder(default)]
    next_run: DateTime<Utc>,

    /// The RAII object that determines if the callback runs, or not.
    /// If the [`JoinHandle`] is dropped, the callback will not run.
    #[educe(Debug(ignore))]
    _handle: JoinHandle<()>,
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
            self.next_run = Utc::now() + repeat;
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

    /// The next explicit call out ID. Per-VM (not process-global) so tests
    /// running in parallel mint disjoint ID sequences. `u64`, so collisions
    /// are effectively impossible even across many VMs.
    next_call_out_id: AtomicU64,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: StableVec::with_capacity(512),
            tx,
            next_call_out_id: AtomicU64::new(0),
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
            func_ref,
            delay,
            repeat,
        } = schedule;
        let tx = self.tx.clone();

        let handle = if_chain! {
            if let Some(repeat) = repeat;
            if repeat.num_milliseconds() > 0;
            then {
                let start = if delay.num_milliseconds() <= 0 {
                    Instant::now()
                } else {
                    Instant::now() + delay.to_std().unwrap()
                };

                tokio::spawn(async move {
                    let mut i = tokio::time::interval_at(start, repeat.to_std().unwrap());

                    loop {
                        i.tick().await;
                        let _ = tx.send(VmOp::PrioritizeCallOut(id)).await;
                    }
                })
            } else {
                tokio::spawn(async move {
                    tokio::time::sleep(delay.to_std().unwrap()).await;
                    let _ = tx.send(VmOp::PrioritizeCallOut(id)).await;
                })
            }
        };

        self.queue.push(CallOut {
            id,
            process,
            func_ref,
            repeat_duration: repeat,
            next_run: Utc::now() + delay,
            _handle: handle,
        });
    }
}
