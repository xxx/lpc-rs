use std::sync::Arc;

use bit_set::BitSet;
use chrono::{DateTime, Duration, Utc};
use delegate::delegate;
use educe::Educe;
use if_chain::if_chain;
use lpc_rs_errors::Result;
use parking_lot::RwLock;
use stable_vec::StableVec;
use tokio::{sync::mpsc::Sender, task::JoinHandle, time::Instant};

use crate::interpreter::{gc::mark::Mark, lpc_ref::LpcRef, process::Process, vm::vm_op::VmOp};

/// A single call out to a function, to be run at a later time, potentially on an interval.
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOut {
    /// The process where `call_out` was called from.
    #[educe(Debug(ignore))]
    // TODO: make this Weak, and drop the call out if it's gone.
    process: Arc<RwLock<Process>>,

    /// The reference to the function that will be run.
    pub func_ref: LpcRef,

    /// How often does this call out repeat?
    repeat_duration: Option<Duration>,

    /// When does this call out run next?
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
    pub fn process(&self) -> &Arc<RwLock<Process>> {
        &self.process
    }
}

impl Mark for CallOut {
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        self.func_ref.mark(marked, processed)
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
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: StableVec::with_capacity(64),
            tx,
        }
    }

    delegate! {
        to self.queue {
            /// Get the number of scheduled [`CallOut`]s
            #[call(num_elements)]
            pub fn len(&self) -> usize;

            /// Is the queue empty?
            pub fn is_empty(&self) -> bool;

            /// Get a reference to a [`CallOut`] by its index
            pub fn get(&self, index: usize) -> Option<&CallOut>;

            /// Get a mutable reference to a [`CallOut`] by its index
            pub fn get_mut(&mut self, index: usize) -> Option<&mut CallOut>;

            /// Remove a [`CallOut`] by its index
            pub fn remove(&mut self, index: usize) -> Option<CallOut>;
        }
    }

    /// Get a reference to the underlying [`StableVec`]
    /// This is only used for iterating, as StableVec's
    /// Iter type is private, so we can't even declare it
    /// as a return type.
    #[inline]
    pub fn queue(&self) -> &StableVec<CallOut> {
        &self.queue
    }

    /// Schedule a [`CallOut`] to be run after a given delay
    pub fn schedule_task(
        &mut self,
        process: Arc<RwLock<Process>>,
        func_ref: LpcRef,
        delay: Duration,
        repeat: Option<Duration>,
    ) -> Result<usize> {
        let index = self.queue.next_push_index();
        let tx = self.tx.clone();

        if_chain! {
            if let Some(repeat) = repeat;
            if repeat.num_milliseconds() > 0;
            then {
                let start = if delay.num_milliseconds() <= 0 {
                    Instant::now()
                } else {
                    Instant::now() + delay.to_std().unwrap()
                };

                let handle = tokio::spawn(async move {
                    let mut i = tokio::time::interval_at(start, repeat.to_std().unwrap());

                    loop {
                        i.tick().await;
                        let _ = tx.send(VmOp::PrioritizeCallOut(index)).await;
                    }
                });

                self.queue.push(CallOut {
                    process,
                    func_ref,
                    repeat_duration: Some(repeat),
                    next_run: Utc::now() + delay,
                    _handle: handle,
                });
            } else {
                let handle = tokio::spawn(async move {
                    tokio::time::sleep(delay.to_std().unwrap()).await;
                    let _ = tx.send(VmOp::PrioritizeCallOut(index)).await;
                });

                self.queue.push(CallOut {
                    process,
                    func_ref,
                    repeat_duration: None,
                    next_run: Utc::now() + delay,
                    _handle: handle,
                });
            }
        }

        Ok(index)
    }
}

impl Mark for CallOuts {
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        for (_idx, call_out) in &self.queue {
            call_out.mark(marked, processed)?;
        }

        Ok(())
    }
}
