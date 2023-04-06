use std::sync::mpsc::Sender;

use bit_set::BitSet;
use chrono::{DateTime, Duration, Utc};
use delegate::delegate;
use educe::Educe;
use lpc_rs_errors::Result;
use qcell::QCellOwner;
use stable_vec::StableVec;
use timer::{Guard, Timer};

use crate::interpreter::{gc::mark::Mark, lpc_ref::LpcRef, vm::vm_op::VmOp};

/// A single call out to a function, to be run at a later time, potentially on an interval.
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOut {
    /// The reference to the function that will be run.
    pub func_ref: LpcRef,

    /// How often does this call out repeat?
    repeat_duration: Option<Duration>,

    /// When does this call out run next?
    next_run: DateTime<Utc>,

    /// The RAII object that determines if the callback runs, or not.
    /// If the [`Guard`](Guard) is dropped, the callback will not run.
    #[educe(Debug(ignore))]
    _guard: Guard,
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
            self.next_run = chrono::Utc::now() + repeat;
        }
    }

    /// How much time is left until this call out runs?
    /// If the call out has already run and will not repeat, this will return `None`.
    pub fn time_remaining(&self) -> Option<Duration> {
        let now = chrono::Utc::now();
        if now > self.next_run {
            None
        } else {
            Some(self.next_run - now)
        }
    }
}

impl Mark for CallOut {
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        cell_key: &QCellOwner,
    ) -> Result<()> {
        self.func_ref.mark(marked, processed, cell_key)
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

    /// stored timer object
    #[educe(Debug(ignore))]
    timer: Timer,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: StableVec::with_capacity(64),
            tx,
            timer: Timer::new(),
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

    /// Schedule a [`CallOut`] to be run after a given delay
    pub fn schedule_task(
        &mut self,
        func_ref: LpcRef,
        delay: Duration,
        repeat: Option<Duration>,
    ) -> Result<usize> {
        let index = self.queue.next_push_index();
        let tx = self.tx.clone();
        let date = Utc::now() + delay;
        let guard = self.timer.schedule(date, repeat, move || {
            // This needs to run as fast as possible, and not fail.
            let _ = tx.send(VmOp::PrioritizeCallOut(index));
        });

        self.queue.push(CallOut {
            func_ref,
            repeat_duration: repeat,
            next_run: date,
            _guard: guard,
        });

        Ok(index)
    }
}

impl Mark for CallOuts {
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        cell_key: &QCellOwner,
    ) -> Result<()> {
        for (_idx, call_out) in &self.queue {
            call_out.mark(marked, processed, cell_key)?;
        }

        Ok(())
    }
}
