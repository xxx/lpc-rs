use std::sync::mpsc::Sender;

use chrono::Duration;
use delegate::delegate;
use educe::Educe;
use lpc_rs_errors::Result;
use stable_vec::StableVec;
use timer::{Guard, Timer};

use crate::interpreter::{lpc_ref::LpcRef, vm_op::VmOp};

/// A single call out to a function, to be run at a later time, potentially on an interval.
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOut {
    /// The reference to the function that will be run.
    // TODO: need to GC this
    pub func_ref: LpcRef,

    /// The RAII object that determines if the callback runs, or not.
    /// If the [`Guard`](Guard) is dropped, the callback will not run.
    #[educe(Debug(ignore))]
    _guard: Guard,
}

/// The handlers for scheduled [`Task`](crate::interpreter::task::Task)s
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOuts {
    /// The collection of call out data
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
            /// Get the number of scheduled [`Task`](crate::interpreter::task::Task)s
            #[call(num_elements)]
            pub fn len(&self) -> usize;

            /// Is the queue empty?
            pub fn is_empty(&self) -> bool;

            /// Get a reference to a [`Task`](crate::interpreter::task::Task) by its index
            pub fn get(&self, index: usize) -> Option<&CallOut>;

            /// Remove a [`Task`](crate::interpreter::task::Task) by its index
            pub fn remove(&mut self, index: usize) -> Option<CallOut>;
        }
    }

    /// Schedule a [`Task`](crate::interpreter::task::Task) to be run after a given delay
    pub fn schedule_task(&mut self, func_ref: LpcRef, delay: Duration) -> Result<usize> {
        let index = self.queue.next_push_index();
        let tx = self.tx.clone();
        let guard =
            self.timer
                .schedule_with_delay(delay, move || match tx.send(VmOp::RunCallOut(index)) {
                    Ok(_) => {}
                    Err(e) => {
                        panic!("Failed to send task to VM: {}", e);
                    }
                });

        self.queue.push(CallOut {
            func_ref,
            _guard: guard,
        });

        Ok(index)
    }
}
