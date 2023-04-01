use std::sync::mpsc::Sender;
use chrono::Duration;
use delegate::delegate;
use slab::Slab;
use timer::Timer;
use crate::interpreter::task::Task;
use crate::interpreter::vm_op::VmOp;


// enqueue job
  // add to queu8e
  // set timer to return ???

/// The handlers for scheduled [`Task`]s
#[derive(Debug)]
pub struct CallOuts {
    queue: Slab<()>,
    /// A channel to talk to the [`Vm`](crate::interpreter::vm::Vm)
    tx: Sender<VmOp>,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: Slab::with_capacity(64),
            tx
        }
    }

    delegate! {
        to self.queue {
            /// Get the number of scheduled [`Task`]s
            pub fn len(&self) -> usize;

            /// Is the queue empty?
            pub fn is_empty(&self) -> bool;

            /// Get a reference to a [`Task`] by its index
            pub fn get(&self, index: usize) -> Option<&()>;
        }
    }

    /// Schedule a [`Task`] to be run after a given delay
    pub fn schedule_task(&mut self, task: (), delay: Duration) {
        let index = self.queue.insert(task);
        let timer = Timer::new();
        let tx = self.tx.clone();
        timer.schedule_with_delay(delay, move || {
            match tx.send(VmOp::RunTask(index)) {
                Ok(_) => {},
                Err(e) => {
                    panic!("Failed to send task to VM: {}", e);
                }
            }
        });
    }
}
