use std::sync::mpsc::Sender;
use chrono::Duration;
use delegate::delegate;
use educe::Educe;
use slab::Slab;
use timer::{Guard, Timer};
use crate::interpreter::vm_op::VmOp;
use lpc_rs_errors::Result;

/// The handlers for scheduled [`Task`](crate::interpreter::task::Task)s
#[derive(Educe)]
#[educe(Debug)]
pub struct CallOuts {
    queue: Slab<()>,
    #[educe(Debug(ignore))]
    guards: Vec<Guard>,
    /// A channel to talk to the [`Vm`](crate::interpreter::vm::Vm)
    tx: Sender<VmOp>,
    #[educe(Debug(ignore))]
    timer: Timer,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new(tx: Sender<VmOp>) -> Self {
        Self {
            queue: Slab::with_capacity(64),
            guards: Vec::with_capacity(64),
            tx,
            timer: Timer::new(),
        }
    }

    delegate! {
        to self.queue {
            /// Get the number of scheduled [`Task`](crate::interpreter::task::Task)s
            pub fn len(&self) -> usize;

            /// Is the queue empty?
            pub fn is_empty(&self) -> bool;

            /// Get a reference to a [`Task`](crate::interpreter::task::Task) by its index
            pub fn get(&self, index: usize) -> Option<&()>;
        }
    }

    /// Schedule a [`Task`](crate::interpreter::task::Task) to be run after a given delay
    pub fn schedule_task(&mut self, task: (), delay: Duration) -> Result<usize> {
        let index = self.queue.insert(task);
        let tx = self.tx.clone();
        let guard = self.timer.schedule_with_delay(delay, move || {
            println!("in callback");
            match tx.send(VmOp::RunTask(index)) {
                Ok(_) => {},
                Err(e) => {
                    panic!("Failed to send task to VM: {}", e);
                }
            }
        });

        self.guards.insert(index, guard);

        Ok(index)
    }
}
