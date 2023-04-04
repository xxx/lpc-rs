use std::collections::VecDeque;

use crate::compile_time_config::MAX_CALL_STACK_SIZE;
// use delegate::delegate;
use crate::interpreter::task::Task;

#[derive(Debug)]
pub struct TaskQueue<const STACKSIZE: usize = MAX_CALL_STACK_SIZE> {
    current: Option<Task<STACKSIZE>>,
    ready: VecDeque<Task<STACKSIZE>>,
}

impl<const STACKSIZE: usize> TaskQueue<STACKSIZE> {
    /// Create a new, empty [`TaskQueue`]
    pub fn new() -> Self {
        Self::default()
    }
    //
    // delegate! {
    //     to self.ready {
    //         /// Get the number of tasks in the ready queue
    //         #[call(len)]
    //         pub fn ready_len(&self) -> usize;
    //
    //         /// Get whether or not the queue is empty
    //         pub fn is_empty(&self) -> bool;
    //     }
    // }

    pub fn push(&mut self, task: Task<STACKSIZE>) -> &mut Task<STACKSIZE> {
        self.ready.push_back(task);

        self.ready.back_mut().unwrap()
    }

    /// Pause the current Task, and switch to the next one in the ready queue.
    pub fn execute_next(&mut self) -> Option<&Task<STACKSIZE>> {
        if let Some(task) = self.current.take() {
            self.ready.push_back(task);
        };

        self.current = self.ready.pop_front();

        self.current.as_ref()
    }
}

impl<const STACKSIZE: usize> Default for TaskQueue<STACKSIZE> {
    fn default() -> Self {
        Self {
            current: None,
            ready: VecDeque::with_capacity(32),
        }
    }
}
