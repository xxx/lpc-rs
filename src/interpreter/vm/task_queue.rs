use std::collections::VecDeque;

// use delegate::delegate;
use crate::interpreter::task::Task;
use crate::{compile_time_config::MAX_CALL_STACK_SIZE, interpreter::task::task_state::TaskState};

#[derive(Debug)]
pub struct TaskQueue<const STACKSIZE: usize = MAX_CALL_STACK_SIZE> {
    ready: VecDeque<Task<STACKSIZE>>,
}

impl<const STACKSIZE: usize> TaskQueue<STACKSIZE> {
    /// Create a new, empty [`TaskQueue`]
    #[inline]
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

    /// Get a reference to the current [`Task`]
    #[inline]
    pub fn current(&self) -> Option<&Task<STACKSIZE>> {
        self.ready.front()
    }

    /// Get a mutable reference to the current [`Task`]
    #[inline]
    pub async fn current_mut(&mut self) -> Option<&mut Task<STACKSIZE>> {
        self.ready.front_mut()
    }

    /// Push a new value onto the ready queue. Returns the index of the new value.
    #[inline]
    pub fn push(&mut self, task: Task<STACKSIZE>) {
        self.ready.push_back(task);
    }

    /// Push a new value onto the front of the ready queue.
    #[inline]
    pub fn push_front(&mut self, task: Task<STACKSIZE>) {
        self.ready.push_front(task);
    }

    /// Pause the current Task, and switch to the next one in the ready queue.
    pub fn switch_to_next(&mut self) {
        if self.ready.len() > 1 {
            debug_assert!(
                self.ready.front().unwrap().state == TaskState::Paused,
                "switching away a Task that is not paused: {}",
                self.ready.front().unwrap().state
            );
            self.ready.rotate_left(1);
        }
    }

    /// Remove the current Task and make the next one current
    #[inline]
    pub fn finish_current(&mut self) {
        self.ready.pop_front();
    }
}

impl<const STACKSIZE: usize> Default for TaskQueue<STACKSIZE> {
    #[inline]
    fn default() -> Self {
        Self {
            ready: VecDeque::with_capacity(32),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOuts, gc::gc_bank::GcRefBank, object_space::ObjectSpace,
            process::Process, task_context::TaskContextBuilder,
        },
        test_support::test_config,
    };

    #[test]
    fn test_task_queue() {
        let mut queue = TaskQueue::<1>::new();

        assert!(queue.current().is_none());


        let (tx, _) = tokio::sync::mpsc::channel(128);

        let ctx = TaskContextBuilder::default()
            .config(test_config())
            .process(RwLock::new(Process::default()))
            .object_space(RwLock::new(ObjectSpace::default()))
            .vm_upvalues(RwLock::new(GcRefBank::default()))
            .call_outs(RwLock::new(CallOuts::new(tx.clone())))
            .tx(tx)
            .build()
            .unwrap();

        let task = Task::new(ctx.clone());
        let id = task.id;
        let task2 = Task::new(ctx);
        let id2 = task2.id;

        queue.push(task);
        queue.push_front(task2);
        assert_eq!(queue.current().unwrap().id, id2);

        queue.current_mut().unwrap().state = TaskState::Paused; // avoid an assertion failure
        queue.switch_to_next();
        assert_eq!(queue.current().unwrap().id, id);

        queue.finish_current();
        assert_eq!(queue.current().unwrap().id, id2);

        queue.finish_current();
        assert!(queue.current().is_none());
    }
}
