use chrono::Duration;
use timer::Timer;
use crate::interpreter::task::Task;

/// The handlers for scheduled [`Task`]s
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallOuts {
    queue: Vec<()>,
}

impl CallOuts {
    /// Create a new [`CallOuts`]
    pub fn new() -> Self {
        Self {
            queue: vec![],
        }
    }

    /// Schedule a [`Task`] to be run after a given delay
    pub fn schedule_task<const STACKSIZE: usize>(&mut self, task: Task<STACKSIZE>, delay: Duration) {
        let timer = Timer::new();
        timer.schedule_with_delay(delay, move || {
            // task.resume();
        });
    }
}
