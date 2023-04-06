use std::fmt::{Display, Formatter};

/// The state of a [`Task`](crate::interpreter::task::Task)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TaskState {
    /// The task has never run
    New,

    /// The task is currently running
    Running,

    /// The task is currently paused
    Paused,

    /// The task has completed
    Complete,

    /// There was an error in the task
    Error,
}

impl Display for TaskState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TaskState::New => write!(f, "New"),
            TaskState::Running => write!(f, "Running"),
            TaskState::Paused => write!(f, "Paused"),
            TaskState::Complete => write!(f, "Completed"),
            TaskState::Error => write!(f, "Error"),
        }
    }
}

