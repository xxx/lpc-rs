use std::sync::Arc;

use crate::interpreter::{process::Process, task_context::TaskContext};

pub trait IntoTaskContext {
    /// Create a [`TaskContext`] from a [`TaskTemplate`], and a [`Process`].
    fn into_task_context(self, proc: Arc<Process>) -> TaskContext;
}
