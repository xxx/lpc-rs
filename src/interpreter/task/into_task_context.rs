use std::sync::Arc;

use crate::interpreter::{
    object_space::ObjectSpace, process::Process, task_context::TaskContext,
};

pub trait IntoTaskContext: AsRef<ObjectSpace> {
    /// Create a new [`TaskContext`].
    fn into_task_context(self, proc: Arc<Process>) -> TaskContext;
}
