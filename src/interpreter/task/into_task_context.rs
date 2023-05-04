use std::sync::Arc;

use crate::interpreter::{
    heap::Heap, object_space::ObjectSpace, process::Process, task_context::TaskContext,
};

pub trait IntoTaskContext: AsRef<Heap> + AsRef<ObjectSpace> {
    /// Create a new [`TaskContext`].
    fn into_task_context(self, proc: Arc<Process>) -> TaskContext;
}
