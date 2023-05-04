use std::sync::Arc;

use crate::interpreter::{process::Process, task_context::TaskContext};
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::heap::Heap;

pub trait IntoTaskContext: AsRef<Heap> + AsRef<ObjectSpace> {
    /// Create a new [`TaskContext`].
    fn into_task_context(self, proc: Arc<Process>) -> TaskContext;
}
