use std::{
    fmt::{Display, Formatter},
    sync::atomic::{AtomicUsize, Ordering},
};

use serde::{Deserialize, Serialize};

/// A unique ID, suitable for uniquely identifying an object that contains
/// references that could be garbage-collected.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TaskId(pub usize);

impl TaskId {
    /// Create a new [`TaskId`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for TaskId {
    fn default() -> Self {
        next_id()
    }
}

impl Display for TaskId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<usize> for TaskId {
    fn as_ref(&self) -> &usize {
        &self.0
    }
}

/// Get the next ID.
fn next_id() -> TaskId {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    TaskId(COUNTER.fetch_add(1, Ordering::Relaxed))
}
