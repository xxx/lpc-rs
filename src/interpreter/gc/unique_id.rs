use std::{
    fmt::{Display, Formatter},
    sync::atomic::{AtomicUsize, Ordering},
};

use serde::{Deserialize, Serialize};

/// A unique ID, suitable for uniquely identifying an object that contains
/// references that could be garbage-collected.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct UniqueId(usize);

impl UniqueId {
    /// Create a new [`UniqueId`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for UniqueId {
    fn default() -> Self {
        next_id()
    }
}

impl Display for UniqueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<usize> for UniqueId {
    fn as_ref(&self) -> &usize {
        &self.0
    }
}

/// Get the next ID.
/// This ID is used so we can avoid track objects that have been marked,
/// to avoid infinitely processing a looping graph.
fn next_id() -> UniqueId {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    UniqueId(COUNTER.fetch_add(1, Ordering::Relaxed))
}
