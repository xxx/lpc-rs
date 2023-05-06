use std::collections::HashSet;
use std::sync::Arc;

use if_chain::if_chain;

use crate::interpreter::process::Process;

/// An iterator over all of the environments of a [`Process`]. The iterator can be created
/// prior to the object moving to a new environment, and will iterate over the
/// environment that is current at the time of the call to `next`.
#[derive(Debug)]
pub struct AllEnvironment {
    /// The current environment. Calling `next` will return the environment of this `Process`.
    current: Option<Arc<Process>>,

    /// The set of environments that have already been returned. This is used to prevent
    /// infinite loops in the case of circular environments.
    seen: HashSet<Arc<Process>>,
}

impl AllEnvironment {
    pub fn new(starter: Arc<Process>) -> Self {
        Self {
            current: Some(starter),
            seen: HashSet::new(),
        }
    }
}

impl Iterator for AllEnvironment {
    type Item = Arc<Process>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;

        if_chain! {
            if let Some(next) = &*current.position.environment.load();
            if let Some(next) = next.upgrade();
            if self.seen.insert(next.clone());
            then {
                self.current = Some(next.clone());
                Some(next)
            } else {
                self.current = None;
                None
            }
        }
    }
}
