use std::{collections::HashSet, sync::Arc};

use crate::interpreter::{process::Process, stm::TxnHandle};

/// An iterator over all of the environments of a [`Process`]. The chain is
/// followed through the passed transaction, so a mover's in-flight (not yet
/// committed) move is what the rest of its task sees.
#[derive(Debug)]
pub struct AllEnvironment {
    /// The current environment. Calling `next` will return the environment of this `Process`.
    current: Option<Arc<Process>>,

    /// Already returned, by filename; the second line behind `move_to`'s
    /// refusal.
    seen: HashSet<Arc<Process>>,

    /// The transaction whose view this chain is followed through.
    txn: TxnHandle,
}

impl AllEnvironment {
    pub(crate) fn new(txn: TxnHandle, starter: Arc<Process>) -> Self {
        Self {
            current: Some(starter),
            seen: HashSet::new(),
            txn,
        }
    }
}

impl Iterator for AllEnvironment {
    type Item = Arc<Process>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;

        if let Some(next) = Process::environment_of(&self.txn, &current)
            && self.seen.insert(next.clone())
        {
            self.current = Some(next.clone());
            Some(next)
        } else {
            self.current = None;
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::program::Program;

    #[test]
    fn staged_environments_are_returned_in_order_then_the_iterator_stays_empty() {
        let object = |path: &str| {
            Arc::new(Process::new(Program {
                filename: Arc::new(path.into()),
                ..Default::default()
            }))
        };
        let item = object("/item.c");
        let bag = object("/bag.c");
        let room = object("/room.c");
        let txn = TxnHandle::empty();
        Process::move_to(&txn, &item, &bag).unwrap();
        Process::move_to(&txn, &bag, &room).unwrap();

        let mut environments = AllEnvironment::new(txn, item.clone());
        assert!(Arc::ptr_eq(&environments.next().unwrap(), &bag));
        assert!(Arc::ptr_eq(&environments.next().unwrap(), &room));
        assert!(environments.next().is_none());
        assert!(environments.next().is_none());
        assert!(
            AllEnvironment::new(TxnHandle::empty(), item)
                .next()
                .is_none()
        );
    }
}
