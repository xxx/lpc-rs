use std::sync::Arc;

use arc_swap::{ArcSwapAny, ArcSwapOption};
use if_chain::if_chain;
use lpc_rs_errors::{lpc_error, LpcError, Result};
use tokio::sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError};

use crate::interpreter::task::task_id::TaskId;

/// Some states so we can determine which call to acquire the lock actually takes it.
/// This is used to know when it's time to release the lock.
#[derive(Debug, PartialEq, Eq)]
pub enum ProcessLockStatus {
    Acquired,
    AlreadyAcquired,
}

/// The lock used for synchronized functions.
#[derive(Debug)]
pub struct ProcessLock {
    /// The semaphore that's used for `synchronized` functions.
    semaphore: Arc<Semaphore>,

    /// The permit that's used for `synchronized` functions. We store it in the process,
    /// because synchronized functions are allowed to skip synchronization if the process has
    /// already been synchronized previously, _within *THE SAME* `Task`_. This indicates that a
    /// previous synchronized function has already acquired the permit, so we don't need to
    /// acquire it again. This means our locks are re-entrant.
    pair: ArcSwapOption<(OwnedSemaphorePermit, TaskId)>,
}

impl ProcessLock {
    /// is the Process currently locked?
    #[inline]
    pub fn is_locked(&self) -> bool {
        self.pair.load().is_some()
    }

    /// Acquire the permit for this process, but only if it's not already acquired. This will
    /// wait for the permit if already acquired by another process.
    pub async fn try_acquire(&self, task_id: TaskId) -> Result<ProcessLockStatus> {
        match self.semaphore.clone().try_acquire_owned() {
            Ok(permit) => {
                self.pair.store(Some(Arc::new((permit, task_id))));
                Ok(ProcessLockStatus::Acquired)
            }
            Err(TryAcquireError::NoPermits) => {
                let permit_task_id = {
                    let pair = self.pair.load();
                    let permit = pair.as_ref();

                    permit.map(|p| p.1)
                };

                // This works under the assumption that nothing else will drop the lock between loading the task
                // id and execution, in the case we already have the lock. As it requires the Task ID to release
                // the lock, and Task IDs are unique, this should be safe.
                // Another way to put it, is that it is not possible for a single Task ID to be requesting
                // the lock from multiple threads at the same time.
                if_chain! {
                    if let Some(permit_task_id) = permit_task_id;
                    if permit_task_id == task_id;
                    then {
                        // We already have the permit. We can skip synchronization.
                        Ok(ProcessLockStatus::AlreadyAcquired)
                    }
                    else {
                        // It's in use by another Task. Get in line.
                        let permit = self.semaphore.clone().acquire_owned().await
                            .map_err::<Box<LpcError>, _>(|_| lpc_error!("ProcessLock::try_acquire: semaphore closed"))?;
                        self.pair.store(Some(Arc::new((permit, task_id))));
                        Ok(ProcessLockStatus::Acquired)
                    }
                }
            }
            Err(TryAcquireError::Closed) => {
                Err(lpc_error!("ProcessLock::try_acquire: semaphore closed"))
            }
        }
    }

    /// Release the permit for this process, if the passed [`TaskId`] is the one that owns the permit.
    /// This will panic if the permit is not currently held, or a release for the wrong `Task` is requested.
    pub fn release(&self, task_id: TaskId) {
        let guard = self.pair.load();

        let Some(arc) = &*guard else {
            panic!("Call to release() with TID {} without taking the lock. This is a bug.", task_id);
        };

        let (_, permit_task_id) = &**arc;

        if permit_task_id == &task_id {
            self.pair.store(None);
        } else {
            panic!(
                "Call to release() with TID {} that does not hold the lock (held by TID {}). This is a bug.",
                task_id,
                permit_task_id
            );
        }
    }

    /// Release the permit if `task_id` matches. This is only used for tasks that error out completely.
    /// Use `release()` for normal release.
    pub fn try_release(&self, task_id: TaskId) {
        let guard = self.pair.load();

        let Some(arc) = &*guard else {
            return;
        };

        let (_, permit_task_id) = &**arc;

        if permit_task_id == &task_id {
            self.pair.store(None);
        }
    }
}

impl Default for ProcessLock {
    fn default() -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(1)),
            pair: ArcSwapAny::from(None),
        }
    }
}
