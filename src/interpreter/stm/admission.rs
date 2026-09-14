//! Retry turns for owners whose reads were invalidated on the same cell.

use std::sync::Arc;

use dashmap::{DashMap, mapref::entry::Entry};
use lpc_rs_errors::{Result, lpc_bug};
use tokio::sync::{OwnedSemaphorePermit, Semaphore};

use super::VarId;

#[derive(Debug, Default)]
pub(super) struct Admission {
    queues: DashMap<VarId, Queue, ahash::RandomState>,
}

#[derive(Debug)]
struct Queue {
    semaphore: Arc<Semaphore>,
    enrolled: usize,
}

impl Admission {
    pub(super) async fn acquire(&self, cell: VarId) -> Result<Turn<'_>> {
        let semaphore = {
            let mut queue = self.queues.entry(cell).or_insert_with(|| Queue {
                semaphore: Arc::new(Semaphore::new(1)),
                enrolled: 0,
            });
            queue.enrolled += 1;
            queue.semaphore.clone()
        };
        let enrollment = Enrollment {
            admission: self,
            cell,
        };
        let permit = semaphore
            .acquire_owned()
            .await
            .map_err(|_| lpc_bug!("an admission queue was closed"))?;
        Ok(Turn {
            _permit: permit,
            _enrollment: enrollment,
        })
    }

    #[cfg(test)]
    pub(super) fn is_idle(&self) -> bool {
        self.queues.is_empty()
    }
}

pub(super) struct Turn<'a> {
    // Release the semaphore before the last enrollment can remove its queue.
    _permit: OwnedSemaphorePermit,
    _enrollment: Enrollment<'a>,
}

struct Enrollment<'a> {
    admission: &'a Admission,
    cell: VarId,
}

impl Drop for Enrollment<'_> {
    fn drop(&mut self) {
        // Cleanup shares enrollment's entry lock so a live queue cannot split.
        if let Entry::Occupied(mut queue) = self.admission.queues.entry(self.cell) {
            queue.get_mut().enrolled -= 1;
            if queue.get().enrolled == 0 {
                queue.remove();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        future::Future,
        pin::Pin,
        sync::atomic::{AtomicUsize, Ordering},
        task::{Context, Poll, Waker},
        time::Duration,
    };

    use super::*;

    fn poll<F: Future>(future: Pin<&mut F>) -> Poll<F::Output> {
        future.poll(&mut Context::from_waker(Waker::noop()))
    }

    #[tokio::test]
    async fn waiters_take_turns_in_enrollment_order() {
        let admission = Admission::default();
        let cell = VarId::new();
        let first = admission.acquire(cell).await.unwrap();
        let mut second = Box::pin(admission.acquire(cell));
        let mut third = Box::pin(admission.acquire(cell));
        assert!(poll(second.as_mut()).is_pending());
        assert!(poll(third.as_mut()).is_pending());
        drop(first);
        assert!(poll(third.as_mut()).is_pending());
        let second = second.await.unwrap();
        drop(second);
        drop(third.await.unwrap());
        assert!(admission.is_idle());
    }

    #[tokio::test]
    async fn cancelling_a_waiter_preserves_the_following_turn() {
        let admission = Admission::default();
        let cell = VarId::new();
        let first = admission.acquire(cell).await.unwrap();
        let mut cancelled = Box::pin(admission.acquire(cell));
        let mut next = Box::pin(admission.acquire(cell));
        assert!(poll(cancelled.as_mut()).is_pending());
        assert!(poll(next.as_mut()).is_pending());
        drop(cancelled);
        drop(first);
        drop(next.await.unwrap());
        assert!(admission.is_idle());
    }

    #[tokio::test]
    async fn cancelling_an_unpolled_grant_releases_the_queue() {
        let admission = Admission::default();
        let cell = VarId::new();
        let first = admission.acquire(cell).await.unwrap();
        let mut waiter = Box::pin(admission.acquire(cell));
        assert!(poll(waiter.as_mut()).is_pending());
        drop(first);
        drop(waiter);
        assert!(admission.is_idle());
        drop(admission.acquire(cell).await.unwrap());
        assert!(admission.is_idle());
    }

    #[tokio::test(start_paused = true)]
    async fn expired_waiters_release_their_enrollment() {
        let admission = Admission::default();
        let cell = VarId::new();
        let holder = admission.acquire(cell).await.unwrap();
        assert!(
            tokio::time::timeout(Duration::from_millis(10), admission.acquire(cell))
                .await
                .is_err()
        );
        drop(holder);
        assert!(admission.is_idle());
    }

    #[tokio::test]
    async fn other_cells_and_vms_progress_independently() {
        let admission = Admission::default();
        let other_vm = Admission::default();
        let cell = VarId::new();
        let held = admission.acquire(cell).await.unwrap();
        drop(admission.acquire(VarId::new()).await.unwrap());
        drop(other_vm.acquire(cell).await.unwrap());
        drop(held);
        assert!(admission.is_idle());
        assert!(other_vm.is_idle());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn last_release_racing_with_enrollment_never_splits_a_queue() {
        let admission = Arc::new(Admission::default());
        let active = Arc::new(AtomicUsize::new(0));
        let cell = VarId::new();
        let mut tasks = tokio::task::JoinSet::new();
        for _ in 0..8 {
            let admission = admission.clone();
            let active = active.clone();
            tasks.spawn(async move {
                for _ in 0..500 {
                    let turn = admission.acquire(cell).await.unwrap();
                    assert_eq!(active.fetch_add(1, Ordering::SeqCst), 0);
                    tokio::task::yield_now().await;
                    assert_eq!(active.fetch_sub(1, Ordering::SeqCst), 1);
                    drop(turn);
                }
            });
        }
        while let Some(task) = tasks.join_next().await {
            task.unwrap();
        }
        assert!(admission.is_idle());
    }

    #[tokio::test]
    async fn transient_cells_leave_no_idle_entries() {
        let admission = Admission::default();
        for _ in 0..1000 {
            drop(admission.acquire(VarId::new()).await.unwrap());
        }
        assert!(admission.is_idle());
    }
}
