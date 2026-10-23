use std::time::Instant;

use imbl::HashMap;
use rayon::{ThreadPool, ThreadPoolBuildError, ThreadPoolBuilder, prelude::*};
use tracing::{info, warn};

use super::VarId;

#[derive(Debug)]
pub(super) struct Reclaimer {
    minimum_cells: usize,
    pool: Option<Result<ThreadPool, ThreadPoolBuildError>>,
}

impl Reclaimer {
    pub(super) fn new() -> Self {
        Self {
            // Small graph tests exercise disposal on workers without seeding large heaps.
            minimum_cells: if cfg!(test) { 0 } else { 262_144 },
            pool: None,
        }
    }

    pub(super) fn reclaim<V: Clone + Send + Sync>(&mut self, retired: HashMap<VarId, V>) {
        let started = Instant::now();
        let cells = retired.len();
        let (mode, chunks, workers) = if cells < self.minimum_cells {
            drop(retired);
            ("serial", 0, 0)
        } else {
            let chunks = retired.into_drop_chunks();
            let count = chunks.len();
            if count < 2 {
                drop(chunks);
                ("direct", count, 0)
            } else {
                match self.pool.get_or_insert_with(|| {
                    ThreadPoolBuilder::new()
                        .num_threads(2)
                        .stack_size(crate::compile_time_config::THREAD_STACK)
                        .thread_name(|index| format!("gc-reclaim-{index}"))
                        .build()
                }) {
                    Ok(pool) => {
                        pool.install(|| chunks.into_par_iter().for_each(drop));
                        ("parallel", count, 2)
                    }
                    Err(error) => {
                        warn!(target: "lpc_rs::gc", %error, "GC worker creation failed; using serial disposal");
                        drop(chunks);
                        ("fallback", count, 0)
                    }
                }
            }
        };
        info!(target: "lpc_rs::gc", mode, chunks, workers, retired_cells = cells,
            disposal_ms = started.elapsed().as_secs_f64() * 1000.0, "owned branch disposal");
    }

    #[cfg(test)]
    pub(super) fn initialized_workers(&self) -> Option<usize> {
        self.pool
            .as_ref()
            .and_then(|pool| pool.as_ref().ok())
            .map(ThreadPool::current_num_threads)
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, thread, time::Duration};

    use super::*;

    #[test]
    fn small_collections_do_not_start_workers() {
        let value = Arc::new(17);
        let weak = Arc::downgrade(&value);
        let world = [(VarId::new(), value)].into_iter().collect();
        let mut reclaimer = Reclaimer {
            minimum_cells: 262_144,
            pool: None,
        };
        reclaimer.reclaim(world);
        assert!(weak.upgrade().is_none());
        assert_eq!(reclaimer.initialized_workers(), None);
    }

    #[test]
    fn a_single_owned_chunk_stays_serial() {
        let value = Arc::new(17);
        let weak = Arc::downgrade(&value);
        let world = [(VarId::new(), value)].into_iter().collect();
        let mut reclaimer = Reclaimer::new();
        reclaimer.reclaim(world);
        assert!(weak.upgrade().is_none());
        assert_eq!(reclaimer.initialized_workers(), None);
    }

    #[test]
    fn shared_snapshots_remain_readable_without_starting_workers() {
        let id = VarId::new();
        let world: HashMap<_, _> = [(id, 17)].into_iter().collect();
        let retained = world.clone();
        let mut reclaimer = Reclaimer::new();
        reclaimer.reclaim(world);
        assert_eq!(retained.get(&id), Some(&17));
        assert_eq!(reclaimer.initialized_workers(), None);
    }

    #[test]
    fn caller_waits_for_worker_destruction() {
        struct BlockingDrop {
            started: flume::Sender<()>,
            release: flume::Receiver<()>,
        }
        impl Drop for BlockingDrop {
            fn drop(&mut self) {
                self.started.send(()).unwrap();
                self.release.recv().unwrap();
            }
        }
        let (started, observed) = flume::bounded(1);
        let (release, wait) = flume::bounded(1);
        let value = Arc::new(BlockingDrop {
            started,
            release: wait,
        });
        let world = (0..512).map(|_| (VarId::new(), value.clone())).collect();
        drop(value);
        let (finished, completion) = flume::bounded(1);
        let worker = thread::spawn(move || {
            let mut reclaimer = Reclaimer::new();
            reclaimer.reclaim(world);
            assert_eq!(reclaimer.initialized_workers(), Some(2));
            finished.send(()).unwrap();
        });
        observed.recv_timeout(Duration::from_secs(5)).unwrap();
        assert_eq!(completion.try_recv(), Err(flume::TryRecvError::Empty));
        release.send(()).unwrap();
        completion.recv_timeout(Duration::from_secs(5)).unwrap();
        worker.join().unwrap();
    }

    #[test]
    fn worker_creation_failure_disposes_values_synchronously() {
        let error = ThreadPoolBuilder::new()
            .num_threads(2)
            .spawn_handler(|_| Err(std::io::Error::other("injected worker creation failure")))
            .build()
            .unwrap_err();
        let value = Arc::new(17);
        let weak = Arc::downgrade(&value);
        let world = (0..512).map(|_| (VarId::new(), value.clone())).collect();
        drop(value);
        let mut reclaimer = Reclaimer {
            minimum_cells: 0,
            pool: Some(Err(error)),
        };
        reclaimer.reclaim(world);
        assert!(weak.upgrade().is_none());
        assert_eq!(reclaimer.initialized_workers(), None);
    }
}
