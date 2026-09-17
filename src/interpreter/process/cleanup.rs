use std::{sync::Arc, time::Duration};

use parking_lot::Mutex;
use tokio::time::Instant;

use super::Process;
use crate::interpreter::{lpc_ref::LpcRef, stm::SVar};

/// Scheduling metadata is advisory; the opt-out is committed with the hook.
#[derive(Debug)]
pub(crate) struct Cleanup {
    pub(crate) disabled: SVar<LpcRef>,
    timing: Mutex<Timing>,
}

#[derive(Debug)]
struct Timing {
    used: Instant,
    queried: Instant,
    running: bool,
}

impl Default for Cleanup {
    fn default() -> Self {
        let now = Instant::now();
        Self {
            disabled: SVar::new(),
            timing: Mutex::new(Timing {
                used: now,
                queried: now,
                running: false,
            }),
        }
    }
}

impl Cleanup {
    pub(crate) fn touch(&self) {
        self.timing.lock().used = Instant::now();
    }

    pub(crate) fn is_idle(&self, interval: Duration) -> bool {
        self.timing.lock().used.elapsed() >= interval
    }

    pub(crate) fn claim(process: &Arc<Process>, interval: Duration) -> Option<CleanupLease> {
        let cleanup = process.cleanup.as_ref()?;
        let mut timing = cleanup.timing.lock();
        if timing.running || timing.used.elapsed() < interval || timing.queried.elapsed() < interval
        {
            return None;
        }
        timing.running = true;
        Some(CleanupLease(process.clone()))
    }
}

/// Cancellation must release the reservation just as a completed query does.
pub(crate) struct CleanupLease(Arc<Process>);

impl Drop for CleanupLease {
    fn drop(&mut self) {
        if let Some(cleanup) = &self.0.cleanup {
            let mut timing = cleanup.timing.lock();
            timing.running = false;
            timing.queried = Instant::now();
        }
    }
}
