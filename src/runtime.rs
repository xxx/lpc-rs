//! The runtime the binaries run the VM on.

use std::future::Future;

use crate::compile_time_config::THREAD_STACK;

/// Run `f` to completion on a multi-thread runtime whose every worker has a
/// `THREAD_STACK` stack — `f` runs on a worker, never on the calling thread,
/// which a runtime's stack size does not cover.
pub fn run<F>(f: F) -> F::Output
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_stack_size(THREAD_STACK)
        .build()
        .expect("the runtime builds")
        .block_on(async {
            match tokio::spawn(f).await {
                Ok(output) => output,
                // A task nobody aborts fails only by panicking.
                Err(e) => std::panic::resume_unwind(e.into_panic()),
            }
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_future_runs_on_a_worker_and_its_output_comes_back() {
        let caller = std::thread::current().id();
        let (on, answer) = run(async move { (std::thread::current().id(), 42) });
        assert_ne!(on, caller, "not the calling thread");
        assert_eq!(answer, 42);
    }
}
