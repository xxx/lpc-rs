use crate::interpreter::vm::global_state::GlobalState;

tokio::task_local! {
    static GIL_HELD: ();
}

pub async fn run_with_gil<F, R>(global_state: &GlobalState, fut: F) -> R
where
    F: Future<Output = R>,
{
    if !global_state.config.gil {
        return fut.await;
    }
    // Already inside this thread of execution's critical section: a nested
    // call_other / efun apply / sub-task. Pass through, or self-deadlock.
    if GIL_HELD.try_with(|_| ()).is_ok() {
        return fut.await;
    }

    let _guard = global_state.gil.lock().await;
    GIL_HELD.scope((), fut).await
}
