use std::{sync::Arc, time::Duration};

#[cfg(test)]
mod tests;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        process::{Process, cleanup::Cleanup},
        task::{
            SeedEntry, Task, TaskSeed, apply_function::report_runtime_error,
            task_template::TaskTemplate,
        },
        vm::global_state::GlobalState,
    },
};

impl GlobalState {
    /// Query idle objects without retaining map guards or retired instances across awaits.
    pub(crate) async fn clean_up(self: &Arc<Self>) {
        let interval = self.config.clean_up_interval;
        if interval == 0 {
            return;
        }
        let objects: Vec<_> = self
            .object_space
            .iter()
            .filter(|entry| {
                entry.cleanup.is_some() && !self.object_space.is_system_key(entry.key())
            })
            .map(|entry| Arc::downgrade(entry.value()))
            .collect();
        for object in objects {
            if let Some(process) = object.upgrade() {
                self.clean_up_object(process, Duration::from_secs(interval))
                    .await;
            }
            tokio::task::yield_now().await;
        }
    }

    async fn clean_up_object(self: &Arc<Self>, process: Arc<Process>, interval: Duration) {
        let Some(_lease) = Cleanup::claim(&process, interval) else {
            return;
        };
        let template = TaskTemplate::from(self.clone());
        let mut task: Task<MAX_CALL_STACK_SIZE> =
            Task::new(template.clone().into_task_context(process.clone()));
        let seed = TaskSeed {
            process: process.clone(),
            entry: SeedEntry::CleanUp,
            args: vec![],
            initializes: false,
        };
        if let Err(error) = task
            .timed_eval_seed(seed, self.config.max_execution_time)
            .await
        {
            report_runtime_error(&error, Some(process), template).await;
        }
    }
}
