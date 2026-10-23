use std::{
    collections::VecDeque,
    sync::{
        Arc, Weak,
        atomic::{AtomicBool, Ordering},
    },
};

use parking_lot::Mutex;
use tokio::sync::Notify;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        process::Process,
        stm::{Transaction, VarId, start_txn},
        task::{SeedArg, SeedEntry, Task, TaskSeed, task_template::TaskTemplate},
        vm::global_state::GlobalState,
    },
    telnet::connection::Connection,
};

const MAX_RECORDS: usize = 1024;

/// An opaque registration carried by committed connection controls.
#[derive(Debug)]
pub struct Registration {
    pub(crate) id: VarId,
    pub(crate) observer: Weak<Process>,
    pub(crate) target: Weak<Process>,
    pub(super) connection: Weak<Connection>,
    limit: usize,
    budget: Arc<Mutex<Budget>>,
    queue: Mutex<Queue>,
    wake: Notify,
    started: AtomicBool,
}

impl PartialEq for Registration {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Registration {}

#[derive(Debug, Default)]
struct Queue {
    records: VecDeque<Record>,
    closed: bool,
    failure: Option<&'static str>,
}

#[derive(Debug, Default)]
pub(super) struct Budget {
    bytes: usize,
    count: usize,
}

#[derive(Debug)]
struct Record {
    text: String,
    direction: &'static str,
    _lease: BudgetLease,
}

#[derive(Debug)]
struct BudgetLease {
    budget: Arc<Mutex<Budget>>,
    bytes: usize,
}

impl Drop for BudgetLease {
    fn drop(&mut self) {
        let mut budget = self.budget.lock();
        budget.bytes -= self.bytes;
        budget.count -= 1;
    }
}

impl Registration {
    pub(super) fn new(
        observer: &Arc<Process>,
        target: &Arc<Process>,
        connection: &Arc<Connection>,
        limit: usize,
    ) -> Self {
        Self {
            id: VarId::new(),
            observer: Arc::downgrade(observer),
            target: Arc::downgrade(target),
            connection: Arc::downgrade(connection),
            limit,
            budget: super::slots(observer).budget.clone(),
            queue: Mutex::default(),
            wake: Notify::new(),
            started: AtomicBool::new(false),
        }
    }

    pub(crate) fn capture(&self, text: &str, direction: &'static str, newline: bool) {
        if text.is_empty() && !newline {
            return;
        }
        let mut queue = self.queue.lock();
        if queue.closed {
            return;
        }
        let length = text.len().saturating_add(usize::from(newline));
        let mut budget = self.budget.lock();
        if length > self.limit.saturating_sub(budget.bytes) || budget.count >= MAX_RECORDS {
            queue.closed = true;
            queue.failure = Some("pending text limit exceeded");
            drop(budget);
            queue.records.clear();
        } else {
            let mut text = text.to_owned();
            if newline {
                text.push('\n');
            }
            budget.bytes += length;
            budget.count += 1;
            drop(budget);
            queue.records.push_back(Record {
                text,
                direction,
                _lease: BudgetLease {
                    budget: self.budget.clone(),
                    bytes: length,
                },
            });
        }
        drop(queue);
        self.wake.notify_one();
    }

    pub(crate) fn close(&self) {
        let mut queue = self.queue.lock();
        queue.closed = true;
        queue.records.clear();
        drop(queue);
        self.wake.notify_one();
    }

    pub(crate) fn is_closed(&self) -> bool {
        self.queue.lock().closed
    }

    pub(crate) async fn activate(
        self: &Arc<Self>,
        global: &Arc<GlobalState>,
        connection: &Connection,
    ) {
        if self.queue.lock().closed {
            return;
        }
        let Ok(live) = start_txn(&global.committer_tx).await else {
            return;
        };
        let mut txn = Transaction::new(live.inner.clone());
        if !self
            .connection
            .upgrade()
            .is_some_and(|c| std::ptr::eq(c.as_ref(), connection))
            || !self.valid(&mut txn)
        {
            self.close();
            return;
        }
        connection.set_snoop(Some(self.clone()));
        if !self.started.swap(true, Ordering::AcqRel) {
            let registration = self.clone();
            let global = Arc::downgrade(global);
            tokio::spawn(async move {
                registration.deliver(global).await;
            });
        }
    }

    async fn deliver(self: Arc<Self>, global: Weak<GlobalState>) {
        loop {
            let notified = self.wake.notified();
            let (record, closed, failure) = {
                let mut queue = self.queue.lock();
                (
                    queue.records.pop_front(),
                    queue.closed,
                    queue.failure.take(),
                )
            };
            if closed {
                if let Some(failure) = failure
                    && let Some(global) = global.upgrade()
                {
                    self.failed(&global, failure).await;
                }
                return;
            }
            let Some(record) = record else {
                notified.await;
                continue;
            };
            let Some(global) = global.upgrade() else {
                return;
            };
            let Some(observer) = self.observer.upgrade() else {
                return;
            };
            let serial = super::slots(&observer).serial.clone();
            let _serial = serial.lock().await;
            let Record {
                text,
                direction,
                _lease,
            } = record;
            let args = vec![
                SeedArg::Value(text.into()),
                SeedArg::Value(super::weak_object(&self.target)),
                SeedArg::Value(direction.into()),
            ];
            let result = self
                .run(&global, observer, SeedEntry::Snoop(self.clone()), args)
                .await;
            if let Err(error) = result {
                self.failed(&global, &error.to_string()).await;
                return;
            }
        }
    }

    async fn failed(self: &Arc<Self>, global: &Arc<GlobalState>, reason: &str) {
        self.close();
        let name = |endpoint: &Weak<Process>| {
            endpoint
                .upgrade()
                .map(|p| p.filename().into_owned())
                .unwrap_or_else(|| "<destructed>".into())
        };
        let message = format!(
            "snoop: {} observing {} retired: {reason}\n",
            name(&self.observer),
            name(&self.target)
        );
        tracing::warn!("{message}");
        global.config.debug_log(message).await;
        if let Some(process) = self.observer.upgrade().or_else(|| self.target.upgrade())
            && let Err(error) = self
                .run(
                    global,
                    process,
                    SeedEntry::RetireSnoop(self.clone()),
                    vec![],
                )
                .await
        {
            tracing::error!("snoop cleanup failed: {error}");
        }
    }

    async fn run(
        self: &Arc<Self>,
        global: &Arc<GlobalState>,
        process: Arc<Process>,
        entry: SeedEntry,
        args: Vec<SeedArg>,
    ) -> lpc_rs_errors::Result<()> {
        let context = TaskTemplate::from(global.clone()).into_task_context(process.clone());
        let mut task = Task::<MAX_CALL_STACK_SIZE>::new(context);
        task.timed_eval_seed(
            TaskSeed {
                process,
                entry,
                args,
                initializes: false,
            },
            global.config.max_execution_time,
        )
        .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn registration(observer: &Arc<Process>, limit: usize) -> Registration {
        let (tx, _rx) = tokio::sync::mpsc::unbounded_channel();
        let connection = Arc::new(Connection::new("127.0.0.1:1".parse().unwrap(), tx));
        Registration::new(observer, &Arc::new(Process::default()), &connection, limit)
    }

    #[test]
    fn snoop_budget_includes_in_flight_text_across_reattachment() {
        let observer = Arc::new(Process::default());
        let old = registration(&observer, 4);
        old.capture("abc", "input", true);
        let in_flight = old.queue.lock().records.pop_front().unwrap();
        old.close();
        let new = registration(&observer, 4);
        new.capture("x", "output", false);
        assert!(new.is_closed());
        drop(in_flight);
        let next = registration(&observer, 4);
        next.capture("abc", "input", true);
        assert!(!next.is_closed());
        next.close();
        assert_eq!(next.budget.lock().bytes, 0);
    }

    #[test]
    fn snoop_record_bound_and_empty_output_do_not_leave_unbounded_queues() {
        let observer = Arc::new(Process::default());
        let registration = registration(&observer, 65536);
        for _ in 0..2048 {
            registration.capture("", "output", false);
        }
        assert_eq!(registration.budget.lock().count, 0);
        for _ in 0..MAX_RECORDS {
            registration.capture("x", "output", false);
        }
        assert!(!registration.is_closed());
        registration.capture("x", "output", false);
        assert!(registration.is_closed());
        assert_eq!(registration.budget.lock().count, 0);
        assert!(registration.queue.lock().failure.take().is_some());
        registration.capture("again", "output", false);
        assert!(registration.queue.lock().failure.take().is_none());
    }
}
