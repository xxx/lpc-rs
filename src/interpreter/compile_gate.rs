//! The master's verdicts on what an LPC-triggered compile may read, asked in
//! the loading task's transaction.

use std::sync::Arc;

use async_trait::async_trait;
use lpc_rs_errors::Result;

use crate::{
    compiler::compile_gate::CompileGate,
    interpreter::{
        VALID_INHERIT, VALID_READ, apply::valid_apply, lpc_ref::LpcRef, task_context::TaskContext,
    },
};

/// `valid_inherit` and `valid_read("include")` through [`valid_apply`],
/// nested in the loading task's transaction.
#[derive(Debug)]
pub(crate) struct MasterGate {
    ctx: TaskContext,
}

impl MasterGate {
    /// The gate for compiles `ctx` triggers.
    #[cfg_attr(not(test), expect(dead_code))]
    #[expect(
        clippy::new_ret_no_self,
        reason = "callers only ever want the trait object"
    )]
    pub(crate) fn new(ctx: &TaskContext) -> Arc<dyn CompileGate> {
        Arc::new(Self { ctx: ctx.clone() })
    }
}

#[async_trait]
impl CompileGate for MasterGate {
    async fn inherit(&self, path: &str, from: &str) -> Result<bool> {
        valid_apply(
            &self.ctx,
            VALID_INHERIT,
            &[LpcRef::from(path), LpcRef::from(from)],
        )
        .await
    }

    async fn include(&self, path: &str, from: &str) -> Result<bool> {
        valid_apply(
            &self.ctx,
            VALID_READ,
            &[
                LpcRef::from(path),
                LpcRef::from("include"),
                LpcRef::from(0),
                LpcRef::from(from),
            ],
        )
        .await
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader, lpc_ref::LpcRef, process::Process, task::task_template::TaskTemplate,
            vm::Vm,
        },
        test_support::{TempLib, committed_string, temp_lib_config},
    };

    /// A fresh top-level context seated in `/seat`: its transaction is not
    /// joinable, so a nested apply opens and commits its own attempt. (A
    /// finished task's context would join its dead transaction and the
    /// master's writes would never commit.)
    async fn seated(vm: &Vm) -> TaskContext {
        let seat: Arc<Process> = vm
            .create_process_from_code("/seat.c", "")
            .await
            .expect("the seat compiles");
        TaskTemplate::from(vm.global_state.clone()).into_task_context(seat)
    }

    const RECORDER: &str = indoc! { r#"
        string inherit_path; string inherit_from;
        string read_path; string read_func; mixed read_caller; string read_from;
        int valid_inherit(string path, string from) {
            inherit_path = path; inherit_from = from; return 1;
        }
        int valid_read(string path, string func, object caller, mixed program) {
            read_path = path; read_func = func; read_caller = caller; read_from = program;
            return 0;
        }
    "# };

    #[tokio::test]
    async fn inherit_asks_valid_inherit_with_path_and_from() {
        let root = TempLib::new("master-gate-inherit");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code("/secure/master.c", RECORDER)
            .await
            .unwrap()
            .context
            .process;
        let ctx = seated(&vm).await;
        let gate = MasterGate::new(&ctx);
        assert!(gate.inherit("/parent.c", "/child.c").await.unwrap());
        assert_eq!(committed_string(&vm, &master, 0), "/parent.c");
        assert_eq!(committed_string(&vm, &master, 1), "/child.c");
    }

    /// An include is a read: `valid_read(path, "include", 0, from)`.
    #[tokio::test]
    async fn include_asks_valid_read_as_a_read_by_the_includer() {
        let root = TempLib::new("master-gate-include");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code("/secure/master.c", RECORDER)
            .await
            .unwrap()
            .context
            .process;
        let ctx = seated(&vm).await;
        let gate = MasterGate::new(&ctx);
        assert!(!gate.include("/secret.h", "/a.h").await.unwrap());
        assert_eq!(committed_string(&vm, &master, 2), "/secret.h");
        assert_eq!(committed_string(&vm, &master, 3), "include");
        assert_eq!(
            vm.global_state.committed_global(&master, 4u16),
            LpcRef::from(0)
        );
        assert_eq!(committed_string(&vm, &master, 5), "/a.h");
    }

    #[tokio::test]
    async fn without_a_master_both_answers_are_no() {
        let root = TempLib::new("master-gate-none");
        let vm = Vm::new(temp_lib_config(&root));
        let ctx = seated(&vm).await;
        let gate = MasterGate::new(&ctx);
        assert!(!gate.inherit("/parent.c", "/child.c").await.unwrap());
        assert!(!gate.include("/h.h", "/child.c").await.unwrap());
    }
}
