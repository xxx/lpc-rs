//! `shutdown([code])`: stop the driver once the task commits, with the
//! master's leave.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_SHUTDOWN, apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef,
    stm::Effect,
};

/// `shutdown([code])`: leave the main loop with exit code `code` (0 when
/// absent) once this task commits. The master's `valid_shutdown(caller,
/// program)` must allow it; no master or no apply refuses.
pub async fn shutdown<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let code = if context.arg_count() == 0 {
        0
    } else {
        match context.arg(0) {
            LpcRef::Int(n) => i32::try_from(n.0).map_err(|_| {
                context.runtime_error(format!("shutdown: exit code {} is out of range", n.0))
            })?,
            other => {
                return Err(
                    context.runtime_error(format!("shutdown: {} is not an int", other.type_name()))
                );
            }
        }
    };
    let args = [
        LpcRef::from(Arc::downgrade(context.process())),
        context.calling_program(),
    ];
    if !valid_apply(
        context.task_context(),
        Some(context.chain()),
        VALID_SHUTDOWN,
        &args,
    )
    .await?
    {
        return Err(context.runtime_error("shutdown: permission denied"));
    }
    context.record_effect(Effect::Shutdown { code });
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, vm::Vm, vm::vm_op::VmOp},
        test_support::{committed_string, test_config},
    };

    async fn vm_with_master(master: &str) -> Vm {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        vm
    }

    const ALLOWING: &str = "int valid_shutdown(object caller, string program) { return 1; }";

    /// Run `body` as an object's `create()`; the error it caught, or `""`.
    async fn run(vm: &Vm, body: &str) -> String {
        let code = format!("string err; void create() {{ err = catch({body}); }}");
        let p = vm
            .initialize_process_from_code("/caller.c", &code)
            .await
            .unwrap()
            .context
            .process;
        let err = vm.global_state.committed_global(&p, 0u16);
        err.as_str().unwrap_or_default().to_owned()
    }

    #[tokio::test]
    async fn an_allowing_master_queues_the_shutdown_at_commit() {
        let mut vm = vm_with_master(ALLOWING).await;
        let err = run(&vm, "shutdown(3)").await;
        assert!(err.is_empty(), "{err}");
        assert_eq!(vm.next_op(), Some(VmOp::Shutdown(3)));
    }

    #[tokio::test]
    async fn the_exit_code_defaults_to_zero() {
        let mut vm = vm_with_master(ALLOWING).await;
        run(&vm, "shutdown()").await;
        assert_eq!(vm.next_op(), Some(VmOp::Shutdown(0)));
    }

    #[tokio::test]
    async fn a_refusing_master_is_an_error_and_queues_nothing() {
        let mut vm =
            vm_with_master("int valid_shutdown(object caller, string program) { return 0; }").await;
        let err = run(&vm, "shutdown(1)").await;
        assert!(err.contains("shutdown: permission denied"), "{err}");
        assert_eq!(vm.next_op(), None);
    }

    #[tokio::test]
    async fn a_master_without_the_apply_refuses() {
        let mut vm = vm_with_master("").await;
        let err = run(&vm, "shutdown(1)").await;
        assert!(err.contains("shutdown: permission denied"), "{err}");
        assert_eq!(vm.next_op(), None);
    }

    #[tokio::test]
    async fn the_master_hears_the_caller_and_its_program() {
        let mut vm = vm_with_master(indoc! { r#"
            string seen;
            int valid_shutdown(object caller, string program) {
                seen = file_name(caller) + " " + program;
                return 1;
            }
        "# })
        .await;
        run(&vm, "shutdown(2)").await;
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(committed_string(&vm, &master, 0), "/caller /caller.c");
        assert_eq!(vm.next_op(), Some(VmOp::Shutdown(2)));
    }

    #[tokio::test]
    async fn a_task_that_fails_after_the_call_queues_nothing() {
        let mut vm = vm_with_master(ALLOWING).await;
        let result = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"void create() { shutdown(1); throw("boom"); }"#,
            )
            .await;
        assert!(result.is_err());
        assert_eq!(vm.next_op(), None);
    }

    #[tokio::test]
    async fn an_exit_code_outside_the_c_int_range_is_an_error() {
        let mut vm = vm_with_master(ALLOWING).await;
        let err = run(&vm, "shutdown(4294967296)").await;
        assert!(
            err.contains("shutdown: exit code 4294967296 is out of range"),
            "{err}"
        );
        assert_eq!(vm.next_op(), None);
    }

    #[tokio::test]
    async fn a_non_int_code_is_an_error() {
        let vm = vm_with_master(ALLOWING).await;
        let code = r#"string err; void create() { mixed s = "now"; err = catch(shutdown(s)); }"#;
        let p = vm
            .initialize_process_from_code("/caller.c", code)
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("shutdown: string is not an int"), "{err}");
    }

    /// The main loop leaves on the op and reports the code.
    #[tokio::test]
    async fn the_main_loop_exits_with_the_code() {
        let mut vm = vm_with_master("").await;
        vm.global_state.tx.send(VmOp::Shutdown(7)).await.unwrap();
        assert_eq!(vm.run().await.unwrap(), 7);
    }
}
