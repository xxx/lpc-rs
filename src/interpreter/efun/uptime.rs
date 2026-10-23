use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Whole monotonic seconds since this VM's state was created.
pub fn uptime<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let seconds = context
        .task_context()
        .global_state
        .started_at
        .elapsed()
        .as_secs();
    context.return_efun_result(LpcRef::from(
        LpcIntInner::try_from(seconds).unwrap_or(LpcIntInner::MAX),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{run_prog, test_config},
    };

    #[tokio::test]
    async fn reports_elapsed_vm_seconds_as_an_integer() {
        let vm = Vm::new(test_config());
        let before = vm.global_state.started_at.elapsed().as_secs() as i64;
        let result = vm
            .initialize_process_from_code("/uptime.c", "int create() { return uptime(); }")
            .await
            .unwrap()
            .result();
        let after = vm.global_state.started_at.elapsed().as_secs() as i64;
        let Some(LpcRef::Int(seconds)) = result else {
            panic!("expected an integer, got {result:?}");
        };
        assert!((before..=after).contains(&seconds.0), "{seconds}");
    }

    #[tokio::test]
    async fn can_be_called_through_a_bare_efun_pointer() {
        let result = run_prog(
            "int create() { function clock = uptime; return intp(clock()) && clock() >= 0; }",
        )
        .await
        .result();
        assert_eq!(result, Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn rejects_arguments() {
        let error = Vm::new(test_config())
            .initialize_process_from_code("/uptime.c", "int create() { return uptime(1); }")
            .await
            .expect_err("uptime takes no arguments")
            .to_string();
        assert!(error.contains("incorrect argument count"), "{error}");
    }
}
