use lpc_rs_errors::Result;
use tracing::trace;

use crate::{
    interpreter::{
        efun::{add_action::handler_from, efun_context::EfunContext},
        lpc_ref::LpcRef,
    },
    telnet::{connection::InputTo, ops::ConnectionOp},
};

/// `input_to`, an efun for registering a function to be called when the user
/// types something into the game.
pub fn input_to<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = handler_from(context, context.arg(0).clone(), "input_to")?;

    let LpcRef::Int(no_echo) = context.arg(1) else {
        return Err(context.runtime_error("non-integer sent as second argument to `input_to`"));
    };

    let input_to = InputTo {
        ptr,
        no_echo: (*no_echo).into(),
    };

    let process = context.this_player().load();

    if let Some(process) = &*process
        && let Some(connection) = context
            .txn()
            .with(|t| t.read_connection(process.connection.id))
    {
        let _ = connection.send(ConnectionOp::InputTo(input_to));
        context.return_efun_result(LpcRef::from(1));
        return Ok(());
    } else {
        // No connection to receive the message, so do nothing.
        trace!("input_to on non-interactive process");
        // 0 is already returned by default
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    async fn error_of(code: &str) -> String {
        Vm::new(test_config())
            .initialize_process_from_code("/asker.c", code)
            .await
            .unwrap_err()
            .to_string()
    }

    #[tokio::test]
    async fn a_name_the_object_lacks_is_an_error() {
        let code = indoc! { r#"
            void create() { input_to("nope"); }
        "# };
        let err = error_of(code).await;
        assert!(
            err.contains("input_to: no function `nope` in /asker"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn an_unbound_pointer_is_an_error() {
        let code = indoc! { r#"
            void create() { input_to(&->heard()); }
        "# };
        let err = error_of(code).await;
        assert!(err.contains("input_to: the receiver"), "{err}");
    }

    #[tokio::test]
    async fn a_name_without_a_player_registers_nothing() {
        let code = indoc! { r#"
            int r = -1;
            void create() { r = input_to("heard"); }
            void heard(string s) {}
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/asker.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(0)
        );
    }
}
