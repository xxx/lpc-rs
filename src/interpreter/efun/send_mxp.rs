use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    interpreter::efun::{self, efun_context::EfunContext},
    telnet::ops::ConnectionOp,
};

/// `send_mxp`, an efun sending MXP markup to an object's connection as
/// written; the session drops it while the client has MXP off.
pub async fn send_mxp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let markup = context
        .resolve_local_register(2 as RegisterSize)
        .with_string(|s| s.to_string())?;
    efun::send_to_connection(context, ConnectionOp::Mxp(markup));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        telnet::ops::ConnectionOp,
        test_support::{connect, test_config},
    };

    #[tokio::test]
    async fn markup_is_queued_on_the_targets_connection() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;
        let main = indoc! { r#"
            void create() { send_mxp(find_object("/player"), "<b>hi</b>"); }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
        assert_eq!(
            connected.rx.try_recv(),
            Ok(ConnectionOp::Mxp("<b>hi</b>".into()))
        );
    }
}
