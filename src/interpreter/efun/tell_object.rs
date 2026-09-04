use lpc_rs_errors::Result;

use crate::interpreter::{
    apply::deliver, efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::Effect,
};

pub async fn tell_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let msg = context.arg(1).with_string(|s| s.to_string())?;
    let ob_ref = context.arg(0);
    let proc = if let Some(path) = ob_ref.as_str() {
        Some(context.load_object(path).await?)
    } else {
        ob_ref.live_object(context.txn())
    };

    let received = match proc {
        Some(proc) => {
            deliver(
                context.task_context(),
                || Some(context.chain()),
                &proc,
                None,
                &msg,
            )
            .await?
        }
        None => {
            context.record_effect(Effect::DebugLog(msg));
            false
        }
    };
    context.return_efun_result(LpcRef::from(received));

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        telnet::ops::ConnectionOp,
        test_support::{connect, test_config},
    };

    #[tokio::test]
    async fn catch_tell_sees_the_callers_command_giver() {
        let target = indoc! { r#"
            object giver;

            void create() {
                enable_commands();
            }

            void catch_tell(string message) {
                giver = this_player();
            }
        "# };

        let master = indoc! { r#"
            void create() {
                set_this_player(this_object());
                tell_object("/target", "hi");
            }
        "# };

        let vm = Vm::new(test_config());
        let target = vm
            .initialize_process_from_code("/target.c", target)
            .await
            .unwrap()
            .context
            .process;
        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap()
            .context
            .process;

        assert_eq!(
            vm.global_state.committed_global(&target, 0u16),
            crate::interpreter::lpc_ref::LpcRef::from(std::sync::Arc::downgrade(&master_proc))
        );
    }

    #[tokio::test]
    async fn any_object_with_catch_tell_is_told_living_or_not() {
        let master = indoc! { r#"
            void create() {
                object ob = clone_object("/enabled");
                tell_object(ob, "i herd");
                tell_object("/enabled#0", "u liek mudkips?");

                ob = clone_object("/disabled");
                tell_object(ob, "i don't herd");
            }
        "# };

        let enabled = indoc! { r#"
            inherit "/hears";

            void create() {
                enable_commands();
            }
        "# };

        let disabled = indoc! { r#"
            inherit "/hears";
        "# };

        let vm = Vm::new(test_config());
        let _enabled_proc = vm
            .create_process_from_code("/enabled.c", enabled)
            .await
            .unwrap();
        let _disabled_proc = vm
            .create_process_from_code("/disabled.c", disabled)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("master.c", master)
            .await
            .unwrap();

        let space = master_proc.context.object_space();

        let enabled = space.lookup("/enabled#0").unwrap();

        let g_enabled = vm.global_state.committed_global(&enabled, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g_enabled else {
            panic!("global holds an array cell, actually {g_enabled:?}");
        };
        let arr = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        assert_eq!(
            &arr.iter().map(|s| s.to_string()).collect_vec(),
            &["i herd", "u liek mudkips?"]
        );

        let disabled = space.lookup("/disabled#1").unwrap();
        let g_disabled = vm.global_state.committed_global(&disabled, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g_disabled else {
            panic!("global holds an array cell, actually {g_disabled:?}");
        };
        let arr = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        assert_eq!(
            &arr.iter().map(|s| s.to_string()).collect_vec(),
            &["i don't herd"]
        );
    }

    #[tokio::test]
    async fn an_object_without_catch_tell_is_told_on_its_connection() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;

        let main = indoc! { r#"
            int r;
            void create() { r = tell_object(find_object("/player"), "hi"); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(
            connected.rx.try_recv(),
            Ok(ConnectionOp::SendMessage("hi".into()))
        );
    }

    #[tokio::test]
    async fn an_object_with_neither_is_logged_and_not_received() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/mute.c", "").await.unwrap();
        let main = indoc! { r#"
            int r;
            void create() { r = tell_object(find_object("/mute"), "hi"); }
        "# };
        let main = vm
            .initialize_process_from_code("/main.c", main)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&main, 0u16),
            LpcRef::from(0)
        );
    }
}
