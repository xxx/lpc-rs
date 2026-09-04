use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process};

pub fn destruct<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.arg(0);
    let destruct = |proc: Arc<Process>| context.remove_process(proc);

    if matches!(lpc_ref, LpcRef::Array(_)) {
        lpc_ref.with_array(context.txn(), |arr| {
            arr.iter()
                .filter_map(|r| r.live_object(context.txn()))
                .for_each(&destruct)
        })?;
    } else if let Some(proc) = lpc_ref.live_object(context.txn()) {
        destruct(proc);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        telnet::ops::ConnectionOp,
        test_support::{allow_exec, connect, run_prog, test_config},
    };

    #[tokio::test]
    async fn test_destruct() {
        let code = r##"
            void create() {
                dump(file_name(this_object()));
                object ob = clone_object("/clone_target");
                dump(file_name(ob));
                destruct(ob);
            }
        "##;

        let result = run_prog(code).await;

        let space = result
            .context
            .object_space()
            .iter()
            .map(|x| x.key().to_owned())
            .collect::<Vec<_>>();

        assert!(space.contains(&"/clone_target".to_owned()));
        assert!(!space.contains(&"/clone_target#0".to_owned()));
        // This file, the clone's prototype, and the simul-efun object and
        // permissive master `run_prog` inserts.
        assert_eq!(result.context.object_space().len(), 4);
    }

    #[tokio::test]
    async fn destructing_a_connected_object_closes_its_connection_after_its_output() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;
        let main = indoc! { r#"
            void create() {
                object p = find_object("/player");
                set_this_player(p);
                write("bye");
                destruct(p);
            }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
        assert_eq!(
            connected.rx.try_recv(),
            Ok(ConnectionOp::SendMessage("bye".into()))
        );
        assert_eq!(connected.rx.try_recv(), Ok(ConnectionOp::Close));
        assert!(connected.connection.body().is_none());
    }

    #[tokio::test]
    async fn exec_then_destruct_of_the_old_body_keeps_the_connection() {
        let vm = Vm::new(test_config());
        allow_exec(&vm).await;
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;
        vm.create_process_from_code("/body.c", "").await.unwrap();
        let main = indoc! { r#"
            void create() {
                object p = find_object("/player");
                exec(find_object("/body"), p);
                destruct(p);
            }
        "# };
        vm.initialize_process_from_code("/main.c", main)
            .await
            .unwrap();
        assert_eq!(connected.rx.try_recv(), Ok(ConnectionOp::Attached));
        assert!(connected.rx.try_recv().is_err());
        assert_eq!(
            connected
                .connection
                .body()
                .as_ref()
                .map(|body| body.to_string()),
            Some("/body".to_owned())
        );
    }
}
