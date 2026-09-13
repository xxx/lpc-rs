use std::{collections::HashSet, sync::Arc};

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_DESTRUCT, apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef,
};

/// Authorize every live target before scheduling transactional removal.
pub async fn destruct<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let targets = match context.arg(0) {
        arg @ LpcRef::Array(_) => arg.with_array(context.txn(), |arr| {
            let mut seen = HashSet::new();
            arr.iter()
                .filter_map(|r| r.live_object(context.txn()))
                .filter(|p| seen.insert(p.filename().into_owned()))
                .collect::<Vec<_>>()
        })?,
        arg => arg.live_object(context.txn()).into_iter().collect(),
    };
    let caller = LpcRef::from(Arc::downgrade(context.process()));
    let program = context.calling_program();

    // Authorize the whole batch before removal, since LPC can catch a refusal.
    for target in &targets {
        if !target.is_live(context.txn()) {
            continue;
        }
        let args = [
            caller.clone(),
            LpcRef::from(Arc::downgrade(target)),
            program.clone(),
        ];
        if !valid_apply(
            context.task_context(),
            Some(context.chain()),
            VALID_DESTRUCT,
            &args,
        )
        .await?
        {
            return Err(context.runtime_error("destruct: permission denied"));
        }
    }
    for target in targets {
        if target.is_live(context.txn()) {
            context.remove_process(target);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use crate::{
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            vm::{Vm, global_state::GlobalState},
        },
        telnet::ops::ConnectionOp,
        test_support::{
            allow_exec, committed_string, connect, lib_holding, permissive_master, run_prog,
            temp_lib_config, test_config,
        },
    };

    async fn vm_with_master(master: Option<&str>) -> Vm {
        let vm = Vm::new(test_config());
        if let Some(master) = master {
            vm.initialize_process_from_code("/secure/master.c", master)
                .await
                .unwrap();
        }
        vm.initialize_process_from_code("/target.c", "")
            .await
            .unwrap();
        vm
    }

    async fn caught(vm: &Vm, body: &str) -> String {
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                format!("string err; void create() {{ err = catch({body}); }}"),
            )
            .await
            .unwrap();
        vm.global_state
            .committed_global(&task.context.process, 0u16)
            .as_str()
            .unwrap_or_default()
            .to_owned()
    }

    #[tokio::test]
    async fn missing_or_refusing_authorization_preserves_the_target_and_connection() {
        for master in [None, Some(""), Some("int valid_destruct() { return 0; }")] {
            let vm = vm_with_master(master).await;
            let target = vm.global_state.object_space.lookup("/target").unwrap();
            let mut connected = connect(&vm, &target).await;
            let err = caught(&vm, "destruct(find_object(\"/target\"))").await;
            assert!(err.contains("destruct: permission denied"), "{err}");
            assert!(vm.global_state.object_space.lookup("/target").is_some());
            assert!(connected.connection.body().is_some());
            assert!(connected.rx.try_recv().is_err());
        }
    }

    #[tokio::test]
    async fn self_destruction_and_master_calls_require_authorization() {
        for allowed in [0, 1] {
            let master =
                format!("int calls; int valid_destruct() {{ calls++; return {allowed}; }}");
            let vm = vm_with_master(Some(&master)).await;
            let err = caught(&vm, "destruct(this_object())").await;
            assert_eq!(err.contains("destruct: permission denied"), allowed == 0);
            assert_eq!(
                vm.global_state.object_space.lookup("/caller").is_some(),
                allowed == 0
            );
            let master = vm.global_state.object_space.master_object().unwrap();
            assert_eq!(
                vm.global_state.committed_global(&master, 0u16),
                LpcRef::from(1)
            );

            let vm = Vm::new(test_config());
            let task = vm
                .initialize_process_from_code(
                    "/secure/master.c",
                    format!(
                        "string err; int valid_destruct() {{ return {allowed}; }}
                 void create() {{ err = catch(destruct(this_object())); }}"
                    ),
                )
                .await
                .unwrap();
            assert_eq!(
                vm.global_state.object_space.master_object().is_some(),
                allowed == 0
            );
            if allowed == 0 {
                assert!(
                    committed_string(&vm, &task.context.process, 0)
                        .contains("destruct: permission denied")
                );
            }
        }
    }

    #[tokio::test]
    async fn authorization_receives_the_caller_target_defining_program_and_player() {
        let root = lib_holding(
            "destruct-authority",
            &[(
                "parent.c",
                indoc! { r#"
            void remove(object target) { destruct(target); }
            function pointer() { return destruct; }
        "# },
            )],
        );
        for invocation in ["remove(target)", "pointer()(target)"] {
            let vm = Vm::new(temp_lib_config(&root));
            vm.initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                int valid_destruct(object caller, object target, string program) {
                    return caller == previous_object() && caller == this_player()
                        && file_name(caller) == "/child" && file_name(target) == "/target"
                        && program == "/parent.c" && this_object() == find_object("/secure/master");
                }
            "# },
            )
            .await
            .unwrap();
            vm.initialize_process_from_code("/target.c", "")
                .await
                .unwrap();
            vm.initialize_process_from_code(
                "/child.c",
                format!(
                    "inherit \"/parent\";
                 void create() {{ set_this_player(this_object());
                     object target = find_object(\"/target\"); {invocation}; }}"
                ),
            )
            .await
            .unwrap();
            assert!(vm.global_state.object_space.lookup("/target").is_none());
        }
    }

    #[tokio::test]
    async fn array_authorization_finishes_before_any_removal_and_skips_duplicates() {
        let vm = vm_with_master(Some(indoc! { r#"
            string seen = "";
            int valid_destruct(object caller, object target, string program) {
                seen += file_name(target) + " ";
                return objectp(find_object("/target")) && objectp(find_object("/other"));
            }
        "# }))
        .await;
        vm.initialize_process_from_code("/other.c", "")
            .await
            .unwrap();
        let err = caught(&vm, "destruct(({ find_object(\"/target\"), 0, find_object(\"/other\"), find_object(\"/target\") }))").await;
        assert!(err.is_empty(), "{err}");
        assert!(vm.global_state.object_space.lookup("/target").is_none());
        assert!(vm.global_state.object_space.lookup("/other").is_none());
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(committed_string(&vm, &master, 0), "/target /other ");
    }

    #[tokio::test]
    async fn a_caught_array_refusal_or_apply_error_leaves_all_targets_alive() {
        for refusal in ["return 0;", "throw(\"policy failure\");"] {
            let vm = vm_with_master(Some(&format!(
                "int calls; int valid_destruct(object caller, object target, string program) {{
                     calls++; if (file_name(target) == \"/target\") return 1; {refusal} }}"
            )))
            .await;
            vm.initialize_process_from_code("/other.c", "")
                .await
                .unwrap();
            let err = caught(
                &vm,
                "destruct(({ find_object(\"/target\"), find_object(\"/other\") }))",
            )
            .await;
            let expected = if refusal == "return 0;" {
                "destruct: permission denied"
            } else {
                "policy failure"
            };
            assert!(err.contains(expected), "{err}");
            assert!(vm.global_state.object_space.lookup("/target").is_some());
            assert!(vm.global_state.object_space.lookup("/other").is_some());
            let master = vm.global_state.object_space.master_object().unwrap();
            assert_eq!(
                vm.global_state.committed_global(&master, 0u16),
                LpcRef::from(2)
            );
        }
    }

    #[tokio::test]
    async fn authorization_cannot_change_targets_by_mutating_the_input_array() {
        let vm = vm_with_master(Some(indoc! { r#"
            int valid_destruct(object caller, object target, string program) {
                caller->change_targets();
                return 1;
            }
        "# }))
        .await;
        vm.initialize_process_from_code("/other.c", "")
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/caller.c",
            indoc! { r#"
            object *targets;
            void change_targets() { targets[0] = find_object("/other"); }
            void create() {
                targets = ({ find_object("/target") });
                destruct(targets);
            }
        "# },
        )
        .await
        .unwrap();
        assert!(vm.global_state.object_space.lookup("/target").is_none());
        assert!(vm.global_state.object_space.lookup("/other").is_some());
    }

    #[tokio::test]
    async fn null_empty_and_destructed_targets_do_not_call_the_apply() {
        let vm = vm_with_master(Some(indoc! { r#"
            int calls;
            int valid_destruct() { calls++; return calls == 1; }
        "# }))
        .await;
        let err = caught(&vm, "destruct(0)").await;
        assert!(err.is_empty(), "{err}");
        vm.initialize_process_from_code(
            "/noop.c",
            indoc! { r#"
            void create() {
                object target = find_object("/target");
                destruct(target);
                destruct(target);
                destruct(({ target, 0 }));
                destruct(({}));
            }
        "# },
        )
        .await
        .unwrap();
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn destruction_during_authorization_is_not_applied_twice() {
        let vm = vm_with_master(Some(indoc! { r#"
            int calls;
            int valid_destruct(object caller, object target, string program) {
                calls++;
                if (calls == 1) destruct(target);
                return 1;
            }
        "# }))
        .await;
        let target = vm.global_state.object_space.lookup("/target").unwrap();
        let mut connected = connect(&vm, &target).await;
        let err = caught(&vm, "destruct(find_object(\"/target\"))").await;
        assert!(err.is_empty(), "{err}");
        assert!(vm.global_state.object_space.lookup("/target").is_none());
        assert_eq!(connected.rx.try_recv(), Ok(ConnectionOp::Close));
        assert!(connected.rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn an_uncaught_error_rolls_back_authorization_destruction_and_disconnect() {
        let vm = vm_with_master(Some(
            "int calls; int valid_destruct() { calls++; return 1; }",
        ))
        .await;
        let target = vm.global_state.object_space.lookup("/target").unwrap();
        let mut connected = connect(&vm, &target).await;
        let err = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! { r#"
            void create() { destruct(find_object("/target")); throw("later failure"); }
        "# },
            )
            .await
            .unwrap_err();
        assert!(err.to_string().contains("later failure"));
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(0)
        );
        assert!(vm.global_state.object_space.lookup("/target").is_some());
        assert!(connected.connection.body().is_some());
        assert!(connected.rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn retries_commit_authorization_and_destruction_together_once() {
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(Arc::new(test_config()), tx, 8));
        state
            .object_space
            .create_process_from_code("/target.c", "")
            .await
            .unwrap();
        let master = state
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    int calls;
                    int valid_destruct() { calls++; return 1; }
                    void create() { destruct(clone_object("/target")); }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(state.attempt_telemetry().conflicts, 8);
        assert_eq!(state.committed_global(&master, 0u16), LpcRef::from(1));
        assert!(state.object_space.iter().all(|p| !p.is_clone()));
    }

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
        permissive_master(&vm.global_state.object_space).await;
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
