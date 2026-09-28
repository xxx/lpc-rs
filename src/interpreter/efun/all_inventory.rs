use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, process::Process};

/// `all_inventory`, an efun for returning an object's inventory.
pub async fn all_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    efun::return_objects_of(context, |txn, object| Process::inventory_of(txn, &object)).await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            process::Process,
            stm::{Transaction, TxnHandle, commit_changeset, start_txn},
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_config_builder,
        test_support::{permissive_master, test_config},
    };

    #[tokio::test]
    async fn test_all_inventory() {
        let room = indoc! { r#"
            object *inv;

            void set_inv() {
                inv = all_inventory(this_object());
            }
        "# };

        let ob = indoc! { r#"
            void create() {
                object room = find_object("/room");
                move_object(room);
                room->set_inv();
            }
        "# };

        let vm = Vm::new(test_config());

        let room_proc = vm
            .initialize_process_from_code("/room.c", room)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));

        let _foo_proc = vm
            .initialize_process_from_code("/all_inv_foo.c", ob)
            .await
            .unwrap();
        let _bar_proc = vm
            .initialize_process_from_code("/all_inv_bar.c", ob)
            .await
            .unwrap();
        let _baz_proc = vm
            .initialize_process_from_code("/all_inv_baz.c", ob)
            .await
            .unwrap();

        let room_proc_arc = room_proc.context.process.clone();

        let g0 = vm.global_state.committed_global(&room_proc_arc, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g0 else {
            panic!("global holds an array cell, actually {g0:?}");
        };
        let array = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        let globals = array.iter().map(|w| w.to_string()).sorted().collect_vec();

        assert_eq!(globals, &["/all_inv_bar", "/all_inv_baz", "/all_inv_foo"]);
    }

    #[tokio::test]
    async fn all_inventory_by_path() {
        let ob = indoc! { r#"
            void create() {
                move_object("/room");
            }
        "# };
        let checker = indoc! { r#"
            object *inv = all_inventory("/room");
        "# };

        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/room.c", "")
            .await
            .unwrap();
        vm.initialize_process_from_code("/all_inv_foo.c", ob)
            .await
            .unwrap();
        vm.initialize_process_from_code("/all_inv_bar.c", ob)
            .await
            .unwrap();
        let checker_proc = vm
            .initialize_process_from_code("/checker.c", checker)
            .await
            .unwrap()
            .context
            .process;

        let g0 = vm.global_state.committed_global(&checker_proc, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g0 else {
            panic!("global holds an array cell, actually {g0:?}");
        };
        let array = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed");
        let names = array.iter().map(|w| w.to_string()).sorted().collect_vec();

        assert_eq!(names, &["/all_inv_bar", "/all_inv_foo"]);
    }

    #[tokio::test]
    async fn destruction_omits_retained_members_immediately_and_after_commit_in_arrival_order() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code("/room.c", "")
                .await
                .unwrap();
            let mut members = Vec::new();
            for name in ["first", "dead", "last"] {
                members.push(
                    vm.initialize_process_from_code(
                        format!("/{name}.c"),
                        "void create() { move_object(\"/room\"); }",
                    )
                    .await
                    .unwrap()
                    .context
                    .process,
                );
            }
            let probe = vm
                .initialize_process_from_code(
                    "/probe.c",
                    indoc! { r#"
                        int inspect() {
                            object *items = all_inventory("/room");
                            return sizeof(items) == 2
                                && items[0] == find_object("/first")
                                && items[1] == find_object("/last");
                        }
                        int create() {
                            destruct(find_object("/dead"));
                            return inspect();
                        }
                    "# },
                )
                .await
                .unwrap();
            assert_eq!(probe.result().unwrap(), LpcRef::from(1), "{mode:?}");
            let result = apply_function_by_name(
                "inspect",
                &[],
                probe.context.process.clone(),
                TaskTemplate::from(vm.global_state.clone()),
                Some(5000),
            )
            .await
            .unwrap()
            .unwrap();
            assert_eq!(result, LpcRef::from(1), "{mode:?}");
            drop(members);
        }
    }

    #[tokio::test]
    async fn caught_destruction_stays_absent_from_inventory() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code("/room.c", "")
                .await
                .unwrap();
            let _victim = vm
                .initialize_process_from_code(
                    "/victim.c",
                    "void create() { move_object(\"/room\"); }",
                )
                .await
                .unwrap();
            let probe = vm
                .initialize_process_from_code(
                    "/probe.c",
                    indoc! { r#"
                        void remove_then_throw() {
                            destruct(find_object("/victim"));
                            throw("after destruction");
                        }
                        int create() {
                            mixed err = catch(remove_then_throw());
                            return stringp(err) && sizeof(all_inventory("/room")) == 0;
                        }
                    "# },
                )
                .await
                .unwrap();
            assert_eq!(probe.result().unwrap(), LpcRef::from(1), "{mode:?}");
        }
    }

    #[tokio::test]
    async fn uncaught_destruction_restores_inventory_on_rollback() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code("/room.c", "")
                .await
                .unwrap();
            let _prototype = vm
                .initialize_process_from_code(
                    "/victim.c",
                    "void enter() { move_object(\"/room\"); }",
                )
                .await
                .unwrap();
            let probe = vm
                .initialize_process_from_code(
                    "/probe.c",
                    indoc! { r#"
                        object victim;
                        int empty_during;
                        void create() {
                            victim = clone_object("/victim");
                            victim->enter();
                        }
                        void fail() {
                            destruct(victim);
                            empty_during = !sizeof(all_inventory("/room"));
                            throw("after destruction");
                        }
                        string inspect() {
                            object *items = all_inventory("/room");
                            return json_encode(({empty_during, sizeof(items),
                                objectp(victim),
                                sizeof(items) ? items[0] == find_object(file_name(victim)) : 0}));
                        }
                    "# },
                )
                .await
                .unwrap()
                .context
                .process;
            let template = TaskTemplate::from(vm.global_state.clone());
            let error =
                apply_function_by_name("fail", &[], probe.clone(), template.clone(), Some(5000))
                    .await
                    .unwrap()
                    .unwrap_err();
            assert!(error.to_string().contains("after destruction"), "{error}");
            let result = apply_function_by_name("inspect", &[], probe, template, Some(5000))
                .await
                .unwrap()
                .unwrap();
            assert_eq!(result.to_string(), "[0,1,1,1]", "{mode:?}");
        }
    }

    #[tokio::test]
    async fn destruction_invalidates_inventory_reads_of_bootstrap_objects() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let room = vm
            .initialize_process_from_code("/room.c", "")
            .await
            .unwrap()
            .context
            .process;
        let victim = vm
            .initialize_process_from_code("/victim.c", "void create() { move_object(\"/room\"); }")
            .await
            .unwrap()
            .context
            .process;
        let remover = vm
            .initialize_process_from_code(
                "/remover.c",
                "void remove() { destruct(find_object(\"/victim\")); }",
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(victim.cell.get().is_none());
        let mut live = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let reader = TxnHandle::new(Transaction::new(live.inner.clone()));
        assert_eq!(
            Process::inventory_of(&reader, &room).as_slice(),
            std::slice::from_ref(&victim)
        );
        apply_function_by_name(
            "remove",
            &[],
            remover,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(Process::inventory_of(&reader, &room), [victim]);
        live.disarm();
        assert!(
            commit_changeset(
                &vm.global_state.committer_tx,
                reader.with(|t| t.take_changeset())
            )
            .await
            .unwrap()
            .is_err()
        );
        assert!(vm.global_state.committed_inventory(&room).is_empty());
    }
}
