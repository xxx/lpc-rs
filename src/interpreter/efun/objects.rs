use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Every loaded object visible to the calling transaction, in unspecified order.
pub fn objects<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let objects = context.txn().with(|txn| {
        txn.objects()
            .map(|process| LpcRef::from(Arc::downgrade(&process)))
            .collect::<Vec<_>>()
    });
    context.return_array(objects);
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            object_space::ObjectSpace,
            process::Process,
            program::Program,
            stm::{
                Committer, Transaction, TxnHandle, VarId, commit_changeset, start_txn,
                txn_insert_process, txn_undo_insert,
            },
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_config_builder,
        test_support::{permissive_master, run_prog_with_config, test_config},
    };

    #[tokio::test]
    async fn includes_prototypes_clones_and_self_during_initialization() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let task = run_prog_with_config(
                indoc! { r#"
                int create() {
                    object clone = clone_object("/clone_target");
                    object prototype = find_object("/clone_target");
                    object *all = objects();
                    return member_array(this_object(), all) >= 0
                        && member_array(clone, all) >= 0
                        && member_array(prototype, all) >= 0
                        && member_array(find_object("/secure/master"), all) >= 0
                        && member_array(find_object("/secure/simul_efuns"), all) >= 0;
                }
            "# },
                Arc::new(test_config_builder!().null_mode(mode).build().unwrap()),
            )
            .await;
            assert_eq!(task.result().unwrap(), LpcRef::from(1), "{mode:?}");
        }
    }

    #[tokio::test]
    async fn each_result_has_independent_membership_and_storage() {
        let task = run_prog_with_config(
            indoc! { r#"
            int create() {
                object *before = objects();
                int count = sizeof(before);
                before[0] = nil;
                object clone = clone_object("/clone_target");
                int added = sizeof(objects()) - count;
                destruct(clone);
                return sizeof(before) == count && before[0] == nil
                    && added == 2 && sizeof(objects()) == count + 1;
            }
        "# },
            Arc::new(test_config()),
        )
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn keeps_old_clone_groups_after_prototype_replacement_and_gc() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/creator.c",
                indoc! { r#"
            object old;
            object fresh;
            void create() {
                old = clone_object("/clone_target");
                destruct(find_object("/clone_target"));
                fresh = clone_object("/clone_target");
            }
            int check() {
                object *all = objects();
                return member_array(old, all) >= 0 && member_array(fresh, all) >= 0
                    && member_array(find_object("/clone_target"), all) >= 0
                    && sizeof(object_clones(fresh)) == 1;
            }
        "# },
            )
            .await
            .unwrap();
        vm.global_state.gc().await.unwrap().unwrap();
        let result = apply_function_by_name(
            "check",
            &[],
            task.context.process.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(1));
    }

    #[tokio::test]
    async fn failed_initializer_removes_its_object_from_enumeration() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
            void create() {
                if (member_array(this_object(), objects()) < 0) throw("not listed");
                throw("failed initializer");
            }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! { r#"
            int create() {
                int count = sizeof(objects());
                mixed error = catch(clone_object("/target"));
                return error == "failed initializer" && sizeof(objects()) == count;
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_aborted_call_does_not_publish_created_clones() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.create_process_from_code("/target.c", "").await.unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! { r#"
            int count() { return sizeof(objects()); }
            void fail() { clone_object("/target"); throw("abort"); }
        "# },
            )
            .await
            .unwrap();
        let process = task.context.process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let before =
            apply_function_by_name("count", &[], process.clone(), template.clone(), Some(5000))
                .await
                .unwrap()
                .unwrap();
        assert!(
            apply_function_by_name("fail", &[], process.clone(), template.clone(), Some(5000))
                .await
                .unwrap()
                .is_err()
        );
        let after = apply_function_by_name("count", &[], process, template, Some(5000))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(before, after);
    }

    #[tokio::test]
    async fn rejects_predicate_arguments() {
        let error = Vm::new(test_config())
            .initialize_process_from_code("/bad.c", "void create() { objects((: 1 :)); }")
            .await
            .unwrap_err();
        assert!(error.to_string().contains("argument"), "{error}");
    }

    fn contains(txn: &mut Transaction, object: &Arc<Process>) -> bool {
        txn.objects().any(|p| Arc::ptr_eq(&p, object))
    }

    #[test]
    fn empty_enumeration_is_invalidated_by_concurrent_insertions_without_conflicting_writers() {
        let mut committer = Committer::new();
        let space = Arc::new(ObjectSpace::default());
        let program = Arc::new(Program::new("/target.c"));
        let mut reader = Transaction::new(committer.snapshot_clone());
        assert_eq!(reader.objects().count(), 0);
        let a = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let b = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&a, &space, &space.create_clone_process(program.clone()));
        txn_insert_process(&b, &space, &space.create_clone_process(program));
        committer.commit(a.with(|t| t.take_changeset())).unwrap();
        committer.commit(b.with(|t| t.take_changeset())).unwrap();
        assert!(committer.commit(reader.take_changeset()).is_err());
        assert_eq!(
            Transaction::new(committer.snapshot_clone())
                .objects()
                .count(),
            2
        );
    }

    #[test]
    fn pending_and_committed_unflushed_objects_are_visible() {
        let mut committer = Committer::new();
        let space = Arc::new(ObjectSpace::default());
        let clone = space.create_clone_process(Arc::new(Program::new("/target.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &clone);
        assert!(writer.with(|t| contains(t, &clone)));
        assert!(space.is_empty());
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        assert!(contains(
            &mut Transaction::new(committer.snapshot_clone()),
            &clone
        ));
        assert!(space.is_empty());
    }

    #[test]
    fn removed_objects_remain_in_older_snapshots_but_not_the_removing_attempt() {
        let mut committer = Committer::new();
        let space = Arc::new(ObjectSpace::default());
        let clone = space.create_clone_process(Arc::new(Program::new("/target.c")));
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &clone);
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        let mut reader = Transaction::new(committer.snapshot_clone());
        let remover = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_undo_insert(&remover, &space, &clone);
        assert!(!remover.with(|t| contains(t, &clone)));
        committer
            .commit(remover.with(|t| t.take_changeset()))
            .unwrap();
        assert!(contains(&mut reader, &clone));
        assert!(committer.commit(reader.take_changeset()).is_err());
        assert_eq!(
            Transaction::new(committer.snapshot_clone())
                .objects()
                .count(),
            0
        );
    }

    #[test]
    fn replacement_appears_once_after_removal_and_reinsertion() {
        let mut committer = Committer::new();
        let space = Arc::new(ObjectSpace::default());
        let old = Arc::new(Process::new(Arc::new(Program::new("/target.c"))));
        let new = Arc::new(Process::new(Arc::new(Program::new("/target.c"))));
        let txn = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&txn, &space, &old);
        txn_undo_insert(&txn, &space, &old);
        txn_insert_process(&txn, &space, &new);
        assert_eq!(txn.with(|t| t.objects().count()), 1);
        assert!(txn.with(|t| contains(t, &new)));
        committer.commit(txn.with(|t| t.take_changeset())).unwrap();
        let mut reader = Transaction::new(committer.snapshot_clone());
        assert!(contains(&mut reader, &new));
        assert!(!contains(&mut reader, &old));
    }

    #[test]
    fn unrelated_global_writes_do_not_invalidate_enumeration() {
        let mut committer = Committer::new();
        let mut reader = Transaction::new(committer.snapshot_clone());
        assert_eq!(reader.objects().count(), 0);
        let mut writer = Transaction::new(committer.snapshot_clone());
        writer.write(VarId::new(), 42.into());
        committer.commit(writer.take_changeset()).unwrap();
        committer.commit(reader.take_changeset()).unwrap();
    }

    #[tokio::test]
    async fn bootstrap_placements_are_snapshotted_and_invalidate_older_enumerations() {
        let vm = Vm::new(test_config());
        let mut old = start_txn(&vm.global_state.committer_tx).await.unwrap();
        vm.create_process_from_code("/bootstrap.c", "")
            .await
            .unwrap();
        let fresh = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let mut reader = Transaction::new(old.inner.clone());
        assert_eq!(reader.objects().count(), 0);
        assert_eq!(Transaction::new(fresh.inner.clone()).objects().count(), 1);
        old.disarm();
        assert!(
            commit_changeset(&vm.global_state.committer_tx, reader.take_changeset())
                .await
                .unwrap()
                .is_err()
        );
    }

    #[tokio::test]
    async fn bootstrap_replacement_preserves_snapshot_membership_and_liveness() {
        let vm = Vm::new(test_config());
        let old = vm
            .create_process_from_code("/bootstrap.c", "")
            .await
            .unwrap();
        let before = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let new = vm
            .create_process_from_code("/bootstrap.c", "")
            .await
            .unwrap();
        let after = start_txn(&vm.global_state.committer_tx).await.unwrap();
        assert!(old.cell.get().is_none());
        assert!(new.cell.get().is_none());

        let mut before = Transaction::new(before.inner.clone());
        assert!(contains(&mut before, &old));
        assert!(!contains(&mut before, &new));
        assert!(old.is_live_in(&mut before));

        let mut after = Transaction::new(after.inner.clone());
        assert!(!contains(&mut after, &old));
        assert!(contains(&mut after, &new));
        assert!(!old.is_live_in(&mut after));
        assert!(new.is_live_in(&mut after));
    }
}
