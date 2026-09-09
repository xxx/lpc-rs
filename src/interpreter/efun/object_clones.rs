use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

/// `object_clones`, the live clones sharing the argument's compiled program.
pub fn object_clones<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let clones = context
        .arg(0)
        .live_object(context.txn())
        .and_then(|object| {
            context.txn().with(|t| {
                // Older snapshots can keep a destructed process allocated.
                if let Some(&cell) = object.cell.get()
                    && !t
                        .read_object(cell)
                        .is_some_and(|live| Arc::ptr_eq(&live, &object))
                {
                    return None;
                }
                t.read_array(object.program.clones.id)
            })
        });
    context.return_array(
        clones
            .iter()
            .flat_map(|clones| clones.iter().rev().cloned())
            .collect::<Vec<_>>(),
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            object_space::ObjectSpace,
            program::Program,
            stm::{Committer, Transaction, TxnHandle, txn_insert_process, txn_undo_insert},
            vm::Vm,
        },
        test_support::{permissive_master, run_prog, test_config},
    };

    #[tokio::test]
    async fn prototype_and_clone_return_only_their_clones_newest_first() {
        let task = run_prog(indoc! { r#"
            int *create() {
                object first = clone_object("/clone_target");
                object second = clone_object("/clone_target");
                object other = clone_object("/empty");
                object *from_prototype = object_clones(find_object("/clone_target"));
                object *from_clone = object_clones(first);
                return ({ sizeof(from_prototype), from_prototype[0] == second,
                    from_prototype[1] == first, sizeof(from_clone),
                    from_clone[0] == second, from_clone[1] == first });
            }
        "# })
        .await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[2, 1, 1, 2, 1, 1][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn empty_null_and_destructed_arguments_return_empty_arrays() {
        let task = run_prog(indoc! { r#"
            int *create() {
                object clone = clone_object("/clone_target");
                object prototype = find_object("/clone_target");
                destruct(clone);
                return ({ sizeof(object_clones(0)), sizeof(object_clones(clone)),
                    sizeof(object_clones(prototype)), sizeof(object_clones(this_object())) });
            }
        "# })
        .await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[0, 0, 0, 0][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn changing_a_result_does_not_change_clone_membership() {
        let task = run_prog(indoc! { r#"
            int create() {
                object clone = clone_object("/clone_target");
                object *clones = object_clones(clone);
                clones[0] = 0;
                return object_clones(clone)[0] == clone;
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn committed_destruction_updates_the_list() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "/creator.c",
            indoc! { r#"
            void create() {
                clone_object("/clone_target");
                clone_object("/clone_target");
            }
        "# },
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/destructor.c",
            indoc! { r#"
            void create() { destruct(find_object("/clone_target#0")); }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/checker.c",
                indoc! { r#"
            int create() {
                object *clones = object_clones(find_object("/clone_target"));
                return sizeof(clones) == 1 && clones[0] == find_object("/clone_target#1");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_committed_destructed_argument_cannot_enumerate_surviving_siblings() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "/creator.c",
            indoc! { r#"
            object dead;
            void create() {
                dead = clone_object("/clone_target");
                clone_object("/clone_target");
            }
            int count() { return sizeof(object_clones(dead)); }
        "# },
        )
        .await
        .unwrap();
        let retained = vm
            .global_state
            .object_space
            .lookup("/clone_target#0")
            .unwrap();
        vm.initialize_process_from_code(
            "/destructor.c",
            indoc! { r#"
            void create() { destruct(find_object("/clone_target#0")); }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/checker.c",
                indoc! { r#"
            int create() { return "/creator"->count(); }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(0));
        drop(retained);
    }

    #[tokio::test]
    async fn reloaded_prototypes_have_separate_clone_groups() {
        let task = run_prog(indoc! { r#"
            int *create() {
                object old = clone_object("/clone_target");
                destruct(find_object("/clone_target"));
                object fresh = clone_object("/clone_target");
                object *old_group = object_clones(old);
                object *new_group = object_clones(find_object("/clone_target"));
                return ({ sizeof(old_group), old_group[0] == old,
                    sizeof(new_group), new_group[0] == fresh });
            }
        "# })
        .await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[1, 1, 1, 1][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn a_clone_is_listed_during_its_initializer() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/target.c", indoc! { r#"
            int listed;
            void create() { listed = member_array(this_object(), object_clones(this_object())) >= 0; }
            int was_listed() { return listed; }
        "# }).await.unwrap();
        let task = vm
            .initialize_process_from_code(
                "/creator.c",
                indoc! { r#"
            int create() { return clone_object("/target")->was_listed(); }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_failed_initializer_removes_the_clone_from_the_list() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
            void create() {
                if (sizeof(object_clones(this_object()))) { throw("failed clone"); }
            }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/creator.c",
                indoc! { r#"
            int *create() {
                mixed error = catch(clone_object("/target"));
                return ({ stringp(error), sizeof(object_clones(find_object("/target"))) });
            }
        "# },
            )
            .await
            .unwrap();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                assert_eq!(array, &[1, 0][..]);
            })
            .unwrap();
    }

    #[tokio::test]
    async fn gc_keeps_clone_groups_after_the_prototype_is_destructed() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "/creator.c",
            indoc! { r#"
            void create() {
                clone_object("/clone_target");
                destruct(find_object("/clone_target"));
            }
        "# },
        )
        .await
        .unwrap();
        vm.global_state.gc().await.unwrap().unwrap();
        let task = vm
            .initialize_process_from_code(
                "/checker.c",
                indoc! { r#"
            int create() {
                object clone = find_object("/clone_target#0");
                object *clones = object_clones(clone);
                return sizeof(clones) == 1 && clones[0] == clone;
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[test]
    fn concurrent_clone_insertions_merge_and_invalidate_an_empty_enumeration() {
        let mut committer = Committer::new();
        let space = ObjectSpace::default();
        let program = Arc::new(Program::new("/target.c"));
        let first = space.create_clone_process(program.clone());
        let second = space.create_clone_process(program.clone());
        let a = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let b = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let mut reader = Transaction::new(committer.snapshot_clone());
        assert!(reader.read_array(program.clones.id).is_none());
        txn_insert_process(&a, &space, &first);
        txn_insert_process(&b, &space, &second);
        committer.commit(a.with(|t| t.take_changeset())).unwrap();
        committer.commit(b.with(|t| t.take_changeset())).unwrap();
        assert!(committer.commit(reader.take_changeset()).is_err());
        let mut fresh = Transaction::new(committer.snapshot_clone());
        let members = fresh.read_array(program.clones.id).unwrap();
        assert_eq!(
            members.array.as_slice(),
            &[
                LpcRef::from(Arc::downgrade(&first)),
                LpcRef::from(Arc::downgrade(&second)),
            ]
        );
    }

    #[test]
    fn removal_preserves_an_old_snapshot_but_invalidates_its_enumeration() {
        let mut committer = Committer::new();
        let space = ObjectSpace::default();
        let program = Arc::new(Program::new("/target.c"));
        let clone = space.create_clone_process(program.clone());
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_insert_process(&writer, &space, &clone);
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        let mut reader = Transaction::new(committer.snapshot_clone());
        let remover = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        txn_undo_insert(&remover, &space, &clone);
        committer
            .commit(remover.with(|t| t.take_changeset()))
            .unwrap();
        assert_eq!(reader.read_array(program.clones.id).unwrap().len(), 1);
        assert!(committer.commit(reader.take_changeset()).is_err());
        let mut fresh = Transaction::new(committer.snapshot_clone());
        assert!(fresh.read_array(program.clones.id).unwrap().is_empty());
    }
}
