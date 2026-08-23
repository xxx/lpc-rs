use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process};

/// `move_object`, for moving objects between each other.
///
/// A pure transactional read-modify-write over the position cells: the move
/// is staged into this task's in-flight changeset and becomes visible to other
/// tasks only when the attempt commits. A conflicting concurrent move makes
/// one attempt re-run from its snapshot, so the room inventories and
/// environment pointers always converge to a consistent state.
pub async fn move_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);
    let destination = match arg_ref {
        LpcRef::Float(_)
        | LpcRef::Int(_)
        | LpcRef::Array(_)
        | LpcRef::Mapping(_)
        | LpcRef::Function(_) => {
            return Err(
                context.runtime_error(format!("move_object: invalid destination {}", arg_ref))
            );
        }
        LpcRef::String(_) => {
            let path = arg_ref.with_string(|string| context.in_game_path(string.to_str()))?;

            context.load_object(&path).await?
        }
        LpcRef::Object(proc) => {
            let Some(destination) = proc.upgrade() else {
                return Err(
                    context.runtime_error("new environment has been destructed. cannot move.")
                );
            };

            destination
        }
    };

    let this_object = &context.frame().process;

    // TODO: remove old commands

    Process::move_to(context.txn(), this_object, &destination);

    Ok(())

    // TODO: add new commands
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_multi_move() {
        let foo = indoc! { r#"
            void do_moves() {
                object bar = find_object("/bar");
                move_object(bar);
                move_object("baz");
                move_object("quux");
            }
        "# };

        let master = indoc! { r#"
            void create() {
                object foo = clone_object("/foo");
                foo->do_moves();
            }
        "# };

        let vm = Vm::new(test_config());

        let _foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let bar_proc = vm.create_process_from_code("/bar.c", "").await.unwrap();
        let baz_proc = vm.create_process_from_code("/baz.c", "").await.unwrap();
        let quux_proc = vm.create_process_from_code("/quux.c", "").await.unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let foo_clone = master_proc.context.object_space().lookup("/foo#0").unwrap();
        assert!(vm.global_state.is_initialized(&foo_clone));

        // The committed world is the source of truth: the clone's environment
        // pointer and the rooms' inventory cells.
        assert_eq!(
            vm.global_state.committed_environment(&foo_clone),
            Some(quux_proc.clone())
        );
        let quux_inventory = vm.global_state.committed_inventory(&quux_proc);
        assert!(
            quux_inventory
                .iter()
                .any(|item| item.as_ref() == foo_clone.as_ref())
        );
        assert!(
            !vm.global_state
                .committed_inventory(&bar_proc)
                .iter()
                .any(|item| item.as_ref() == foo_clone.as_ref())
        );
        assert!(
            !vm.global_state
                .committed_inventory(&baz_proc)
                .iter()
                .any(|item| item.as_ref() == foo_clone.as_ref())
        );
    }
}
