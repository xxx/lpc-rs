use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::presence::{after_move, before_move},
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process},
};

/// `move_object`, for moving objects between each other.
///
/// A pure transactional read-modify-write over the position cells: the move
/// is staged into this task's in-flight changeset and becomes visible to other
/// tasks only when the attempt commits. A conflicting concurrent move makes
/// one attempt re-run from its snapshot, so the room inventories and
/// environment pointers always converge to a consistent state. It also fires
/// `init()` on the mover and its new surroundings, and expires the rules the
/// move leaves out of scope.
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
            let path = arg_ref.with_string(|s| s.to_string())?;

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

    let this_object = context.frame().process.clone();
    let ctx = context.task_context();
    let already_there = Process::environment_of(ctx.txn(), &this_object)
        .is_some_and(|env| Arc::ptr_eq(&env, &destination));
    if already_there {
        return Ok(());
    }

    if let Err(e) = Process::check_move(ctx.txn(), &this_object, &destination) {
        return Err(context.runtime_error(format!("move_object: {this_object} {e}")));
    }
    before_move(ctx.txn(), &this_object, &destination);
    Process::move_to(ctx.txn(), &this_object, &destination)
        .map_err(|e| context.runtime_error(format!("move_object: {this_object} {e}")))?;
    after_move(ctx, &this_object, &destination).await?;

    Ok(())
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

    #[tokio::test]
    async fn moving_into_itself_is_refused() {
        let box_ = indoc! { r#"
            void create() { move_object(this_object()); }
        "# };
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/box.c", box_)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("move_object: /box would contain itself"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn moving_into_own_contents_is_refused() {
        let bag = indoc! { r#"
            void create() { move_object("/box"); }
        "# };
        let box_ = indoc! { r#"
            void try_it() { move_object(find_object("/bag")); }
        "# };
        let master = indoc! { r#"
            void create() { "/box"->try_it(); }
        "# };
        let vm = Vm::new(test_config());
        let box_proc = vm.create_process_from_code("/box.c", box_).await.unwrap();
        let bag_proc = vm
            .initialize_process_from_code("/bag.c", bag)
            .await
            .unwrap();
        let err = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("move_object: /box would contain itself"),
            "{err}"
        );
        assert_eq!(vm.global_state.committed_environment(&box_proc), None);
        assert_eq!(
            vm.global_state
                .committed_environment(&bag_proc.context.process),
            Some(box_proc.clone())
        );
    }

    /// A caught refusal commits its task, so a hook fired before the check
    /// would cost the stationary living the mover's verb.
    #[tokio::test]
    async fn a_refused_move_fires_no_hook() {
        let room = "void create() {}";
        let stationary = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let traveller = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
            void init() { add_action("do_wave", "wave"); }
            int do_wave(string arg) { return 1; }
            void try_it() {
                if (!catch(move_object(find_object("/bag")))) {
                    throw("not refused");
                }
            }
        "# };
        let bag = indoc! { r#"
            void create() { move_object("/traveller"); }
        "# };
        let master = indoc! { r#"
            void create() { "/traveller"->try_it(); }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room.c", room).await.unwrap();
        let stationary_proc = vm
            .initialize_process_from_code("/stationary.c", stationary)
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code("/traveller.c", traveller)
            .await
            .unwrap();
        vm.initialize_process_from_code("/bag.c", bag)
            .await
            .unwrap();
        vm.initialize_process_from_code("/master.c", master)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));
        let verbs: Vec<String> = vm
            .global_state
            .committed_rules(&stationary_proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        assert_eq!(verbs, vec!["wave"]);
    }
}
