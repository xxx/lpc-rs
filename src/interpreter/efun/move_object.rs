use lpc_rs_core::{lpc_path::LpcPath, RegisterSize};
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process};

/// `move_object`, for moving objects between each other.
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
        LpcRef::String(string) => {
            let path = {
                let lock = string.read();

                LpcPath::new_in_game(&*lock, context.in_game_cwd(), &*context.config().lib_dir)
            };

            context.load_object(&path).await?
        }
        LpcRef::Object(proc) => {
            let Some(destination) = proc.upgrade() else {
                return Err(context.runtime_error("new environment has been destructed. cannot move."));
            };

            destination
        }
    };

    let this_object = &context.frame().process;

    // TODO: remove old commands

    Process::move_to(this_object, destination).await

    // TODO: add new commands
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{object_flags::ObjectFlags, vm::Vm},
        test_support::test_config,
        util::process_builder::{ProcessCreator, ProcessInitializer},
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

        let _foo_proc = vm.process_create_from_code("/foo.c", foo).await.unwrap();
        let bar_proc = vm.process_create_from_code("/bar.c", "").await.unwrap();
        let baz_proc = vm.process_create_from_code("/baz.c", "").await.unwrap();
        let quux_proc = vm.process_create_from_code("/quux.c", "").await.unwrap();

        let master_proc = vm
            .process_initialize_from_code("/master.c", master)
            .await
            .unwrap();

        let foo_clone = master_proc.context.object_space().lookup("/foo#0").unwrap();
        assert!(foo_clone.flags.test(ObjectFlags::Initialized));

        assert_eq!(
            foo_clone
                .position
                .environment
                .swap(None)
                .unwrap()
                .upgrade()
                .unwrap(),
            quux_proc
        );
        assert!(quux_proc
            .position
            .inventory
            .contains(foo_clone.position.environment_inventory_id()));
        assert!(!bar_proc
            .position
            .inventory
            .contains(foo_clone.position.environment_inventory_id()));
        assert!(!baz_proc
            .position
            .inventory
            .contains(foo_clone.position.environment_inventory_id()));
    }
}
