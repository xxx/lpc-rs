use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, process::Process};

/// `all_inventory`, an efun for returning an object's inventory.
pub async fn all_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    efun::return_objects_of(context, |txn, object| Process::inventory_of(txn, &object));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
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
}
