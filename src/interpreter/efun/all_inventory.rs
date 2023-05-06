use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun, efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray,
};

/// `all_inventory`, an efun for returning an object's inventory.
pub async fn all_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let Some(current_env) = efun::arg_or_this_object(arg_ref, context) else {
        let result = LpcArray::default().into_lpc_ref(context.memory());
        context.return_efun_result(result);
        return Ok(());
    };

    let result = current_env.position.weak_inventory_iter().map(|item| {
        let weak = item.clone();
        weak.into_lpc_ref(context.memory())
    }).collect::<LpcArray>().into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
        util::process_builder::ProcessInitializer,
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
            .process_initialize_from_code("/room.c", room)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));

        let _foo_proc = vm
            .process_initialize_from_code("/all_inv_foo.c", ob)
            .await
            .unwrap();
        let _bar_proc = vm
            .process_initialize_from_code("/all_inv_bar.c", ob)
            .await
            .unwrap();
        let _baz_proc = vm
            .process_initialize_from_code("/all_inv_baz.c", ob)
            .await
            .unwrap();

        let room_proc_arc = room_proc.context.process.clone();
        let LpcRef::Array(array) = room_proc_arc.globals.read().first().unwrap().clone() else {
            panic!("Expected array");
        };

        let globals = array
            .read()
            .iter()
            .map(|w| w.to_string())
            .sorted()
            .collect_vec();

        assert_eq!(globals, &["/all_inv_bar", "/all_inv_baz", "/all_inv_foo"]);
    }
}
