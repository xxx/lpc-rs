use std::{collections::HashSet, sync::Arc};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun, efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_array::LpcArray,
    process::Process,
};

/// `deep_inventory`, an efun for recursively returning the inventories of all objects contained by an object.
pub async fn deep_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.resolve_local_register(1 as RegisterSize);

    let Some(current_env) = efun::arg_or_this_object(arg_ref, context) else {
        let result = LpcArray::default().into_lpc_ref(context.memory());
        context.return_efun_result(result);
        return Ok(());
    };

    // [`Process`]' hashes are based solely on their filename, which never changes after creation.
    #[allow(clippy::mutable_key_type)]
    let mut collection = HashSet::with_capacity(10);
    recurse_deep_inventory(&current_env, &mut collection);

    let result = collection
        .into_iter()
        .map(|arc| Arc::downgrade(&arc).into_lpc_ref(context.memory()))
        .collect::<LpcArray>()
        .into_lpc_ref(context.memory());

    context.return_efun_result(result);

    Ok(())
}

#[allow(clippy::mutable_key_type)]
fn recurse_deep_inventory(env: &Process, collection: &mut HashSet<Arc<Process>>) {
    for item in env.position.inventory_iter() {
        if collection.insert(item.clone()) {
            recurse_deep_inventory(&item, collection);
        }
    }
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
    async fn test_deep_inventory() {
        let ob = indoc! { r#"
            object *inv;
            
            void update_inv() {
                dump("updating");
                inv = deep_inventory();
            }

            void move_to_foo() {
                move_object("/deep_inv_foo");
                update_inv();
            }
        "# };

        let foo = indoc! { r#"
            void create() {
                move_object("/deep_inv_ob");
                // "/deep_inv_ob"->update_inv();
            }
        "# };

        let bar = indoc! { r#"
            void create() {
                move_object("/deep_inv_foo");
                // "/deep_inv_ob"->update_inv();
            }
        "# };

        let baz = indoc! { r#"
            void create() {
                move_object("/deep_inv_bar");
                // "/deep_inv_ob"->update_inv();
            }
        "# };

        let move_ob = indoc! { r#"
            void create() {
                "/deep_inv_ob"->move_to_foo(); // create a loop to ensure we handle them
            }
        "# };

        let vm = Vm::new(test_config());

        let ob_proc = vm
            .initialize_process_from_code("/deep_inv_ob.c", ob)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));

        let _foo_proc = vm
            .initialize_process_from_code("/deep_inv_foo.c", foo)
            .await
            .unwrap();
        let _bar_proc = vm
            .initialize_process_from_code("/deep_inv_bar.c", bar)
            .await
            .unwrap();
        let _baz_proc = vm
            .initialize_process_from_code("/deep_inv_baz.c", baz)
            .await
            .unwrap();

        let _move_proc = vm
            .initialize_process_from_code("/deep_inv_move.c", move_ob)
            .await
            .unwrap();

        let ob_proc_arc = ob_proc.context.process.clone();
        let LpcRef::Array(array) = ob_proc_arc.globals.read().first().unwrap().clone() else {
            panic!("Expected array");
        };

        let globals = array
            .read()
            .iter()
            .map(|w| w.to_string())
            .sorted()
            .collect_vec();

        assert_eq!(
            globals,
            &[
                "/deep_inv_bar",
                "/deep_inv_baz",
                "/deep_inv_foo",
                "/deep_inv_ob"
            ]
        );
    }
}
