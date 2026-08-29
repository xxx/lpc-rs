use lpc_rs_errors::Result;

use crate::{
    command::scope,
    interpreter::{efun, efun::efun_context::EfunContext},
};

/// `deep_inventory`, an efun for recursively returning the inventories of
/// all objects contained by an object, depth-first in arrival order.
pub async fn deep_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    efun::return_objects_of(context, |txn, object| {
        scope::deep(txn, &object).into_iter().skip(1)
    })
    .await
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
        let g0 = vm.global_state.committed_global(&ob_proc_arc, 0u16);
        let crate::interpreter::lpc_ref::LpcRef::Array(cell) = g0 else {
            panic!("global holds an array cell, actually {g0:?}");
        };
        let globals = vm
            .global_state
            .committed_array(cell.id)
            .expect("array payload committed")
            .iter()
            .map(|w| w.to_string())
            .sorted()
            .collect_vec();

        // The loop moves /deep_inv_ob into /deep_inv_foo, so /deep_inv_ob is
        // its own indirect member; `deep` seeds the root as already seen, so
        // the cycle back to it is dropped rather than re-added.
        assert_eq!(
            globals,
            &["/deep_inv_bar", "/deep_inv_baz", "/deep_inv_foo"]
        );
    }
}
