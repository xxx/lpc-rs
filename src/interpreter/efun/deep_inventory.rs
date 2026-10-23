use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, process::Process};

/// `deep_inventory`, an efun for recursively returning the inventories of
/// all objects contained by an object, depth-first in arrival order, each
/// object once and never `ob` itself.
pub async fn deep_inventory<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    efun::return_objects_of(context, |txn, object| {
        Process::deep_inventory_of(txn, &object).into_iter().skip(1)
    })
    .await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use itertools::Itertools;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_config_builder,
        test_support::{permissive_master, test_config},
    };

    #[tokio::test]
    async fn test_deep_inventory() {
        let ob = indoc! { r#"
            object *inv;
            
            void update_inv() {
                dump("updating");
                inv = deep_inventory();
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
                "/deep_inv_ob"->update_inv();
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

        assert_eq!(
            globals,
            &["/deep_inv_bar", "/deep_inv_baz", "/deep_inv_foo"]
        );
    }

    #[tokio::test]
    async fn destruction_preserves_depth_first_order_of_surviving_members() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code("/room.c", "")
                .await
                .unwrap();
            let mut members = Vec::new();
            for (name, parent) in [
                ("box", "room"),
                ("dead_leaf", "box"),
                ("leaf", "box"),
                ("dead_sibling", "room"),
                ("last", "room"),
            ] {
                members.push(
                    vm.initialize_process_from_code(
                        format!("/{name}.c"),
                        format!("void create() {{ move_object(\"/{parent}\"); }}"),
                    )
                    .await
                    .unwrap(),
                );
            }
            let probe = vm
                .initialize_process_from_code(
                    "/probe.c",
                    indoc! { r#"
                        int create() {
                            destruct(find_object("/dead_leaf"));
                            destruct(find_object("/dead_sibling"));
                            return implode(map(deep_inventory("/room"), file_name), ",")
                                == "/box,/leaf,/last";
                        }
                    "# },
                )
                .await
                .unwrap();
            assert_eq!(probe.result().unwrap(), LpcRef::from(1), "{mode:?}");
        }
    }

    #[tokio::test]
    async fn destroyed_containers_are_not_traversed() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code("/room.c", "")
                .await
                .unwrap();
            let _box = vm
                .initialize_process_from_code("/box.c", "void create() { move_object(\"/room\"); }")
                .await
                .unwrap();
            let _leaf = vm
                .initialize_process_from_code("/leaf.c", "void create() { move_object(\"/box\"); }")
                .await
                .unwrap();
            let probe = vm
                .initialize_process_from_code(
                    "/probe.c",
                    indoc! { r#"
                        int create() {
                            destruct(find_object("/box"));
                            return sizeof(deep_inventory("/room")) == 0
                                && objectp(find_object("/leaf"));
                        }
                    "# },
                )
                .await
                .unwrap();
            assert_eq!(probe.result().unwrap(), LpcRef::from(1), "{mode:?}");
        }
    }
}
