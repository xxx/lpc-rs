use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, inspection_target},
    lpc_mapping::LpcMapping,
    memory::object_memory,
};

/// Estimate one object's current program, instance storage and reachable LPC data.
pub async fn object_memory_info<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(target) = inspection_target(context, "object_memory_info")? else {
        context.return_mapping(LpcMapping::default());
        return Ok(());
    };
    let memory = object_memory(&target, context.txn()).await?;
    context.return_mapping(LpcMapping::new(
        memory
            .fields()?
            .into_iter()
            .map(|(key, value)| (key.into(), value.into()))
            .collect(),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{lib_holding, permissive_master, run_prog, temp_lib_config},
    };

    #[tokio::test]
    async fn returns_fresh_estimates_with_an_additive_total_and_works_through_a_pointer() {
        let task = run_prog(
            r#"
            int create() {
                mapping a = object_memory_info(this_object());
                if (sizeof(a) != 4 || a["object_bytes"] <= 0 || a["program_bytes"] <= 0
                    || a["total_bytes"] != a["object_bytes"] + a["program_bytes"] + a["data_bytes"])
                    return 0;
                int total = a["total_bytes"];
                a["total_bytes"] = -1;
                mapping b = evaluate(object_memory_info, this_object());
                return b["total_bytes"] == total;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn reads_staged_global_assignments_and_container_edits() {
        let task = run_prog(
            r#"
            mixed data;
            int create() {
                mapping before = object_memory_info(this_object());
                data = "é" * 2048;
                mapping grown = object_memory_info(this_object());
                if (grown["data_bytes"] < before["data_bytes"] + 4096) return 0;
                data = allocate(8);
                mapping empty = object_memory_info(this_object());
                data[3] = "x" * 8192;
                mapping after = object_memory_info(this_object());
                return after["data_bytes"] >= empty["data_bytes"] + 8192
                    && before["object_bytes"] == after["object_bytes"]
                    && before["program_bytes"] == after["program_bytes"];
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn shares_array_payloads_across_globals_and_terminates_on_a_cycle() {
        let task = run_prog(
            r#"
            mixed a, b;
            int create() {
                a = allocate(8);
                int first = object_memory_info(this_object())["data_bytes"];
                b = a;
                int shared = object_memory_info(this_object())["data_bytes"];
                a[0] = a;
                int cyclic = object_memory_info(this_object())["data_bytes"];
                return first == shared && shared == cyclic;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn measures_mapping_keys_and_values_and_shared_byte_values() {
        let task = run_prog(
            r#"
            mapping data;
            bytes first, second;
            int create() {
                data = ([ "κ" * 2048 : "v" * 4096 ]);
                int map = object_memory_info(this_object())["data_bytes"];
                if (map < 8192) return 0;
                first = to_bytes("z" * 8192, "UTF-8");
                int one = object_memory_info(this_object())["data_bytes"];
                second = first;
                return one >= map + 8192 && object_memory_info(this_object())["data_bytes"] == one;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn counts_class_storage_and_handles_self_references() {
        let task = run_prog(
            r#"
            class box { mixed value; }
            class box a, b;
            int create() {
                a = new(class box, value: "x" * 4096);
                int first = object_memory_info(this_object())["data_bytes"];
                b = a;
                int shared = object_memory_info(this_object())["data_bytes"];
                a.value = a;
                int cyclic = object_memory_info(this_object())["data_bytes"];
                return first >= 4096 && first == shared && cyclic < first;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn follows_closure_captures_and_partial_arguments() {
        let task = run_prog(
            r#"
            function saved;
            int size(string text) { return sizeof(text); }
            function capture() {
                string text = "c" * 4096;
                return (: sizeof(text) :);
            }
            int create() {
                saved = capture();
                int captured = object_memory_info(this_object())["data_bytes"];
                saved = nil;
                int cleared = object_memory_info(this_object())["data_bytes"];
                saved = &size("p" * 8192);
                int partial = object_memory_info(this_object())["data_bytes"];
                return captured >= cleared + 4096 && partial >= cleared + 8192;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn ordinary_object_references_do_not_include_the_targets_data() {
        let root = lib_holding(
            "object-memory-reference",
            &[(
                "other.c",
                r#"
            string text;
            void grow() { text = "x" * 4096; }
        "#,
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/main.c",
                r#"
            object other;
            int create() {
                other = clone_object("/other");
                int own = object_memory_info(this_object())["data_bytes"];
                int target = object_memory_info(other)["data_bytes"];
                other->grow();
                return own == object_memory_info(this_object())["data_bytes"]
                    && object_memory_info(other)["data_bytes"] >= target + 4096;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn clones_share_program_estimates_and_keep_independent_data() {
        let root = lib_holding(
            "object-memory-clones",
            &[(
                "other.c",
                r#"
            string text;
            void grow() { text = "x" * 8192; }
        "#,
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/main.c",
                r#"
            int create() {
                object a = clone_object("/other"), b = clone_object("/other");
                a->grow();
                mapping one = object_memory_info(a), two = object_memory_info(b);
                return one["program_bytes"] == two["program_bytes"]
                    && one["object_bytes"] == two["object_bytes"]
                    && one["data_bytes"] >= two["data_bytes"] + 8192;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn null_and_destroyed_targets_return_empty_mappings_and_invalid_values_fail() {
        let root = lib_holding("object-memory-targets", &[("other.c", "void create() {}")]);
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/main.c",
                r#"
            int create() {
                object target = clone_object("/other");
                destruct(target);
                mixed bad = "not an object";
                return sizeof(object_memory_info(nil)) == 0
                    && sizeof(object_memory_info(target)) == 0
                    && stringp(catch(object_memory_info(bad)));
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
