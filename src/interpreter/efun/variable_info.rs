use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_VARIABLE_INFO,
    apply::valid_apply,
    efun::{efun_context::EfunContext, inspection_target},
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
};

pub async fn variable_info<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(target) = inspection_target(context, "variable_info")? else {
        context.return_array(Vec::new());
        return Ok(());
    };
    if !Arc::ptr_eq(context.process(), &target) {
        let args = [
            LpcRef::from(Arc::downgrade(context.process())),
            LpcRef::from(Arc::downgrade(&target)),
            context.calling_program(),
        ];
        if !valid_apply(
            context.task_context(),
            Some(context.chain()),
            VALID_VARIABLE_INFO,
            &args,
        )
        .await?
        {
            return Err(context.runtime_error("variable_info: permission denied"));
        }
    }
    if !target.is_live(context.txn()) {
        context.return_array(Vec::new());
        return Ok(());
    }

    let paths = context.config().paths();
    let image = target.image(context.txn());
    let entries = image.program.global_variable_info.iter().map(|variable| {
        let value = context
            .txn()
            .with(|t| t.read(image.var_id(variable.slot)))
            .unwrap_or(NULL);
        let fields = [
            ("name", variable.name.as_str().into()),
            (
                "program",
                paths.source_name(&variable.filename).to_string().into(),
            ),
            ("type", variable.type_.to_string().into()),
            ("flags", variable.flags.to_string().into()),
            ("value", value),
        ];
        let mapping = LpcMapping::new(
            fields
                .into_iter()
                .map(|(key, value)| (key.into(), value))
                .collect(),
        );
        LpcRef::Mapping(context.txn().with(|t| t.mint_mapping(mapping)))
    });
    let result = context.mint_array(entries);
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{lib_holding, run_prog, temp_lib_config, test_config, try_run_prog},
    };

    #[tokio::test]
    async fn hidden_declarations_survive_relocation_and_diamond_inheritance() {
        let root = lib_holding(
            "variable-info",
            &[
                ("padding.c", "int pad = 5;"),
                (
                    "base.c",
                    "private int n = 7; protected static nomask int hidden = 8; string tag = \"base\";",
                ),
                (
                    "left.c",
                    "inherit \"/padding\"; inherit \"/base\"; private int n = 11;",
                ),
                ("right.c", "inherit \"/base\"; int n = 12;"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/left";
            inherit "/right";
            int n = 13;
            mapping *create() { n = 14; return variable_info(this_object()); }
        "# },
            )
            .await
            .unwrap();
        let rows = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |items| {
                items
                    .iter()
                    .map(|item| {
                        item.with_mapping(task.context.txn(), |entry| {
                            ["name", "program", "type", "flags", "value"]
                                .map(|key| entry.get(&LpcRef::from(key)).unwrap().to_string())
                        })
                        .unwrap()
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(
            rows,
            [
                ["pad", "/padding.c", "int", "public", "5"],
                ["n", "/base.c", "int", "private", "7"],
                ["hidden", "/base.c", "int", "protected static nomask", "8"],
                ["tag", "/base.c", "string", "public", "base"],
                ["n", "/left.c", "int", "private", "11"],
                ["n", "/right.c", "int", "public", "12"],
                ["n", "/child.c", "int", "public", "14"],
            ]
        );
    }

    #[tokio::test]
    async fn entry_fields_are_independent_but_values_keep_lpc_sharing() {
        let task = run_prog(indoc! { r#"
            int *a = ({ 1 });
            mapping bag = ([ "k": 1 ]);
            int create() {
                mapping *info = variable_info(this_object());
                info[0]["value"][0] = 2;
                info[1]["value"]["k"] = 3;
                info[0]["value"] = 0;
                info[1]["name"] = "changed";
                return a[0] + bag["k"];
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(5));
    }

    #[tokio::test]
    async fn inspecting_other_objects_requires_the_master_apply() {
        for master in [
            None,
            Some(""),
            Some("int valid_variable_info() { return 0; }"),
        ] {
            let vm = Vm::new(test_config());
            if let Some(code) = master {
                vm.create_process_from_code("/secure/master.c", code)
                    .await
                    .unwrap();
            }
            vm.initialize_process_from_code("/target.c", "private int secret = 42;")
                .await
                .unwrap();
            let error = vm
                .initialize_process_from_code(
                    "/inspector.c",
                    indoc! { r#"
                void create() { variable_info(find_object("/target")); }
            "# },
                )
                .await
                .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("variable_info: permission denied")
            );
        }
    }

    #[tokio::test]
    async fn authorization_sees_the_caller_target_and_defining_program() {
        let root = lib_holding(
            "variable-info-access",
            &[(
                "parent.c",
                indoc! { r#"
            mapping *inspect() { return variable_info(find_object("/target")); }
        "# },
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            indoc! { r#"
            int valid_variable_info(object caller, object target, string program) {
                return caller == previous_object() && file_name(caller) == "/inspector"
                    && file_name(target) == "/target" && program == "/parent.c";
            }
        "# },
        )
        .await
        .unwrap();
        vm.initialize_process_from_code("/target.c", "private int secret = 42;")
            .await
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            inherit "/parent";
            int create() { return inspect()[0]["value"]; }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(42));
    }

    #[tokio::test]
    async fn authorization_errors_propagate_and_destroyed_targets_are_rechecked() {
        for (body, expected) in [
            ("throw(\"inspection denied by policy\");", None),
            ("destruct(target); return 1;", Some(0)),
        ] {
            let vm = Vm::new(test_config());
            vm.create_process_from_code(
                "/secure/master.c",
                format!(
                "int valid_variable_info(object caller, object target, string program) {{ {body} }}
                 int valid_destruct(object caller, object target, string program) {{ return 1; }}"
            ),
            )
            .await
            .unwrap();
            vm.initialize_process_from_code("/target.c", "private int secret = 42;")
                .await
                .unwrap();
            let result = vm
                .initialize_process_from_code(
                    "/inspector.c",
                    indoc! { r#"
                int create() { return sizeof(variable_info(find_object("/target"))); }
            "# },
                )
                .await;
            match expected {
                Some(value) => assert_eq!(result.unwrap().result().unwrap(), LpcRef::from(value)),
                None => assert!(
                    result
                        .unwrap_err()
                        .to_string()
                        .contains("inspection denied by policy")
                ),
            }
        }
    }

    #[tokio::test]
    async fn null_destroyed_and_empty_objects_return_empty_arrays() {
        let task = run_prog(indoc! { r#"
            int create() {
                int count = sizeof(variable_info(this_object()));
                object ob = clone_object("/empty");
                destruct(ob);
                return count + sizeof(variable_info(ob)) + sizeof(variable_info(0));
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(0));
        let error = try_run_prog("void create() { mixed bad = 17; variable_info(bad); }")
            .await
            .unwrap_err();
        assert!(error.to_string().contains("expected an object"));
    }
}
