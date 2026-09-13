use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, inspection_target},
    lpc_ref::LpcRef,
};

pub fn inherit_list<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let target = inspection_target(context, "inherit_list")?;
    let paths = context.config().paths();
    let parents = target.iter().flat_map(|target| {
        target
            .program
            .direct_inherits
            .iter()
            .map(|path| LpcRef::from(paths.source_name(path).to_string()))
    });
    let result = context.mint_array(parents);
    context.return_efun_result(result);
    Ok(())
}

pub fn deep_inherit_list<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let target = inspection_target(context, "deep_inherit_list")?;
    let paths = context.config().paths();
    let ancestors = target.iter().flat_map(|target| {
        target
            .program
            .layout
            .iter()
            .filter(|region| region.filename != target.program.filename)
            .map(|region| LpcRef::from(paths.source_name(&region.filename).to_string()))
    });
    let result = context.mint_array(ancestors);
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{lib_holding, run_prog, temp_lib_config, try_run_prog},
    };

    #[tokio::test]
    async fn direct_parents_and_shared_ancestors_include_empty_programs() {
        let root = lib_holding(
            "inherit-info",
            &[
                ("base.c", ""),
                ("left.c", "inherit \"/base\";"),
                ("right.c", "inherit \"/base\";"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
                inherit "/left";
                inherit "/right";
                string *create() {
                    return inherit_list(this_object()) + deep_inherit_list(this_object());
                }
            "# },
            )
            .await
            .unwrap();
        let result = task.result().unwrap();
        result
            .with_array(task.context.txn(), |items| {
                let names: Vec<_> = items.iter().map(ToString::to_string).collect();
                assert_eq!(
                    names,
                    ["/left.c", "/right.c", "/base.c", "/left.c", "/right.c"]
                );
            })
            .unwrap();
    }

    #[tokio::test]
    async fn automatic_inheritance_and_clones_use_compiled_metadata() {
        let root = lib_holding("auto-inherit-info", &[("auto.c", "")]);
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .auto_inherit_file("/auto.c")
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.create_process_from_code("/target.c", "").await.unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            int create() {
                object ob = clone_object("/target");
                string *parents = inherit_list(ob);
                string *ancestors = deep_inherit_list(ob);
                parents[0] = "changed";
                return sizeof(parents) == 1 && sizeof(ancestors) == 1
                    && ancestors[0] == "/auto.c" && inherit_list(ob)[0] == "/auto.c";
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(1));
    }

    #[tokio::test]
    async fn null_destroyed_and_parentless_objects_have_empty_lists() {
        let task = run_prog(indoc! { r#"
            int create() {
                object ob = clone_object("/empty");
                int n = sizeof(inherit_list(ob)) + sizeof(deep_inherit_list(ob));
                destruct(ob);
                return n + sizeof(inherit_list(ob)) + sizeof(deep_inherit_list(ob))
                    + sizeof(inherit_list(0)) + sizeof(deep_inherit_list(0));
            }
        "# })
        .await;
        assert_eq!(task.result().unwrap(), LpcRef::from(0));
        for name in ["inherit_list", "deep_inherit_list"] {
            let error = try_run_prog(&format!("void create() {{ mixed bad = 17; {name}(bad); }}"))
                .await
                .unwrap_err();
            assert!(error.to_string().contains("expected an object"));
        }
    }
}
