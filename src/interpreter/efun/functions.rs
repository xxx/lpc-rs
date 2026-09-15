use std::sync::Arc;

use lpc_rs_core::{INIT_GLOBALS, INIT_PROGRAM};
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{arg_or_this_object, efun_context::EfunContext},
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
};

/// List visible function names, optionally excluding inherited definitions.
pub async fn functions<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if !matches!(
        context.arg(0),
        LpcRef::Int(LpcInt(0)) | LpcRef::Object(_) | LpcRef::String(_)
    ) {
        return Err(context.runtime_error("functions: expected an object or string"));
    }
    let LpcRef::Int(local_only) = context.arg(1) else {
        return Err(context.runtime_error("functions: local_only must be an int"));
    };
    let local_only = local_only.0 != 0;
    let Some(target) = arg_or_this_object(context.arg(0), context)
        .await?
        .filter(|target| target.is_live(context.txn()))
    else {
        context.return_array([]);
        return Ok(());
    };

    let own = Arc::ptr_eq(&target, context.process());
    let paths = context.config().paths();
    let mut names: Vec<_> = target
        .program
        .unmangled_functions
        .values()
        .filter(|function| !matches!(function.name().as_ref(), INIT_GLOBALS | INIT_PROGRAM))
        .filter(|function| own || function.public())
        .filter(|function| {
            !local_only
                || paths.program_path(&function.prototype.filename) == *target.program.filename
        })
        .map(|function| function.name().as_ref())
        .collect();
    names.sort_unstable();
    context.return_array(names.into_iter().map(LpcRef::from));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{
            lib_holding, permissive_master, run_prog, strings_of, temp_lib_config, try_run_prog,
        },
    };

    #[tokio::test]
    async fn the_default_target_includes_own_hidden_functions_but_no_generated_names() {
        let names = strings_of(indoc! { r#"
            private void hidden() {}
            protected void guarded() {}
            void visible() {}
            string *create() {
                function closure = (: 42 :);
                return functions();
            }
        "# })
        .await;
        assert_eq!(names, ["create", "guarded", "hidden", "visible"]);
    }

    #[tokio::test]
    async fn inherited_names_are_unique_and_local_overrides_count_as_local() {
        let root = lib_holding(
            "functions-inheritance",
            &[
                ("base.c", "void ancestor() {} void overridden() {}"),
                ("left.c", "inherit \"/base\"; void left() {}"),
                ("right.c", "inherit \"/base\"; void right() {}"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
                    inherit "/left";
                    inherit "/right";
                    void overridden() {}
                    mixed *create() {
                        return ({ functions(this_object()), functions(this_object(), 1) });
                    }
                "# },
            )
            .await
            .unwrap();
        let lists = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |lists| {
                lists
                    .iter()
                    .map(|list| {
                        list.with_array(task.context.txn(), |names| {
                            names.iter().map(ToString::to_string).collect::<Vec<_>>()
                        })
                        .unwrap()
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(
            lists,
            [
                vec!["ancestor", "create", "left", "overridden", "right"],
                vec!["create", "overridden"],
            ]
        );
    }

    #[tokio::test]
    async fn inspecting_another_object_hides_private_and_protected_functions() {
        let root = lib_holding(
            "functions-visibility",
            &[(
                "base.c",
                "private void ancestor_hidden() {} protected void ancestor_guarded() {} void ancestor() {}",
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
                inherit "/base";
                private void hidden() {}
                protected void guarded() {}
                void visible() {}
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
                    string *create() {
                        object ob = find_object("/target");
                        return functions(ob) + functions(ob, 1);
                    }
                "# },
            )
            .await
            .unwrap();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |names| {
                assert_eq!(
                    names.iter().map(ToString::to_string).collect::<Vec<_>>(),
                    ["ancestor", "visible", "visible"]
                );
            })
            .unwrap();
    }

    #[tokio::test]
    async fn a_string_target_is_loaded_and_its_names_are_returned() {
        let root = lib_holding("functions-load", &[("target.c", "void visible() {}")]);
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
                    int create() {
                        int absent = !find_object("/target");
                        string *names = functions("/target");
                        return absent && objectp(find_object("/target"))
                            && sizeof(names) == 1 && names[0] == "visible";
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_string_targets_load_errors_propagate() {
        let root = lib_holding(
            "functions-load-denied",
            &[("target.c", "void visible() {}")],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let error = vm
            .initialize_process_from_code(
                "/inspector.c",
                "string *create() { return functions(\"/target\"); }",
            )
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("functions: permission denied"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn local_filtering_normalizes_host_paths_and_works_for_clones() {
        let root = lib_holding("functions-host-path", &[("base.c", "void ancestor() {}")]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            LpcPath::new_server(root.join("target.c")),
            "inherit \"/base\"; void local() {}",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
                    string *create() {
                        object ob = clone_object("/target");
                        return functions(find_object("/target"), 1) + functions(ob, 1);
                    }
                "# },
            )
            .await
            .unwrap();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |names| {
                assert_eq!(
                    names.iter().map(ToString::to_string).collect::<Vec<_>>(),
                    ["local", "local"]
                );
            })
            .unwrap();
    }

    #[tokio::test]
    async fn zero_names_the_caller_and_each_result_is_fresh() {
        let task = run_prog(indoc! { r#"
            int create() {
                string *names = functions(0, 1);
                names[0] = "changed";
                return sizeof(names) == 1 && functions(0)[0] == "create";
            }
        "# })
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn empty_and_destructed_objects_have_no_functions() {
        let task = run_prog(indoc! { r#"
            int create() {
                object empty = clone_object("/empty");
                object dead = clone_object("/clone_target");
                destruct(dead);
                return sizeof(functions(empty)) + sizeof(functions(dead));
            }
        "# })
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn invalid_argument_types_raise_errors() {
        for (call, message) in [
            ("functions(bad)", "functions: expected an object or string"),
            (
                "functions(this_object(), wrong_flag)",
                "functions: local_only must be an int",
            ),
        ] {
            let code =
                format!("void create() {{ mixed bad = 17; mixed wrong_flag = \"yes\"; {call}; }}");
            let error = try_run_prog(&code).await.unwrap_err();
            assert!(error.to_string().contains(message), "{error}");
        }
    }
}
