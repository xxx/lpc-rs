use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{arg_or_this_object, efun_context::EfunContext},
    lpc_ref::{LpcRef, NULL},
};

/// `function_exists(name [, ob])`: the file defining `name` in `ob` (the
/// caller when absent), as `file_name` spells it; 0 when there is none.
/// Another object's private and protected functions are hidden.
/// Qualified names use the caller's source scope for self, or the target's root scope.
pub async fn function_exists<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "function_exists: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let Some(target) = arg_or_this_object(context.arg(1), context).await? else {
        context.return_efun_result(NULL);
        return Ok(());
    };
    let own = Arc::ptr_eq(&target, context.process());
    let program = target.program(context.txn());
    let function = match name.split_once("::") {
        Some(_) if own => context
            .lookup_inherited_function(name)
            .map(|(_, function)| function),
        Some((namespace, name)) => program
            .lookup_inherited_function(&program.filename, namespace, name)
            .filter(|function| !function.prototype.flags.private())
            .cloned(),
        None => program.lookup_function(name).cloned(),
    };
    let result = function
        .filter(|function| own || function.prototype.flags.public())
        .map_or(NULL, |function| {
            let file = function.prototype.filename.with_extension("");
            LpcRef::from(context.config().paths().source_name(&file).to_string())
        });
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{lib_holding, permissive_master, run_prog, temp_lib_config, test_config},
    };

    async fn result_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    #[tokio::test]
    async fn function_exists_names_the_file_defining_the_function() {
        let r = result_of(r#"string create() { return function_exists("create"); }"#).await;
        assert_eq!(r.as_str(), Some("/my_file"));
    }

    #[tokio::test]
    async fn an_inherited_function_names_the_parent() {
        let code = r#"
            inherit "/grandparent";
            string create() { return function_exists("grandparent_method"); }
        "#;
        assert_eq!(result_of(code).await.as_str(), Some("/grandparent"));
    }

    #[tokio::test]
    async fn an_absent_function_is_zero() {
        let r = result_of(r#"mixed create() { return function_exists("nope"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn an_efun_is_not_a_function_of_the_object() {
        let r = result_of(r#"mixed create() { return function_exists("write"); }"#).await;
        assert_eq!(r, LpcRef::from(0));
    }

    #[tokio::test]
    async fn the_object_argument_is_searched_instead() {
        let code =
            r#"mixed create() { return function_exists("grandparent_method", "/grandparent"); }"#;
        assert_eq!(result_of(code).await.as_str(), Some("/grandparent"));
    }

    #[tokio::test]
    async fn a_private_function_of_another_object_is_hidden() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/hider.c", "private void hidden() {}")
            .await
            .unwrap();
        let code = r#"mixed create() { return function_exists("hidden", find_object("/hider")); }"#;
        let task = vm.initialize_process_from_code("/t.c", code).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn an_objects_own_private_function_is_found() {
        let code = r#"
            private void hidden() {}
            string create() { return function_exists("hidden"); }
        "#;
        assert_eq!(result_of(code).await.as_str(), Some("/my_file"));
    }

    #[tokio::test]
    async fn a_destructed_object_has_no_functions() {
        let code = r#"
            mixed create() {
                object o = clone_object("/clone_target");
                destruct(o);
                return function_exists("create", o);
            }
        "#;
        assert_eq!(result_of(code).await, LpcRef::from(0));
    }

    #[tokio::test]
    async fn qualified_lookup_matches_calls_through_a_diamond_despite_a_local_override() {
        let root = lib_holding(
            "qualified-function-diamond",
            &[
                ("base.c", "string foo() { return \"/base\"; }"),
                (
                    "left.c",
                    "inherit \"/base\"; string foo() { return \"/left\"; }",
                ),
                ("right.c", "inherit \"/base\";"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/left" left;
            inherit "/right" right;
            string foo() { return "/child"; }
            int create() {
                string name = "::" + "foo";
                return function_exists("foo") == "/child"
                    && function_exists(name) == "/base"
                    && function_exists(name) == ::foo()
                    && function_exists("left::foo") == left::foo()
                    && function_exists("right::foo") == right::foo();
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn inherited_code_keeps_its_parents_and_aliases() {
        let root = lib_holding(
            "qualified-function-scope",
            &[
                ("base.c", "void foo() {}"),
                (
                    "middle.c",
                    indoc! { r#"
                    inherit "/base" parent;
                    void foo() {}
                    string probe() {
                        return implode(({
                            function_exists("::foo"),
                            function_exists("parent::foo", this_object()),
                            function_exists("::foo", 0),
                            function_exists("foo")
                        }), ",");
                    }
                "# },
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/middle" parent;
            void foo() {}
            string create() { return probe(); }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(
            task.result().unwrap().as_str(),
            Some("/base,/base,/base,/child")
        );
    }

    #[tokio::test]
    async fn parentless_inherited_code_does_not_find_itself_or_a_childs_alias() {
        let root = lib_holding(
            "qualified-function-parentless",
            &[(
                "base.c",
                indoc! { r#"
            void foo() {}
            int probe() {
                return !function_exists("::foo") && !function_exists("parent::foo");
            }
        "# },
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/base" parent;
            int create() { return probe(); }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn qualified_lookup_hides_private_parents_without_falling_back() {
        let root = lib_holding(
            "qualified-function-visibility",
            &[
                ("left.c", "void foo() {}"),
                (
                    "right.c",
                    "private void foo() {} protected void guarded() {}",
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/left" left;
            inherit "/right" right;
            int create() {
                return !function_exists("::foo") && !function_exists("right::foo")
                    && function_exists("left::foo") == "/left"
                    && function_exists("::guarded") == "/right"
                    && function_exists("right::guarded") == "/right";
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn another_object_uses_its_root_scope_and_exposes_only_public_parents() {
        let root = lib_holding(
            "qualified-function-external",
            &[
                (
                    "base.c",
                    "void foo() {} private void hidden() {} protected void guarded() {}",
                ),
                ("other.c", "void foo() {}"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
            inherit "/base" parent;
            void foo() {}
        "# },
        )
        .await
        .unwrap();
        std::fs::remove_file(root.join("base.c")).unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            inherit "/other" parent;
            int create() {
                object ob = find_object("/target");
                return function_exists("foo", ob) == "/target"
                    && function_exists("::foo", ob) == "/base"
                    && function_exists("parent::foo", ob) == "/base"
                    && !function_exists("::hidden", ob)
                    && !function_exists("parent::guarded", ob)
                    && !find_object("/base");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_qualified_string_target_loads_only_the_target_object() {
        let root = lib_holding(
            "qualified-function-load",
            &[
                ("base.c", "void foo() {}"),
                ("target.c", "inherit \"/base\" parent; void foo() {}"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            int create() {
                int absent = !find_object("/target");
                return absent && function_exists("parent::foo", "/target") == "/base"
                    && function_exists("::foo", "/target") == "/base"
                    && objectp(find_object("/target")) && !find_object("/base");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn qualified_lookup_uses_auto_inherits_in_host_loaded_programs_and_clones() {
        let root = lib_holding("qualified-function-clones", &[("auto.c", "void foo() {}")]);
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .auto_inherit_file("/auto.c")
            .build()
            .unwrap();
        let vm = Vm::new(config);
        permissive_master(&vm.global_state.object_space).await;
        vm.create_process_from_code(
            LpcPath::new_server(root.join("target.c")),
            indoc! { r#"
            void foo() {}
            mixed probe() { return function_exists("::foo"); }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            int create() {
                object ob = clone_object("/target");
                int found = ob->probe() == "/auto" && function_exists("::foo", ob) == "/auto";
                destruct(ob);
                return found && !function_exists("::foo", ob);
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn efun_pointers_and_closures_keep_the_scope_that_created_them() {
        let root = lib_holding(
            "qualified-function-pointers",
            &[
                ("base.c", "void foo() {}"),
                (
                    "middle.c",
                    indoc! { r#"
                inherit "/base" parent;
                void foo() {}
                function efun_probe() { return &function_exists("parent::foo"); }
                function closure_probe() { return (: function_exists("::foo") :); }
            "# },
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code("/target.c", "inherit \"/middle\" parent;")
            .await
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            string create() {
                object ob = find_object("/target");
                function direct = ob->efun_probe();
                function closure = ob->closure_probe();
                return direct() + "," + closure();
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap().as_str(), Some("/base,/base"));
    }

    #[tokio::test]
    async fn global_initializers_use_their_defining_programs_scope() {
        let root = lib_holding(
            "qualified-function-initializers",
            &[
                ("base.c", "void foo() {}"),
                (
                    "middle.c",
                    indoc! { r#"
                inherit "/base";
                void foo() {}
                mixed inherited_file = function_exists("::foo");
            "# },
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/middle";
            mixed local_file = function_exists("::foo");
            string create() { return inherited_file + "," + local_file; }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap().as_str(), Some("/base,/middle"));
    }

    #[tokio::test]
    async fn unknown_or_malformed_qualifications_do_not_fall_back() {
        let result = result_of(indoc! { r#"
            inherit "/grandparent" parent;
            void foo() {}
            int create() {
                return !function_exists("::foo") && !function_exists("parent::foo")
                    && !function_exists("unknown::grandparent_method")
                    && !function_exists("efun::write") && !function_exists("::write")
                    && !function_exists("parent::write") && !function_exists("parent::")
                    && !function_exists("::") && !function_exists("parent::::grandparent_method")
                    && !function_exists("parent::grandparent_method()");
            }
        "# })
        .await;
        assert_eq!(result, LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_existence_guard_does_not_make_an_unknown_direct_call_compile() {
        let vm = Vm::new(test_config());
        let error = vm
            .initialize_process_from_code(
                "/guard.c",
                indoc! { r#"
            void create() {
                if (function_exists("::missing")) { ::missing(); }
            }
        "# },
            )
            .await
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("call to unknown function `::missing`"),
            "{error}"
        );
    }
}
