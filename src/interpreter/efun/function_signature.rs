use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, function_exists::visible_function},
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
};

/// Inspect a compiled definition without invoking or initializing its object.
pub fn function_signature<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error("function_signature: name must be a string"));
    };
    if context.arg(1) != &context.txn().nil() && !matches!(context.arg(1), LpcRef::Object(_)) {
        return Err(context.runtime_error("function_signature: target must be an object"));
    }
    let Some(function) = context
        .arg(1)
        .live_object(context.txn())
        .and_then(|target| visible_function(context, &target, name))
    else {
        context.return_efun_result(context.txn().nil());
        return Ok(());
    };
    let prototype = &function.prototype;
    let parameter_types = context.mint_array(
        prototype
            .arg_types
            .iter()
            .map(|type_| LpcRef::from(type_.to_string())),
    );
    let parameter_refs = context.mint_array(
        (0..prototype.arg_types.len())
            .map(|position| LpcRef::from(prototype.is_ref_param(position))),
    );
    let fields = [
        ("name", prototype.name.as_ref().into()),
        (
            "program",
            context
                .config()
                .paths()
                .source_name(&prototype.filename)
                .to_string()
                .into(),
        ),
        ("return_type", prototype.return_type.to_string().into()),
        ("parameter_types", parameter_types),
        ("parameter_refs", parameter_refs),
        ("num_args", i64::from(prototype.arity.num_args).into()),
        (
            "num_default_args",
            i64::from(prototype.arity.num_default_args).into(),
        ),
        ("varargs", prototype.flags.varargs().into()),
        ("ellipsis", prototype.flags.ellipsis().into()),
        ("visibility", function.visibility().to_string().into()),
        ("nomask", prototype.flags.nomask().into()),
    ];
    context.return_mapping(LpcMapping::new(
        fields
            .into_iter()
            .map(|(key, value)| (key.into(), value))
            .collect(),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::{lpc_path::LpcPath, null_mode::NullMode};
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::{Vm, vm_op::VmOp},
        },
        test_support::{
            PERMISSIVE_MASTER, lib_holding, permissive_master, run_prog, temp_lib_config,
        },
    };

    #[tokio::test]
    async fn reports_fixed_types_defaults_references_and_modifiers() {
        let task = run_prog(indoc! { r#"
            protected nomask string *describe(int ref number, string label = "default", mixed rest...) {
                throw("inspection must not invoke the definition");
                return ({});
            }
            int create() {
                mapping info = function_signature("describe", this_object());
                return sizeof(info) == 11 && info["name"] == "describe"
                    && info["program"] == "/my_file.c" && info["return_type"] == "string *"
                    && implode(info["parameter_types"], ",") == "int,string"
                    && info["parameter_refs"][0] == 1 && info["parameter_refs"][1] == 0
                    && info["num_args"] == 2 && info["num_default_args"] == 1
                    && info["varargs"] == 0 && info["ellipsis"] == 1
                    && info["visibility"] == "protected" && info["nomask"] == 1;
            }
        "# }).await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn zero_arguments_and_bare_ellipsis_have_no_fixed_parameters() {
        let task = run_prog(indoc! { r#"
            void empty() {}
            void variadic(...) {}
            varargs void optional(int number) {}
            int create() {
                mapping a = function_signature("empty", this_object());
                mapping b = function_signature("variadic", this_object());
                mapping optional = function_signature("optional", this_object());
                return a["return_type"] == "void" && a["num_args"] == 0
                    && !sizeof(a["parameter_types"]) && !sizeof(a["parameter_refs"])
                    && !a["varargs"] && !a["ellipsis"] && !a["nomask"]
                    && a["visibility"] == "public" && !a["num_default_args"]
                    && b["ellipsis"] && b["num_args"] == 0 && !sizeof(b["parameter_types"])
                    && optional["varargs"] && !optional["ellipsis"] && optional["num_args"] == 1;
            }
        "# })
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn type_names_preserve_nominal_classes_and_arrays() {
        let task = run_prog(indoc! { r#"
            class record { int number; }
            class record typed(class record value, string *labels, bytes *data) { return value; }
            int create() {
                mapping info = function_signature("typed", this_object());
                return info["return_type"] == "class record (/my_file.c)"
                    && info["parameter_types"][0] == info["return_type"]
                    && info["parameter_types"][1] == "string *"
                    && info["parameter_types"][2] == "bytes *";
            }
        "# })
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn metadata_arrays_are_fresh_and_pointer_bindings_remain_separate() {
        let task = run_prog(indoc! { r#"
            int typed(int ref number, string label) { throw("must not invoke"); return 0; }
            int create() {
                mapping changed = function_signature("typed", this_object());
                changed["name"] = "changed";
                changed["parameter_types"][0] = "float";
                changed["parameter_refs"][0] = 0;
                mapping current = function_signature("typed", this_object());
                return current["name"] == "typed" && current["parameter_types"][0] == "int"
                    && current["parameter_refs"][0] == 1 && current["num_args"] == 2
                    && !sizeof(function_info(typed)["arguments"]);
            }
        "# })
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn inspection_does_not_initialize_invoke_or_load_the_target() {
        let root = lib_holding("function-signature-inspection", &[]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
            int initialized = fail();
            int fail() { throw("global initializer must not run"); return 0; }
            void create() { throw("create must not run"); }
            int typed(int number = fail()) { throw("method must not run"); return 0; }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            int create() {
                mapping info = function_signature("typed", find_object("/target"));
                return info["num_args"] == 1 && info["num_default_args"] == 1
                    && info["program"] == "/target.c" && !find_object("/unloaded");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn foreign_root_lookup_follows_overrides_and_hides_nonpublic_bindings() {
        let root = lib_holding(
            "function-signature-foreign",
            &[
                (
                    "base.c",
                    "string original(int n) { return \"base\"; } protected void guarded() {} private void hidden() {}",
                ),
                ("other.c", "string original(int n) { return \"other\"; }"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/target.c",
            indoc! { r#"
            inherit "/base" parent;
            string original(int n) { return "target"; }
            private void secret() {}
        "# },
        )
        .await
        .unwrap();
        std::fs::remove_file(root.join("base.c")).unwrap();
        let task = vm.initialize_process_from_code("/inspector.c", indoc! { r#"
            inherit "/other" parent;
            int create() {
                object ob = find_object("/target");
                return function_signature("original", ob)["program"] == "/target.c"
                    && function_signature("parent::original", ob)["program"] == "/base.c"
                    && function_signature("::original", ob)["program"] == "/base.c"
                    && !function_signature("guarded", ob) && !function_signature("hidden", ob)
                    && !function_signature("secret", ob) && !function_signature("parent::guarded", ob)
                    && !function_signature("missing::original", ob) && !find_object("/base");
            }
        "# }).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn self_lookup_keeps_lexical_parent_aliases_and_inheritance_visibility() {
        let root = lib_holding(
            "function-signature-scope",
            &[
                ("base.c", "void original() {}"),
                (
                    "middle.c",
                    indoc! { r#"
                protected inherit "/base" parent;
                private void secret() {}
                void original() {}
                int inspect() {
                    object self = this_object();
                    mapping inherited = function_signature("parent::original", self);
                    return inherited["program"] == "/base.c" && inherited["visibility"] == "protected"
                        && function_signature("::original", self)["program"] == "/base.c"
                        && function_signature("original", self)["program"] == "/child.c"
                        && function_signature("secret", self)["visibility"] == "private";
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
            void original() {}
            int create() { return inspect(); }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        let task = vm
            .initialize_process_from_code(
                "/observer.c",
                indoc! { r#"
            int create() {
                object ob = find_object("/child");
                return !function_signature("parent::parent::original", ob)
                    && !function_signature("parent::secret", ob);
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn missing_dead_and_null_targets_return_configured_nil_and_bad_types_error() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = lib_holding("function-signature-null", &[("empty.c", "")]);
            let config = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .build()
                .unwrap();
            let vm = Vm::new(config);
            permissive_master(&vm.global_state.object_space).await;
            let task = vm
                .initialize_process_from_code(
                    "/inspector.c",
                    indoc! { r#"
                int create() {
                    object ob = clone_object("/empty");
                    destruct(ob);
                    mixed number = 17;
                    mixed path = "/unloaded";
                    return nilp(function_signature("absent", this_object()))
                        && nilp(function_signature("write", this_object()))
                        && nilp(function_signature("efun::write", this_object()))
                        && nilp(function_signature("::", this_object()))
                        && nilp(function_signature("create", nil))
                        && nilp(function_signature("create", ob))
                        && stringp(catch(function_signature(number, this_object())))
                        && stringp(catch(function_signature("create", number)))
                        && stringp(catch(function_signature("create", path)))
                        && !find_object("/unloaded");
                }
            "# },
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode:?}");
        }
    }

    #[tokio::test]
    async fn host_paths_and_existing_clones_follow_program_recompilation() {
        let root = lib_holding(
            "function-signature-recompile",
            &[("target.c", "int original(int n) { return n; }")],
        );
        let mut vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", format!("{PERMISSIVE_MASTER}\nint valid_clone() {{ return 1; }}\nint valid_recompile() {{ return 1; }}")).await.unwrap();
        vm.initialize_process_from_code(
            LpcPath::new_server(root.join("target.c")),
            "int original(int n) { return n; }",
        )
        .await
        .unwrap();
        let inspector = vm.initialize_process_from_code("/inspector.c", indoc! { r#"
            object prototype = find_object("/target");
            object copy = clone_object("/target");
            int request() { return request_object_recompile(prototype); }
            int inspect(string name, string result_type, int count) {
                foreach (object ob in ({ prototype, copy })) {
                    mapping info = function_signature(name, ob);
                    if (!info || info["program"] != "/target.c" || info["return_type"] != result_type || info["num_args"] != count) return 0;
                }
                return 1;
            }
            int missing() { return !function_signature("original", prototype) && !function_signature("original", copy); }
        "# }).await.unwrap().context.process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let before = apply_function_by_name(
            "inspect",
            &["original".into(), "int".into(), 1.into()],
            inspector.clone(),
            template.clone(),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(before, LpcRef::from(1));
        std::fs::write(
            root.join("target.c"),
            "string replacement(int n, string text = \"default\") { return text; }",
        )
        .unwrap();
        apply_function_by_name(
            "request",
            &[],
            inspector.clone(),
            template.clone(),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("recompilation queued");
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
        assert_eq!(status.updated, 2);
        let after = apply_function_by_name(
            "inspect",
            &["replacement".into(), "string".into(), 2.into()],
            inspector.clone(),
            template.clone(),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(after, LpcRef::from(1));
        let missing = apply_function_by_name("missing", &[], inspector, template, Some(5000))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(missing, LpcRef::from(1));
    }
}
