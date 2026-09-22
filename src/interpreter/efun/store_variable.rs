use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_STORE_VARIABLE, apply::valid_apply, efun::efun_context::EfunContext, lpc_ref::LpcRef,
};

/// Replace an unambiguous global after authorization and declared-type checking.
pub async fn store_variable<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let target = context
        .arg(0)
        .live_object(context.txn())
        .ok_or_else(|| context.runtime_error("store_variable: target must be a live object"))?;
    let Some(name) = context.arg(1).as_str() else {
        return Err(context.runtime_error("store_variable: name must be a string"));
    };
    let source = if context.arg(3) == &context.txn().nil() {
        None
    } else {
        Some(context.arg(3).as_str().ok_or_else(|| {
            context.runtime_error("store_variable: declaring_program must be a string or nil")
        })?)
    };
    let value = context.arg(2);

    if !Arc::ptr_eq(context.process(), &target) {
        let args = [
            LpcRef::from(Arc::downgrade(context.process())),
            LpcRef::from(Arc::downgrade(&target)),
            context.arg(1).clone(),
            context.arg(3).clone(),
            value.clone(),
            context.calling_program(),
        ];
        if !valid_apply(
            context.task_context(),
            Some(context.chain()),
            VALID_STORE_VARIABLE,
            &args,
        )
        .await?
        {
            return Err(context.runtime_error("store_variable: permission denied"));
        }
    }
    if !target.is_live(context.txn()) {
        return Err(
            context.runtime_error("store_variable: target was destructed during authorization")
        );
    }

    let paths = context.config().paths();
    let image = target.image(context.txn());
    let mut matches = image
        .program
        .global_variable_info
        .iter()
        .filter(|variable| {
            variable.name == name
                && source
                    .is_none_or(|source| paths.source_name(&variable.filename).as_str() == source)
        });
    let Some(variable) = matches.next() else {
        return Err(context.runtime_error(match source {
            Some(source) => format!("store_variable: no global named `{name}` in `{source}`"),
            None => format!("store_variable: no global named `{name}`"),
        }));
    };
    if matches.next().is_some() {
        return Err(context.runtime_error(format!(
            "store_variable: ambiguous global `{name}`; specify its declaring program"
        )));
    }
    if !value.passes_cast(variable.type_, context.txn()) {
        return Err(context.runtime_error(format!(
            "store_variable: global `{name}` in `{}` expects {}, found {}",
            paths.source_name(&variable.filename),
            variable.type_,
            value.as_lpc_type(context.txn())
        )));
    }
    context
        .txn()
        .with(|t| t.write(image.var_id(variable.slot), value.clone()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            process::Process,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::{Vm, global_state::GlobalState, vm_op::VmOp},
        },
        test_support::{TempLib, lib_holding, run_prog, temp_lib_config},
    };

    fn vm(mode: NullMode) -> (TempLib, Vm) {
        let root = TempLib::new("store-variable");
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        (root, vm)
    }

    async fn with_target(master: Option<&str>) -> (TempLib, Vm, Arc<Process>) {
        let (root, vm) = vm(NullMode::Zero);
        if let Some(master) = master {
            vm.initialize_process_from_code("/secure/master.c", master)
                .await
                .unwrap();
        }
        let target = vm
            .initialize_process_from_code("/target.c", "int number = 7;")
            .await
            .unwrap()
            .context
            .process;
        (root, vm, target)
    }

    #[tokio::test]
    async fn self_writes_include_private_protected_nosave_and_nomask_globals() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/self.c",
                    r#"
                private int secret = 1;
                protected nosave int cache = 2;
                nomask int fixed_name = 3;
                mixed empty = nil;
                int create() {
                    store_variable(this_object(), "secret", 4);
                    store_variable(this_object(), "cache", 5, nil);
                    store_variable(this_object(), "fixed_name", 6, "/self.c");
                    store_variable(this_object(), "empty", 7);
                    foreach (mapping entry : variable_info(this_object())) {
                        if (entry["name"] == "secret" && entry["value"] != 4) return 0;
                    }
                    return secret == 4 && cache == 5 && fixed_name == 6 && empty == 7;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{mode}");
        }
    }

    #[tokio::test]
    async fn inherited_helpers_select_hidden_declarations_and_shared_diamonds_by_source() {
        let root = lib_holding(
            "store-variable-inherits",
            &[
                (
                    "base.c",
                    "private int shared = 1; int shared_value() { return shared; }",
                ),
                (
                    "left.c",
                    "inherit \"/base\"; private int n = 2; int left() { return n; }",
                ),
                (
                    "right.c",
                    "inherit \"/base\"; private int n = 3; int right() { return n; }",
                ),
                (
                    "writer.c",
                    r#"
                    void change() {
                        store_variable(this_object(), "n", 12, "/left.c");
                        store_variable(this_object(), "n", 13, "/right.c");
                        store_variable(this_object(), "n", 14, "/child.c");
                        store_variable(this_object(), "shared", 15);
                    }
                "#,
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                r#"
            inherit "/left";
            inherit "/right";
            inherit "/writer";
            int n = 4;
            int create() {
                mixed err = catch(store_variable(this_object(), "n", 99));
                if (!stringp(err) || strindex(err, "ambiguous global") < 0
                    || left() != 2 || right() != 3 || n != 4) return 0;
                change();
                int shared_count;
                foreach (mapping entry : variable_info(this_object()))
                    if (entry["name"] == "shared") shared_count++;
                return left() == 12 && right() == 13 && n == 14
                    && shared_value() == 15 && shared_count == 1;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(1.into()));
    }

    #[tokio::test]
    async fn missing_names_and_noncanonical_selectors_leave_the_global_unchanged() {
        let (_root, vm) = vm(NullMode::Zero);
        let task = vm
            .initialize_process_from_code(
                "/self.c",
                r#"
            int n = 7;
            int create() {
                foreach (string source : ({ "", "self", "self.c", "/self", "/./self.c",
                    "/other/../self.c", "/other.c" })) {
                    mixed err = catch(store_variable(this_object(), "n", 9, source));
                    if (!stringp(err) || strindex(err, "no global named") < 0 || n != 7) return 0;
                }
                foreach (string name : ({ "", "absent", "self::n" })) {
                    mixed err = catch(store_variable(this_object(), name, 9));
                    if (!stringp(err) || strindex(err, "no global named") < 0 || n != 7) return 0;
                }
                store_variable(this_object(), "n", 10, "/self.c");
                return n == 10;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(1.into()));
    }

    #[tokio::test]
    async fn invalid_runtime_arguments_are_rejected_in_both_null_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/self.c",
                    r#"
                int n = 7;
                int create() {
                    foreach (mixed target : ({ nil, 0, 1, "/self", ({}) })) {
                        mixed err = catch(store_variable(target, "n", 9));
                        if (!stringp(err) || strindex(err, "target must be a live object") < 0) return 0;
                    }
                    foreach (mixed name : ({ nil, 0, 1, ({}) })) {
                        mixed err = catch(store_variable(this_object(), name, 9));
                        if (!stringp(err) || strindex(err, "name must be a string") < 0) return 0;
                    }
                    foreach (mixed source : ({ 1, ({}), ([]) })) {
                        mixed err = catch(store_variable(this_object(), "n", 9, source));
                        if (!stringp(err) || strindex(err, "declaring_program must be") < 0) return 0;
                    }
                    mixed zero = 0;
                    mixed err = catch(store_variable(this_object(), "n", 8, zero));
                    return nil == 0 ? !err && n == 8 : stringp(err) && n == 7;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{mode}");
        }
    }

    #[tokio::test]
    async fn the_compiler_checks_argument_types_and_arity() {
        let (_root, vm) = vm(NullMode::Zero);
        for (call, expected) in [
            (
                "store_variable(this_object(), \"n\")",
                "incorrect argument count",
            ),
            (
                "store_variable(this_object(), \"n\", 1, nil, 2)",
                "incorrect argument count",
            ),
            ("store_variable(1, \"n\", 1)", "unexpected argument type"),
            (
                "store_variable(this_object(), 1, 1)",
                "unexpected argument type",
            ),
            (
                "store_variable(this_object(), \"n\", 1, 2)",
                "unexpected argument type",
            ),
        ] {
            let error = vm
                .initialize_process_from_code("/self.c", &format!("void create() {{ {call}; }}"))
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(expected), "{call}: {error}");
        }
    }

    #[tokio::test]
    async fn writes_check_the_declared_type_instead_of_the_current_value() {
        let task = run_prog(
            r#"
            int number = 7;
            float real = 1.5;
            string text = "old";
            bytes raw = to_bytes(({ 1 }));
            mapping map = ([]);
            function callback = (: 1 :);
            object owner;
            mixed anything;
            void corrupt(mixed value) { number = value; }
            int create() {
                foreach (mixed *pair : ({ ({ "number", "bad" }), ({ "number", 1.5 }),
                    ({ "real", 1 }), ({ "text", 3 }), ({ "raw", "bad" }),
                    ({ "map", ({}) }), ({ "callback", "bad" }), ({ "owner", 2 }) })) {
                    mixed err = catch(store_variable(this_object(), pair[0], pair[1]));
                    if (!stringp(err) || strindex(err, "expects") < 0) return 0;
                }
                if (number != 7 || real != 1.5 || text != "old" || raw[0] != 1) return 0;
                corrupt("existing wrong type");
                if (!stringp(catch(store_variable(this_object(), "number", "still wrong")))) return 0;
                store_variable(this_object(), "number", 8);
                store_variable(this_object(), "owner", this_object());
                store_variable(this_object(), "anything", ({ "any type" }));
                return number == 8 && owner == this_object() && anything[0] == "any type";
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(1.into()));
    }

    #[tokio::test]
    async fn writes_preserve_the_selected_null_rules_without_coercion() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/self.c",
                    r#"
                int number = 7;
                float real = 1.5;
                string text = "old";
                mixed anything = 3;
                int create() {
                    store_variable(this_object(), "anything", nil);
                    store_variable(this_object(), "text", nil);
                    mixed int_error = catch(store_variable(this_object(), "number", nil));
                    mixed float_error = catch(store_variable(this_object(), "real", nil));
                    mixed string_error = catch(store_variable(this_object(), "text", 0));
                    if (anything != nil) return 0;
                    if (nil == 0) return !int_error && !float_error && !string_error
                        && number == 0 && intp(real) && text == 0;
                    return stringp(int_error) && stringp(float_error) && stringp(string_error)
                        && number == 7 && real == 1.5 && text == nil;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{mode}");
        }
    }

    #[tokio::test]
    async fn nominal_classes_and_shallow_arrays_keep_their_existing_type_rules() {
        let (_root, vm) = vm(NullMode::Distinct);
        vm.initialize_process_from_code(
            "/other.c",
            "class record { int n; } mixed make() { return new(class record, n: 9); }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/self.c",
                r#"
            class record { int n; }
            class record stored = new(class record, n: 1);
            class record *records;
            int *numbers;
            int create() {
                mixed other = find_object("/other")->make();
                mixed err = catch(store_variable(this_object(), "stored", other));
                if (!stringp(err) || stored.n != 1) return 0;
                class record value = new(class record, n: 2);
                store_variable(this_object(), "stored", value);
                value.n = 3;
                store_variable(this_object(), "records", ({ other }));
                store_variable(this_object(), "numbers", ({ "shallow" }));
                mixed untyped_records = records;
                if (stored.n != 3 || untyped_records[0] != other || numbers[0] != "shallow") return 0;
                if (!stringp(catch(records[0]))) return 0;
                store_variable(this_object(), "stored", nil);
                return stored == nil;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(1.into()));
    }

    #[tokio::test]
    async fn function_value_calls_share_containers_and_leave_a_void_result() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/self.c",
                    r#"
                mixed *items;
                mapping map;
                function callback;
                int create() {
                    function store = store_variable;
                    mixed *a = ({ 1 });
                    mapping m = ([ "n": 2 ]);
                    mixed result = store(this_object(), "items", a);
                    evaluate(store_variable, this_object(), "map", m, "/self.c");
                    function set_callback = &store_variable(this_object(), "callback");
                    set_callback((: 3 :));
                    a[0] = 4;
                    m["n"] = 5;
                    return result == nil && items == a && map == m
                        && items[0] == 4 && map["n"] == 5 && callback() == 3;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{mode}");
        }
    }

    #[tokio::test]
    async fn foreign_writes_require_their_own_permission_before_name_or_type_lookup() {
        for master in [
            None,
            Some(""),
            Some("int valid_store_variable() { return 0; }"),
            Some("int valid_variable_info() { return 1; }"),
        ] {
            let (_root, vm, target) = with_target(master).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int create() {
                    object target = find_object("/target");
                    foreach (mixed *args : ({ ({ "number", 8, nil }),
                        ({ "missing", 8, nil }), ({ "number", "wrong type", "/wrong.c" }) })) {
                        mixed err = catch(store_variable(target, args[0], args[1], args[2]));
                        if (!stringp(err) || strindex(err, "store_variable: permission denied") < 0)
                            return 0;
                    }
                    return 1;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "master: {master:?}");
            assert_eq!(
                vm.global_state.committed_global(&target, 0),
                LpcRef::from(7)
            );
        }
    }

    #[tokio::test]
    async fn authorization_receives_the_request_and_inherited_or_pointer_source_context() {
        let root = lib_holding(
            "store-variable-context",
            &[(
                "parent.c",
                r#"
                void change(object target) { store_variable(target, "number", 8); }
                function setter() { return &store_variable(find_object("/target"), "number", 9, "/target.c"); }
            "#,
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"
            int calls;
            int valid_store_variable(object caller, object target, string name,
                    string declaring_program, mixed value, string program) {
                calls++;
                return this_object() == find_object("/secure/master")
                    && caller == previous_object() && this_player() == caller
                    && file_name(caller) == "/caller" && file_name(target) == "/target"
                    && name == "number" && program == "/parent.c"
                    && (calls == 1 ? declaring_program == nil && value == 8
                        : declaring_program == "/target.c" && value == 9);
            }
        "#,
        )
        .await
        .unwrap();
        let target = vm
            .initialize_process_from_code("/target.c", "private int number = 7;")
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code("/relay.c", "void fire(function f) { f(); }")
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/caller.c",
            r#"
            inherit "/parent";
            void create() {
                set_this_player(this_object());
                change(find_object("/target"));
                find_object("/relay")->fire(setter());
            }
        "#,
        )
        .await
        .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&target, 0),
            LpcRef::from(9)
        );
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, 0),
            LpcRef::from(2)
        );
    }

    #[tokio::test]
    async fn simul_wrappers_are_authorized_as_the_simul_object_with_an_explicit_target() {
        let root = lib_holding(
            "store-variable-simul",
            &[(
                "simul.c",
                r#"
                void change_my_global(string name, mixed value) {
                    store_variable(previous_object(), name, value);
                }
            "#,
            )],
        );
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .simul_efun_file("/simul.c")
                .build()
                .unwrap(),
        );
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"
            int valid_store_variable(object caller, object target, string name,
                    string declaring_program, mixed value, string program) {
                return caller == previous_object() && file_name(caller) == "/simul"
                    && file_name(target) == "/caller" && program == "/simul.c"
                    && name == "number" && declaring_program == nil && value == 8;
            }
        "#,
        )
        .await
        .unwrap();
        vm.initialize_simul_efuns().await.unwrap().unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                "private int number = 7; int create() { change_my_global(\"number\", 8); return number; }",
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(8.into()));
    }

    #[tokio::test]
    async fn caught_policy_errors_and_side_effects_do_not_apply_the_requested_write() {
        for body in ["return 0;", "throw(\"policy error\");", "return 1;"] {
            let master = format!(
                r#"
                int calls;
                int valid_store_variable(object caller, object target, string name,
                        string declaring_program, mixed value, string program) {{
                    calls++;
                    value[0] = 99;
                    {body}
                }}
            "#
            );
            let (_root, vm, target) = with_target(Some(&master)).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int create() {
                    mixed *value = ({ 1 });
                    mixed err = catch(store_variable(find_object("/target"), "number", value));
                    return stringp(err) && value[0] == 99;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{body}");
            assert_eq!(
                vm.global_state.committed_global(&target, 0),
                LpcRef::from(7)
            );
            let master = vm.global_state.object_space.master_object().unwrap();
            assert_eq!(
                vm.global_state.committed_global(&master, 0),
                LpcRef::from(1)
            );
        }
    }

    #[tokio::test]
    async fn a_target_destructed_before_or_during_authorization_is_rejected() {
        let (_root, vm, _target) = with_target(Some(
            r#"
            int calls;
            int valid_destruct() { return 1; }
            int valid_store_variable(object caller, object target) {
                calls++;
                destruct(target);
                return 1;
            }
        "#,
        ))
        .await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"
            int create() {
                object target = find_object("/target");
                mixed during = catch(store_variable(target, "number", 8));
                mixed before = catch(store_variable(target, "number", 9));
                return stringp(during) && strindex(during, "destructed during authorization") >= 0
                    && stringp(before) && strindex(before, "target must be a live object") >= 0;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(1.into()));
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, 0),
            LpcRef::from(1)
        );
        assert!(vm.global_state.object_space.lookup("/target").is_none());
    }

    #[tokio::test]
    async fn type_checks_observe_value_destruction_during_authorization() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            vm.initialize_process_from_code(
                "/secure/master.c",
                r#"
                int valid_destruct() { return 1; }
                int valid_store_variable(object caller, object target, string name,
                        string declaring_program, mixed value, string program) {
                    destruct(value);
                    return 1;
                }
            "#,
            )
            .await
            .unwrap();
            vm.initialize_process_from_code("/victim.c", "")
                .await
                .unwrap();
            vm.initialize_process_from_code(
                "/target.c",
                "string text = \"old\"; int cleared() { return text == nil; }",
            )
            .await
            .unwrap();
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int create() {
                    object target = find_object("/target");
                    store_variable(target, "text", find_object("/victim"));
                    return target->cleared();
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(1.into()), "{mode}");
        }
    }

    #[tokio::test]
    async fn abort_discards_the_write_and_policy_changes() {
        let (_root, vm, target) = with_target(Some(
            "int calls; int valid_store_variable() { calls++; return 1; }",
        ))
        .await;
        let error = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"void create() { store_variable(find_object("/target"), "number", 8); throw("abort"); }"#,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("abort"));
        assert_eq!(
            vm.global_state.committed_global(&target, 0),
            LpcRef::from(7)
        );
        let master = vm.global_state.object_space.master_object().unwrap();
        assert_eq!(
            vm.global_state.committed_global(&master, 0),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn approved_writes_share_policy_mutations_and_pending_values() {
        let (_root, vm) = vm(NullMode::Distinct);
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"
            int valid_store_variable(object caller, object target, string name,
                    string declaring_program, mixed value, string program) {
                value[0]++;
                return 1;
            }
        "#,
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/target.c",
            "private mixed *values; mixed *get() { return values; }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"
            int create() {
                mixed *values = ({ 1 });
                object target = find_object("/target");
                store_variable(target, "values", values);
                if (values[0] != 2 || target->get() != values) return 0;
                store_variable(target, "values", target->get());
                return values[0] == 3 && target->get()[0] == 3;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(1.into()));
    }

    #[tokio::test]
    async fn retries_commit_authorization_and_the_write_together_once() {
        let root = TempLib::new("store-variable-retry");
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(
            Arc::new(temp_lib_config(&root)),
            tx,
            3,
        ));
        let master = state
            .object_space
            .create_process_from_code(
                "/secure/master.c",
                r#"
                int number;
                int calls;
                int get() { return number; }
                int valid_store_variable() { calls++; return 1; }
            "#,
            )
            .await
            .unwrap();
        state
            .initialize_process_from_code(
                "/caller.c",
                r#"
                void create() {
                    object master = find_object("/secure/master");
                    store_variable(master, "number", master->get() + 1);
                }
            "#,
            )
            .await
            .unwrap();
        assert_eq!(state.attempt_telemetry().conflicts, 3);
        assert_eq!(state.committed_global(&master, 0), LpcRef::from(1));
        assert_eq!(state.committed_global(&master, 1), LpcRef::from(1));
    }

    #[tokio::test]
    async fn existing_pointers_resolve_current_declarations_after_recompilation() {
        let root = lib_holding(
            "store-variable-recompile",
            &[("target.c", "int number = 7; int get() { return number; }")],
        );
        let mut vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"
            int valid_load() { return 1; }
            int valid_clone() { return 1; }
            int valid_recompile() { return 1; }
            int valid_store_variable() { return 1; }
        "#,
        )
        .await
        .unwrap();
        let admin = vm
            .initialize_process_from_code(
                "/admin.c",
                r#"
            object prototype = load_object("/target");
            object copy = clone_object("/target");
            function setter = &store_variable(copy, "number");
            void create() { setter(8); }
            int request() { return request_object_recompile(prototype); }
            int check() {
                mixed err = catch(setter(9));
                if (!stringp(err) || strindex(err, "expects string") < 0) return 0;
                setter("clone");
                store_variable(prototype, "number", "prototype", "/target.c");
                return copy->get() == "clone" && prototype->get() == "prototype"
                    && copy->padding_ok() && prototype->padding_ok();
            }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        std::fs::write(
            root.join("target.c"),
            r#"
            int padding = 42;
            string number = "new";
            string get() { return number; }
            int padding_ok() { return padding == 42; }
        "#,
        )
        .unwrap();
        apply_function_by_name(
            "request",
            &[],
            admin.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("recompilation was not queued");
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
        assert_eq!(status.updated, 2);
        let result = apply_function_by_name(
            "check",
            &[],
            admin,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(1));
    }
}
