use lpc_rs_errors::Result;

use crate::interpreter::efun::efun_context::EfunContext;

/// Return the dispatch mechanism that entered the current function.
pub fn origin<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if context.arg_count() != 0 {
        return Err(context.runtime_error("origin: expected no arguments"));
    }
    context.return_efun_result(context.call_origin().as_str().into());
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        compiler::CompilerBuilder,
        interpreter::{
            lpc_ref::LpcRef,
            tests::{run, run_with, s},
            vm::Vm,
        },
        test_support::{
            PERMISSIVE_MASTER, committed_global, lib_holding, run_prog, strings_of,
            temp_lib_config, test_config, try_run_prog,
        },
    };

    #[tokio::test]
    async fn initialization_is_driver_but_explicit_constructor_calls_are_local() {
        let task = run_prog(
            r#"
            string initialized = origin();
            string helper = probe();
            int entered;
            string probe() { return origin(); }
            mixed *create() {
                if (entered++) return ({ origin() });
                string first = origin();
                return ({ initialized, helper, first, create()[0], origin() });
            }
        "#,
        )
        .await;
        let result = task.result().unwrap();
        let values = result
            .with_array(task.context.txn(), |a| {
                a.iter().cloned().collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(
            values,
            vec![
                s("driver"),
                s("local"),
                s("driver"),
                s("local"),
                s("driver")
            ]
        );
    }

    #[tokio::test]
    async fn local_self_external_and_pointer_calls_are_distinct() {
        let result = strings_of(
            r#"
            string probe() { return origin(); }
            mixed *create() {
                function named = probe;
                function bound = &(this_object())->probe();
                function dynamic = &->probe();
                function door = &call_other(this_object(), "probe");
                return ({ probe(), this_object()->probe(),
                    call_other(this_object(), "probe"), named(), bound(),
                    dynamic(this_object()), door(), origin() });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            [
                "local",
                "call_other",
                "call_other",
                "function_pointer",
                "function_pointer",
                "function_pointer",
                "function_pointer",
                "driver"
            ]
        );
    }

    #[tokio::test]
    async fn evaluate_is_transparent_and_closures_do_not_retag_their_local_calls() {
        let result = strings_of(
            r#"
            string probe() { return origin(); }
            mixed *create() {
                function closure = (: origin() :);
                function nested = (: probe() :);
                function builtin = origin;
                function eval = evaluate;
                return ({ closure(), nested(), evaluate(closure), evaluate(probe),
                    builtin(), evaluate(origin), eval(origin), origin() });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            [
                "closure",
                "local",
                "closure",
                "function_pointer",
                "function_pointer",
                "function_pointer",
                "function_pointer",
                "driver"
            ]
        );
    }

    #[tokio::test]
    async fn inherited_and_virtual_calls_are_local() {
        let root = lib_holding(
            "origin-inheritance",
            &[(
                "base.c",
                r#"
            string initialized = origin();
            string base() { return origin(); }
            string virtual_call() { return probe(); }
            string probe() { return "wrong"; }
            string initial() { return initialized; }
        "#,
            )],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
            inherit "/base" parent;
            string probe() { return origin(); }
            mixed *create() {
                return ({ base(), parent::base(), ::base(), call_inherited("base"),
                    call_inherited("parent::base"), virtual_call(), initial() });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            vec![
                s("local"),
                s("local"),
                s("local"),
                s("local"),
                s("local"),
                s("local"),
                s("driver")
            ]
        );
    }

    #[tokio::test]
    async fn cold_external_and_collection_receivers_keep_their_dispatch() {
        let root = lib_holding(
            "origin-loading",
            &[
                ("target.c", "string probe() { return origin(); }"),
                ("second.c", "string probe() { return origin(); }"),
                ("third.c", "string probe() { return origin(); }"),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
            mixed *create() {
                string first = "/target"->probe();
                mixed *many = ({ "/second", "/target" })->probe();
                function dynamic = &->probe();
                return ({ first }) + many + ({ dynamic("/third"), origin() });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            vec![
                s("call_other"),
                s("call_other"),
                s("call_other"),
                s("function_pointer"),
                s("driver")
            ]
        );
    }

    #[tokio::test]
    async fn efun_callbacks_override_named_and_anonymous_pointer_kinds() {
        let result = strings_of(
            r#"
            string compared;
            string probe(mixed ignored) { return origin(); }
            int compare(int a, int b) { compared = origin(); return a - b; }
            mixed *create() {
                mixed *named = map(({ 1 }), probe);
                mixed *anonymous = map(({ 1 }), (: origin() :));
                mixed *kept = filter(({ 1 }), (: origin() == "efun" :));
                string reduced = reduce(({ 1, 2 }), (: origin() :));
                sort_array(({ 2, 1 }), compare);
                return ({ named[0], anonymous[0], sizeof(kept), reduced, compared,
                    map(({ 1 }), (: evaluate((: origin() :)) :))[0], origin() });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            ["efun", "efun", "1", "efun", "efun", "closure", "driver"]
        );
    }

    #[tokio::test]
    async fn suspended_efun_callbacks_and_caught_errors_restore_the_enclosing_origin() {
        let root = lib_holding(
            "origin-callback-loading",
            &[
                ("target.c", "string probe() { return origin(); }"),
                ("broken.c", "void create() { throw(origin()); }"),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
            string probe(mixed ignored) {
                mixed error = catch("/broken"->probe());
                return origin() + ":" + error;
            }
            mixed *create() {
                mixed *dynamic = map(({ "/target" }), &->probe());
                mixed *nested = map(({ 1 }), probe);
                return dynamic + nested + ({ origin() });
            }
        "#,
        )
        .await;
        assert_eq!(result, vec![s("efun"), s("efun:driver"), s("driver")]);
    }

    #[tokio::test]
    async fn present_id_callbacks_are_efun_calls() {
        let result = run(
            PERMISSIVE_MASTER,
            &[(
                "/item.c",
                r#"
            string seen;
            int id(string name) { seen = origin(); return 1; }
            void enter(object holder) { move_object(holder); }
            string seen() { return seen; }
        "#,
            )],
            r#"
            mixed *create() {
                object item = find_object("/item");
                item->enter(this_object());
                return ({ present("item") == item, item->seen(), origin() });
            }
        "#,
        )
        .await;
        assert_eq!(result, vec![1.into(), s("efun"), s("driver")]);
    }

    #[tokio::test]
    async fn simul_efun_calls_by_name_differ_from_pointer_calls() {
        let root = lib_holding(
            "origin-simul",
            &[(
                "secure/simul_efuns.c",
                r#"
            string probe() { return origin(); }
            string helper() { return origin(); }
            string via_helper() { return helper(); }
        "#,
            )],
        );
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .simul_efun_file("/secure/simul_efuns")
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.global_state
            .initialize_simul_efuns()
            .await
            .unwrap()
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"
            mixed *create() {
                function f = probe;
                return ({ probe(), via_helper(), f(), evaluate(probe), origin() });
            }
        "#,
            )
            .await
            .unwrap();
        let values = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |a| {
                a.iter().cloned().collect::<Vec<_>>()
            })
            .unwrap();
        assert_eq!(
            values,
            vec![
                s("simul_efun"),
                s("local"),
                s("function_pointer"),
                s("function_pointer"),
                s("driver")
            ]
        );
    }

    #[tokio::test]
    async fn permission_hooks_and_shadow_dispatch_have_separate_origins() {
        let result = run(
            r#"
            string seen;
            int query_allow_shadow(object ob) { seen = origin(); return 1; }
            string seen() { return seen; }
        "#,
            &[
                ("/target.c", "string probe() { return origin(); }"),
                (
                    "/shadow.c",
                    r#"
            void attach(object ob) { shadow(ob, 1); }
            string probe() { return origin(); }
          "#,
                ),
            ],
            r#"
            mixed *create() {
                object target = find_object("/target");
                find_object("/shadow")->attach(target);
                return ({ target->probe(), "/secure/master"->seen(), origin() });
            }
        "#,
        )
        .await;
        assert_eq!(result, vec![s("call_other"), s("driver"), s("driver")]);
    }

    #[tokio::test]
    async fn scheduled_named_and_anonymous_callbacks_report_call_out() {
        for callback in ["tick", "(: seen = origin() :)"] {
            let task = run_prog(&format!(
                r#"
                string seen;
                void tick() {{ seen = origin(); }}
                int create() {{ return call_out({callback}, 3600); }}
            "#
            ))
            .await;
            let LpcRef::Int(id) = task.result().unwrap() else {
                panic!("timer id");
            };
            task.context
                .global_state
                .prioritize_call_out(id.0 as u64)
                .await
                .await
                .unwrap();
            assert_eq!(committed_global(&task, "seen"), s("call_out"), "{callback}");
        }
    }

    #[tokio::test]
    async fn prepared_driver_callbacks_override_pointer_kinds() {
        for callback in ["tick", "(: seen = origin() :)"] {
            let task = run_prog(&format!(
                r#"
                string seen;
                void tick() {{ seen = origin(); }}
                function create() {{ return {callback}; }}
            "#
            ))
            .await;
            let LpcRef::Function(ptr) = task.result().unwrap() else {
                panic!("callback");
            };
            task.context
                .global_state
                .prepare_function_ptr(&ptr, &[], None)
                .await
                .unwrap()
                .unwrap()
                .execute(1000)
                .await
                .unwrap();
            assert_eq!(committed_global(&task, "seen"), s("driver"), "{callback}");
        }
    }

    #[tokio::test]
    async fn command_handlers_are_driver_calls_even_from_the_command_efun() {
        for handler in ["\"do_probe\"", "do_probe", "(: seen = origin(), 1 :)"] {
            let task = run_prog(&format!(
                r#"
                string seen;
                int do_probe(string argument) {{ seen = origin(); return 1; }}
                string create() {{
                    set_this_player(this_object());
                    enable_commands();
                    add_action({handler}, "probe");
                    command("probe");
                    return seen;
                }}
            "#
            ))
            .await;
            assert_eq!(task.result(), Some(s("driver")), "{handler}");
        }
    }

    #[tokio::test]
    async fn parser_actions_use_driver_dispatch_and_restore_the_callers_origin() {
        let result = strings_of(
            r#"
            mixed *action(mixed *tree) { return ({ origin() }); }
            mixed *create() {
                return parse_string("word = /[a-z]+/\nentry: word ? action", "hello")
                    + ({ origin() });
            }
        "#,
        )
        .await;
        assert_eq!(result, ["driver", "driver"]);
    }

    #[tokio::test]
    async fn composition_components_report_their_own_pointer_kind() {
        let result = strings_of(
            r#"
            string named(mixed ignored) { return origin(); }
            mixed *create() {
                function first = compose(named, (: origin() :));
                function second = compose((: origin() + ":" + $1 :), named);
                return ({ first(0), second(0) });
            }
        "#,
        )
        .await;
        assert_eq!(result, ["function_pointer", "closure:function_pointer"]);
    }

    #[tokio::test]
    async fn identical_caller_chains_can_have_different_origins() {
        let result = strings_of(
            r#"
            mixed *inspect() {
                return ({ origin(), this_object(), previous_object(-1),
                    calling_object(-1), calling_function(-1), calling_program(-1) });
            }
            mixed *middle(int external) {
                if (external) return this_object()->inspect();
                return inspect();
            }
            mixed *create() {
                mixed *a = middle(1);
                mixed *b = this_object()->middle(0);
                int same = a[1] == b[1];
                for (int i = 2; i < sizeof(a); i++) {
                    if (sizeof(a[i]) != sizeof(b[i])) same = 0;
                    for (int j = 0; j < sizeof(a[i]); j++) {
                        if (a[i][j] != b[i][j]) same = 0;
                    }
                }
                return ({ a[0], b[0], same });
            }
        "#,
        )
        .await;
        assert_eq!(result, ["call_other", "local", "1"]);
    }

    #[tokio::test]
    async fn unexpected_arguments_fail_in_direct_spread_and_pointer_calls() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        let error = compiler
            .compile_string("/origin_arity.c", "void create() { origin(1); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("incorrect argument count in call to `origin`"),
            "{error}"
        );
        for call in ["origin(({ 1 })...)", "f(1)", "evaluate(origin, 1)"] {
            let error = try_run_prog(&format!(
                "mixed create() {{ function f = origin; return {call}; }}"
            ))
            .await
            .unwrap_err()
            .to_string();
            assert!(
                error.contains("origin: expected no arguments"),
                "{call}: {error}"
            );
        }
    }
}
