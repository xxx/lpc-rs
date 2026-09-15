use lpc_rs_errors::Result;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    stm::TxnHandle,
};

/// Call an accessible inherited definition, returning 0 when it is absent.
pub fn call_inherited<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "call_inherited: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let Some(function) = context.lookup_inherited_function(name).cloned() else {
        context.return_efun_result(NULL);
        return Ok(());
    };
    let callee = Callee::Inherited {
        process: context.process().clone(),
        function,
        args: (1..context.arg_count())
            .map(|i| context.arg(i).clone())
            .collect(),
    };
    context.continue_with(Box::new(CallInherited(Some(callee))));
    Ok(())
}

#[derive(Debug, Clone)]
struct CallInherited(Option<Callee>);

impl Continuation for CallInherited {
    fn advance(&mut self, result: Option<LpcRef>, _txn: &TxnHandle) -> Result<Next> {
        Ok(match self.0.take() {
            Some(callee) => Next::Call(callee),
            None => Next::Done(result.unwrap_or(NULL)),
        })
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::lpc_path::LpcPath;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            tests::{fails_with, run, run_with, s},
            vm::Vm,
        },
        test_support::{PERMISSIVE_MASTER, lib_holding, permissive_master, temp_lib_config},
    };

    #[tokio::test]
    async fn inherited_calls_bypass_private_overrides_and_update_the_receivers_globals() {
        let root = lib_holding(
            "call-inherited-globals",
            &[
                ("padding.c", "int padding = 1000;"),
                (
                    "base.c",
                    indoc! { r#"
                int total = 5;
                static int foo(int n) { total += n; return total; }
                int amount() { return total; }
            "# },
                ),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/padding";
            inherit "/base" parent;
            private int foo(int n) { return 999; }
            mixed *create() {
                return ({ call_inherited("foo", 3), call_inherited("::foo", 2),
                    call_inherited("parent::foo", 4), amount(), foo(0), !find_object("/base") });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(8),
                LpcRef::from(10),
                LpcRef::from(14),
                LpcRef::from(14),
                LpcRef::from(999),
                LpcRef::from(1)
            ]
        );
    }

    #[tokio::test]
    async fn named_inherits_and_diamond_precedence_match_direct_calls() {
        let root = lib_holding(
            "call-inherited-diamond",
            &[
                ("base.c", "int foo() { return 1; }"),
                ("left.c", "inherit \"/base\"; int foo() { return 2; }"),
                ("right.c", "inherit \"/base\";"),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/left" left;
            inherit "/right" right;
            int foo() { return 3; }
            mixed *create() {
                return ({ call_inherited("foo"), call_inherited("left::foo"),
                    call_inherited("right::foo"), ::foo(), left::foo(), right::foo() });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(1),
                LpcRef::from(2),
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(2),
                LpcRef::from(1)
            ]
        );
    }

    #[tokio::test]
    async fn nested_inherited_calls_keep_each_sources_parents_and_aliases() {
        let root = lib_holding(
            "call-inherited-scopes",
            &[
                (
                    "base.c",
                    "int foo() { return 1 + call_inherited(\"foo\"); }",
                ),
                (
                    "middle.c",
                    indoc! { r#"
                inherit "/base" parent;
                int foo() { return 10 + call_inherited("parent::foo"); }
            "# },
                ),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/middle" parent;
            int foo() { return 100 + call_inherited("foo"); }
            mixed *create() { return ({ foo(), call_inherited("parent::foo") }); }
        "# },
        )
        .await;
        assert_eq!(result, vec![LpcRef::from(111), LpcRef::from(11)]);
    }

    #[tokio::test]
    async fn a_private_parent_does_not_fall_back_to_an_earlier_public_definition() {
        let root = lib_holding(
            "call-inherited-private",
            &[
                ("left.c", "int foo() { return 7; }"),
                (
                    "right.c",
                    "private int foo() { throw(\"private hook ran\"); }",
                ),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/left" left;
            inherit "/right" right;
            mixed *create() {
                return ({ call_inherited("foo"), call_inherited("right::foo"),
                    call_inherited("left::foo") });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![LpcRef::from(0), LpcRef::from(0), LpcRef::from(7)]
        );
    }

    #[tokio::test]
    async fn inherited_calls_preserve_the_object_and_local_call_history() {
        let root = lib_holding(
            "call-inherited-history",
            &[(
                "base.c",
                indoc! { r#"
            protected mixed *hook(int n) {
                return ({ n, file_name(this_object()), file_name(previous_object()),
                    calling_function(), calling_program() });
            }
        "# },
            )],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[(
                "/target.c",
                indoc! { r#"
            inherit "/base" parent;
            mixed *ask() { return call_inherited("parent::hook", 42); }
        "# },
            )],
            r#"mixed *create() { return "/target"->ask(); }"#,
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(42),
                s("/target"),
                s("/main"),
                s("ask"),
                s("/target.c")
            ]
        );
    }

    #[tokio::test]
    async fn absent_targets_return_zero_and_only_a_guard_skips_argument_evaluation() {
        let result = run(
            "",
            &[],
            indoc! { r#"
            void local_only() { throw("local function ran"); }
            mixed *create() {
                int count = 0;
                mixed absent = call_inherited("missing", ++count);
                if (function_exists("::missing")) { call_inherited("missing", ++count); }
                return ({ absent, count, call_inherited("local_only"),
                    call_inherited("efun::write", "wrong"), call_inherited("write", "wrong"),
                    call_inherited("unknown::local_only"), call_inherited("::"),
                    call_inherited("parent::::foo"), call_inherited("foo()") });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(0),
                LpcRef::from(1),
                LpcRef::from(0),
                LpcRef::from(0),
                LpcRef::from(0),
                LpcRef::from(0),
                LpcRef::from(0),
                LpcRef::from(0),
                LpcRef::from(0)
            ]
        );
    }

    #[tokio::test]
    async fn arguments_support_defaults_varargs_ellipsis_and_spreading() {
        let root = lib_holding(
            "call-inherited-arguments",
            &[(
                "base.c",
                indoc! { r#"
            int add(int a, int b = 4) { return a + b; }
            varargs int optional(int a, int b) { return a + b; }
            int many(int n, ...) { return n + sizeof(argv); }
            void empty() { add(1); }
        "# },
            )],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/base" parent;
            mixed *create() {
                string name = "parent::" + "add";
                return ({ call_inherited(name, 5), call_inherited("add", ({ 2, 3 })...),
                    call_inherited("optional", 6), call_inherited("many", 10, 1, 2),
                    call_inherited("empty") });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(9),
                LpcRef::from(5),
                LpcRef::from(6),
                LpcRef::from(12),
                LpcRef::from(0)
            ]
        );
    }

    #[tokio::test]
    async fn invalid_names_and_arguments_report_runtime_errors() {
        let root = lib_holding(
            "call-inherited-errors",
            &[(
                "base.c",
                indoc! { r#"
            int add(int a, int b) { return a + b; }
            void mutate(int ref value) { value = 1; }
        "# },
            )],
        );
        for (call, expected) in [
            ("call_inherited(bad)", "call_inherited: int is not a string"),
            (
                "call_inherited(\"add\", 1)",
                "incorrect argument count in call to `add`",
            ),
            (
                "call_inherited(\"add\", 1, 2, 3)",
                "incorrect argument count in call to `add`",
            ),
            (
                "call_inherited(\"add\", \"bad\", 2)",
                "unexpected argument type to `add`",
            ),
            (
                "call_inherited(\"mutate\", 0)",
                "takes argument 1 by reference; call it directly",
            ),
        ] {
            let code = format!("inherit \"/base\"; void create() {{ mixed bad = 5; {call}; }}");
            let error = fails_with(temp_lib_config(&root), "", &[], &code).await;
            assert!(error.contains(expected), "{call}: {error}");
        }
    }

    #[tokio::test]
    async fn suspension_and_caught_errors_leave_the_caller_ready_for_another_call() {
        let root = lib_holding(
            "call-inherited-suspension",
            &[
                ("payload.c", ""),
                (
                    "base.c",
                    indoc! { r#"
                object loaded() { return load_object("/payload"); }
                void fail() { throw("broken hook"); }
                int okay() { return 42; }
            "# },
                ),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            indoc! { r#"
            inherit "/base";
            mixed *create() {
                object ob = call_inherited("loaded");
                mixed err = catch(call_inherited("fail"));
                return ({ file_name(ob), err, call_inherited("okay") });
            }
        "# },
        )
        .await;
        assert_eq!(
            result,
            vec![s("/payload"), s("broken hook"), LpcRef::from(42)]
        );
    }

    #[tokio::test]
    async fn efun_pointers_and_closures_keep_the_owners_inherit_scope() {
        let root = lib_holding(
            "call-inherited-pointers",
            &[
                ("base.c", "protected int foo(int n) { return n + 1; }"),
                (
                    "middle.c",
                    indoc! { r#"
                inherit "/base" parent;
                int foo(int n) { return n + 100; }
                function pointer() { return &call_inherited("parent::foo"); }
                function closure() { return (: call_inherited("foo", $1) :); }
            "# },
                ),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "",
            &[("/target.c", "inherit \"/middle\" parent;")],
            indoc! { r#"
                mixed *create() {
                    function p = "/target"->pointer();
                    function c = "/target"->closure();
                    return ({ p(5), c(6) });
                }
            "# },
        )
        .await;
        assert_eq!(result, vec![LpcRef::from(6), LpcRef::from(7)]);
    }

    #[tokio::test]
    async fn host_loaded_clones_run_inherited_initializers_on_their_own_state() {
        let root = lib_holding(
            "call-inherited-clones",
            &[
                ("base.c", "int count; int bump() { return ++count; }"),
                (
                    "middle.c",
                    indoc! { r#"
                inherit "/base";
                int initial = call_inherited("bump");
            "# },
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            LpcPath::new_server(root.join("target.c")),
            indoc! { r#"
            inherit "/middle";
            int probe() { return call_inherited("bump"); }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/main.c",
                indoc! { r#"
            int create() {
                object clone = clone_object("/target");
                return clone->probe() == 2 && clone->probe() == 3
                    && find_object("/target")->probe() == 2;
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn errors_from_an_existing_hook_propagate_to_the_caller() {
        let root = lib_holding(
            "call-inherited-throw",
            &[("base.c", "void foo() { throw(\"hook failed\"); }")],
        );
        let error = fails_with(
            temp_lib_config(&root),
            "",
            &[],
            indoc! { r#"
            inherit "/base";
            void create() { call_inherited("foo"); }
        "# },
        )
        .await;
        assert!(error.contains("hook failed"), "{error}");
    }
}
