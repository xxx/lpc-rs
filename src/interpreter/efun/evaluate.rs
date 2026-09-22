use lpc_rs_errors::Result;

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    stm::TxnHandle,
};

/// Call a function with the remaining arguments, or return a non-function unchanged.
pub fn evaluate<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if context.arg_count() == 0 {
        return Err(context.runtime_error("evaluate: expected at least one argument"));
    }
    let LpcRef::Function(ptr) = context.arg(0) else {
        context.return_efun_result(context.arg(0).clone());
        return Ok(());
    };
    let callee = Callee::Pointer {
        ptr: ptr.clone(),
        args: (1..context.arg_count())
            .map(|i| context.arg(i).clone())
            .collect(),
    };
    context.continue_with(Box::new(Evaluate(Some(callee))));
    Ok(())
}

#[derive(Debug, Clone)]
struct Evaluate(Option<Callee>);

impl Continuation for Evaluate {
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
    use lpc_rs_asm::instruction::Instruction;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        compiler::CompilerBuilder,
        interpreter::{
            lpc_ref::LpcRef,
            task::{
                apply_function::apply_function_by_name, eval_loop::Slice,
                task_template::TaskTemplate,
            },
            tests::{fails, run, run_with},
            vm::Vm,
        },
        test_support::{
            PERMISSIVE_MASTER, lib_holding, run_prog, strings_of, task_at, temp_lib_config,
            test_config, try_run_prog,
        },
    };

    #[tokio::test]
    async fn non_functions_keep_their_values_types_and_container_identities() {
        let code = r#"
            class record { int n; }
            int create() {
                mixed *array = ({ 1 });
                mapping map = ([ "n": 2 ]);
                class record record = new(class record, n: 3);
                mixed *values = ({ 0, 17, -2, 1.5, "Name: ", to_bytes("bytes", "UTF-8"),
                    array, map, record, this_object() });
                foreach (mixed value : values) {
                    mixed result = evaluate(value);
                    if (result != value || typeof(result) != typeof(value)) return 0;
                }
                mixed *same = evaluate(array);
                same[0] = 9;
                return array[0] == 9;
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn unused_arguments_are_evaluated_once_in_source_order() {
        let code = r#"
            int order;
            mixed mark(int n, mixed value) { order = order * 10 + n; return value; }
            mixed *create() {
                mixed result = evaluate(mark(1, "value"), mark(2, 99), mark(3, (: 1/0 :)));
                return ({ result, order });
            }
        "#;
        assert_eq!(strings_of(code).await, ["value", "123"]);
    }

    #[tokio::test]
    async fn callbacks_receive_defaults_bound_holes_and_spread_arguments() {
        let code = r#"
            int add(int a, int b = 4) { return a + b; }
            int digits(int a, int b, int c) { return a + 10*b + 100*c; }
            int tail(int n, mixed rest...) { return n + sizeof(rest); }
            mixed *create() {
                return ({ evaluate(&add(), 3), evaluate(&digits(, 2), ({ 1, 3 })...),
                    evaluate(&tail(), 10, "a", 1, ([])),
                    evaluate(({ &digits(1), 2, 3 })...) });
            }
        "#;
        assert_eq!(strings_of(code).await, ["7", "321", "13", "321"]);
    }

    #[tokio::test]
    async fn implicit_and_explicit_captures_keep_their_native_behavior() {
        let code = r#"
            int create() {
                int n = 1;
                function shared = (: ++n :);
                function saved = (: $(n) + $1 :);
                n = 10;
                return evaluate(shared) == 11 && n == 11 && evaluate(saved, 4) == 5;
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn callback_results_are_returned_without_copying_or_evaluating_again() {
        let code = r#"
            int calls;
            mixed produce(mixed value) { calls++; return value; }
            int create() {
                function callback = (: 1 / 0 :);
                mixed *array = ({ 3 });
                return evaluate(&produce(), callback) == callback &&
                    evaluate(&produce(), array) == array && calls == 2;
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn void_callbacks_return_zero() {
        let code = r#"
            int calls;
            void notify() { calls++; }
            int create() { return evaluate(&notify()) == 0 && calls == 1; }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn efun_pointers_can_evaluate_values_and_other_efun_pointers() {
        let code = r#"
            mixed *create() {
                function f = &evaluate();
                function bound = &evaluate("literal");
                return ({ f((: $1 + 1 :), 4), bound(1, 2),
                    evaluate(&strindex(), "abcabc", "bc", -1),
                    evaluate(&evaluate(), (: 7 :)) });
            }
        "#;
        assert_eq!(strings_of(code).await, ["5", "literal", "4", "7"]);
    }

    #[tokio::test]
    async fn nested_callback_efuns_keep_each_continuation_separate() {
        let code = r#"
            mixed *create() {
                mixed *values = map(({ 1, (: 2 :), "prompt" }), &evaluate());
                return values + ({ evaluate(&reduce(), ({ 2, 3, 4 }), &operator(*)()) });
            }
        "#;
        assert_eq!(strings_of(code).await, ["1", "2", "prompt", "24"]);
    }

    #[tokio::test]
    async fn dynamic_receivers_load_and_missing_methods_keep_their_normal_result() {
        let root = lib_holding(
            "evaluate-loading",
            &[("target.c", "int add(int n) { return n + 1; }")],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
                mixed *create() {
                    function method = &->add(41);
                    int answer = evaluate(method, "/target");
                    object loaded = evaluate(&load_object(), "/target");
                    return ({ answer,
                        evaluate(&(loaded)->add(), 8), evaluate(&->missing(), loaded) });
                }
            "#,
        )
        .await;
        assert_eq!(result, vec![LpcRef::from(42), 9.into(), 0.into()]);
    }

    #[tokio::test]
    async fn callback_context_matches_a_direct_pointer_call() {
        let result = run(
            "",
            &[(
                "/target.c",
                r#"
            mixed *context() {
                return ({ this_object(), previous_object(), this_player(),
                    calling_function(), calling_program() });
            }
            function callback() { return &context(); }
        "#,
            )],
            r#"
            mixed *create() {
                enable_commands();
                set_this_player(this_object());
                function f = "/target"->callback();
                mixed *direct = f();
                mixed *indirect = evaluate(f);
                foreach (int i : ({ 0, 1, 2, 3, 4 })) {
                    if (direct[i] != indirect[i]) return ({ 0 });
                }
                return ({ indirect[0] == find_object("/target"),
                    indirect[1] == this_object(), indirect[2] == this_object() });
            }
        "#,
        )
        .await;
        assert_eq!(result, vec![LpcRef::from(1), 1.into(), 1.into()]);
    }

    #[tokio::test]
    async fn caught_errors_keep_prior_writes_and_allow_another_evaluation() {
        let code = r#"
            int calls;
            void fail() { calls++; throw("callback failed"); }
            mixed *create() {
                mixed error = catch(evaluate(&fail()));
                return ({ error, calls, evaluate((: 42 :)) });
            }
        "#;
        assert_eq!(strings_of(code).await, ["callback failed", "1", "42"]);
    }

    #[tokio::test]
    async fn callbacks_share_the_task_stack_and_yield_to_its_budget() {
        let vm = Vm::new(test_config());
        let (mut task, _live) = task_at(
            &vm,
            "int add(int n) { return n + 1; } int create() { return evaluate(&add(), 41); }",
            |at| matches!(at, Instruction::CallEfun(..)),
        )
        .await;
        assert!(matches!(task.run_slice(&mut 1).unwrap(), Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let callee = task.stack.current_frame().unwrap();
        assert_eq!(callee.function.name(), "add");
        assert_eq!(callee.registers[1], LpcRef::from(41));
        assert!(task.stack.get(0).unwrap().pending.is_some());
        assert!(matches!(task.run_slice(&mut 100).unwrap(), Slice::Halt));
        assert_eq!(task.result(), Some(LpcRef::from(42)));
    }

    #[tokio::test]
    async fn uncaught_callback_errors_abort_all_of_the_callers_writes() {
        let vm = Vm::new(test_config());
        let task = vm
            .initialize_process_from_code(
                "/evaluate_abort.c",
                r#"
            int n = 1;
            int *items = ({ 2 });
            function saved = (: n :);
            void fail() { evaluate(saved); items[0] = 20; throw("abort callback"); }
            void invoke() { n = 10; saved = (: ++n :); evaluate(&fail()); }
            int read() { return n == 1 && items[0] == 2 && evaluate(saved) == 1; }
        "#,
            )
            .await
            .unwrap();
        let process = task.context.process().clone();
        drop(task);
        let template = TaskTemplate::from(vm.global_state.clone());
        let error = apply_function_by_name("invoke", &[], process.clone(), template.clone(), None)
            .await
            .unwrap()
            .unwrap_err();
        assert!(error.to_string().contains("abort callback"));
        let result = apply_function_by_name("read", &[], process, template, None)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(result, LpcRef::from(1));
    }

    #[tokio::test]
    async fn callbacks_resume_after_loading_and_catch_async_errors() {
        let root = lib_holding(
            "evaluate-callback-loading",
            &[
                ("target.c", "int answer() { return 42; }"),
                ("broken.c", "void create() { throw(\"load failed\"); }"),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            PERMISSIVE_MASTER,
            &[],
            r#"
            int answer() { return load_object("/target")->answer(); }
            void fail() { "/broken"->answer(); }
            mixed *create() {
                int answer = evaluate(&answer());
                mixed error = catch(evaluate(&fail()));
                return ({ answer, error, evaluate((: 7 :)) });
            }
        "#,
        )
        .await;
        assert_eq!(
            result,
            vec![
                LpcRef::from(42),
                crate::interpreter::tests::s("load failed"),
                7.into()
            ]
        );
    }

    #[tokio::test]
    async fn simul_efun_pointers_run_in_the_simul_efun_object() {
        let root = lib_holding(
            "evaluate-simul",
            &[(
                "secure/simul_efuns.c",
                r#"
            string me(string prefix) { return prefix + file_name(this_object()); }
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
            string create() { return evaluate(&me("bound:")); }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(
            task.result().unwrap().as_str(),
            Some("bound:/secure/simul_efuns")
        );
    }

    #[tokio::test]
    async fn callback_argument_checks_match_direct_pointer_calls() {
        let results = strings_of(
            r#"
            int add(int a, int b) { return a + b; }
            mixed *create() {
                function f = &add();
                return ({ evaluate(f, 1), f(1), evaluate(f, 1, 2, 3), f(1, 2, 3) });
            }
        "#,
        )
        .await;
        assert_eq!(results, ["1", "1", "3", "3"]);
        for (call, expected) in [
            ("evaluate(&add(), 1, \"bad\")", "unexpected argument type"),
            ("evaluate(&mutate(), 1)", "by reference"),
        ] {
            let code = format!(
                r#"
                int add(int a, int b) {{ return a + b; }}
                void mutate(int ref n) {{ n++; }}
                mixed create() {{ return {call}; }}
            "#
            );
            let error = try_run_prog(&code).await.expect_err(call).to_string();
            assert!(error.contains(expected), "{call}: {error}");
        }
    }

    #[tokio::test]
    async fn destructed_callback_owners_raise_the_native_error() {
        let error = fails(
            PERMISSIVE_MASTER,
            &[(
                "/target.c",
                r#"
            int answer() { return 42; }
            function callback() { return &answer(); }
        "#,
            )],
            r#"
            mixed *create() {
                object ob = find_object("/target");
                function callback = ob->callback();
                destruct(ob);
                return ({ evaluate(callback) });
            }
        "#,
        )
        .await;
        assert!(error.contains("destructed"), "{error}");
    }

    #[tokio::test]
    async fn a_missing_first_argument_is_an_error_even_through_spreads_and_pointers() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        let error = compiler
            .compile_string("/evaluate_arity.c", "void create() { evaluate(); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("incorrect argument count in call to `evaluate`"),
            "{error}"
        );
        for expression in ["evaluate(({})...)", "f()"] {
            let code =
                format!("mixed create() {{ function f = &evaluate(); return {expression}; }}");
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                error.contains("evaluate: expected at least one argument"),
                "{error}"
            );
        }
    }
}
