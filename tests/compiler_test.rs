#[allow(dead_code)]
mod support;

use std::sync::Arc;

use indoc::indoc;
use lpc_rs::{
    compiler::{Compiler, CompilerBuilder},
    interpreter::{CommittedReader, lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
};
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::RegisterSize;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use lpc_rs_utils::lpc_string::LpcString;

use crate::support::{run_prog, test_config, test_config_builder};

fn default_compiler() -> Compiler {
    let config: Arc<Config> = ConfigBuilder::default()
        .lib_dir("tests/fixtures/code")
        .build()
        .unwrap()
        .into();
    CompilerBuilder::default().config(config).build().unwrap()
}

mod callable_visibility {
    use lpc_rs::{
        compile_time_config::MAX_CALL_STACK_SIZE,
        interpreter::{process::Process, task::task_template::TaskTemplate},
        util::process_builder::process_insert_and_initialize_program,
    };
    use lpc_rs_core::null_mode::NullMode;

    use super::*;

    async fn result(code: &str, mode: NullMode) -> LpcRef {
        let config: Arc<Config> = test_config_builder()
            .null_mode(mode)
            .build()
            .unwrap()
            .into();
        let program = Compiler::new(config.clone())
            .compile_string("/child.c", code)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .program;
        let vm = Vm::new(config);
        process_insert_and_initialize_program::<MAX_CALL_STACK_SIZE>(
            Arc::new(Process::new(program)),
            TaskTemplate::from(vm.global_state.clone()),
        )
        .await
        .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
        .context
        .result()
        .unwrap()
    }

    #[tokio::test]
    async fn hidden_fields_do_not_displace_inherited_methods() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for expression in ["token()", "evaluate(&token(1))", "evaluate((: token() :))"] {
                let code = format!(
                    "inherit \"/callable_private\"; int create() {{ return {expression}; }}"
                );
                let expected = if expression == "evaluate(&token(1))" {
                    43
                } else {
                    42
                };
                assert_eq!(result(&code, mode).await, LpcRef::from(expected));
            }
        }
    }

    #[tokio::test]
    async fn hidden_callable_fields_cannot_be_called_or_partially_applied() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for name in ["hidden", "hidden_mixed"] {
                for expression in [
                    format!("{name}()"),
                    format!("evaluate(&{name}())"),
                    format!("evaluate((: {name}() :))"),
                ] {
                    let code = format!(
                        "inherit \"/callable_private\"; int create() {{ return {expression}; }}"
                    );
                    let compiler =
                        Compiler::new(test_config_builder().null_mode(mode).build().unwrap());
                    let error = compiler.compile_string("/child.c", code).await.unwrap_err();
                    assert!(
                        error.to_string().contains(&format!(
                            "private variable `{name}` accessed outside of its file"
                        )),
                        "{expression}: {error}"
                    );
                }
            }
        }
    }

    #[tokio::test]
    async fn inherited_methods_keep_access_to_their_own_private_callbacks() {
        let code = r#"
            inherit "/callable_private";
            int create() { return parent_private() + parent_partial() + parent_closure()(); }
        "#;
        assert_eq!(result(code, NullMode::Distinct).await, LpcRef::from(1281));
    }

    #[tokio::test]
    async fn visible_callable_globals_win_in_either_inherit_order() {
        for parents in [
            "inherit \"/callable_private\"; inherit \"/callable_visible\";",
            "inherit \"/callable_visible\"; inherit \"/callable_private\";",
        ] {
            let code =
                format!("{parents} int create() {{ return shared() + evaluate(&shared()); }}");
            assert_eq!(result(&code, NullMode::Distinct).await, LpcRef::from(14));
        }
    }

    #[tokio::test]
    async fn local_callbacks_shadow_methods_and_capture_in_closures() {
        for type_ in ["function", "mixed"] {
            let code = format!(
                r#"
                inherit "/callable_private";
                int create() {{
                    {type_} token = (: $1 * 2 :);
                    function direct = (: token(3) :);
                    function partial = (: evaluate(&token(4)) :);
                    return token(2) + direct() + partial();
                }}
            "#
            );
            assert_eq!(result(&code, NullMode::Distinct).await, LpcRef::from(18));
        }
    }

    #[tokio::test]
    async fn qualified_and_receiver_calls_ignore_callable_variables() {
        let code = r#"
            inherit "/callable_private" base;
            int create() {
                function token = (: 999 :);
                return ::token() + base::token() + callable_private::token()
                    + this_object()->token() + evaluate(&(this_object())->token());
            }
        "#;
        assert_eq!(result(code, NullMode::Distinct).await, LpcRef::from(210));
    }

    #[tokio::test]
    async fn method_signatures_are_checked_when_a_same_named_field_is_hidden() {
        for (expression, diagnostic) in [
            ("token(1, 2)", "incorrect argument count"),
            ("token(\"wrong\")", "unexpected argument type"),
            ("token()", "invalid return type"),
        ] {
            let code =
                format!("inherit \"/callable_private\"; string f() {{ return {expression}; }}");
            let error = default_compiler()
                .compile_string("/child.c", code)
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains(diagnostic),
                "{expression}: {error}"
            );
        }
    }

    #[tokio::test]
    async fn callable_variables_do_not_use_shadowed_function_signatures() {
        for type_ in ["function", "mixed"] {
            let code = format!(
                r#"
                private void callback(int ref n) {{ n++; }}
                {type_} callback = (: $1 + $2 :);
                int create() {{ return callback(2, 3) + callback(4, 5); }}
            "#
            );
            assert_eq!(result(&code, NullMode::Distinct).await, LpcRef::from(14));
        }
    }

    #[tokio::test]
    async fn callbacks_named_after_output_efuns_keep_value_arguments() {
        let code = r#"
            class Value { int n; }
            int create() {
                function sscanf = (: $3 + 1 :);
                int *values = ({ 3 });
                class Value value = new(class Value, n: 4);
                return sscanf("", "", values[0]) + sscanf("", "", value.n);
            }
        "#;
        assert_eq!(result(code, NullMode::Distinct).await, LpcRef::from(9));
    }

    #[tokio::test]
    async fn visible_callbacks_can_shadow_private_inherited_methods() {
        let code = r#"
            inherit "/std/object";
            function private_function = (: 12 :);
            int create() { return private_function() + evaluate(&private_function()); }
        "#;
        assert_eq!(result(code, NullMode::Distinct).await, LpcRef::from(24));
    }

    #[tokio::test]
    async fn qualified_calls_keep_the_method_return_type() {
        for expression in [
            "::token()",
            "callable_private::token()",
            "efun::sizeof(\"x\")",
        ] {
            let code = format!(
                r#"
                inherit "/callable_private";
                string f() {{
                    function token = (: "text" :);
                    function sizeof = (: "text" :);
                    return {expression};
                }}
            "#
            );
            let error = default_compiler()
                .compile_string("/child.c", code)
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains("invalid return type int"),
                "{expression}: {error}"
            );
        }
    }

    #[tokio::test]
    async fn private_imports_allow_callbacks_in_the_importing_source() {
        let code = r#"
            private inherit "/callable_visible";
            int create() { return shared() + evaluate(&shared()); }
        "#;
        assert_eq!(result(code, NullMode::Distinct).await, LpcRef::from(14));
    }

    #[tokio::test]
    async fn callback_shadowing_does_not_allow_reference_arguments() {
        let error = default_compiler()
            .compile_string(
                "/child.c",
                r#"
            void callback(int ref n) { n++; }
            void create() {
                function callback = (: $1 :);
                int n = 1;
                callback(ref n);
            }
        "#,
            )
            .await
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("a function pointer cannot take an argument by reference"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn trailing_call_commas_preserve_argument_checks() {
    for (definition, call, diagnostic) in [
        (
            "int f(int a, int b) { return a+b; }",
            "f(1,)",
            "incorrect argument count",
        ),
        (
            "int f(int a) { return a; }",
            "f(1,2,)",
            "incorrect argument count",
        ),
        (
            "int f(int a) { return a; }",
            r#"f("wrong",)"#,
            "unexpected argument type",
        ),
        (
            "void f(int ref a) { a++; }",
            "f(n,)",
            "must be passed by reference",
        ),
    ] {
        let code = format!("{definition} void create() {{ int n; {call}; }}");
        let error = default_compiler()
            .compile_string("foo.c", code)
            .await
            .unwrap_err();
        assert!(error.to_string().contains(diagnostic), "{call}: {error}");
    }
}

#[tokio::test]
async fn unnamed_prototypes_allow_forward_calls_to_named_definitions() {
    let code = indoc! { r#"
        #pragma strict_types
        class Record { int value; }
        void increment(int ref);
        int add(int, int);
        int total(int, int *...);
        int read(class Record);

        int create() {
            int n = 2;
            increment(ref n);
            return add(n) + total(1, 2, 3) + read(new(class Record, value: 7));
        }

        void increment(int ref value) { value++; }
        int add(int left, int right = 4) { return left + right; }
        int total(int first, int *rest...) { return first + rest[0] + rest[1]; }
        int read(class Record record) { return record.value; }
    "# };
    let task = run_prog(code).await;
    assert_eq!(task.context.result().unwrap(), LpcRef::from(20));
}

#[tokio::test]
async fn unnamed_prototypes_do_not_declare_missing_functions() {
    let error = default_compiler()
        .compile_string("foo.c", "void missing(int); void create() { missing(1); }")
        .await
        .unwrap_err();
    assert!(error.to_string().contains("unknown function `missing`"));
}

#[tokio::test]
async fn unnamed_prototypes_preserve_definition_argument_counts() {
    for call in ["f()", "f(1, 2)"] {
        let code = format!("void f(int); void create() {{ {call}; }} void f(int n) {{}}");
        let error = default_compiler()
            .compile_string("foo.c", code)
            .await
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("incorrect argument count in call to `f`"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn unnamed_prototypes_preserve_definition_argument_types() {
    for (prototype, definition, argument) in [
        ("int", "int n", r#""wrong""#),
        ("int *", "int *values", "1"),
        ("class Record", "class Record record", "new(class Other)"),
        ("int, string", "int n", r#""wrong""#),
    ] {
        let code = format!(
            "class Record {{ int n; }} class Other {{ int n; }}
             void f({prototype}); void create() {{ f({argument}); }} void f({definition}) {{}}"
        );
        let error = default_compiler()
            .compile_string("foo.c", code)
            .await
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("unexpected argument type to `f`"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn unnamed_prototypes_preserve_definition_reference_requirements() {
    for (prototype, definition, argument, message) in [
        ("int ref", "int ref n", "n", "must be passed by reference"),
        (
            "int",
            "int n",
            "ref n",
            "does not take argument 1 by reference",
        ),
        ("int", "int ref n", "n", "must be passed by reference"),
    ] {
        let code = format!(
            "void f({prototype}); void create() {{ int n = 1; f({argument}); }} void f({definition}) {{}}"
        );
        let error = default_compiler()
            .compile_string("foo.c", code)
            .await
            .unwrap_err();
        assert!(error.to_string().contains(message), "{error}");
    }
}

#[tokio::test]
async fn unnamed_prototype_signatures_do_not_override_definitions() {
    let code = indoc! { r#"
        string f(string, object);
        int create() { return f(5); }
        int f(int value) { return value + 1; }
    "# };
    let task = run_prog(code).await;
    assert_eq!(task.context.result().unwrap(), LpcRef::from(6));
}

#[tokio::test]
async fn minimum_int_macro_compiles_and_runs() {
    for minus in ["-", "−"] {
        let code = format!(
            "#define MININT ({minus}9223372036854775808)\n\
             #if MININT != (-9223372036854775807 - 1)\n\
             #error incorrect minimum integer\n\
             #endif\n\
             int create() {{ return MININT; }}"
        );
        let task = run_prog(&code).await;
        assert_eq!(task.context.result().unwrap(), LpcRef::Int(LpcInt::MIN));
    }
}

#[tokio::test]
async fn errors_on_max_inherit_depth() {
    let code = r#"inherit "/std/inherit_loop1";"#;

    let compiler = default_compiler();
    let result = compiler.compile_string("foo.c", code).await;

    assert_eq!(
        result.unwrap_err().to_string(),
        "maximum inheritance depth reached"
    );
}

#[tokio::test]
async fn test_inheritance() {
    let code = indoc! { r##"
        inherit "/parent";

        void create() {
            dump("child create");
            dump(get_vars());
            grandparent_method();
            parent_method();
            overridden_method();
        }
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let prog = &proc.initial_program();

    assert_eq!(prog.num_globals, 5);
}

#[tokio::test]
async fn inherit_macro_string_expression_compiles_and_runs() {
    let code = indoc! { r#"
        #define ARMOUR "/std"
        inherit (ARMOUR + "/object");

        string create() {
            return public_function();
        }
    "# };

    let task = run_prog(code).await;
    assert_eq!(
        task.context.result().unwrap().to_string(),
        "/std/object public"
    );
}

#[tokio::test]
async fn inherit_nested_macro_expression_preserves_namespace() {
    let code = indoc! { r#"
        #define ROOT "/"
        #define ARMOUR (ROOT + "std")
        #define BASE(dir) (dir + "/object")
        inherit BASE(ARMOUR) armour;

        string create() {
            return armour::public_function();
        }

        string public_function() {
            return "child";
        }
    "# };

    let task = run_prog(code).await;
    assert_eq!(
        task.context.result().unwrap().to_string(),
        "/std/object public"
    );
}

#[tokio::test]
async fn inherit_undefined_macro_reports_constant_string_error() {
    let compiler = default_compiler();
    let error = compiler
        .compile_string("foo.c", r#"inherit (ARMOUR + "/base");"#)
        .await
        .unwrap_err();

    assert_eq!(
        error.to_string(),
        "inherit path must be a constant string expression"
    );
}

#[tokio::test]
async fn test_dynamic_receiver() {
    let code = indoc! { r##"
        void create() {
            function f = &->tacos();
            dump(f(this_object(), "assmar"));
        }

        string tacos(string s) {
            dump(s);
            return "tacos";
        }
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let prog = &proc.initial_program();

    assert_eq!(prog.num_globals, 0);
}

#[tokio::test]
async fn test_duffs_device() {
    let code = indoc! { r##"
        int *copy(int *array, int count) {
            int n = (count + 7) / 8, idx = 0;
            int *result = ({ 0, 0, 0, 0, 0, 0, 0, 0 });

            switch (count % 8) {
            case 0: do { result[0] = array[0];
            case 7:      result[7] = array[7];
            case 6:      result[6] = array[6];
            case 5:      result[5] = array[5];
            case 4:      result[4] = array[4];
            case 3:      result[3] = array[3];
            case 2:      result[2] = array[2];
            case 1:      result[1] = array[1];
                    } while (--n > 0);
            }

            return result;
        }

        mixed a = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
        mixed b = copy(a, 6);
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let b = &ctx.global_state.committed_global(proc, 1u16);

    let LpcRef::Array(cell) = b else {
        panic!("expected array");
    };
    let arr = ctx
        .global_state
        .committed_array(cell.id)
        .expect("array payload committed");
    assert_eq!(
        &*arr,
        &[
            LpcRef::Int(LpcInt(0)),
            LpcRef::Int(LpcInt(2)),
            LpcRef::Int(LpcInt(3)),
            LpcRef::Int(LpcInt(4)),
            LpcRef::Int(LpcInt(5)),
            LpcRef::Int(LpcInt(6)),
            LpcRef::Int(LpcInt(7)),
            LpcRef::Int(LpcInt(0)),
        ]
        .to_vec()
    );
}

#[tokio::test]
async fn test_closures() {
    let code = indoc! { r##"
        function f = (:
            function f = &->tacos(,);
            function g = (: f($1, $2, $3) :);
            return g(this_object(), 4, "crema");
        :);
        string str;

        void create() {
            str = f();
        }

        string tacos(int i, string t) {
            return "I'll take " + i + " tacos with " + t + " on the side, por favor.";
        }
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();

    let count = ctx.global_state.global_slot_count(proc);
    let last = ctx
        .global_state
        .committed_global(proc, (count - 1) as RegisterSize);
    assert_eq!(count, 2);
    assert_eq!(
        last.to_string(),
        "I'll take 4 tacos with crema on the side, por favor.".to_string()
    )
}

#[tokio::test]
async fn test_multi_dimensional_arrays() {
    let code = indoc! { r##"
        int *a = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
        mixed *b = ({ 9, 10, 11, 12, 13, ({ "14a", "14b", "14c" }), 15, 16 });
        int *c = ({ 17, 18, 19, 20, 21, 22, 23, 24 });
        int *d = ({ 25, 26, 27, 28, 29, 30, 31, 32 });

        mixed *arr = ({ a, b, c, d });

        string *x = arr[1][5][1..];
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let count = ctx.global_state.global_slot_count(proc);
    let x_ref = ctx
        .global_state
        .committed_global(proc, (count - 1) as RegisterSize);
    let LpcRef::Array(arr) = x_ref else {
        panic!("this shouldn't be reachable.");
    };
    let lpc_array = ctx
        .global_state
        .committed_array(arr.id)
        .expect("array payload committed");

    let vals = lpc_array
        .array
        .iter()
        .map(|a| {
            let LpcRef::String(s) = a else {
                panic!("this shouldn't be reachable.");
            };
            (**s).clone()
        })
        .collect::<Vec<_>>();

    assert_eq!(vals, vec![LpcString::from("14b"), LpcString::from("14c")]);
}

#[tokio::test]
async fn test_positional_vars_into_argv() {
    let code = indoc! { r##"
        mixed create() {
            function f = (: [...] $2 :);
            return f(666, 777);
        }
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    assert_eq!(LpcRef::Int(LpcInt(777)), ctx.result().unwrap());
}

#[tokio::test]
async fn test_inherited_create_called_when_not_overridden() {
    let vm = Vm::new(test_config());
    let child = indoc! { r#"
        inherit "/create_parent";
        inherit "/create_parent2";
    "# };

    let child_ctx = vm
        .initialize_string(child, "child_of_create_parents.c")
        .await
        .inspect_err(|e| {
            e.emit_diagnostics();
        })
        .unwrap();

    let init = child_ctx
        .process()
        .initial_program()
        .initializer
        .clone()
        .unwrap();

    // parent2's create is inherited last, so it wins.
    let inst = &init.instructions;
    assert!(
        matches!(
            inst[inst.len() - 2],
            Instruction::Call(name, _) if name == "create__v__/create_parent2.c__pb__"
        ),
        "{inst:?}"
    );
    assert_eq!(inst[inst.len() - 1], Instruction::Ret);
}

#[tokio::test]
async fn test_calls_simul_efuns() {
    let config = test_config_builder()
        .simul_efun_file("/secure/simul_efuns.c")
        .build()
        .unwrap();

    let vm = Vm::new(config);
    vm.initialize_simul_efuns()
        .await
        .expect("no simul efuns?")
        .expect("init error");

    let code = indoc! { r##"
        mixed create() {
            return simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("this is a simul_efun: cool!", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        mixed create() {
            return simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("local simul_efun: cool!", val.to_string());

    let code = indoc! { r##"
        mixed create() {
            function f = &simul_efun("pointed!");
            return f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("this is a simul_efun: pointed!", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        mixed create() {
            function f = &simul_efun("pointed!");
            return f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("local simul_efun: pointed!", val.to_string());
}

#[tokio::test]
async fn test_nomask_children() {
    let child = indoc! { r#"
        inherit "/nomask_parent";

        void noooo() {
            write("this shouldn't work");
        }
    "# };

    let vm = Vm::new(test_config());
    let child_proc = vm.initialize_process_from_code("/child.c", child).await;

    assert_eq!(
        child_proc.unwrap_err().to_string(),
        "attempt to redefine nomask function `noooo`"
    );
}

#[tokio::test]
async fn test_nomask_grandchildren() {
    let grandchild = indoc! { r#"
        inherit "/nomask_child";

        void noooo() {
            write("this shouldn't work");
        }
    "# };

    let vm = Vm::new(test_config());
    let grandchild_proc = vm
        .initialize_process_from_code("/grandchild.c", grandchild)
        .await;
    assert_eq!(
        grandchild_proc.unwrap_err().to_string(),
        "attempt to redefine nomask function `noooo`"
    );
}
