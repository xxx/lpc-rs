//! A call from a parent's code reaches the object's most-derived definition
//! of the name; `::` reaches the parent's own (CD, LDMud, FluffOS, DGD;
//! survey local/bench-drivers/ovprobe).

use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
    test_support::lib_holding,
};

const MASTER: &str = "int valid_load(mixed a, mixed b, mixed c, mixed d) { return 1; } \
    int valid_inherit(mixed a, mixed b, mixed c, mixed d) { return 1; }";

/// `f()` calls `g()` and `x()` reads what `g` set.
const PARENT: &str = "int x;\nvoid g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n";

mod shared_code {
    use std::sync::Arc;

    use super::*;
    use crate::{
        interpreter::{
            process::Process,
            task::{apply_function::apply_function, task_template::TaskTemplate},
        },
        test_support::temp_lib_config,
    };

    async fn call(vm: &Vm, process: &Arc<Process>, name: &str) -> LpcRef {
        apply_function(
            process
                .initial_program()
                .lookup_function(name)
                .unwrap()
                .clone(),
            &[],
            TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone()),
            None,
        )
        .await
        .unwrap()
    }

    #[tokio::test]
    async fn reordered_diamonds_share_code_and_keep_independent_cells() {
        let root = lib_holding(
            "shared-inheritance",
            &[
                ("base.c", "int common = 1;"),
                (
                    "left.c",
                    r#"
                inherit "/base";
                int left = 10;
                void bump(int ref value) { value++; }
                void exercise() { bump(ref common); bump(ref left); }
                function later() { return (: common += 2; left += 3; return common + left; :); }
                int total() { return common + left; }
            "#,
                ),
                ("right.c", "inherit \"/base\"; int right = 100;"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let create = "void create() { exercise(); function f = later(); f(); }";
        let a = vm
            .initialize_process_from_code(
                "/a.c",
                format!("inherit \"/left\"; inherit \"/right\"; {create}"),
            )
            .await
            .unwrap()
            .context
            .process;
        let b = vm
            .initialize_process_from_code(
                "/b.c",
                format!("inherit \"/right\"; inherit \"/left\"; {create}"),
            )
            .await
            .unwrap()
            .context
            .process;
        for name in ["bump", "exercise", "later", "total"] {
            assert!(
                Arc::ptr_eq(
                    &a.initial_program().lookup_function(name).unwrap().code,
                    &b.initial_program().lookup_function(name).unwrap().code
                ),
                "{name}"
            );
        }
        assert_ne!(
            a.initial_program().global_variables["left"].location,
            b.initial_program().global_variables["left"].location
        );
        assert_eq!(call(&vm, &a, "total").await, LpcRef::from(18));
        assert_eq!(call(&vm, &b, "total").await, LpcRef::from(18));
        call(&vm, &a, "exercise").await;
        assert_eq!(call(&vm, &a, "total").await, LpcRef::from(20));
        assert_eq!(call(&vm, &b, "total").await, LpcRef::from(18));
        assert_ne!(a.initial_program().clones.id, b.initial_program().clones.id);
        let roots = b.world_var_ids();
        assert_eq!(
            roots.len(),
            roots.iter().collect::<std::collections::HashSet<_>>().len()
        );
        assert!(a.world_var_ids().iter().all(|id| !roots.contains(id)));
    }

    #[tokio::test]
    async fn source_and_header_edits_preserve_live_descendants() {
        let root = lib_holding(
            "shared-inheritance-edits",
            &[
                ("value.h", "#define VALUE 1\n"),
                (
                    "parent.c",
                    "#include \"/value.h\"\nint value() { return VALUE; }\n",
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let load = |name| vm.initialize_process_from_code(name, "inherit \"/parent\";");
        let old = load("/old.c").await.unwrap().context.process;
        let equal = load("/equal.c").await.unwrap().context.process;
        assert!(Arc::ptr_eq(
            &old.initial_program().lookup_function("value").unwrap().code,
            &equal
                .initial_program()
                .lookup_function("value")
                .unwrap()
                .code
        ));
        std::fs::write(root.join("value.h"), "#define VALUE 2\n").unwrap();
        let changed_header = load("/header.c").await.unwrap().context.process;
        std::fs::write(root.join("parent.c"), "int value() { return 3; }\n").unwrap();
        let changed_parent = load("/parent_edit.c").await.unwrap().context.process;
        assert_eq!(call(&vm, &old, "value").await, LpcRef::from(1));
        assert_eq!(call(&vm, &changed_header, "value").await, LpcRef::from(2));
        assert_eq!(call(&vm, &changed_parent, "value").await, LpcRef::from(3));
        let span = old
            .initial_program()
            .lookup_function("value")
            .unwrap()
            .prototype
            .span
            .unwrap();
        assert!(span.code().unwrap().contains("value"));
        assert_ne!(
            span.file_id(),
            changed_parent
                .initial_program()
                .lookup_function("value")
                .unwrap()
                .prototype
                .span
                .unwrap()
                .file_id()
        );
    }
}

mod nomask_variables {
    use lpc_rs_errors::Result;

    use super::*;
    use crate::compiler::{Compiled, Compiler};

    async fn compile(files: &[(&str, &str)], code: &str) -> Result<Compiled> {
        let root = lib_holding("nomask-variables", files);
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .build()
            .unwrap();
        Compiler::new(config).compile_string("/child.c", code).await
    }

    #[tokio::test]
    async fn a_child_cannot_redeclare_a_nomask_variable() {
        let error = compile(
            &[("parent.c", "nomask int value;")],
            "inherit \"/parent\"; int value;",
        )
        .await
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            "attempt to redefine nomask variable `value`"
        );
        let rendered = error.diagnostic_string();
        assert!(rendered.contains("/child.c"), "{rendered}");
        assert!(rendered.contains("/parent.c"), "{rendered}");
        assert!(rendered.contains("defined here"), "{rendered}");
    }

    #[tokio::test]
    async fn a_grandchild_cannot_redeclare_a_nomask_variable() {
        let error = compile(
            &[
                ("base.c", "protected nomask int value;"),
                ("parent.c", "inherit \"/base\";"),
            ],
            "inherit \"/parent\"; private int value;",
        )
        .await
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            "attempt to redefine nomask variable `value`"
        );
    }

    #[tokio::test]
    async fn unrelated_parents_cannot_mask_a_nomask_variable_in_either_order() {
        let files = [
            ("sealed.c", "nomask int value;"),
            ("ordinary.c", "int value;"),
        ];
        for code in [
            "inherit \"/sealed\"; inherit \"/ordinary\";",
            "inherit \"/ordinary\"; inherit \"/sealed\";",
        ] {
            let error = compile(&files, code).await.unwrap_err();
            assert_eq!(
                error.to_string(),
                "attempt to redefine nomask variable `value`",
                "{code}"
            );
        }
    }

    #[tokio::test]
    async fn a_diamond_can_share_a_nomask_variable() {
        let program = compile(
            &[
                ("padding.c", "int padding;"),
                ("base.c", "nomask int value;"),
                ("left.c", "inherit \"/padding\"; inherit \"/base\";"),
                ("right.c", "inherit \"/base\";"),
            ],
            "inherit \"/left\"; inherit \"/right\"; int get() { return value; }",
        )
        .await
        .unwrap();
        assert_eq!(program.program.num_globals, 2);
        assert!(program.program.global_variables["value"].flags.nomask());
    }

    #[tokio::test]
    async fn a_private_nomask_variable_does_not_reserve_its_name_in_children() {
        compile(
            &[("parent.c", "private nomask int value;")],
            "inherit \"/parent\"; int value;",
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn a_hidden_sibling_cannot_erase_an_inherited_nomask_flag() {
        let error = compile(
            &[
                ("sealed.c", "nomask int value;"),
                ("hidden.c", "private int value;"),
                ("parent.c", "inherit \"/sealed\"; inherit \"/hidden\";"),
            ],
            "inherit \"/parent\"; int value;",
        )
        .await
        .unwrap_err();
        assert_eq!(
            error.to_string(),
            "attempt to redefine nomask variable `value`"
        );
    }

    #[tokio::test]
    async fn a_hidden_sibling_keeps_the_nomask_variable_visible_to_grandchildren() {
        let x = x_after_f(&[
            ("base.c", "nomask int x; int x() { return x; }"),
            ("hidden.c", "private int x;"),
            ("parent.c", "inherit \"/base\"; inherit \"/hidden\";"),
            ("child.c", "inherit \"/parent\"; void f() { x = 42; }"),
        ])
        .await;
        assert_eq!(x, 42);
    }

    #[tokio::test]
    async fn locals_and_parameters_can_shadow_a_nomask_global() {
        compile(
            &[("parent.c", "nomask int value;")],
            "inherit \"/parent\"; int f(int value) { return value; } int g() { int value = 2; return value; }",
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn a_nomask_declaration_can_replace_an_ordinary_inherited_variable() {
        compile(
            &[("parent.c", "int value;")],
            "inherit \"/parent\"; nomask int value;",
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn a_nomask_variable_remains_assignable_by_a_child() {
        let x = x_after_f(&[
            (
                "parent.c",
                "nomask int x; void f() { x = 1; } int x() { return x; }",
            ),
            (
                "child.c",
                "inherit \"/parent\"; void f() { ::f(); x += 41; }",
            ),
        ])
        .await;
        assert_eq!(x, 42);
    }
}

/// Clone `/child.c` from `files` plus the master, call `f()`, return `x()`.
async fn x_after_f(files: &[(&str, &str)]) -> i64 {
    let root = lib_holding("inheritance", files);
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.initialize_process_from_code("/secure/master.c", MASTER)
        .await
        .unwrap();
    let main = r#"int create() { object o = clone_object("/child"); o->f(); return o->x(); }"#;
    let task = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap();
    let LpcRef::Int(LpcInt(x)) = task.result().expect("create() returns") else {
        panic!("create() did not return an int");
    };
    x
}

#[tokio::test]
async fn a_parents_call_reaches_the_childs_redefinition() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_qualified_call_reaches_the_parents_own() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        (
            "child.c",
            "inherit \"/parent\";\nvoid g() { ::g(); x += 10; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 11);
}

#[tokio::test]
async fn a_private_parent_function_is_not_overridden() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nprivate void g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 1);
}

#[tokio::test]
async fn a_private_redefinition_intercepts() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        (
            "child.c",
            "inherit \"/parent\";\nprivate void g() { x = 2; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_static_redefinition_intercepts() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nstatic void g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        (
            "child.c",
            "inherit \"/parent\";\nstatic void g() { x = 2; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_later_siblings_definition_intercepts_an_earlier_siblings_call() {
    // CD and FluffOS; LDMud keeps the earlier sibling's own, DGD refuses the file.
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("sibling.c", "int y;\nvoid g() { y = 3; }\nint y() { return y; }\n"),
        (
            "child.c",
            "inherit \"/parent\" p;\ninherit \"/sibling\";\nint x() { return p::x() * 10 + y(); }\n",
        ),
    ])
    .await;
    assert_eq!(x, 3);
}

#[tokio::test]
async fn a_pointer_taken_in_the_parent_reaches_the_childs_redefinition() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nvoid g() { x = 1; }\nvoid f() { function fp = &g(); fp(); }\nint x() { return x; }\n",
        ),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_short_call_through_a_varargs_parent_runs_the_redefinition_with_zeros() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nvarargs void g(int n) { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        (
            "child.c",
            "inherit \"/parent\";\nvoid g(int n, int m) { x = 2 + n + m; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_grandparents_call_reaches_the_grandchilds_redefinition() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("middle.c", "inherit \"/parent\";\n"),
        ("child.c", "inherit \"/middle\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}

mod inheritance_visibility {
    use std::sync::Arc;

    use lpc_rs_core::visibility::Visibility;

    use super::*;
    use crate::{
        compiler::{Compiled, Compiler},
        interpreter::program::Function,
        test_support::{permissive_master, temp_lib_config},
    };

    async fn compile(files: &[(&str, &str)], code: &str) -> lpc_rs_errors::Result<Compiled> {
        let root = lib_holding("inheritance-visibility-compile", files);
        Compiler::new(temp_lib_config(&root))
            .compile_string("/child.c", code)
            .await
    }

    async fn result(files: &[(&str, &str)], code: &str) -> LpcRef {
        let root = lib_holding("inheritance-visibility-run", files);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/child.c", code)
            .await
            .unwrap()
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn private_inheritance_keeps_local_globals_calls_and_pointers_accessible() {
        let value = result(
            &[("base.c", "int value = 7; int get() { return value; }")],
            r#"
                private inherit "/base" base;
                int create() {
                    value++;
                    function pointer = &get();
                    return get() + ::get() + base::get() + pointer() + call_inherited("base::get");
                }
            "#,
        )
        .await;
        assert_eq!(value, LpcRef::from(40));
    }

    #[tokio::test]
    async fn private_inheritance_hides_functions_and_globals_from_descendants() {
        let files = [
            ("base.c", "int value; int get() { return value; }"),
            ("parent.c", "private inherit \"/base\";"),
        ];
        for expression in ["get()", "::get()", "parent::get()", "&get()", "value"] {
            let error = compile(
                &files,
                &format!("inherit \"/parent\" parent; mixed create() {{ return {expression}; }}"),
            )
            .await
            .unwrap_err();
            assert!(
                error.diagnostic_string().contains("private"),
                "{expression}: {error:?}"
            );
        }
    }

    #[tokio::test]
    async fn public_inheritance_cannot_expose_a_private_declaration() {
        for modifier in ["public", "protected", "private"] {
            for expression in ["secret()", "value"] {
                let error = compile(
                    &[(
                        "base.c",
                        "private int value; private int secret() { return value; }",
                    )],
                    &format!(
                        "{modifier} inherit \"/base\"; int create() {{ return {expression}; }}"
                    ),
                )
                .await
                .unwrap_err();
                assert!(error.diagnostic_string().contains("private"), "{error:?}");
            }
        }
    }

    #[tokio::test]
    async fn protected_inheritance_keeps_descendant_access_without_public_exports() {
        let compiled = compile(
            &[
                ("base.c", "int value = 9; int get() { return value; }"),
                ("parent.c", "protected inherit \"/base\";"),
            ],
            "public inherit \"/parent\"; int create() { return ++value + get(); }",
        )
        .await
        .unwrap();
        assert_eq!(
            compiled
                .program
                .lookup_function("get")
                .unwrap()
                .visibility(),
            Visibility::Protected
        );
        assert_eq!(
            compiled.program.global_variables["value"]
                .flags
                .visibility(),
            Visibility::Protected
        );
        assert_eq!(
            result(
                &[
                    ("base.c", "int value = 9; int get() { return value; }"),
                    ("parent.c", "protected inherit \"/base\";"),
                ],
                "inherit \"/parent\"; int create() { return ++value + get(); }",
            )
            .await,
            LpcRef::from(20)
        );
    }

    #[tokio::test]
    async fn external_calls_and_introspection_respect_inheritance_visibility() {
        for modifier in ["private", "protected"] {
            let root = lib_holding(
                "inheritance-external-visibility",
                &[("base.c", "int secret() { return 7; }")],
            );
            let vm = Vm::new(temp_lib_config(&root));
            permissive_master(&vm.global_state.object_space).await;
            vm.initialize_process_from_code(
                "/target.c",
                format!("{modifier} inherit \"/base\"; int wrapper() {{ return secret(); }}"),
            )
            .await
            .unwrap();
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int create() {
                    object target = find_object("/target");
                    return target->wrapper() == 7 && target->secret() == 0
                        && !function_exists("secret", target)
                        && sizeof(functions(target)) == 1;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)));
            let error = vm
                .initialize_process_from_code(
                    "/pointer.c",
                    r#"
                mixed create() { return &("/target")->secret(); }
            "#,
                )
                .await
                .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains(&format!("pointer to {modifier} function")),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn inheritance_visibility_does_not_copy_executable_code() {
        let root = lib_holding(
            "inheritance-shared-visibility",
            &[("base.c", "int value; int get() { return value; }")],
        );
        let compiler = Compiler::new(temp_lib_config(&root));
        let public = compiler
            .compile_string("/public.c", "inherit \"/base\";")
            .await
            .unwrap()
            .program;
        let private = compiler
            .compile_string("/private.c", "private inherit \"/base\";")
            .await
            .unwrap()
            .program;
        let a = public.lookup_function("get").unwrap();
        let b = private.lookup_function("get").unwrap();
        assert!(Arc::ptr_eq(&a.code, &b.code));
        assert!(a.public());
        assert!(!b.public());
        assert!(b.prototype.flags.public());
        assert_eq!(
            private.global_variable_info[0].flags.visibility(),
            Visibility::Public
        );
        assert_eq!(
            private.global_variables["value"].flags.visibility(),
            Visibility::Private
        );
        assert_eq!(
            std::mem::size_of::<Function>(),
            2 * std::mem::size_of::<u64>()
        );
    }

    #[tokio::test]
    async fn private_inheritance_allows_overrides_in_the_inheriting_source() {
        assert_eq!(result(
            &[("base.c", "int get() { return 1; } int through() { return get(); }")],
            "private inherit \"/base\"; int get() { return 2; } int create() { return through(); }",
        ).await, LpcRef::from(2));
    }

    #[tokio::test]
    async fn descendants_cannot_redirect_hidden_calls_or_erase_existing_overrides() {
        assert_eq!(
            result(
                &[
                    (
                        "base.c",
                        "int get() { return 1; } int through() { return get(); }"
                    ),
                    (
                        "parent.c",
                        r#"
                    private inherit "/base";
                    int get() { return 2; }
                    int wrapper() { return through(); }
                "#
                    ),
                ],
                "inherit \"/parent\"; int get() { return 3; } int create() { return wrapper(); }",
            )
            .await,
            LpcRef::from(2)
        );
    }

    #[tokio::test]
    async fn a_hidden_sibling_cannot_redirect_a_visible_parents_calls() {
        assert_eq!(result(
            &[
                ("visible.c", "int get() { return 1; } int visible() { return get(); }"),
                ("base.c", "int get() { return 2; }"),
                ("hidden.c", "private inherit \"/base\"; int wrapper() { return get(); }"),
            ],
            "inherit \"/visible\"; inherit \"/hidden\"; int create() { return 10 * visible() + wrapper(); }",
        ).await, LpcRef::from(12));
    }

    #[tokio::test]
    async fn inherited_dynamic_lookup_keeps_the_defining_sources_access() {
        assert_eq!(
            result(
                &[
                    ("base.c", "int get() { return 7; }"),
                    (
                        "parent.c",
                        r#"
                    private inherit "/base" base;
                    int wrapper() {
                        return call_inherited("base::get") + (function_exists("base::get") != 0);
                    }
                "#
                    ),
                ],
                r#"
                inherit "/parent" parent;
                int create() {
                    return wrapper() + (call_inherited("parent::get") == 0)
                        + (function_exists("parent::get") == 0);
                }
            "#,
            )
            .await,
            LpcRef::from(10)
        );
    }

    #[tokio::test]
    async fn private_inheritance_preserves_nomask_enforcement() {
        for (declaration, replacement) in [
            ("nomask int get() { return 1; }", "int get() { return 2; }"),
            ("nomask int value;", "int value;"),
        ] {
            let error = compile(
                &[("base.c", declaration)],
                &format!("private inherit \"/base\"; {replacement}"),
            )
            .await
            .unwrap_err();
            assert!(error.to_string().contains("nomask"), "{error}");
        }
    }
}
