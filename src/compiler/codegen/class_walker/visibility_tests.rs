use std::sync::Arc;

use lpc_rs_core::{class::ClassId, null_mode::NullMode, visibility::Visibility};
use lpc_rs_utils::config::{Config, ConfigBuilder};
use ustr::ustr;

use crate::{
    compiler::Compiler,
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::{initialize_program, lib_holding, temp_lib_config},
};

async fn run(source: &str, config: Config) -> LpcRef {
    let config = Arc::new(config);
    let compiled = Compiler::new(config.clone())
        .compile_string("/child.c", source)
        .await
        .unwrap();
    let vm = Vm::new(config);
    initialize_program::<256>(compiled.program, vm.global_state.clone())
        .await
        .unwrap()
        .result()
        .unwrap()
}

#[tokio::test]
async fn class_modifiers_preserve_identity_and_do_not_modify_variable_or_function_flags() {
    for (modifier, visibility) in [
        ("", Visibility::Public),
        ("public", Visibility::Public),
        ("protected", Visibility::Protected),
        ("private", Visibility::Private),
    ] {
        let source = format!(
            "{modifier} class Record {{ int value; }};
            private class Record record;
            protected class Record make() {{ return new(class Record); }}"
        );
        let compiled = Compiler::default()
            .compile_string("/record.c", source)
            .await
            .unwrap();
        let id = ClassId {
            source: ustr("/record.c"),
            name: ustr("Record"),
        };
        assert_eq!(compiled.program.class_scope.definitions[0].id, id);
        assert_eq!(compiled.program.class_scope.visibility(id), visibility);
        assert_eq!(
            compiled.program.global_variables["record"]
                .flags
                .visibility(),
            Visibility::Private
        );
        assert_eq!(
            compiled
                .program
                .lookup_function("make")
                .unwrap()
                .visibility(),
            Visibility::Protected
        );
    }
}

#[tokio::test]
async fn class_definitions_reject_non_visibility_modifiers() {
    for modifier in ["static", "nosave", "nomask", "varargs", "private nosave"] {
        let error = Compiler::default()
            .compile_string(
                "/test.c",
                format!("{modifier} class Record {{ int value; }}"),
            )
            .await
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("not a class visibility modifier"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn class_definitions_reject_multiple_visibilities() {
    for modifiers in ["private public", "public protected", "private private"] {
        let error = Compiler::default()
            .compile_string("/test.c", format!("{modifiers} class Record {{}}"))
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "multiple visibilities specified");
    }
}

#[tokio::test]
async fn private_classes_in_headers_allow_forward_and_recursive_use_in_both_null_modes() {
    let root = lib_holding(
        "private-class-header",
        &[(
            "records.h",
            r#"
        private class First { class Second next; int value; }
        private class Second { class First next; }
    "#,
        )],
    );
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        let value = run(
            r#"
            #include "records.h"
            int create() {
                class First first = new(class First, value: 7);
                first.next = new(class Second, next: first);
                return first.next.next.value;
            }
        "#,
            config,
        )
        .await;
        assert_eq!(value, LpcRef::from(7));
    }
}

#[tokio::test]
async fn private_class_names_are_hidden_in_every_descendant_type_position() {
    let root = lib_holding(
        "private-class-types",
        &[
            ("base.c", "private\nclass Record\n{ int value; }"),
            ("middle.c", "inherit \"/base\";"),
        ],
    );
    let compiler = Compiler::new(temp_lib_config(&root));
    for parent in ["/base", "/middle"] {
        for declaration in [
            "class Record value;",
            "void f(class Record value) {}",
            "class Record f() { return 0; }",
            "void f() { new(class Record); }",
            "void f(mixed x) { (class Record)x; }",
            "void f(mixed *xs) { foreach (class Record x : xs) {} }",
        ] {
            let error = compiler
                .compile_string("/child.c", format!("inherit \"{parent}\"; {declaration}"))
                .await
                .unwrap_err();
            assert_eq!(
                error.to_string(),
                "unknown class `Record`",
                "{parent}: {declaration}"
            );
        }
    }
}

#[tokio::test]
async fn inherited_initializers_methods_and_closures_retain_private_classes() {
    let root = lib_holding(
        "private-class-code",
        &[(
            "base.c",
            r#"
        private class Record { int value; }
        private class Record state = new(class Record, value: 4);
        private int increment() { return ++state.value; }
        function callback() { return (: increment() :); }
        int read() { return state.value; }
    "#,
        )],
    );
    let value = run(
        r#"
        inherit "/base";
        int create() { function f = callback(); return f() == 5 && read() == 5; }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(1));
}

#[tokio::test]
async fn private_values_can_be_passed_back_but_child_member_reads_and_writes_fail() {
    let root = lib_holding(
        "private-class-opaque",
        &[(
            "base.c",
            r#"
        private class Record { int value; }
        class Record make() { return new(class Record, value: 7); }
        int read(class Record record) { return record.value; }
    "#,
        )],
    );
    let value = run(
        r#"
        inherit "/base";
        int create() {
            mixed record = make();
            mixed read_error = catch(record.value);
            mixed write_error = catch(record.value = 9);
            return strindex(read_error, "no visible definition") >= 0
                && strindex(write_error, "no visible definition") >= 0 && read(record) == 7;
        }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(1));
}

#[tokio::test]
async fn inferred_private_return_types_do_not_grant_member_access() {
    let root = lib_holding(
        "private-class-return",
        &[(
            "base.c",
            r#"
        private class Record { int value; }
        class Record make() { return new(class Record); }
    "#,
        )],
    );
    let error = Compiler::new(temp_lib_config(&root))
        .compile_string(
            "/child.c",
            r#"
        inherit "/base";
        int read() { return make().value; }
    "#,
        )
        .await
        .unwrap_err();
    assert!(
        error.to_string().contains("no visible definition"),
        "{error}"
    );
}

#[tokio::test]
async fn unrelated_private_classes_do_not_make_child_class_names_ambiguous() {
    let root = lib_holding(
        "private-class-collision",
        &[
            (
                "left.c",
                "private class Record { int left; } int left() { return new(class Record, left: 1).left; }",
            ),
            (
                "right.c",
                "private class Record { int right; } int right() { return new(class Record, right: 2).right; }",
            ),
        ],
    );
    let value = run(
        r#"
        inherit "/left";
        inherit "/right";
        class Record { int own; }
        int create() { return left() + right() + new(class Record, own: 3).own; }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(6));
}

#[tokio::test]
async fn default_public_and_protected_classes_remain_visible_through_inheritance() {
    for modifier in ["", "public", "protected"] {
        let base = format!("{modifier} class Record {{ int value; }}");
        let root = lib_holding(
            "class-visible-descendants",
            &[("base.c", &base), ("middle.c", "inherit \"/base\";")],
        );
        let value = run(
            r#"
            inherit "/middle";
            int create() { class Record record = new(class Record, value: 8); return record.value; }
        "#,
            temp_lib_config(&root),
        )
        .await;
        assert_eq!(value, LpcRef::from(8), "{modifier}");
    }
}

#[tokio::test]
async fn private_inheritance_keeps_classes_local_and_retains_access_for_inherited_code() {
    let root = lib_holding(
        "class-private-inherit",
        &[
            ("base.c", "class Record { int value; }"),
            (
                "middle.c",
                r#"
            private inherit "/base";
            class Record make() { return new(class Record, value: 9); }
            int read(mixed record) { return record.value; }
        "#,
            ),
        ],
    );
    let value = run(
        r#"
        inherit "/middle";
        int create() {
            mixed record = make();
            mixed error = catch(record.value);
            return read(record) == 9 && strindex(error, "no visible definition") >= 0;
        }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(1));
    let error = Compiler::new(temp_lib_config(&root))
        .compile_string("/child.c", "inherit \"/middle\"; class Record record;")
        .await
        .unwrap_err();
    assert_eq!(error.to_string(), "unknown class `Record`");
}

#[tokio::test]
async fn inheritance_cannot_expose_a_parents_private_class() {
    let root = lib_holding(
        "class-import-private",
        &[("base.c", "private class Record {}")],
    );
    for access in ["public", "protected", "private"] {
        let error = Compiler::new(temp_lib_config(&root))
            .compile_string(
                "/child.c",
                format!("{access} inherit \"/base\"; class Record record;"),
            )
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "unknown class `Record`");
    }
}

#[tokio::test]
async fn protected_inheritance_restricts_classes_without_hiding_them() {
    let root = lib_holding(
        "class-protected-inherit",
        &[
            ("base.c", "public class Record {}"),
            ("middle.c", "protected inherit \"/base\";"),
        ],
    );
    let compiled = Compiler::new(temp_lib_config(&root))
        .compile_string(
            "/child.c",
            "public inherit \"/middle\"; class Record record;",
        )
        .await
        .unwrap();
    let id = compiled.program.class_scope.definitions[0].id;
    assert_eq!(
        compiled.program.class_scope.visibility(id),
        Visibility::Protected
    );
}

#[tokio::test]
async fn a_diamond_can_expose_a_class_through_its_public_path_in_either_order() {
    let root = lib_holding(
        "class-diamond-visibility",
        &[
            ("base.c", "class Record { int value; }"),
            ("hidden.c", "private inherit \"/base\";"),
            ("visible.c", "inherit \"/base\";"),
        ],
    );
    for inherits in [
        "inherit \"/hidden\"; inherit \"/visible\";",
        "inherit \"/visible\"; inherit \"/hidden\";",
    ] {
        let value = run(
            &format!("{inherits} int create() {{ return new(class Record, value: 6).value; }}"),
            temp_lib_config(&root),
        )
        .await;
        assert_eq!(value, LpcRef::from(6));
    }
}

#[tokio::test]
async fn simultaneous_private_and_public_imports_keep_the_more_visible_path() {
    for (left, right) in [
        ("private", "public"),
        ("public", "private"),
        ("private", "protected"),
    ] {
        let middle = format!("{left} inherit \"/left\"; {right} inherit \"/right\";");
        let root = lib_holding(
            "class-import-merge",
            &[
                ("base.c", "class Record { int value; }"),
                ("left.c", "inherit \"/base\";"),
                ("right.c", "inherit \"/base\";"),
                ("middle.c", &middle),
            ],
        );
        let value = run(
            "inherit \"/middle\"; int create() { return new(class Record, value: 8).value; }",
            temp_lib_config(&root),
        )
        .await;
        assert_eq!(value, LpcRef::from(8));
    }
}

#[tokio::test]
async fn including_a_private_header_gives_each_source_its_own_visible_definition() {
    let root = lib_holding(
        "class-private-reinclude",
        &[
            ("record.h", "private class Record { int value; }"),
            (
                "base.c",
                "#include \"record.h\"\nclass Record make() { return new(class Record, value: 3); }",
            ),
        ],
    );
    let value = run(
        r#"
        inherit "/base";
        #include "record.h"
        int create() { class Record record = make(); return record.value; }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(3));
}

#[tokio::test]
async fn a_local_header_declaration_controls_reexport_of_the_same_identity() {
    for (modifier, visible) in [("private", false), ("public", true)] {
        let middle =
            format!("private inherit \"/base\";\n#define ACCESS {modifier}\n#include \"record.h\"");
        let root = lib_holding(
            "class-local-visibility",
            &[
                ("record.h", "ACCESS class Record {}"),
                ("base.c", "#define ACCESS public\n#include \"record.h\""),
                ("middle.c", &middle),
            ],
        );
        let result = Compiler::new(temp_lib_config(&root))
            .compile_string("/child.c", "inherit \"/middle\"; class Record record;")
            .await;
        assert_eq!(result.is_ok(), visible, "{result:?}");
    }
}

#[tokio::test]
async fn private_records_can_be_copied_and_serialized_without_exposing_members() {
    let root = lib_holding(
        "private-class-values",
        &[(
            "base.c",
            r#"
        private class Record { int value; }
        mixed make() { return new(class Record, value: 5); }
        int read(class Record record) { return record.value; }
    "#,
        )],
    );
    let value = run(
        r#"
        inherit "/base";
        int create() {
            mixed original = make();
            mixed copy = copy_class(original);
            mixed deep = deep_copy(original);
            mixed restored = restore_value(save_value(original));
            return read(copy) == 5 && read(deep) == 5 && read(restored) == 5
                && stringp(catch(restored.value)) && original != restored;
        }
    "#,
        temp_lib_config(&root),
    )
    .await;
    assert_eq!(value, LpcRef::from(1));
}

#[tokio::test]
async fn recompilation_retains_old_visibility_without_changing_class_identity_or_layout() {
    let compiler = Compiler::default();
    let mut versions = Vec::new();
    for modifier in ["private", "public"] {
        versions.push(
            compiler
                .compile_string(
                    "/record.c",
                    format!("{modifier} class Record {{ int value; }} int f() {{ return 1; }}"),
                )
                .await
                .unwrap(),
        );
    }
    let old = versions[0].program.lookup_function("f").unwrap();
    let new = versions[1].program.lookup_function("f").unwrap();
    let id = old.class_scope.definitions[0].id;
    assert_eq!(old.class_scope.definitions, new.class_scope.definitions);
    assert_eq!(old.class_scope.visibility(id), Visibility::Private);
    assert_eq!(new.class_scope.visibility(id), Visibility::Public);
    assert!(!Arc::ptr_eq(&old.code, &new.code));
}

#[tokio::test]
async fn receiving_composition_calls_can_forward_private_values_without_exposing_their_class() {
    let root = lib_holding(
        "private-class-composition",
        &[
            ("caller.c", "mixed forward() { return make(); }"),
            (
                "supplier.c",
                r#"
            private class Record { int value; }
            class Record make() { return new(class Record, value: 4); }
            int read(class Record record) { return record.value; }
        "#,
            ),
        ],
    );
    let value = run(r#"
        inherit "/caller";
        inherit "/supplier";
        int create() { mixed record = forward(); return read(record) == 4 && stringp(catch(record.value)); }
    "#, temp_lib_config(&root)).await;
    assert_eq!(value, LpcRef::from(1));
}
