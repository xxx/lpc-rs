use std::sync::Arc;

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    compiler::Compiler,
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::{initialize_program, lib_holding},
};

async fn returns(files: &[(&str, &str)], source: &str, expected: i64) {
    let root = lib_holding("composition-execution", files);
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        let config = Arc::new(config);
        let compiled = Compiler::new(config.clone())
            .compile_string("/child.c", source)
            .await
            .unwrap_or_else(|error| panic!("{}", error.diagnostic_string()));
        let vm = Vm::new(config);
        let task = initialize_program::<256>(compiled.program, vm.global_state.clone())
            .await
            .unwrap();
        assert_eq!(task.context.result().unwrap(), LpcRef::from(expected));
    }
}

async fn rejects(files: &[(&str, &str)], source: &str, message: &str) {
    let root = lib_holding("composition-rejection", files);
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        let error = Compiler::new(config)
            .compile_string("/child.c", source)
            .await
            .unwrap_err();
        assert!(error.to_string().contains(message), "{error}");
    }
}

#[tokio::test]
async fn siblings_resolve_in_either_order_without_declarations() {
    let files = [
        ("consumer.c", "int result() { return calculate(3); }"),
        ("provider.c", "int calculate(int n) { return n + 4; }"),
    ];
    for inherits in [
        "inherit \"/consumer\"; inherit \"/provider\";",
        "inherit \"/provider\"; inherit \"/consumer\";",
    ] {
        returns(
            &files,
            &format!("{inherits} int create() {{ return result(); }}"),
            7,
        )
        .await;
    }
}

#[tokio::test]
async fn child_definitions_supply_inherited_global_initializers_and_callbacks() {
    returns(
        &[(
            "consumer.c",
            r#"
        int value = calculate(3);
        function bare = calculate;
        function bound = &calculate(4);
        function closure = (: calculate(5) :);
        int result() { return value + bare(3) + bound() + closure(); }
    "#,
        )],
        r#"inherit "/consumer"; int calculate(int n) { return n + 4; }
        int create() { return result(); }"#,
        31,
    )
    .await;
}

#[tokio::test]
async fn sibling_calls_use_separate_globals_and_share_diamond_ancestors() {
    returns(&[
        ("base.c", "int shared = 1; int bump() { return ++shared; }"),
        ("left.c", "inherit \"/base\"; int left = 10; int result() { return left + supply() + bump(); }"),
        ("right.c", "inherit \"/base\"; int right = 20; int supply() { return right + bump(); }"),
    ], "inherit \"/left\"; inherit \"/right\"; int create() { return result(); }", 35).await;
}

#[tokio::test]
async fn mutual_calls_are_checked_against_actual_definitions() {
    returns(
        &[
            ("even.c", "int even(int n) { return n ? odd(n - 1) : 1; }"),
            ("odd.c", "int odd(int n) { return n ? even(n - 1) : 0; }"),
        ],
        "inherit \"/even\"; inherit \"/odd\"; int create() { return even(8) + odd(9); }",
        2,
    )
    .await;
}

#[tokio::test]
async fn receiver_definitions_preserve_defaults_varargs_and_reference_arguments() {
    returns(&[("consumer.c", r#"
        int result() { int n = 1; increment(ref n); return n + defaults() + optional() + total(3, 4, 5); }
    "#)], r#"inherit "/consumer";
        void increment(int ref n) { n++; }
        int defaults(int n = 6) { return n; }
        varargs int optional(int n) { return n ? 0 : 10; }
        int total(int first, int *rest...) { return first + rest[0] + rest[1]; }
        int create() { return result(); }
    "#, 30).await;
}

#[tokio::test]
async fn receiver_definitions_reject_wrong_arguments_returns_and_references() {
    for (consumer, provider, message) in [
        (
            "int result() { return supply(3); }",
            "int supply(string n) { return 1; }",
            "unexpected argument type",
        ),
        (
            "int result() { return supply(3); }",
            "string supply(int n) { return \"bad\"; }",
            "return",
        ),
        (
            "int result() { return supply(); }",
            "int supply(int n) { return n; }",
            "incorrect argument count",
        ),
        (
            "int result() { int n = 1; supply(n); return n; }",
            "void supply(int ref n) { n++; }",
            "must be passed by reference",
        ),
        (
            "int result() { return supply() + 1; }",
            "void supply() {}",
            "void",
        ),
    ] {
        rejects(
            &[("consumer.c", consumer)],
            &format!("inherit \"/consumer\"; {provider}"),
            message,
        )
        .await;
    }
}

#[tokio::test]
async fn prototypes_do_not_override_actual_supplier_types() {
    returns(&[("consumer.c", "string supply(string, object); int result() { return supply(3); }")],
        "inherit \"/consumer\"; int supply(int n) { return n + 4; } int create() { return result(); }", 7).await;
}

#[tokio::test]
async fn missing_functions_fail_even_in_unused_bodies_and_standalone_modules() {
    let module = "int unused() { return missing(); }";
    rejects(&[], module, "unknown function `missing`").await;
    rejects(
        &[("consumer.c", module)],
        "inherit \"/consumer\"; int create() { return 1; }",
        "unknown function `missing`",
    )
    .await;
}

#[tokio::test]
async fn private_suppliers_are_ineligible_and_protected_suppliers_work() {
    let files = [("consumer.c", "int result() { return supply(); }")];
    rejects(
        &files,
        "inherit \"/consumer\"; private int supply() { return 7; }",
        "unknown function `supply`",
    )
    .await;
    returns(&files, "inherit \"/consumer\"; protected int supply() { return 7; } int create() { return result(); }", 7).await;
}

#[tokio::test]
async fn inaccessible_lexical_definitions_block_receiver_fallback() {
    rejects(
        &[
            ("hidden.c", "private int supply() { return 1; }"),
            (
                "consumer.c",
                "inherit \"/hidden\"; int result() { return supply(); }",
            ),
        ],
        "inherit \"/consumer\"; int supply() { return 7; }",
        "private function `supply`",
    )
    .await;
}

#[tokio::test]
async fn descendant_supplier_signatures_survive_public_intermediate_inherits() {
    returns(
        &[
            ("consumer.c", "int result() { return supply(3); }"),
            ("provider.c", "string supply(string text) { return text; }"),
            ("middle.c", "inherit \"/consumer\"; inherit \"/provider\";"),
        ],
        r#"inherit "/middle";
            int supply(int n) { return n + 4; }
            int create() { return result(); }
        "#,
        7,
    )
    .await;
}

#[tokio::test]
async fn private_import_suppliers_keep_their_binding_in_descendants() {
    returns(&[
        ("consumer.c", "int result() { return supply() << 1; }"),
        ("provider.c", "int supply() { return 4; }"),
        ("middle.c", "inherit \"/consumer\"; private inherit \"/provider\"; int exposed() { return result(); }"),
    ], "inherit \"/middle\"; string supply() { return \"bad\"; } int create() { return exposed(); }", 8).await;
}

#[tokio::test]
async fn existing_efun_binding_and_local_variable_shadowing_are_preserved() {
    returns(&[("consumer.c", "int result() { return intp(3); } int shadow() { function supply = (: 6 :); return supply(); }")],
        "inherit \"/consumer\"; int intp(mixed n) { return 55; } int supply() { return 7; } int create() { return result() + shadow(); }", 7).await;
}

#[tokio::test]
async fn receiver_aliases_globals_and_classes_do_not_enter_parent_lexical_scope() {
    for (consumer, message) in [
        (
            "int result() { return provider::supply(); }",
            "unknown namespace",
        ),
        (
            "int result() { return sibling_global; }",
            "undefined variable `sibling_global`",
        ),
        (
            "class Record result() { return make(); }",
            "unknown class `Record`",
        ),
    ] {
        rejects(&[("consumer.c", consumer), ("provider.c", "class Record { int value; } int sibling_global; int supply() { return 7; } class Record make() { return new(class Record); }")],
            "inherit \"/consumer\"; inherit \"/provider\";", message).await;
    }
}

#[tokio::test]
async fn inherited_class_interfaces_are_available_before_body_checks() {
    returns(
        &[
            ("types.c", "class Record { int value; }"),
            (
                "consumer.c",
                "inherit \"/types\"; int result() { class Record r = make(); return r.value; }",
            ),
            (
                "provider.c",
                "inherit \"/types\"; class Record make() { return new(class Record, value: 7); }",
            ),
        ],
        "inherit \"/consumer\"; inherit \"/provider\"; int create() { return result(); }",
        7,
    )
    .await;
}

#[tokio::test]
async fn analysis_and_auto_inheritance_use_receiving_definitions() {
    let root = lib_holding(
        "composition-auto",
        &[("auto.c", "int result() { return supply(); }")],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .auto_inherit_file("/auto.c")
        .build()
        .unwrap();
    let compiler = Compiler::new(config);
    compiler
        .analyze_string("/child.c", "int supply() { return 7; }")
        .await
        .unwrap();
    let error = compiler.compile_file("/auto.c").await.unwrap_err();
    assert!(error.to_string().contains("unknown function `supply`"));
}

#[tokio::test]
async fn code_sharing_depends_on_the_resolved_signature_not_the_helper_body() {
    let root = lib_holding(
        "composition-sharing",
        &[("consumer.c", "mixed result() { return supply(); }")],
    );
    let compiler = Compiler::new(crate::test_support::temp_lib_config(&root));
    let compile =
        |body| compiler.compile_string("/child.c", format!("inherit \"/consumer\"; {body}"));
    let first = compile("int supply() { return 1; }").await.unwrap();
    let second = compile("int supply() { return 2; }").await.unwrap();
    let third = compile("string supply() { return \"different\"; }")
        .await
        .unwrap();
    let code = |compiled: &crate::compiler::Compiled| {
        compiled
            .program
            .lookup_function("result")
            .unwrap()
            .code
            .clone()
    };
    assert!(Arc::ptr_eq(&code(&first), &code(&second)));
    assert!(!Arc::ptr_eq(&code(&first), &code(&third)));
}

#[tokio::test]
async fn known_simul_calls_keep_their_target() {
    use crate::compiler::CompilerBuilder;
    let root = lib_holding(
        "composition-simul",
        &[("consumer.c", "int result() { return supply(); }")],
    );
    let config = Arc::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .simul_efun_file("/simul.c")
            .build()
            .unwrap(),
    );
    let simul = Arc::new(
        Compiler::new(config.clone())
            .compile_string("/simul.c", "int supply() { return 7; }")
            .await
            .unwrap()
            .program,
    );
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .simul_efuns(Some(simul.clone()))
        .build()
        .unwrap();
    let compiled = compiler
        .compile_string(
            "/child.c",
            "inherit \"/consumer\"; int supply() { return 99; } int create() { return result(); }",
        )
        .await
        .unwrap();
    let vm = Vm::new(config);
    initialize_program::<256>(simul, vm.global_state.clone())
        .await
        .unwrap();
    let task = initialize_program::<256>(compiled.program, vm.global_state.clone())
        .await
        .unwrap();
    assert_eq!(task.context.result().unwrap(), LpcRef::from(7));
}

#[tokio::test]
async fn discovery_preserves_authorization_order_and_does_not_reread_bodies() {
    use crate::compiler::{
        CompilerBuilder,
        compile_gate::CompileGate,
        source_reader::{DiskSourceReader, SourceKind, SourceReader},
    };
    use async_trait::async_trait;
    use parking_lot::Mutex;
    use std::{io, path::Path};

    #[derive(Debug, Default)]
    struct Access {
        events: Mutex<Vec<String>>,
    }
    #[async_trait]
    impl CompileGate for Access {
        async fn inherit(&self, path: &str, from: &str) -> lpc_rs_errors::Result<bool> {
            self.events.lock().push(format!("inherit {path} {from}"));
            Ok(true)
        }
        async fn include(&self, path: &str, from: &str) -> lpc_rs_errors::Result<bool> {
            self.events.lock().push(format!("include {path} {from}"));
            Ok(true)
        }
        async fn efun_override(
            &self,
            source: &str,
            name: &str,
            _: &str,
        ) -> lpc_rs_errors::Result<bool> {
            self.events.lock().push(format!("efun {source} {name}"));
            Ok(true)
        }
    }
    #[async_trait]
    impl SourceReader for Access {
        async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
            self.events.lock().push(format!(
                "read {}",
                path.file_name().unwrap().to_str().unwrap()
            ));
            DiskSourceReader.read(path).await
        }
        async fn kind(&self, path: &Path) -> io::Result<SourceKind> {
            DiskSourceReader.kind(path).await
        }
    }
    let root = lib_holding(
        "composition-access",
        &[
            (
                "consumer.c",
                "#include \"header.h\"\nint result() { return supply() + efun::intp(1); }",
            ),
            ("header.h", "#define VALUE 1\n"),
            ("provider.c", "int supply() { return efun::intp(1); }"),
        ],
    );
    let access = Arc::new(Access::default());
    let compiler = CompilerBuilder::default()
        .config(crate::test_support::temp_lib_config(&root))
        .gate(Some(access.clone() as Arc<dyn CompileGate>))
        .source_reader(Some(access.clone() as Arc<dyn SourceReader>))
        .build()
        .unwrap();
    compiler.compile_string("/child.c", "inherit \"/consumer\"; inherit \"/provider\"; int create() { return result() + efun::intp(1); }").await.unwrap();
    assert_eq!(
        *access.events.lock(),
        [
            "inherit /consumer.c /child.c",
            "read consumer.c",
            "include /header.h /consumer.c",
            "read header.h",
            "efun /consumer.c intp",
            "inherit /provider.c /child.c",
            "read provider.c",
            "efun /provider.c intp",
            "efun /child.c intp",
        ]
    );
}

#[tokio::test]
async fn finite_inheritance_chains_respect_the_configured_depth_boundary() {
    let root = lib_holding(
        "inheritance-depth-boundary",
        &[
            ("base.c", "int value() { return 7; }"),
            ("middle.c", r#"inherit "/base";"#),
        ],
    );
    for (limit, source, accepted) in [
        (0u8, "int value() { return 7; }", true),
        (0, r#"inherit "/base";"#, false),
        (1, r#"inherit "/base";"#, true),
        (1, r#"inherit "/middle";"#, false),
        (2, r#"inherit "/middle";"#, true),
    ] {
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .max_inherit_depth(limit)
            .build()
            .unwrap();
        let result = Compiler::new(config)
            .compile_string("/child.c", source)
            .await;
        match (result, accepted) {
            (Ok(_), true) => {}
            (Err(error), false) => {
                assert_eq!(error.to_string(), "maximum inheritance depth reached");
            }
            (Ok(_), false) => panic!("inheritance deeper than {limit} was accepted"),
            (Err(error), true) => panic!("inheritance within {limit} was rejected: {error}"),
        }
    }
}

#[tokio::test]
async fn private_import_suppliers_do_not_escape_into_descendant_siblings() {
    rejects(
        &[
            ("provider.c", "int supply() { return 7; }"),
            (
                "middle.c",
                r#"private inherit "/provider"; int exposed() { return supply(); }"#,
            ),
            ("consumer.c", "int result() { return supply(); }"),
        ],
        r#"inherit "/middle"; inherit "/consumer"; int create() { return result(); }"#,
        "unknown function `supply`",
    )
    .await;
}

#[tokio::test]
async fn prepared_interfaces_keep_local_basenames_and_private_import_boundaries() {
    use crate::compiler::source::CompilerSource;

    let root = lib_holding(
        "interface-visibility",
        &[
            ("provider.c", "int supply() { return 7; }"),
            (
                "middle.c",
                r#"private inherit "/provider"; int exposed() { return supply(); }"#,
            ),
        ],
    );
    let config = Arc::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .build()
            .unwrap(),
    );
    let compiler = Compiler::new(config.clone());
    let source = Arc::new(CompilerSource::new("/folder/child.c", &config));
    let prepared = super::PreparedProgram::parse(
        &compiler,
        source,
        r#"inherit "/middle"; int visible() { return 7; }"#,
        None,
    )
    .await
    .unwrap();

    assert!(prepared.interface.has_basename("child"));
    for other in ["middle", "/folder/child", "child.c"] {
        assert!(!prepared.interface.has_basename(other));
    }
    assert!(
        !prepared
            .interface
            .receiver_functions()
            .contains_key("supply")
    );
    assert!(
        prepared.parents[0]
            .interface
            .receiver_functions()
            .contains_key("supply")
    );
    assert!(prepared.interface.lookup("visible").is_some());
}
