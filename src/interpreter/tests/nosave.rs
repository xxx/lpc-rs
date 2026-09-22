use super::{run, run_with, s};
use crate::{
    compiler::Compiler,
    interpreter::lpc_ref::LpcRef,
    test_support::{lib_holding, temp_lib_config},
};

#[tokio::test]
async fn nosave_macro_aliases_expand_from_included_headers() {
    let root = lib_holding(
        "nosave-header",
        &[(
            "storage.h",
            r#"
        #define TRANSIENT nosave
        #define DECLARE(name) TRANSIENT int name = 7;
    "#,
        )],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&root),
            "",
            &[],
            r#"
            #include "storage.h"
            DECLARE(cache)
            mixed *create() { return ({ cache, variable_info(this_object())[0]["flags"] }); }
        "#
        )
        .await,
        vec![LpcRef::from(7), s("public static")],
    );
}

#[tokio::test]
async fn nosave_keyword_macros_preserve_limas_fallback_and_stringification() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            #define RAW(x) #x
            #define EXPAND(x) RAW(x)
            #ifndef __SENSIBLE_MODIFIERS__
            #define nosave static
            #endif
            nosave int legacy = 7;
            string raw = RAW(nosave);
            string expanded = EXPAND(nosave);
            #undef nosave
            nosave int native = 8;
            mixed *create() {
                mapping *info = variable_info(this_object());
                return ({ legacy, native, raw, expanded, info[0]["flags"], info[3]["flags"] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            LpcRef::from(8),
            s("nosave"),
            s("static"),
            s("public static"),
            s("public static")
        ],
    );
}

#[tokio::test]
async fn nosave_introspection_preserves_declared_visibility_through_inheritance() {
    let root = lib_holding(
        "nosave-introspection",
        &[(
            "base.c",
            r#"
        nosave int public_cache = 1;
        protected nosave int protected_cache = 2;
        private nosave nomask int private_cache = 3;
    "#,
        )],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&root),
            "",
            &[],
            r#"
            private inherit "/base";
            mixed *create() {
                public_cache++;
                protected_cache++;
                mapping *info = variable_info(this_object());
                return ({ info[0]["flags"], info[1]["flags"], info[2]["flags"],
                    info[0]["value"], info[1]["value"], info[2]["value"] });
            }
        "#
        )
        .await,
        vec![
            s("public static"),
            s("protected static"),
            s("private static nomask"),
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(3)
        ],
    );
}

#[tokio::test]
async fn nosave_does_not_expose_private_globals_to_descendants() {
    let root = lib_holding(
        "nosave-private",
        &[("base.c", "private nosave int cache = 7;")],
    );
    let error = Compiler::new(temp_lib_config(&root))
        .compile_string(
            "/child.c",
            "inherit \"/base\"; int create() { return cache; }",
        )
        .await
        .unwrap_err()
        .to_string();
    assert!(
        error.contains("private") && error.contains("cache"),
        "{error}"
    );
}

#[tokio::test]
async fn nosave_does_not_allow_redeclaration_of_nomask_globals() {
    let root = lib_holding(
        "nosave-nomask",
        &[("base.c", "nosave nomask int cache = 7;")],
    );
    let error = Compiler::new(temp_lib_config(&root))
        .compile_string("/child.c", "inherit \"/base\"; int cache = 8;")
        .await
        .unwrap_err()
        .to_string();
    assert!(error.contains("nomask variable `cache`"), "{error}");
}
