use lpc_rs_errors::LpcError;

use super::run;
use crate::{compiler::Compiler, interpreter::lpc_ref::LpcRef, test_support::test_config};

async fn compile_error(source: &str) -> LpcError {
    Compiler::new(test_config())
        .compile_string("/parameter_defaults.c", source)
        .await
        .unwrap_err()
}

#[tokio::test]
async fn rejects_a_required_parameter_after_a_default() {
    let source = "int f(int interval = 15, int chance) { return chance; }";
    let error = compile_error(source).await;
    assert_eq!(
        error.message(),
        "parameter `chance` must have a default value because it follows a defaulted parameter"
    );
    let span = error.span().unwrap();
    assert!(source[span.l()..span.r()].contains("chance"));
    assert!(error.to_diagnostics()[0].labels.iter().any(|label| {
        label.message == "defaulted parameter declared here"
            && source[label.range.clone()].contains("interval")
    }));
}

#[tokio::test]
async fn rejects_a_nondefaulted_varargs_parameter_after_a_default() {
    let error = compile_error(
        "varargs int *f(mixed text, int interval = 15, int chance) { return ({interval, chance}); }",
    )
    .await;
    assert_eq!(
        error.message(),
        "parameter `chance` must have a default value because it follows a defaulted parameter"
    );
}

#[tokio::test]
async fn rejects_a_nondefaulted_closure_parameter_after_a_default() {
    let error =
        compile_error("function f = (: [int interval = 15, int chance] interval + chance :);")
            .await;
    assert_eq!(
        error.message(),
        "parameter `chance` must have a default value because it follows a defaulted parameter"
    );
}

#[tokio::test]
async fn rejects_nontrailing_defaults_inside_a_nested_closure() {
    let error = compile_error(
        "function f = (: [int outer = 1] (: [int interval = 15, int chance] outer + interval + chance :) :);",
    )
    .await;
    assert_eq!(
        error.message(),
        "parameter `chance` must have a default value because it follows a defaulted parameter"
    );
}

#[tokio::test]
async fn rejects_nontrailing_fixed_defaults_before_variadic_collectors() {
    for source in [
        "int f(int interval = 15, int chance, ...) { return chance; }",
        "varargs int f(int interval = 15, int chance, mixed *rest...) { return chance; }",
        "function f = (: [int interval = 15, int chance, mixed *rest...] chance :);",
    ] {
        let error = compile_error(source).await;
        assert_eq!(
            error.message(),
            "parameter `chance` must have a default value because it follows a defaulted parameter",
            "{source}"
        );
    }
}

#[tokio::test]
async fn trailing_default_initializers_only_run_for_omitted_arguments() {
    let values = run(
        "",
        &[],
        r#"
        int calls;
        int next_default() { calls++; return 15; }
        int f(int interval = next_default()) { return interval; }
        mixed *create() {
            int omitted = f();
            int supplied = f(5);
            int zero = f(0);
            return ({omitted, supplied, zero, calls});
        }
        "#,
    )
    .await;
    assert_eq!(values, vec![15.into(), 5.into(), 0.into(), LpcRef::from(1)]);
}

#[tokio::test]
async fn trailing_defaults_preserve_pointer_cross_object_and_closure_calls() {
    let values = run(
        "",
        &[],
        r#"
        int *f(int interval = 15, int chance = 0) { return ({interval, chance}); }
        mixed *create() {
            function pointer = f;
            function closure = (: [int interval = 15, int chance = 0] ({interval, chance}) :);
            return pointer() + pointer(5) + pointer(0, 100)
                + this_object()->f() + this_object()->f(5) + this_object()->f(0, 100)
                + closure() + closure(5) + closure(0, 100);
        }
        "#,
    )
    .await;
    assert_eq!(
        values,
        [
            15, 0, 5, 0, 0, 100, 15, 0, 5, 0, 0, 100, 15, 0, 5, 0, 0, 100
        ]
        .into_iter()
        .map(LpcRef::from)
        .collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn variadic_collectors_can_follow_trailing_fixed_defaults() {
    let values = run(
        "",
        &[],
        r#"
        mixed *named(int prefix = 7, mixed *rest...) { return ({prefix}) + rest; }
        mixed *implicit(int prefix = 7, ...) { return ({prefix}) + argv; }
        mixed *create() {
            function closure = (: [int prefix = 7, mixed *rest...] ({prefix}) + rest :);
            return named() + named(2, 3) + implicit() + implicit(4, 5)
                + closure() + closure(6, 8);
        }
        "#,
    )
    .await;
    assert_eq!(
        values,
        [7, 2, 3, 7, 4, 5, 7, 6, 8]
            .into_iter()
            .map(LpcRef::from)
            .collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn varargs_can_omit_nondefaulted_parameters_before_trailing_defaults() {
    let values = run(
        "",
        &[],
        r#"
        varargs int *f(int first, int second = 2) { return ({first, second}); }
        mixed *create() { return f() + f(5) + f(5, 0); }
        "#,
    )
    .await;
    assert_eq!(
        values,
        [0, 2, 5, 2, 5, 0]
            .into_iter()
            .map(LpcRef::from)
            .collect::<Vec<_>>()
    );
}
