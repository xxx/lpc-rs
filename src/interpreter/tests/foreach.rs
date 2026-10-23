use super::run;
use crate::{compiler::Compiler, interpreter::lpc_ref::LpcRef, test_support::test_config};

#[tokio::test]
async fn in_and_colon_loops_can_nest_with_break_and_continue() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int total;
            foreach (int outer : ({ 1, 2 })) {
                foreach (int value in ({ 1, 2, 3, 4 })) {
                    if (value == 2) continue;
                    if (value == 4) break;
                    total += outer * value;
                }
            }
            foreach (int value in ({})) total = -1;
            return ({ total });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(12)]);
}

#[tokio::test]
async fn in_mapping_loops_accept_class_values() {
    let result = run(
        "",
        &[],
        r#"
        class relation_data { int count; }
        mixed *create() {
            mapping relations = ([
                "on": new(class relation_data, count: 2),
                "under": new(class relation_data, count: 3)
            ]);
            int total;
            int keys;
            foreach (string test, class relation_data values in relations) {
                total += values.count;
                keys += test == "on" || test == "under";
            }
            return ({ total, keys });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(5), LpcRef::from(2)]);
}

#[tokio::test]
async fn in_remains_a_function_parameter_loop_variable_and_field_name() {
    let result = run(
        "",
        &[],
        r#"
        class holder { int in; }
        int in(int value) { return value + 10; }
        int sum(int *in) {
            int total;
            foreach (int value in in) total += value;
            return total;
        }
        int sum_binding() {
            int total;
            foreach (int in in ({ 1, 2 })) total += in;
            foreach (string in, int value in ([ "key": 4 ]))
                if (in == "key") total += value;
            return total;
        }
        mixed *create() {
            class holder record = new(class holder, in: 7);
            return ({ in(3), sum(({ 4, 5 })), sum_binding(), record.in });
        }
        "#,
    )
    .await;
    assert_eq!(
        result,
        vec![
            LpcRef::from(13),
            LpcRef::from(9),
            LpcRef::from(7),
            LpcRef::from(7)
        ]
    );
}

#[tokio::test]
async fn in_separators_and_bindings_can_come_from_macros() {
    let result = run(
        "",
        &[],
        r#"
        #define EACH in
        #define BINDING int in
        mixed *create() {
            int total;
            foreach (BINDING EACH ({ 2, 3 })) total += in;
            return ({ total });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(5)]);
}

#[tokio::test]
async fn in_loops_preserve_unbraced_if_and_else_binding() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int with_else;
            int without_else;
            foreach (int value in ({ 0, 1 }))
                if (value) with_else += 10; else with_else += 1;
            foreach (int value in ({ 0, 1 }))
                if (value) without_else += 10;
            return ({ with_else, without_else });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(11), LpcRef::from(10)]);
}

#[tokio::test]
async fn in_loops_evaluate_the_collection_once() {
    let result = run(
        "",
        &[],
        r#"
        int calls;
        int *items() { calls++; return ({ 2, 3 }); }
        mixed *create() {
            int total;
            foreach (value in items()) total += value;
            return ({ total, calls });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(5), LpcRef::from(1)]);
}

#[tokio::test]
async fn in_loops_iterate_string_codepoints_and_bytes() {
    let result = run(
        "",
        &[],
        r#"
        mixed *create() {
            int text;
            int octets;
            foreach (int character in "Aé") text += character;
            foreach (int byte in to_bytes("Aé", "UTF-8")) octets += byte;
            return ({ text, octets });
        }
        "#,
    )
    .await;
    assert_eq!(result, vec![LpcRef::from(298), LpcRef::from(429)]);
}

#[tokio::test]
async fn in_loops_keep_existing_type_and_scope_checks() {
    for (body, expected) in [
        ("foreach (string value in ({ 1 })) {}", "mismatched types"),
        ("foreach (key, value in ({ 1 })) {}", "needs a mapping"),
        ("foreach (value in 1) {}", "must iterate over"),
        ("foreach (void value in ({})) {}", "cannot be `void`"),
        (
            "foreach (value in ({ 1 })) {} value = 2;",
            "undefined variable `value`",
        ),
    ] {
        let error = Compiler::new(test_config())
            .compile_string("/foreach.c", format!("void create() {{ {body} }}"))
            .await
            .unwrap_err();
        assert!(error.to_string().contains(expected), "{error}");
    }
}
