use super::{fails, run, run_nested, run_with, s};
use crate::{
    compiler::Compiler,
    interpreter::lpc_ref::LpcRef,
    test_support::{lib_holding, temp_lib_config, test_config},
};

async fn values(code: &str) -> Vec<LpcRef> {
    run("", &[], code).await
}

async fn compile_error(code: &str) -> String {
    Compiler::new(test_config())
        .compile_string("/reverse.c", code)
        .await
        .unwrap_err()
        .to_string()
}

#[tokio::test]
async fn array_distances_accept_expressions_and_leave_comparisons_intact() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({ 10, 20, 30, 40 });
                int n = 3;
                return ({ a[<1], a[<n], a[<1 + 1], a[1 < 2], a[<n > 2] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(40),
            LpcRef::from(20),
            LpcRef::from(30),
            LpcRef::from(20),
            LpcRef::from(40)
        ]
    );
}

#[tokio::test]
async fn string_distances_count_unicode_scalars() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                string text = "aé🙂é";
                return ({ text[<1], text[<2], text[<3], text[<4], text[<5] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(0x301),
            LpcRef::from(101),
            LpcRef::from(0x1f642),
            LpcRef::from(233),
            LpcRef::from(97)
        ]
    );
}

#[tokio::test]
async fn bytes_distances_count_bytes() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                bytes b = to_bytes(({ 0, 128, 255 }));
                return ({ b[<1], b[<2], b[<3], b[<0], b[<4] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(255),
            LpcRef::from(128),
            LpcRef::from(0),
            LpcRef::from(0),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn invalid_array_distances_raise_catchable_errors() {
    for distance in [
        "0",
        "-1",
        "4",
        "9223372036854775807",
        "-9223372036854775808",
    ] {
        let error = fails(
            "",
            &[],
            &format!("int create() {{ int *a = ({{ 1, 2, 3 }}); return a[<{distance}]; }}"),
        )
        .await;
        assert!(error.contains("index"), "{distance}: {error}");
    }
}

#[tokio::test]
async fn invalid_string_distances_return_zero_without_wrapping() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                string text = "abc";
                return ({ text[<0], text[<-1], text[<4], text[<9223372036854775807],
                    text[<-9223372036854775808], ""[<1] });
            }
        "#
        )
        .await,
        vec![LpcRef::from(0); 6]
    );
}

#[tokio::test]
async fn each_reverse_slice_form_works_on_strings() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                string text = "aé🙂z";
                return ({ text[<3..<2], text[<3..2], text[1..<2], text[<2..],
                    text[..<2], text[0.. < 2], text[..<4] });
            }
        "#
        )
        .await,
        vec![
            s("é🙂"),
            s("é🙂"),
            s("é🙂"),
            s("🙂z"),
            s("aé🙂"),
            s("aé🙂"),
            s("a")
        ]
    );
}

#[tokio::test]
async fn reverse_array_slices_clip_each_endpoint() {
    let (result, txn) = run_nested(
        "",
        &[],
        r#"
        mixed *create() {
            int *a = ({ 1, 2, 3 });
            return ({ a[<99..<1], a[<3..<0], a[0..<4], a[<0..], a[<1..<3], a[<2..1], a[..<3] });
        }
    "#,
    )
    .await;
    let slices = result
        .with_array(&txn, |a| a.iter().cloned().collect::<Vec<_>>())
        .unwrap();
    let actual: Vec<Vec<LpcRef>> = slices
        .iter()
        .map(|slice| {
            slice
                .with_array(&txn, |a| a.iter().cloned().collect())
                .unwrap()
        })
        .collect();
    assert_eq!(
        actual,
        vec![
            vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)],
            vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)],
            vec![],
            vec![],
            vec![],
            vec![LpcRef::from(2)],
            vec![LpcRef::from(1)]
        ]
    );
}

#[tokio::test]
async fn reverse_bytes_slices_clip_each_endpoint() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                bytes b = to_bytes("abc", "UTF-8");
                return ({ to_text(b[<99..<1], "UTF-8"), to_text(b[..<0], "UTF-8"),
                    sizeof(b[..<4]), sizeof(b[<0..]), to_text(b[<2..1], "UTF-8"),
                    to_text(b[..<3], "UTF-8") });
            }
        "#
        )
        .await,
        vec![
            s("abc"),
            s("abc"),
            LpcRef::from(0),
            LpcRef::from(0),
            s("b"),
            s("a")
        ]
    );
}

#[tokio::test]
async fn reverse_slice_extremes_do_not_overflow_or_wrap() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                string text = "abc";
                int lo = -9223372036854775808;
                int hi = 9223372036854775807;
                return ({ text[<hi..<1], text[<lo..], text[..<hi], text[..<lo],
                    text[<-1..], text[..<-1], text[<0..<0] });
            }
        "#
        )
        .await,
        vec![s("abc"), s(""), s(""), s("abc"), s(""), s("abc"), s("")]
    );
}

#[tokio::test]
async fn empty_reverse_slices_keep_the_collection_type() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({});
                bytes b = to_bytes(({}));
                return ({ sizeof(a[<1..<0]), ""[<1..<0], sizeof(b[<1..<0]),
                    typeof(a[<1..]), typeof(b[<1..]) });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(0),
            s(""),
            LpcRef::from(0),
            LpcRef::from(5),
            LpcRef::from(8)
        ]
    );
}

#[tokio::test]
async fn native_negative_indices_and_slice_bounds_keep_their_behavior() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({ 10, 20, 30 });
                string text = "abc";
                return ({ a[-1], a[-3], text[-1], text[-2..-1], text[<2..-1],
                    text[-2..<1], text[-99..], text[0..-99] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(30),
            LpcRef::from(10),
            LpcRef::from(99),
            s("bc"),
            s("bc"),
            s("bc"),
            s(""),
            s("abc")
        ]
    );
}

#[tokio::test]
async fn array_assignments_and_steps_use_the_reverse_element() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                int *alias = a;
                a[<1] = 10;
                a[<2] += 5;
                int before = a[<1]++;
                int after = --a[<2];
                return ({ alias[2], alias[1], before, after });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(11),
            LpcRef::from(6),
            LpcRef::from(10),
            LpcRef::from(6)
        ]
    );
}

#[tokio::test]
async fn simple_assignment_and_step_evaluate_receiver_and_distance_once() {
    assert_eq!(
        values(
            r#"
            int *a = ({ 1, 2, 3 });
            int receivers;
            int distances;
            int *receiver() { receivers++; return a; }
            int distance() { distances++; return 1; }
            mixed *create() {
                receiver()[<distance()] = 8;
                int before = receiver()[<distance()]++;
                return ({ receivers, distances, before, a[2] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(2),
            LpcRef::from(2),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn implicit_efun_output_writes_back_to_the_reverse_element() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                int n = 1;
                int matched = sscanf("42", "%d", a[<n++]);
                return ({ matched, n, a[2], a[1] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(42),
            LpcRef::from(2)
        ]
    );
}

#[tokio::test]
async fn invalid_array_writes_leave_elements_unchanged() {
    assert_eq!(
        values(
            r#"
            mixed *create() {
                int *a = ({ 1, 2 });
                mixed zero = catch(a[<0] = 9);
                mixed far = catch(a[<3] = 9);
                return ({ stringp(zero), stringp(far), a[0], a[1] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(2)
        ]
    );
}

#[tokio::test]
async fn reverse_access_in_inherited_closures_survives_register_remapping() {
    let root = lib_holding(
        "reverse-index-inheritance",
        &[
            ("prefix.c", "int padding = 99;"),
            (
                "parent.c",
                r#"
            int *a = ({ 10, 20, 30 });
            function last() { return (: a[<1] :); }
            string tail(string text) { return text[<2..]; }
        "#,
            ),
        ],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&root),
            "",
            &[],
            r#"
            inherit "/prefix";
            inherit "/parent";
            mixed *create() {
                function saved = last();
                return ({ saved(), tail("abc") });
            }
        "#
        )
        .await,
        vec![LpcRef::from(30), s("bc")]
    );
}

#[tokio::test]
async fn statically_invalid_reverse_operands_are_rejected() {
    for expr in [
        "({ 1 })[<1.5]",
        "\"abc\"[<\"x\"]",
        "([ 1: 2 ])[<1]",
        "([ 1: 2 ])[0..<1]",
    ] {
        let error = compile_error(&format!("mixed create() {{ return {expr}; }}")).await;
        assert!(
            error.contains("Expected `int`") || error.contains("Reverse indexing requires"),
            "{expr}: {error}"
        );
    }
}

#[tokio::test]
async fn dynamically_invalid_reverse_operands_raise_errors_even_on_empty_values() {
    for expr in ["a[<distance]", "a[<distance..]", "a[..<distance]"] {
        let error = fails(
            "",
            &[],
            &format!(
                "mixed create() {{ mixed a = ({{}}); mixed distance = \"bad\"; return {expr}; }}"
            ),
        )
        .await;
        assert!(error.contains("integer"), "{expr}: {error}");
    }
    let error = fails(
        "",
        &[],
        "mixed create() { mixed a = ([ 1: 2 ]); return a[<1]; }",
    )
    .await;
    assert!(error.contains("Reverse indexing requires"), "{error}");
}

#[tokio::test]
async fn reverse_marker_is_only_syntax_for_subscript_bounds() {
    for code in [
        "int create() { return <1; }",
        "int create() { return sizeof(<1); }",
    ] {
        let error = compile_error(code).await;
        assert!(error.contains("Unrecognized Token"), "{error}");
    }
}

#[tokio::test]
async fn reverse_slices_cannot_be_stepped_or_used_as_output_arguments() {
    for (statement, diagnostic) in [
        ("a[<2..<1]++;", "Expected `int`"),
        (
            "sscanf(\"42\", \"%d\", a[<2..<1]);",
            "A slice cannot be an output argument",
        ),
    ] {
        let error = compile_error(&format!(
            "void create() {{ int *a = ({{ 1, 2 }}); {statement} }}"
        ))
        .await;
        assert!(error.contains(diagnostic), "{error}");
    }
}
