use lpc_rs_errors::Result;
use lpc_rs_utils::string::MAX_STRING_LENGTH;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Replace a selected range of non-overlapping literal matches in a string.
pub fn replace_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("replace_string: text must be a string"));
    };
    let Some(pattern) = context.arg(1).as_str() else {
        return Err(context.runtime_error("replace_string: pattern must be a string"));
    };
    let Some(replacement) = context.arg(2).as_str() else {
        return Err(context.runtime_error("replace_string: replacement must be a string"));
    };
    let mut bounds = [0, 0];
    for (i, bound) in bounds.iter_mut().enumerate() {
        if context.arg_count() > i + 3 {
            let LpcRef::Int(value) = context.arg(i + 3) else {
                return Err(context
                    .runtime_error(format!("replace_string: argument {} must be an int", i + 4)));
            };
            *bound = value.0;
        }
    }
    let [first, last] = if context.arg_count() == 4 {
        [1, bounds[0]]
    } else {
        bounds
    };
    let skip = usize::try_from(first.max(1) - 1).unwrap_or(usize::MAX);
    let end = if last == 0 {
        usize::MAX
    } else {
        usize::try_from(last.max(0)).unwrap_or(usize::MAX)
    };
    let count = end.saturating_sub(skip);
    if pattern.is_empty() || pattern == replacement || count == 0 {
        context.return_efun_result(context.arg(0).clone());
        return Ok(());
    }

    let matches = text.match_indices(pattern).skip(skip).take(count);
    let replacements = matches.clone().count();
    if replacements == 0 {
        context.return_efun_result(context.arg(0).clone());
        return Ok(());
    }

    let length = replacements
        .checked_mul(replacement.len())
        .and_then(|added| (text.len() - replacements * pattern.len()).checked_add(added))
        .filter(|length| *length <= MAX_STRING_LENGTH)
        .ok_or_else(|| {
            context.runtime_error("replace_string: result exceeds maximum string length")
        })?;
    let mut result = String::with_capacity(length);
    let mut copied = 0;
    for (offset, matched) in matches {
        result.push_str(&text[copied..offset]);
        result.push_str(replacement);
        copied = offset + matched.len();
    }
    result.push_str(&text[copied..]);
    context.return_efun_result(result.into());
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::string::MAX_STRING_LENGTH;

    use crate::{
        compiler::CompilerBuilder,
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, test_config, try_run_prog},
    };

    async fn result_of(expression: &str) -> LpcRef {
        run_prog(&format!("string create() {{ return {expression}; }}"))
            .await
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn replaces_literal_matches_without_changing_the_source() {
        let task = run_prog(
            r#"
            string create() {
                string original = "a.b.a";
                string result = replace_string(original, ".", "--");
                return original + ":" + result;
            }
            "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from("a.b.a:a--b--a")));
        assert_eq!(
            result_of(r#"replace_string("AaA", "a", "$1")"#).await,
            LpcRef::from("A$1A")
        );
    }

    #[tokio::test]
    async fn replacements_are_non_overlapping_and_not_searched_again() {
        for (expression, expected) in [
            (r#"replace_string("aaaaa", "aa", "x")"#, "xxa"),
            (r#"replace_string("aaa", "a", "aa")"#, "aaaaaa"),
            (r#"replace_string("ababa", "aba", "x")"#, "xba"),
            (r#"replace_string("a-b-c", "-", "")"#, "abc"),
        ] {
            assert_eq!(result_of(expression).await, LpcRef::from(expected));
        }
    }

    #[tokio::test]
    async fn optional_count_limits_replacements_and_zero_means_all() {
        for (limit, expected) in [(1, "zyxx"), (2, "zyzx"), (0, "zyzz"), (-1, "xyxx")] {
            assert_eq!(
                result_of(&format!(r#"replace_string("xyxx", "x", "z", {limit})"#)).await,
                LpcRef::from(expected)
            );
        }
    }

    #[tokio::test]
    async fn occurrence_ranges_are_inclusive_and_one_based() {
        for (bounds, expected) in [
            ("2, 3", "xyzz"),
            ("2, 2", "xyzx"),
            ("2, 0", "xyzz"),
            ("0, 2", "zyzx"),
            ("-9, 2", "zyzx"),
            ("3, 2", "xyxx"),
            ("1, -1", "xyxx"),
            ("-9, -1", "xyxx"),
            ("9, 0", "xyxx"),
            ("1, 9223372036854775807", "zyzz"),
            ("9223372036854775807, 0", "xyxx"),
        ] {
            let expression = format!(r#"replace_string("xyxx", "x", "z", {bounds})"#);
            assert_eq!(
                result_of(&expression).await,
                LpcRef::from(expected),
                "{bounds}"
            );
        }
        assert_eq!(
            result_of(r#"replace_string("aaaaa", "aa", "x", 2, 2)"#).await,
            LpcRef::from("aaxa")
        );
    }

    #[tokio::test]
    async fn empty_patterns_and_absent_matches_leave_text_unchanged() {
        for (expression, expected) in [
            (r#"replace_string("abc", "", "x")"#, "abc"),
            (r#"replace_string("", "", "x")"#, ""),
            (r#"replace_string("", "a", "x")"#, ""),
            (r#"replace_string("abc", "abcd", "x")"#, "abc"),
            (r#"replace_string("abc", "b", "b")"#, "abc"),
        ] {
            assert_eq!(result_of(expression).await, LpcRef::from(expected));
        }
    }

    #[tokio::test]
    async fn handles_unicode_scalars_and_embedded_nuls() {
        for (expression, expected) in [
            (r#"replace_string("é猫é猫", "猫", "🐉", 2)"#, "é🐉é🐉"),
            (
                "replace_string(\"e\u{301}e\u{301}\", \"\u{301}\", \"!\", 2, 2)",
                "e\u{301}e!",
            ),
            (
                r#"replace_string("a" + sprintf("%c", 0) + "a", "a", "x")"#,
                "x\0x",
            ),
            (
                r#"replace_string(sprintf("a%cb%c", 0, 0), sprintf("%c", 0), "猫", 2, 2)"#,
                "a\0b猫",
            ),
            (r#"replace_string("aa", "a", sprintf("%c", 0))"#, "\0\0"),
        ] {
            assert_eq!(result_of(expression).await, LpcRef::from(expected));
        }
    }

    #[tokio::test]
    async fn validates_mixed_arguments_even_when_no_replacements_are_needed() {
        for (expression, expected) in [
            (r#"replace_string(bad, "", "x")"#, "text must be a string"),
            (
                r#"replace_string("", bad, "x")"#,
                "pattern must be a string",
            ),
            (
                r#"replace_string("", "", bad)"#,
                "replacement must be a string",
            ),
            (
                r#"replace_string("", "", "", text)"#,
                "argument 4 must be an int",
            ),
            (
                r#"replace_string("", "", "", 0, text)"#,
                "argument 5 must be an int",
            ),
        ] {
            let code = format!(
                "mixed create() {{ mixed bad = 0; mixed text = \"a\"; return {expression}; }}"
            );
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(error.contains(expected), "{expression}: {error}");
        }
    }

    #[tokio::test]
    async fn checks_the_final_utf8_byte_length_before_allocating() {
        assert_eq!(
            result_of(&format!(
                r#"replace_string("a" * {}, "a", "é")"#,
                MAX_STRING_LENGTH / 2
            ))
            .await,
            LpcRef::from("é".repeat(MAX_STRING_LENGTH / 2))
        );
        let code = format!(
            r#"string create() {{ return replace_string("a" * {}, "a", "é"); }}"#,
            MAX_STRING_LENGTH / 2 + 1
        );
        let error = try_run_prog(&code).await.unwrap_err().to_string();
        assert!(
            error.contains("replace_string: result exceeds maximum string length"),
            "{error}"
        );
        assert_eq!(
            result_of(&format!(
                r#"replace_string("a" * {}, "a", "bb", 1)"#,
                MAX_STRING_LENGTH - 1
            ))
            .await,
            LpcRef::from(format!("bb{}", "a".repeat(MAX_STRING_LENGTH - 2)))
        );
    }

    #[tokio::test]
    async fn can_shrink_large_existing_strings_or_return_them_unchanged() {
        let source = "a".repeat(MAX_STRING_LENGTH + 1);
        assert_eq!(
            result_of(&format!(r#"replace_string("{source}", "a", "", 1)"#)).await,
            LpcRef::from("a".repeat(MAX_STRING_LENGTH))
        );
        assert_eq!(
            result_of(&format!(r#"replace_string("{source}", "x", "z")"#)).await,
            LpcRef::from(source)
        );
    }

    #[tokio::test]
    async fn efun_pointers_preserve_the_supplied_optional_argument_count() {
        let task = run_prog(
            r#"
            string create() {
                function f = &replace_string();
                return f("aaa", "a", "x") + ":" +
                       f("aaa", "a", "x", 2) + ":" +
                       f("aaa", "a", "x", 2, 2);
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from("xxx:xxa:axa")));
    }

    #[tokio::test]
    async fn direct_calls_reject_wrong_argument_counts() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        for expression in [
            r#"replace_string("abc", "a")"#,
            r#"replace_string("abc", "a", "b", 1, 2, 3)"#,
        ] {
            let code = format!("string create() {{ return {expression}; }}");
            let error = compiler
                .compile_string("/replace_string_args.c", &code)
                .await
                .unwrap_err()
                .to_string();
            assert!(
                error.contains("incorrect argument count in call to `replace_string`"),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn caught_overflow_leaves_the_assignment_target_unchanged() {
        let code = format!(
            r#"
            int create() {{
                string text = "a" * {MAX_STRING_LENGTH};
                string original = text;
                mixed error = catch(text = replace_string(text, "a", "aa", 1));
                return stringp(error) && text == original;
            }}
        "#
        );
        assert_eq!(run_prog(&code).await.result(), Some(LpcRef::from(1)));
    }
}
