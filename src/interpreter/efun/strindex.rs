use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Find a literal string or Unicode scalar, returning its character index or -1.
pub fn strindex<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("strindex: text must be a string"));
    };
    let mut encoded = [0; 4];
    let needle = match context.arg(1) {
        LpcRef::String(value) => value.to_str(),
        LpcRef::Int(value) => {
            let character = u32::try_from(value.0)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| context.runtime_error("strindex: invalid Unicode scalar value"))?;
            character.encode_utf8(&mut encoded)
        }
        _ => return Err(context.runtime_error("strindex: needle must be a string or int")),
    };
    let LpcRef::Int(reverse) = context.arg(2) else {
        return Err(context.runtime_error("strindex: reverse must be an int"));
    };
    let found = if reverse.0 == 0 {
        text.find(needle)
    } else {
        text.rfind(needle)
    };
    let index = found.map_or(-1, |offset| text[..offset].chars().count() as LpcIntInner);
    context.return_efun_result(index.into());
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::CompilerBuilder,
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, test_config, try_run_prog},
    };

    async fn result_of(expression: &str) -> LpcRef {
        run_prog(&format!("int create() {{ return {expression}; }}"))
            .await
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn finds_first_and_last_literal_matches() {
        for (expression, expected) in [
            (r#"strindex("abcabc", "bc")"#, 1),
            (r#"strindex("abcabc", "bc", 0)"#, 1),
            (r#"strindex("abcabc", "bc", -1)"#, 4),
            (r#"strindex("abcabc", "bc", 1)"#, 4),
            (r#"strindex("abcabc", "bc", 42)"#, 4),
            (r#"strindex("abc", "abc", -1)"#, 0),
            (r#"strindex("ababa", "aba", -1)"#, 2),
            (r#"strindex("a.b", ".")"#, 1),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn absent_matches_return_minus_one() {
        for expression in [
            r#"strindex("abc", "xyz")"#,
            r#"strindex("abc", "abcd")"#,
            r#"strindex("abc", "A")"#,
            r#"strindex("abc", "xyz", -1)"#,
            r#"strindex("", "a")"#,
            r#"strindex("", 'a', 1)"#,
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(-1),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn positions_count_unicode_scalars_and_work_with_slices() {
        for (expression, expected) in [
            (r#"strindex("é猫🐉é猫", "猫")"#, 1),
            (r#"strindex("é猫🐉é猫", "猫", -1)"#, 4),
            ("strindex(\"e\u{301}x\", \"x\")", 2),
            ("strindex(\"e\u{301}x\", \"\u{301}\")", 1),
            ("strindex(\"ab\u{301}c\", \"ab\", -1)", 0),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
        let task = run_prog(
            r#"
            string create() {
                string path = "/域/房间";
                return path[strindex(path, '/', -1) + 1..];
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from("房间")));
    }

    #[tokio::test]
    async fn integer_needles_are_unicode_scalars() {
        for (expression, expected) in [
            (r#"strindex("abcabc", 'b')"#, 1),
            (r#"strindex("abcabc", 'b', -1)"#, 4),
            (r#"strindex("é猫🐉", 0x732b)"#, 1),
            (r#"strindex("é猫🐉", 0x1f409)"#, 2),
            (r#"strindex("é猫🐉", 0x10ffff)"#, -1),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn empty_needles_match_the_first_or_last_boundary() {
        for (expression, expected) in [
            (r#"strindex("abc", "")"#, 0),
            (r#"strindex("é猫", "", -1)"#, 2),
            (r#"strindex("", "")"#, 0),
            (r#"strindex("", "", -1)"#, 0),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn nul_is_searchable_data_without_a_synthetic_terminator() {
        for (expression, expected) in [
            (r#"strindex(sprintf("a%cb%cc", 0, 0), 0)"#, 1),
            (
                r#"strindex(sprintf("a%cb%cc", 0, 0), sprintf("%c", 0), -1)"#,
                3,
            ),
            (
                r#"strindex(sprintf("a%cb%cc", 0, 0), sprintf("b%cc", 0))"#,
                2,
            ),
            (r#"strindex("abc", 0)"#, -1),
            (r#"strindex("", 0)"#, -1),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn invalid_scalars_are_errors_even_for_empty_text() {
        for value in ["-1", "0xd800", "0xdfff", "0x110000", "9223372036854775807"] {
            let code = format!(r#"int create() {{ return strindex("", {value}); }}"#);
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                error.contains("strindex: invalid Unicode scalar value"),
                "{value}: {error}"
            );
        }
    }

    #[tokio::test]
    async fn mixed_arguments_are_checked_at_runtime() {
        for (expression, expected) in [
            (r#"strindex(bad, "a")"#, "text must be a string"),
            (
                r#"strindex("a", bad_array)"#,
                "needle must be a string or int",
            ),
            (
                r#"strindex("a", "a", bad_string)"#,
                "reverse must be an int",
            ),
        ] {
            let code = format!(
                r#"int create() {{ mixed bad = 0; mixed bad_array = ({{}}); mixed bad_string = "a"; return {expression}; }}"#
            );
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(error.contains(expected), "{expression}: {error}");
        }
    }

    #[tokio::test]
    async fn works_through_efun_pointers_with_optional_arguments() {
        let task = run_prog(
            r#"
            int create() {
                function f = &strindex();
                return f("abcabc", "ab") == 0 && f("abcabc", "ab", -1) == 3;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn direct_calls_reject_wrong_argument_counts() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        for expression in [r#"strindex("abc")"#, r#"strindex("abc", "a", 0, 1)"#] {
            let code = format!("int create() {{ return {expression}; }}");
            let error = compiler
                .compile_string("/strindex_args.c", &code)
                .await
                .unwrap_err()
                .to_string();
            assert!(
                error.contains("incorrect argument count in call to `strindex`"),
                "{error}"
            );
        }
    }
}
