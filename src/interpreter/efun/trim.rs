use std::collections::HashSet;

use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;
use lpc_rs_utils::string::MAX_STRING_LENGTH;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Remove matching Unicode scalars from both ends (0), the left (1), or the right (2).
pub fn trim<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("trim: text must be a string"));
    };
    let where_ = if context.arg_count() > 1 {
        let LpcRef::Int(value) = context.arg(1) else {
            return Err(context.runtime_error("trim: where must be an int"));
        };
        value.0
    } else {
        0
    };
    if !(0..=2).contains(&where_) {
        return Err(context.runtime_error("trim: where must be 0 (both), 1 (left), or 2 (right)"));
    }

    let result = if context.arg_count() < 3 {
        trim_ends(text, where_, |c| {
            matches!(c, ' ' | '\t' | '\n' | '\r' | '\u{b}' | '\u{c}')
        })
    } else {
        match context.arg(2) {
            LpcRef::String(value) => {
                let chars: HashSet<char> = value.chars().collect();
                trim_ends(text, where_, |c| chars.contains(&c))
            }
            LpcRef::Int(value) => {
                let character = u32::try_from(value.0)
                    .ok()
                    .and_then(char::from_u32)
                    .ok_or_else(|| context.runtime_error("trim: invalid Unicode scalar value"))?;
                trim_ends(text, where_, |c| c == character)
            }
            _ => return Err(context.runtime_error("trim: chars must be a string or int")),
        }
    };

    let result = if result.len() == text.len() {
        context.arg(0).clone()
    } else {
        if result.len() > MAX_STRING_LENGTH {
            return Err(context.runtime_error("trim: result exceeds maximum string length"));
        }
        result.into()
    };
    context.return_efun_result(result);
    Ok(())
}

fn trim_ends(text: &str, where_: LpcIntInner, matches: impl FnMut(char) -> bool) -> &str {
    match where_ {
        0 => text.trim_matches(matches),
        1 => text.trim_start_matches(matches),
        _ => text.trim_end_matches(matches),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::{
        compiler::CompilerBuilder,
        interpreter::CommittedReader,
        test_support::{run_prog, run_prog_with_config, test_config, try_run_prog},
    };

    async fn result_of(expression: &str) -> LpcRef {
        run_prog(&format!("string create() {{ return {expression}; }}"))
            .await
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn modes_select_ends_and_preserve_interior_whitespace() {
        for (expression, expected) in [
            (r#"trim(" \thello \n world\r\n")"#, "hello \n world"),
            (r#"trim(" \thello \n world\r\n", 0)"#, "hello \n world"),
            (r#"trim(" \thello \n world\r\n", 1)"#, "hello \n world\r\n"),
            (r#"trim(" \thello \n world\r\n", 2)"#, " \thello \n world"),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn default_set_is_exactly_the_six_ascii_whitespace_characters() {
        for scalar in [9, 10, 11, 12, 13, 32] {
            let expression = format!(r#"trim(sprintf("%chello%c", {scalar}, {scalar}))"#);
            assert_eq!(result_of(&expression).await, LpcRef::from("hello"));
        }
        for scalar in [0, 8, 0x1c, 0x85, 0xa0, 0x2003, 0x200b, 0x3000] {
            let expression = format!(r#"trim(sprintf("%chello%c", {scalar}, {scalar}))"#);
            let c = char::from_u32(scalar).unwrap();
            assert_eq!(
                result_of(&expression).await,
                LpcRef::from(format!("{c}hello{c}"))
            );
        }
    }

    #[tokio::test]
    async fn empty_single_and_all_matching_inputs_work_in_every_mode() {
        for mode in 0..=2 {
            for (input, expected) in [("", ""), (" ", ""), ("   ", ""), ("猫", "猫")] {
                let expression = format!(r#"trim("{input}", {mode})"#);
                assert_eq!(result_of(&expression).await, LpcRef::from(expected));
            }
        }
    }

    #[tokio::test]
    async fn custom_sets_replace_defaults_and_ignore_order_and_duplicates() {
        for (expression, expected) in [
            (r#"trim("xx.. hello ..xx", 0, "x.")"#, " hello "),
            (r#"trim("xx.. hello ..xx", 0, ".xxx.")"#, " hello "),
            (r#"trim("xx.. hello ..xx", 1, "x.")"#, " hello ..xx"),
            (r#"trim("xx.. hello ..xx", 2, "x.")"#, "xx.. hello "),
            (r#"trim(" hello ", 0, "")"#, " hello "),
            (r#"trim("　《三字经》　", 0, "　")"#, "《三字经》"),
            (r#"trim("《三字经》", 0, "　")"#, "《三字经》"),
            (r#"trim("🐉猫hello猫🐉", 0, "猫🐉")"#, "hello"),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn scalar_matching_does_not_normalize_or_group_combining_marks() {
        for (expression, expected) in [
            (r#"trim("xxhelloxx", 1, 'x')"#, "helloxx"),
            (r#"trim("猫hello猫", 2, 0x732b)"#, "猫hello"),
            (r#"trim("🐉hello🐉", 0, 0x1f409)"#, "hello"),
            ("trim(\"e\u{301}\", 0, \"é\")", "e\u{301}"),
            ("trim(\"e\u{301}\", 2, 0x301)", "e"),
            ("trim(\"\u{301}e\u{301}\", 1, \"\u{301}\")", "e\u{301}"),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn nul_is_preserved_or_explicitly_trimmed_without_truncating_text() {
        for (expression, expected) in [
            (r#"trim(sprintf("  a%cb  ", 0))"#, "a\0b"),
            (r#"trim(sprintf("%ca%cb%c", 0, 0, 0), 0, 0)"#, "a\0b"),
            (r#"trim(sprintf("%ca%cb%c", 0, 0, 0), 1, 0)"#, "a\0b\0"),
            (
                r#"trim(sprintf("%ca%cb%c", 0, 0, 0), 2, sprintf("%c", 0))"#,
                "\0a\0b",
            ),
            (r#"trim(sprintf("%c", 0), 0, 0)"#, ""),
        ] {
            assert_eq!(
                result_of(expression).await,
                LpcRef::from(expected),
                "{expression}"
            );
        }
    }

    #[tokio::test]
    async fn invalid_modes_and_scalars_are_rejected_even_for_empty_text() {
        for mode in ["-1", "3", "4", "9223372036854775807"] {
            let code = format!(r#"string create() {{ return trim("", {mode}); }}"#);
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                error.contains("trim: where must be 0 (both), 1 (left), or 2 (right)"),
                "{error}"
            );
        }
        for scalar in ["-1", "0xd800", "0xdfff", "0x110000", "9223372036854775807"] {
            let code = format!(r#"string create() {{ return trim("", 0, {scalar}); }}"#);
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                error.contains("trim: invalid Unicode scalar value"),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn mixed_types_are_validated_even_for_noop_inputs() {
        for (expression, expected) in [
            ("trim(bad)", "text must be a string"),
            (r#"trim("", bad)"#, "where must be an int"),
            (r#"trim("", 0, bad)"#, "chars must be a string or int"),
        ] {
            let code = format!("string create() {{ mixed bad = ({{}}); return {expression}; }}");
            let error = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(error.contains(expected), "{error}");
        }
    }

    #[tokio::test]
    async fn direct_calls_check_counts_and_declared_types() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        for (expression, expected) in [
            ("trim()", "incorrect argument count"),
            (r#"trim("x", 0, "x", 0)"#, "incorrect argument count"),
            ("trim(42)", "unexpected argument type"),
            (r#"trim("x", "x")"#, "unexpected argument type"),
            (r#"trim("x", 0, ({}))"#, "unexpected argument type"),
        ] {
            let error = compiler
                .compile_string(
                    "/trim_args.c",
                    &format!("string create() {{ return {expression}; }}"),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(expected), "{expression}: {error}");
        }
    }

    #[tokio::test]
    async fn defaults_and_function_callbacks_work_in_both_null_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let config = crate::test_config_builder!()
                .null_mode(mode)
                .build()
                .unwrap();
            let code = format!(
                "{}\n{}",
                include_str!("../../../ulib/include/strings.h"),
                r#"
                int create() {
                    function f = trim;
                    string *lines = ({ "  one  ", "  two\n" });
                    string nul = sprintf("%c", 0);
                    return f(" x ") == "x" && f(" x ", TRIM_BOTH) == "x" &&
                        f(" x ", TRIM_LEFT) == "x " && f(" x ", TRIM_RIGHT) == " x" &&
                        f(" x ", TRIM_BOTH, "") == " x " &&
                        f(nul + "x" + nul, TRIM_BOTH, 0) == "x" &&
                        implode(map(lines, trim), "/") == "one/two" &&
                        implode(map(lines, trim, TRIM_RIGHT), "/") == "  one/  two" &&
                        lines[0] == "  one  ";
                }
            "#
            );
            let task = run_prog_with_config(&code, config.into()).await;
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode:?}");
        }
    }

    #[tokio::test]
    async fn distinct_nil_is_not_an_omitted_argument() {
        let config = crate::test_config_builder!()
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap();
        let task = run_prog_with_config(
            r#"
            int create() {
                mixed n = nil;
                return stringp(catch(trim(n))) && stringp(catch(trim("", n))) &&
                    stringp(catch(trim("", 0, n)));
            }
        "#,
            config.into(),
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn noop_reuses_the_string_and_changed_results_leave_the_source_intact() {
        let task = run_prog(
            r#"
            string original = "unchanged";
            string create() {
                string changed = trim(original, 0, "u");
                if (changed != "nchanged" || original != "unchanged") return "wrong";
                return trim(original);
            }
        "#,
        )
        .await;
        let LpcRef::String(original) = task
            .context
            .global_state
            .committed_global(&task.context.process, 0u16)
        else {
            panic!("string expected")
        };
        let Some(LpcRef::String(result)) = task.result() else {
            panic!("string expected")
        };
        assert!(Arc::ptr_eq(&original, &result));
    }

    #[tokio::test]
    async fn changed_results_enforce_utf8_byte_limits_while_noops_pass_through() {
        let limit = "é".repeat(MAX_STRING_LENGTH / 2);
        assert_eq!(
            result_of(&format!(r#"trim(" {limit} ")"#)).await,
            LpcRef::from(limit.clone())
        );
        let oversized = format!("{limit}é");
        assert_eq!(
            result_of(&format!(r#"trim("{oversized}")"#)).await,
            LpcRef::from(oversized.clone())
        );
        let code = format!(
            r#"int create() {{ string text = " {oversized}"; string original = text;
            mixed error = catch(text = trim(text));
            return stringp(error) && strindex(error, "trim: result exceeds maximum string length") >= 0 && text == original;
        }}"#
        );
        assert_eq!(run_prog(&code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn local_function_shadows_trim_without_hiding_explicit_efun_calls() {
        let task = run_prog(
            r#"
            string trim(string text) { return "local:" + text; }
            int create() {
                function f = trim;
                return trim(" x ") == "local: x " && f("y") == "local:y" &&
                    efun::trim(" x ") == "x";
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
