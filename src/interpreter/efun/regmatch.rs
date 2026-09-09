//! The first regex match, optionally with captures and a continuation index.

use lpc_rs_errors::Result;
use regex::RegexBuilder;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

const RE_CASELESS: i64 = 0x0004;
const RE_MULTILINE: i64 = 0x0008;
const RE_DOTALL: i64 = 0x0010;
const RE_EXTENDED: i64 = 0x0020;
const RE_ANCHORED: i64 = 0x0040;
const RE_UNGREEDY: i64 = 0x0400;
const RE_MATCH_SUBS: i64 = 0x1000;
const SUPPORTED_FLAGS: i64 = RE_CASELESS
    | RE_MULTILINE
    | RE_DOTALL
    | RE_EXTENDED
    | RE_ANCHORED
    | RE_UNGREEDY
    | RE_MATCH_SUBS;

/// `regmatch(text, pattern [, flags [, start]])`: the first match or 0;
/// `RE_MATCH_SUBS` returns the full match, captures, and next character index.
pub fn regmatch<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("regmatch: text must be a string"));
    };
    let Some(pattern) = context.arg(1).as_str() else {
        return Err(context.runtime_error("regmatch: pattern must be a string"));
    };
    let mut options = [0, 0];
    for (i, name) in ["flags", "start"].into_iter().enumerate() {
        if context.arg_count() > i + 2 {
            let LpcRef::Int(n) = context.arg(i + 2) else {
                return Err(context.runtime_error(format!("regmatch: {name} must be an int")));
            };
            options[i] = n.0;
        }
    }
    let [flags, start] = options;
    if flags & !SUPPORTED_FLAGS != 0 {
        return Err(context.runtime_error(format!("regmatch: unsupported flags {flags:#x}")));
    }
    let byte_start = usize::try_from(start)
        .ok()
        .and_then(|start| {
            text.char_indices()
                .map(|(i, _)| i)
                .chain(std::iter::once(text.len()))
                .nth(start)
        })
        .ok_or_else(|| {
            context.runtime_error(format!(
                "regmatch: start index {start} out of range [0..{}]",
                text.chars().count()
            ))
        })?;
    let re = RegexBuilder::new(pattern)
        .case_insensitive(flags & RE_CASELESS != 0)
        .multi_line(flags & RE_MULTILINE != 0)
        .dot_matches_new_line(flags & RE_DOTALL != 0)
        .ignore_whitespace(flags & RE_EXTENDED != 0)
        .swap_greed(flags & RE_UNGREEDY != 0)
        .build()
        .map_err(|e| {
            context.runtime_error(format!("regmatch: invalid pattern {pattern:?}: {e}"))
        })?;
    let anchored = flags & RE_ANCHORED != 0;
    // Search the original text so an offset cannot create a new ^ or word boundary.
    let result = if flags & RE_MATCH_SUBS == 0 {
        re.find_at(text, byte_start)
            .filter(|m| !anchored || m.start() == byte_start)
            .map_or_else(|| LpcRef::from(0), |m| LpcRef::from(m.as_str()))
    } else if let Some(captures) = re.captures_at(text, byte_start) {
        let matched = captures
            .get(0)
            .ok_or_else(|| context.runtime_bug("regmatch: missing full match"))?;
        if anchored && matched.start() != byte_start {
            LpcRef::from(0)
        } else {
            let next = text[..matched.end()].chars().count() + usize::from(matched.is_empty());
            let items = captures
                .iter()
                .map(|m| m.map_or_else(|| LpcRef::from(0), |m| LpcRef::from(m.as_str())))
                .chain(std::iter::once(LpcRef::from(next as i64)));
            context.mint_array(items)
        }
    } else {
        LpcRef::from(0)
    };
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    const HEADER: &str = r#"
        #define RE_CASELESS   0x0004
        #define RE_MULTILINE  0x0008
        #define RE_DOTALL     0x0010
        #define RE_EXTENDED   0x0020
        #define RE_ANCHORED   0x0040
        #define RE_UNGREEDY   0x0400
        #define RE_MATCH_SUBS 0x1000
    "#;

    async fn result_of(expr: &str) -> LpcRef {
        run_prog(&format!("{HEADER}\nmixed create() {{ return {expr}; }}"))
            .await
            .result()
            .unwrap()
    }

    async fn captures_of(expr: &str) -> Vec<LpcRef> {
        let task = run_prog(&format!("{HEADER}\nmixed *create() {{ return {expr}; }}")).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |a| a.iter().cloned().collect())
            .unwrap()
    }

    #[tokio::test]
    async fn returns_the_first_matching_substring() {
        for (expr, expected) in [
            (r#"regmatch("abcdefcdf", "cd")"#, "cd"),
            (r#"regmatch("abcdefcdf", "cd(e)", 0)"#, "cde"),
            (r#"regmatch("abc12def34", "[0-9]+")"#, "12"),
            (r#"regmatch("café猫", "é.")"#, "é猫"),
        ] {
            assert_eq!(result_of(expr).await, LpcRef::from(expected), "{expr}");
        }
    }

    #[tokio::test]
    async fn no_match_is_zero_in_both_modes() {
        for expr in [
            r#"regmatch("abc", "z")"#,
            r#"regmatch("abc", "z", RE_MATCH_SUBS)"#,
            r#"regmatch("", ".")"#,
        ] {
            assert_eq!(result_of(expr).await, LpcRef::from(0), "{expr}");
        }
    }

    #[tokio::test]
    async fn captures_include_the_full_match_groups_and_absolute_end() {
        let result = captures_of(r#"regmatch("xxabcdyy", "(a(b))(c)(d)", RE_MATCH_SUBS)"#).await;
        assert_eq!(
            result,
            vec![
                LpcRef::from("abcd"),
                "ab".into(),
                "b".into(),
                "c".into(),
                "d".into(),
                6.into()
            ]
        );
        assert_eq!(
            captures_of(r#"regmatch("abcdefcdf", "cd", RE_MATCH_SUBS)"#).await,
            vec![LpcRef::from("cd"), 4.into()]
        );
    }

    #[tokio::test]
    async fn unmatched_groups_are_zero_and_empty_groups_are_strings() {
        assert_eq!(
            captures_of(r#"regmatch("b", "(a)?()(b)(c)?", RE_MATCH_SUBS)"#).await,
            vec![
                LpcRef::from("b"),
                0.into(),
                "".into(),
                "b".into(),
                0.into(),
                1.into()
            ]
        );
    }

    #[tokio::test]
    async fn start_and_continuation_count_unicode_characters() {
        assert_eq!(
            result_of(r#"regmatch("é猫12é猫34", "[0-9]+", 0, 4)"#).await,
            LpcRef::from("34")
        );
        assert_eq!(
            captures_of(r#"regmatch("é猫12é猫34", "(猫)([0-9]+)", RE_MATCH_SUBS, 4)"#).await,
            vec![LpcRef::from("猫34"), "猫".into(), "34".into(), 8.into()]
        );
    }

    #[tokio::test]
    async fn offsets_preserve_anchors_and_word_boundaries() {
        for flags in ["0", "RE_MATCH_SUBS"] {
            for pattern in ["^b", r"\\bb"] {
                let expr = format!(r#"regmatch("ab", "{pattern}", {flags}, 1)"#);
                assert_eq!(result_of(&expr).await, LpcRef::from(0), "{expr}");
            }
        }
        assert_eq!(
            result_of(r#"regmatch("a\nb", "^b", RE_MULTILINE, 2)"#).await,
            LpcRef::from("b")
        );
    }

    #[tokio::test]
    async fn empty_matches_advance_past_the_match_position() {
        assert_eq!(result_of(r#"regmatch("abc", "")"#).await, LpcRef::from(""));
        for (expr, next) in [
            (r#"regmatch("é猫", "", RE_MATCH_SUBS, 1)"#, 2),
            (r#"regmatch("é猫", "$", RE_MATCH_SUBS)"#, 3),
            (r#"regmatch("é猫", "", RE_MATCH_SUBS, 2)"#, 3),
            (r#"regmatch("", "", RE_MATCH_SUBS)"#, 1),
        ] {
            assert_eq!(
                captures_of(expr).await,
                vec![LpcRef::from(""), next.into()],
                "{expr}"
            );
        }
    }

    #[tokio::test]
    async fn continuation_can_scan_successive_matches_without_repeating_them() {
        let code = format!(
            r#"{HEADER}
            string *create() {{
                string text = "é猫12---34";
                string *found = ({{}});
                int start = 0;
                mixed match;
                while (start <= sizeof(text)) {{
                    match = regmatch(text, "[0-9]+", RE_MATCH_SUBS, start);
                    if (!match) break;
                    found += ({{ match[0] }});
                    start = match[1];
                }}
                return found;
            }}"#
        );
        assert_eq!(crate::test_support::strings_of(&code).await, ["12", "34"]);
    }

    #[tokio::test]
    async fn supports_regex_options_and_combinations() {
        for (expr, expected) in [
            (r#"regmatch("ÉCOLE", "école", RE_CASELESS)"#, "ÉCOLE"),
            (r#"regmatch("a\nb", "a.b", RE_DOTALL)"#, "a\nb"),
            (r#"regmatch("ab", "a b # comment", RE_EXTENDED)"#, "ab"),
            (r#"regmatch("a123b456b", "a.*b", RE_UNGREEDY)"#, "a123b"),
            (r#"regmatch("a\nB", "^b", RE_CASELESS | RE_MULTILINE)"#, "B"),
            (r#"regmatch("ABC", "(?i)abc")"#, "ABC"),
        ] {
            assert_eq!(result_of(expr).await, LpcRef::from(expected), "{expr}");
        }
        assert_eq!(
            captures_of(r#"regmatch("ÉCOLE", "(é)cole", RE_CASELESS | RE_MATCH_SUBS)"#).await,
            vec![LpcRef::from("ÉCOLE"), "É".into(), 5.into()]
        );
    }

    #[tokio::test]
    async fn anchored_matches_must_start_at_the_requested_offset() {
        for flags in ["RE_ANCHORED", "RE_ANCHORED | RE_MATCH_SUBS"] {
            let expr = format!(r#"regmatch("é猫ab", "a|b", {flags}, 1)"#);
            assert_eq!(result_of(&expr).await, LpcRef::from(0), "{expr}");
        }
        assert_eq!(
            result_of(r#"regmatch("é猫ab", "a|b", RE_ANCHORED, 2)"#).await,
            LpcRef::from("a")
        );
        assert_eq!(
            captures_of(r#"regmatch("é猫ab", "a|b", RE_ANCHORED | RE_MATCH_SUBS, 2)"#).await,
            vec![LpcRef::from("a"), 3.into()]
        );
    }

    #[tokio::test]
    async fn invalid_arguments_patterns_and_options_are_errors() {
        for (expr, expected) in [
            (r#"regmatch(bad, "a")"#, "text must be a string"),
            (r#"regmatch("a", bad)"#, "pattern must be a string"),
            (r#"regmatch("a", "a", text)"#, "flags must be an int"),
            (r#"regmatch("a", "a", 0, text)"#, "start must be an int"),
            (r#"regmatch("a", "(")"#, "invalid pattern"),
            (r#"regmatch("a", "(?=a)")"#, "invalid pattern"),
            (r#"regmatch("", "(", 0, 0)"#, "invalid pattern"),
            (r#"regmatch("a", "a", -1)"#, "unsupported flags"),
            (r#"regmatch("a", "a", 0x02000000)"#, "unsupported flags"),
            (
                r#"regmatch("é猫", "", 0, -1)"#,
                "start index -1 out of range [0..2]",
            ),
            (
                r#"regmatch("é猫", "", 0, 3)"#,
                "start index 3 out of range [0..2]",
            ),
            (
                r#"regmatch("a", "", 0, 9223372036854775807)"#,
                "out of range",
            ),
        ] {
            let code =
                format!("mixed create() {{ mixed bad = 1; mixed text = \"a\"; return {expr}; }}");
            let err = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(err.contains("regmatch:"), "{expr}: {err}");
            assert!(err.contains(expected), "{expr}: {err}");
        }
    }

    #[tokio::test]
    async fn works_through_an_efun_function_pointer() {
        assert_eq!(
            run_prog(r#"string create() { function f = &regmatch(); return f("abcdef", "cd"); }"#)
                .await
                .result(),
            Some(LpcRef::from("cd"))
        );
    }
}
