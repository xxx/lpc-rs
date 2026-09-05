//! `regexp(strings, pattern [, flag])`: the strings a regular expression
//! matches, in the `regex` crate's dialect.

use lpc_rs_errors::Result;
use regex::Regex;

use crate::interpreter::{
    efun::{callback::array_arg, efun_context::EfunContext},
    lpc_ref::LpcRef,
};

/// `regexp(strings, pattern [, flag])`: the strings `pattern` matches, in
/// order; flag bit 2 keeps the non-matches instead, bit 1 puts each string's
/// one-based index before it. Non-strings are ignored.
pub fn regexp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let strings = array_arg(context, "regexp")?;
    let Some(pattern) = context.arg(1).as_str() else {
        return Err(context.runtime_error(format!(
            "regexp: {} is not a string",
            context.arg(1).type_name()
        )));
    };
    let flag = if context.arg_count() < 3 {
        0
    } else {
        match context.arg(2) {
            LpcRef::Int(n) => n.0,
            other => {
                return Err(
                    context.runtime_error(format!("regexp: {} is not an int", other.type_name()))
                );
            }
        }
    };
    let re = Regex::new(pattern)
        .map_err(|e| context.runtime_error(format!("regexp: invalid pattern {pattern:?}: {e}")))?;
    let (indexed, invert) = (flag & 1 != 0, flag & 2 != 0);
    let mut result = Vec::new();
    for (i, item) in strings.enumerate() {
        let Some(s) = item.as_str() else { continue };
        if re.is_match(s) != invert {
            if indexed {
                result.push(LpcRef::from(i as i64 + 1));
            }
            result.push(item);
        }
    }
    context.return_array(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::test_support::{strings_of, try_run_prog};

    async fn error_of(expr: &str) -> String {
        let code = format!("mixed create() {{ return {expr}; }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    #[tokio::test]
    async fn regexp_keeps_the_strings_the_pattern_matches() {
        let code =
            r#"string *create() { return regexp(({ "d12", "m3", "x" }), "^d[0-9][0-9]*$"); }"#;
        assert_eq!(strings_of(code).await, ["d12"]);
    }

    #[tokio::test]
    async fn regexp_keeps_order_and_duplicates() {
        let code = r#"string *create() { return regexp(({ "ab", "b", "ab" }), "a"); }"#;
        assert_eq!(strings_of(code).await, ["ab", "ab"]);
    }

    #[tokio::test]
    async fn regexp_ignores_non_strings() {
        let code = r#"mixed *create() { return regexp(({ 1, "a1", ({ "1" }) }), "1"); }"#;
        assert_eq!(strings_of(code).await, ["a1"]);
    }

    #[tokio::test]
    async fn flag_two_keeps_the_non_matches() {
        let code = r#"string *create() { return regexp(({ "a", "b", "ab" }), "a", 2); }"#;
        assert_eq!(strings_of(code).await, ["b"]);
    }

    #[tokio::test]
    async fn flag_one_pairs_each_string_with_its_one_based_index() {
        let code = r#"mixed *create() { return regexp(({ "x", "ay", "az" }), "a", 1); }"#;
        assert_eq!(strings_of(code).await, ["2", "ay", "3", "az"]);
        let code = r#"mixed *create() { return regexp(({ "x", "ay" }), "a", 3); }"#;
        assert_eq!(strings_of(code).await, ["1", "x"]);
    }

    #[tokio::test]
    async fn the_dialect_is_the_regex_crates() {
        let code = r#"string *create() { return regexp(({ "a1", "b" }), "\\d"); }"#;
        assert_eq!(strings_of(code).await, ["a1"]);
        // PCRE's look-ahead is not in the dialect.
        let err = error_of(r#"regexp(({ "a" }), "(?=a)")"#).await;
        assert!(err.contains(r#"regexp: invalid pattern "(?=a)""#), "{err}");
    }

    #[tokio::test]
    async fn an_invalid_pattern_is_an_error() {
        let err = error_of(r#"regexp(({ "a" }), "(")"#).await;
        assert!(err.contains(r#"regexp: invalid pattern "(""#), "{err}");
    }

    #[tokio::test]
    async fn an_empty_array_is_an_empty_array() {
        let code = r#"string *create() { return regexp(({ }), "a"); }"#;
        assert!(strings_of(code).await.is_empty());
    }

    #[tokio::test]
    async fn the_arguments_are_typed() {
        let code = r#"mixed create() { mixed s = "a"; return regexp(s, "a"); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("regexp: string is not an array"), "{err}");
        let code = r#"mixed create() { mixed n = 1; return regexp(({ "a" }), n); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("regexp: int is not a string"), "{err}");
        let code = r#"mixed create() { mixed s = "a"; return regexp(({ "a" }), "a", s); }"#;
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("regexp: string is not an int"), "{err}");
    }
}
