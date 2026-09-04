//! `capitalize`, `lower_case` and `upper_case`: Unicode case mapping, as
//! strings are UTF-8.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Return `map` of the string argument; a non-string is an error.
fn map_string<const N: usize>(
    context: &mut EfunContext<'_, N>,
    name: &str,
    map: fn(&str) -> String,
) -> Result<()> {
    let Some(s) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "{name}: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let result = LpcRef::from(map(s));
    context.return_efun_result(result);
    Ok(())
}

/// `capitalize(s)`: the first character upper-cased, the rest as it was.
pub fn capitalize<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    map_string(context, "capitalize", |s| {
        let mut chars = s.chars();
        match chars.next() {
            Some(first) => first.to_uppercase().chain(chars).collect(),
            None => String::new(),
        }
    })
}

/// `lower_case(s)`: every character lower-cased.
pub fn lower_case<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    map_string(context, "lower_case", str::to_lowercase)
}

/// `upper_case(s)`: every character upper-cased.
pub fn upper_case<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    map_string(context, "upper_case", str::to_uppercase)
}

#[cfg(test)]
mod tests {
    use crate::test_support::{run_prog, try_run_prog};

    async fn string_of(code: &str) -> String {
        let result = run_prog(code).await.result();
        result
            .as_ref()
            .and_then(|r| r.as_str())
            .unwrap_or_else(|| panic!("a string, actually {result:?}"))
            .to_owned()
    }

    #[tokio::test]
    async fn capitalize_uppercases_the_first_letter_only() {
        let s = string_of(r#"string create() { return capitalize("hello world"); }"#).await;
        assert_eq!(s, "Hello world");
    }

    #[tokio::test]
    async fn capitalize_of_an_empty_string_is_empty() {
        let s = string_of(r#"string create() { return capitalize(""); }"#).await;
        assert_eq!(s, "");
    }

    #[tokio::test]
    async fn capitalize_leaves_a_leading_non_letter_alone() {
        let s = string_of(r#"string create() { return capitalize("1st"); }"#).await;
        assert_eq!(s, "1st");
    }

    #[tokio::test]
    async fn lower_case_lowers_every_letter() {
        let s = string_of(r#"string create() { return lower_case("HeLLo 1"); }"#).await;
        assert_eq!(s, "hello 1");
    }

    #[tokio::test]
    async fn upper_case_raises_every_letter() {
        let s = string_of(r#"string create() { return upper_case("HeLLo 1"); }"#).await;
        assert_eq!(s, "HELLO 1");
    }

    #[tokio::test]
    async fn case_mapping_reaches_beyond_ascii() {
        let s = string_of(r#"string create() { return lower_case("ÉCOLE"); }"#).await;
        assert_eq!(s, "école");
    }

    #[tokio::test]
    async fn capitalize_of_a_non_string_is_an_error() {
        let code = "mixed create() { mixed i = 1; return capitalize(i); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("capitalize: int is not a string"), "{err}");
    }
}
