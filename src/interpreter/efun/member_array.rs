use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Find an equal array element or an integer Unicode scalar in a string,
/// at or past `start`; return -1 when there is none.
pub fn member_array<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let haystack = context.arg(1);
    if !matches!(haystack, LpcRef::Array(_) | LpcRef::String(_)) {
        return Err(context.runtime_error(format!(
            "member_array: {} is not an array or string",
            haystack.type_name()
        )));
    }
    let start = match context.arg(2) {
        LpcRef::Int(start) if start.0 < 0 => {
            return Err(context.runtime_error(format!("member_array: negative start {start}")));
        }
        LpcRef::Int(start) => usize::try_from(start.0).unwrap_or(usize::MAX),
        other => {
            return Err(
                context.runtime_error(format!("member_array: {} is not an int", other.type_name()))
            );
        }
    };
    let needle = context.arg(0);
    let txn = context.txn();
    let found = if let LpcRef::String(text) = haystack {
        let LpcRef::Int(value) = needle else {
            return Err(context.runtime_error("member_array: string needle must be an int"));
        };
        let character = u32::try_from(value.0)
            .ok()
            .and_then(char::from_u32)
            .ok_or_else(|| context.runtime_error("member_array: invalid Unicode scalar value"))?;
        text.chars().skip(start).position(|c| c == character)
    } else {
        haystack.with_array(txn, |a| {
            a.iter()
                .skip(start)
                .position(|item| item.eq_in(needle, txn))
        })?
    };
    let index = found.map_or(-1, |i| (i + start) as LpcIntInner);
    context.return_efun_result(LpcRef::from(index));
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        compiler::CompilerBuilder,
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, run_prog_with_config, test_config, try_run_prog},
    };

    async fn int_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    #[tokio::test]
    async fn member_array_is_the_index_of_the_first_match() {
        let r = int_of("int create() { return member_array(2, ({ 1, 2, 3, 2 })); }").await;
        assert_eq!(r, LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_absent_item_is_minus_one() {
        let r = int_of("int create() { return member_array(9, ({ 1, 2, 3 })); }").await;
        assert_eq!(r, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn the_search_starts_at_start() {
        let r = int_of("int create() { return member_array(2, ({ 1, 2, 3, 2 }), 2); }").await;
        assert_eq!(r, LpcRef::from(3));
    }

    #[tokio::test]
    async fn a_start_past_the_end_is_minus_one() {
        let r = int_of("int create() { return member_array(1, ({ 1 }), 5); }").await;
        assert_eq!(r, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn strings_match_by_value() {
        let r = int_of(r#"int create() { return member_array("b", ({ "a", "b" })); }"#).await;
        assert_eq!(r, LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_destructed_object_matches_zero() {
        let code = r#"
            int create() {
                object o = clone_object("/clone_target");
                destruct(o);
                return member_array(0, ({ 1, o }));
            }
        "#;
        assert_eq!(int_of(code).await, LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_negative_start_is_an_error() {
        let err = try_run_prog("int create() { return member_array(1, ({ 1 }), -1); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("member_array: negative start -1"), "{err}");
    }

    #[tokio::test]
    async fn an_invalid_haystack_is_an_error() {
        let err = try_run_prog("int create() { mixed x = 7; return member_array(1, x); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("member_array: int is not an array or string"),
            "{err}"
        );
    }

    async fn string_result(code: &str, mode: NullMode) -> LpcRef {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        run_prog_with_config(code, config.into())
            .await
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn string_search_uses_scalar_offsets_without_normalization() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for (expression, expected) in [
                (r#"member_array('y', "xyzzy")"#, 1),
                (r#"member_array('y', "xyzzy", 2)"#, 4),
                (r#"member_array(0x732b, "é猫🐉猫", 2)"#, 3),
                ("member_array(0x301, \"e\u{301}x\")", 1),
                ("member_array('x', \"e\u{301}x\")", 2),
                ("member_array(0xe9, \"e\u{301}x\")", -1),
            ] {
                assert_eq!(
                    string_result(&format!("int create() {{ return {expression}; }}"), mode).await,
                    LpcRef::from(expected),
                    "{mode}: {expression}"
                );
            }
        }
    }

    #[tokio::test]
    async fn empty_or_exhausted_string_search_returns_minus_one() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for expression in [
                r#"member_array('a', "")"#,
                r#"member_array('a', "abc", 3)"#,
                r#"member_array('a', "abc", 9223372036854775807)"#,
                r#"member_array('z', "abc")"#,
            ] {
                assert_eq!(
                    string_result(&format!("int create() {{ return {expression}; }}"), mode).await,
                    LpcRef::from(-1),
                    "{mode}: {expression}"
                );
            }
        }
    }

    #[tokio::test]
    async fn string_search_preserves_nuls_and_has_no_implicit_terminator() {
        let code = r#"
            int create() {
                string s = sprintf("é%cb%cc", 0, 0);
                return member_array(0, s) == 1 && member_array(0, s, 2) == 3
                    && member_array('c', s) == 4 && member_array(0, "abc") == -1
                    && member_array(0, "") == -1;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_needles_are_validated_even_when_the_search_is_empty() {
        let code = r#"
            int create() {
                foreach (mixed bad in ({ "a", 97.0, ({}), ([]), sizeof })) {
                    mixed err = catch(member_array(bad, "", 10));
                    if (!stringp(err) || strindex(err, "string needle must be an int") < 0) return 0;
                }
                foreach (int bad in ({ -1, 0xd800, 0xdfff, 0x110000, 9223372036854775807 })) {
                    mixed err = catch(member_array(bad, "", 10));
                    if (!stringp(err) || strindex(err, "invalid Unicode scalar value") < 0) return 0;
                }
                return 1;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_start_must_be_a_nonnegative_integer() {
        let code = r#"
            int create() {
                foreach (mixed bad in ({ "1", 1.0, ({}), -1 }))
                    if (!stringp(catch(member_array('a', "", bad)))) return 0;
                return 1;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn mixed_strings_and_efun_pointers_use_the_string_overload() {
        let code = r#"
            int create() {
                mixed text = "é猫é";
                function search = member_array;
                function find_cat = &member_array(0x732b);
                return member_array(0xe9, text, 1) == 2 && search(0xe9, text) == 0
                    && search(0xe9, text, 1) == 2 && find_cat(text) == 1;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn array_search_does_not_require_a_scalar_needle() {
        let code = r#"
            int create() {
                mixed *nested = ({1});
                return member_array(-1, ({-1})) == 0 && member_array("a", ({"a"})) == 0
                    && member_array(nested, ({nested})) == 0 && member_array(({1}), ({nested})) == -1
                    && member_array(0xd800, ({0xd800})) == 0;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_overload_keeps_argument_count_and_haystack_type_checks() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        for (expression, diagnostic) in [
            ("member_array(1)", "incorrect argument count"),
            (
                r#"member_array('a', "abc", 0, 2)"#,
                "incorrect argument count",
            ),
            ("member_array(1, ([]))", "unexpected argument type"),
            (
                r#"member_array(1, to_bytes("abc", "UTF-8"))"#,
                "unexpected argument type",
            ),
        ] {
            let error = compiler
                .compile_string(
                    "/search.c",
                    &format!("int create() {{ return {expression}; }}"),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(diagnostic), "{expression}: {error}");
        }
    }
}
