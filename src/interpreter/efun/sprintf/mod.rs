//! `sprintf`: LPC's format string, in the dialect LDMud and FluffOS
//! share; `%=` columns and `%#` tables lay out across rows.

mod layout;
mod render;
mod spec;

use lpc_rs_errors::Result;

use self::{
    layout::Layout,
    spec::{Align, Size, SpecError},
};
use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `sprintf(fmt, ...)`: `fmt` with each `%` conversion replaced by the
/// next argument; see `doc/efun/sprintf.md` for the conversions.
pub fn sprintf<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(fmt) = context.arg(0).as_str() else {
        return Err(context.runtime_error(format!(
            "sprintf: {} is not a string",
            context.arg(0).type_name()
        )));
    };
    let fmt = fmt.to_owned();
    let mut layout = Layout::default();
    let mut chars = fmt.chars().peekable();
    let mut next = 1;
    let mut take = |context: &EfunContext<'_, N>| -> Result<(usize, LpcRef)> {
        let number = next;
        next += 1;
        match context.try_arg(number) {
            Some(value) => Ok((number, value.clone())),
            None => Err(context.runtime_error(format!("sprintf: argument {number} is missing"))),
        }
    };
    while let Some(c) = chars.next() {
        if c != '%' {
            layout.text_char(c);
            continue;
        }
        if chars.peek() == Some(&'%') {
            chars.next();
            layout.text_char('%');
            continue;
        }
        let spec = spec::parse(&mut chars).map_err(|e| {
            context.runtime_error(match e {
                SpecError::Unknown(c) => format!("sprintf: unknown conversion `{c}`"),
                SpecError::Unterminated => "sprintf: unterminated conversion".to_owned(),
            })
        })?;
        let mut align = spec.align;
        let mut size = |size: Option<Size>, context: &EfunContext<'_, N>| -> Result<Option<i64>> {
            Ok(match size {
                None => None,
                Some(Size::Fixed(n)) => Some(n as i64),
                Some(Size::FromArg) => {
                    let (number, value) = take(context)?;
                    let LpcRef::Int(i) = value else {
                        return Err(context.runtime_error(format!(
                            "sprintf: argument {number} is {}, `*` wants an int",
                            render::described(&value)
                        )));
                    };
                    Some(i.0)
                }
            })
        };
        let width = match size(spec.width, context)? {
            Some(w) if w < 0 => {
                align = Align::Left;
                Some(w.unsigned_abs() as usize)
            }
            Some(w) => Some(w as usize),
            None => None,
        };
        let precision = size(spec.precision, context)?.and_then(|p| usize::try_from(p).ok());
        let (number, value) = take(context)?;
        let field = render::field(context, &spec, align, width, precision, &value, number)?;
        layout.field(field);
    }
    let result = layout.finish();
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::test_support::{run_prog, try_run_prog};

    /// `sprintf(<args>)`'s result.
    async fn formatted(args: &str) -> String {
        let code = format!("string create() {{ return sprintf({args}); }}");
        let result = run_prog(&code).await.result();
        result
            .as_ref()
            .and_then(|r| r.as_str())
            .unwrap_or_else(|| panic!("a string, actually {result:?}"))
            .to_owned()
    }

    async fn error_of(args: &str) -> String {
        let code = format!("mixed create() {{ return sprintf({args}); }}");
        try_run_prog(&code).await.unwrap_err().to_string()
    }

    async fn check(cases: &[(&str, &str)]) {
        for (args, expected) in cases {
            assert_eq!(formatted(args).await, *expected, "sprintf({args})");
        }
    }

    #[tokio::test]
    async fn plain_text_and_percent_escapes_pass_through() {
        check(&[
            (r#""foo""#, "foo"),
            (r#""100%%""#, "100%"),
            (r#""%s", "foo""#, "foo"),
            (r#""a%sb%sc", "1", "2""#, "a1b2c"),
        ])
        .await;
    }

    #[tokio::test]
    async fn strings_are_padded_to_the_field_by_the_alignment_flag() {
        check(&[
            (r#""%7s", "foo""#, "    foo"),
            (r#""%-7s", "foo""#, "foo    "),
            (r#""%|7s", "foo""#, "  foo  "),
            (r#""%3s", "foobarbloh""#, "foobarbloh"),
        ])
        .await;
    }

    #[tokio::test]
    async fn a_quoted_pad_string_cycles_over_the_padding() {
        check(&[
            (r#""%7'.'s", "foo""#, "....foo"),
            (r#""%-7'+-'s", "foo""#, "foo+-+-"),
            (r#""%|9'-+'s", "foo""#, "-+-foo-+-"),
        ])
        .await;
    }

    #[tokio::test]
    async fn a_precision_truncates_a_string_and_colon_sets_both() {
        check(&[
            (r#""%3.6s", "foobarbloh""#, "foobar"),
            (r#""%6.3s", "foobarbloh""#, "   foo"),
            (r#""%:6s", "foobarbloh""#, "foobar"),
            (r#""%:3s", "foobarbloh""#, "foo"),
        ])
        .await;
    }

    #[tokio::test]
    async fn a_star_takes_the_width_or_precision_from_the_arguments() {
        check(&[
            (r#""%*.*s", -7, 2, "foobarbloh""#, "fo     "),
            (r#""%*d", 5, 42"#, "   42"),
            (r#""%-*s|", 4, "ab""#, "ab  |"),
        ])
        .await;
    }

    #[tokio::test]
    async fn ints_take_width_alignment_and_sign_flags() {
        check(&[
            (r#""%d", 123"#, "123"),
            (r#""%i", 123"#, "123"),
            (r#""%7d", 123"#, "    123"),
            (r#""%-7d", 123"#, "123    "),
            (r#""%d/%d", 123, -123"#, "123/-123"),
            (r#""% d/% d", 123, -123"#, " 123/-123"),
            (r#""%+d/%+d", 123, -123"#, "+123/-123"),
            (r#""%+5d/%5d", 123, 123"#, " +123/  123"),
            (r#""%|6d", 123"#, "  123 "),
            (r#""%|10d", 123"#, "    123   "),
            (r#""%|10d%3s", 123, "foo""#, "    123   foo"),
        ])
        .await;
    }

    #[tokio::test]
    async fn a_zero_pad_keeps_the_sign_in_front() {
        check(&[
            (r#""%05d", -12"#, "-0012"),
            (r#""%05d", 12"#, "00012"),
            (r#""%'0'3o", 8"#, "010"),
        ])
        .await;
    }

    #[tokio::test]
    async fn ints_render_in_octal_hex_binary_and_as_characters() {
        check(&[
            (r#""%o", 16"#, "20"),
            (r#""%x", 123"#, "7b"),
            (r#""%X", 123"#, "7B"),
            (r#""%b", 5"#, "101"),
            (r#""%c", 65"#, "A"),
            (r#""%c%c", 0x00e9, 0x4e2d"#, "é中"),
        ])
        .await;
    }

    #[tokio::test]
    async fn floats_render_in_fixed_scientific_and_general_forms() {
        check(&[
            (r#""%f", 123.5"#, "123.500000"),
            (r#""%8.3f", 123.5"#, " 123.500"),
            (r#""%.2f", 2"#, "2.00"),
            (r#""%12.4e", 123.5"#, "  1.2350e+02"),
            (r#""%E", 123.5"#, "1.235000E+02"),
            (r#""%g", 123.5"#, "123.5"),
            (r#""%8.3G", 123.5"#, "     124"),
            (r#""%8.6g", 123.5"#, "   123.5"),
            (r#""%g", 0.00001234"#, "1.234e-05"),
            (r#""%+.1f", 1.25"#, "+1.2"),
        ])
        .await;
    }

    #[tokio::test]
    async fn percent_o_dumps_any_value() {
        check(&[
            (r#""%O", 42"#, "42"),
            (r#""%O", "s""#, "s"),
            (r#""%O", ({ 1, 2 })"#, "({\n  1,\n  2\n})"),
            (r#""%O", ([ "a": 1 ])"#, "([\n  a: 1\n])"),
        ])
        .await;
    }

    #[tokio::test]
    async fn column_mode_wraps_a_string_into_lines_of_the_field_width() {
        check(&[
            (
                r#""%=12s", "this is a very long sentence\n""#,
                "   this is a\n   very long\n    sentence\n",
            ),
            (
                r#""%=-12s", "this is a very long sentence\n""#,
                "this is a\nvery long\nsentence\n",
            ),
            (
                r#""%=|12s", "this is a very long sentence\n""#,
                "  this is a\n  very long\n  sentence\n",
            ),
            (
                r#""%=10.6s", "this is a very long sentence\n""#,
                "      this\n      is a\n      very\n      long\n    senten\n        ce\n",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn two_columns_on_one_line_continue_side_by_side() {
        check(&[(
            r#""%=-6s|%=-6s\n", "aa bb cc", "x y z""#,
            "aa bb |x y z\ncc\n",
        )])
        .await;
    }

    #[tokio::test]
    async fn table_mode_lays_words_out_in_columns() {
        check(&[
            (
                r#""%#-40.3s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n""#,
                "one          five         nine\ntwo          six          ten\nthree        seven        \nfour         eight        \n",
            ),
            (
                r#""%#-40s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n""#,
                "one     three   five    seven   nine\ntwo     four    six     eight   ten\n",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn a_wrong_argument_type_is_an_error() {
        let err = error_of(r#""%d", "x""#).await;
        assert!(
            err.contains("sprintf: argument 1 is a string, %d wants an int"),
            "{err}"
        );
        let err = error_of(r#""%s", 1"#).await;
        assert!(
            err.contains("sprintf: argument 1 is an int, %s wants a string"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_missing_argument_is_an_error() {
        let err = error_of(r#""%d %d", 1"#).await;
        assert!(err.contains("sprintf: argument 2 is missing"), "{err}");
    }

    #[tokio::test]
    async fn an_unknown_or_unterminated_conversion_is_an_error() {
        let err = error_of(r#""%y", 1"#).await;
        assert!(err.contains("sprintf: unknown conversion `y`"), "{err}");
        let err = error_of(r#""abc %""#).await;
        assert!(err.contains("sprintf: unterminated conversion"), "{err}");
    }

    #[tokio::test]
    async fn a_non_string_format_is_an_error() {
        let code = "mixed create() { mixed f = 1; return sprintf(f); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(err.contains("sprintf: int is not a string"), "{err}");
    }
}
