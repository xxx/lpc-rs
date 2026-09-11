//! `sprintf`: LPC's format string, in the dialect LDMud and FluffOS
//! share; `%=` columns and `%#` tables lay out across rows.

mod display;
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
        if chars.peek() == Some(&'^') {
            chars.next();
            layout.text_char('%');
            layout.text_char('^');
            continue;
        }
        let mut spec = spec::parse(&mut chars).map_err(|e| {
            context.runtime_error(match e {
                SpecError::Unknown(c) => format!("sprintf: unknown conversion `{c}`"),
                SpecError::Unterminated => "sprintf: unterminated conversion".to_owned(),
            })
        })?;
        let mut align = spec.align;
        let sizes = (0..spec.size_args)
            .map(|_| {
                let (number, value) = take(context)?;
                let LpcRef::Int(i) = value else {
                    return Err(context.runtime_error(format!(
                        "sprintf: argument {number} is {}, `*` wants an int",
                        render::described(&value)
                    )));
                };
                Ok(i.0)
            })
            .collect::<Result<Vec<_>>>()?;
        let size = |size: Option<Size>| {
            size.map(|size| match size {
                Size::Fixed(n) => n as i128,
                Size::FromArg(index) => sizes[index] as i128,
                Size::AbsoluteFromArg(index) => sizes[index].unsigned_abs() as i128,
            })
        };
        let width = match size(spec.width) {
            Some(w) if w < 0 => {
                align = Align::Left;
                spec.justify = false;
                Some(w.unsigned_abs() as usize)
            }
            Some(w) => Some(w as usize),
            None => None,
        };
        let precision = size(spec.precision).and_then(|p| usize::try_from(p).ok());
        let (number, value) = take(context)?;
        if spec.array {
            if !matches!(value, LpcRef::Array(_)) {
                return Err(context.runtime_error(format!(
                    "sprintf: argument {number} is {}, `@` wants an array",
                    render::described(&value)
                )));
            }
            value.with_array(context.txn(), |values| -> Result<()> {
                for (index, value) in values.iter().enumerate() {
                    let argument = format!("{number}[{index}]");
                    layout.field(render::field(
                        context, &spec, align, width, precision, value, &argument,
                    )?);
                }
                Ok(())
            })??;
        } else {
            layout.field(render::field(
                context, &spec, align, width, precision, &value, &number,
            )?);
        }
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
    async fn colour_markers_in_the_format_do_not_consume_arguments() {
        check(&[
            (
                r#""%^RED%^%-5s%^RESET%^:%d", "red", 7"#,
                "%^RED%^red  %^RESET%^:7",
            ),
            (r#""%^""#, "%^"),
            (r#""%^RED%^""#, "%^RED%^"),
            (r#""%%^RED%%^ %s", "x""#, "%^RED%^ x"),
        ])
        .await;
    }

    #[tokio::test]
    async fn ansi_fields_are_aligned_by_their_visible_width() {
        check(&[
            (
                "\"|%6s|\", \"\x1b[31mred\x1b[0m\"",
                "|   \x1b[31mred\x1b[0m|",
            ),
            (
                "\"|%-6s|\", \"\x1b[31mred\x1b[0m\"",
                "|\x1b[31mred\x1b[0m   |",
            ),
            (
                "\"|%|6s|\", \"\x1b[31mred\x1b[0m\"",
                "|  \x1b[31mred\x1b[0m |",
            ),
            (
                "\"|%4s|\", \"\x1b[1;38;2;255;0;0m界\x1b[0m\"",
                "|  \x1b[1;38;2;255;0;0m界\x1b[0m|",
            ),
            ("\"|%3s|\", \"\x1b[31m\x1b[0m\"", "|   \x1b[31m\x1b[0m|"),
            (r#""%-5s", "%^RED%^red%^RESET%^""#, "%^RED%^red%^RESET%^"),
        ])
        .await;
    }

    #[tokio::test]
    async fn precision_uses_columns_and_preserves_sgr_resets() {
        check(&[
            (
                "\"|%6.2s|\", \"\x1b[31mred\x1b[0m\"",
                "|    \x1b[31mre\x1b[0m|",
            ),
            ("\"|%:1s|\", \"\x1b[31m界\x1b[0m\"", "| \x1b[31m\x1b[0m|"),
            ("\"%.0s\", \"\x1b[31mred\x1b[0m\"", "\x1b[31m\x1b[0m"),
            ("\"%4.1s\", \"e\u{301}x\"", "   e\u{301}"),
            (r#""%.2s", "👩‍💻x""#, "👩‍💻"),
            (
                "\"%.2s\", \"e\x1b[31m\u{301}👩\x1b[0m‍💻\"",
                "e\x1b[31m\u{301}\x1b[0m",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn colour_conversion_composes_before_or_after_formatting() {
        let result = run_prog(
            r#"
            string create() {
                return terminal_colour(sprintf("%^RED%^%-6s%^RESET%^", "red"), 1, 0, 0, 3)
                    + "|" + sprintf("%-6s", terminal_colour("%^RED%^red%^RESET%^", 1, 0, 0, 3));
            }
        "#,
        )
        .await
        .result();
        assert_eq!(
            result.and_then(|value| value.as_str().map(str::to_owned)),
            Some("\x1b[31mred   \x1b[0m|\x1b[31mred\x1b[0m   ".to_owned())
        );
    }

    #[tokio::test]
    async fn unicode_and_coloured_custom_padding_fill_terminal_columns() {
        check(&[
            (r#""%5'界's", "x""#, "界界x"),
            (r#""%4'界's", "x""#, "界 x"),
            ("\"%3'e\u{301}'s\", \"x\"", "e\u{301}e\u{301}x"),
            (
                "\"%3'\x1b[31m.\x1b[0m's\", \"x\"",
                "\x1b[31m.\x1b[0m\x1b[31m.\x1b[0mx",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn coloured_columns_wrap_without_splitting_controls_or_graphemes() {
        check(&[
            (
                "\"%=-4s\", \"\x1b[31maa bb\x1b[0m\"",
                "\x1b[31maa\nbb\x1b[0m",
            ),
            (
                "\"%=-3s\", \"\x1b[31mabcdefg\x1b[0m\"",
                "\x1b[31mabc\ndef\ng\x1b[0m",
            ),
            (r#""%=-3s", "界界x""#, "界\n界x\n"),
            (r#""%=-1s", "界x""#, "界\nx\n"),
            ("\"%=-2s\", \"e\u{301}e\u{301}x\"", "e\u{301}e\u{301}\nx"),
        ])
        .await;
    }

    #[tokio::test]
    async fn continuation_columns_use_visible_offsets_in_fields_and_literals() {
        check(&[
            (
                "\"\x1b[31m界\x1b[0m%=-3s\", \"ab cd\"",
                "\x1b[31m界\x1b[0mab\n  cd",
            ),
            (
                "\"%s%=-3s\", \"\x1b[31m界\x1b[0m\", \"ab cd\"",
                "\x1b[31m界\x1b[0mab\n  cd",
            ),
            (
                "\"%=-3s|%=-3s\", \"\x1b[31mab\x1b[0m cd\", \"xy z\"",
                "\x1b[31mab\x1b[0m |xy\ncd  z",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn table_columns_fit_visible_names() {
        check(&[
            (
                "\"%#-10s\", \"\x1b[31mone\x1b[0m\\ntwo\\n界\"",
                "\x1b[31mone\x1b[0m  界\ntwo  ",
            ),
            (
                "\"%#-8.2s\", \"\x1b[31m界\x1b[0m\\nb\\nc\\nd\"",
                "\x1b[31m界\x1b[0m  c\nb   d",
            ),
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
    async fn colon_star_shares_one_size_and_stars_follow_format_order() {
        check(&[
            (r#""%:*s|%d", 3, "foobar", 9"#, "foo|9"),
            (r#""%:*s", -5, "foo""#, "foo  "),
            (r#""%:*s", -3, "foobar""#, "foo"),
            (r#""%:*d", 5, 42"#, "00042"),
            (r#""%.**s", 2, 4, "foobar""#, "  fo"),
            (r#""%:*8s", 3, "foobar""#, "     foo"),
            (r#""%:*.*s|%d", 5, 2, "abcdef", 7"#, "   ab|7"),
            (r#""%.*:*s", 2, 4, "foobar""#, "foob"),
        ])
        .await;
        let err = error_of(r#""%:*s", 3"#).await;
        assert!(err.contains("argument 2 is missing"), "{err}");
    }

    #[tokio::test]
    async fn array_formatting_reuses_sizes_and_consumes_one_array_argument() {
        check(&[
            (r#""%@-4s", ({ "oak", "elm" })"#, "oak elm "),
            (r#""%@04d:%d", ({ 1, -2 }), 7"#, "0001-002:7"),
            (r#""%@*.*s|%d", 4, 2, ({ "abcd", "xyz" }), 9"#, "  ab  xy|9"),
            (r#""%@:*s", 3, ({ "abcdef", "x" })"#, "abc  x"),
            (r#""%@@s", ({ "a", "b" })"#, "ab"),
            (r#""<%@s>%d", ({}), 7"#, "<>7"),
            (r#""<%@*s>%d", 4, ({}), 7"#, "<>7"),
            (r#""%@#Q", ({ ({ "x" }), ({ "y" }) })"#, r#"({"x"})({"y"})"#),
        ])
        .await;
    }

    #[tokio::test]
    async fn array_fields_share_column_layout_and_display_width() {
        check(&[
            (r#""%@=-4s", ({ "aa bb", "x y z" })"#, "aa  x y\nbb  z"),
            (
                "\"%@-4s\", ({ \"界\", \"e\u{301}\", \"👩‍💻\" })",
                "界  e\u{301}   👩‍💻  ",
            ),
            (
                "\"%@-4s\", ({ \"\x1b[31m界\x1b[0m\", \"x\" })",
                "\x1b[31m界\x1b[0m  x   ",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn array_format_errors_identify_the_argument_and_element() {
        let err = error_of(r#""%@s", "abc""#).await;
        assert!(
            err.contains("argument 1 is a string, `@` wants an array"),
            "{err}"
        );
        let err = error_of(r#""%@s", ({ "abc", 12 })"#).await;
        assert!(
            err.contains("argument 1[1] is an int, %s wants a string"),
            "{err}"
        );
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
    async fn integer_precision_sets_a_minimum_digit_count_without_truncating() {
        check(&[
            (r#""%.5d", 42"#, "00042"),
            (r#""%.2d", 42"#, "42"),
            (r#""%.5d", 1234"#, "01234"),
            (r#""%.2d", 1234"#, "1234"),
            (r#""%.0d", 1234"#, "1234"),
            (r#""%.5i", 42"#, "00042"),
            (r#""%.5d", -42"#, "-00042"),
            (r#""%.2d", -1234"#, "-1234"),
            (r#""%+.5d", 42"#, "+00042"),
            (r#""% .5d", 42"#, " 00042"),
            (
                r#""%.20d", -9223372036854775807 - 1"#,
                "-09223372036854775808",
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn integer_precision_pads_digits_in_each_base() {
        check(&[
            (r#""%.4b", 5"#, "0101"),
            (r#""%.2b", 5"#, "101"),
            (r#""%.4o", 9"#, "0011"),
            (r#""%.4x", 123"#, "007b"),
            (r#""%.4X", 123"#, "007B"),
        ])
        .await;
    }

    #[tokio::test]
    async fn zero_integer_precision_omits_zero_digits_but_keeps_sign_and_width() {
        check(&[
            (r#""%d", 0"#, "0"),
            (r#""%.3d", 0"#, "000"),
            (r#""%.0d", 0"#, ""),
            (r#""%.d", 0"#, ""),
            (r#""%.0i|%.0b|%.0o|%.0x|%.0X", 0, 0, 0, 0, 0"#, "||||"),
            (r#""%+.0d", 0"#, "+"),
            (r#""% .0d", 0"#, " "),
            (r#""%5.0d", 0"#, "     "),
            (r#""%08.0d", 0"#, "        "),
        ])
        .await;
    }

    #[tokio::test]
    async fn integer_precision_combines_with_field_sizes_and_padding() {
        check(&[
            (r#""%08.5d", 42"#, "   00042"),
            (r#""%08.5d", -42"#, "  -00042"),
            (r#""%-8.5d", 42"#, "00042   "),
            (r#""%|8.5d", 42"#, "  00042 "),
            (r#""%'.'8.5d", 42"#, "...00042"),
            (r#""%'0'8.5d", 42"#, "00000042"),
            (r#""%.*d", 5, 42"#, "00042"),
            (r#""%*.*d", 8, 5, -42"#, "  -00042"),
            (r#""%05.*d", -1, 42"#, "00042"),
            (r#""%05.*d", 0, 42"#, "   42"),
            (r#""%:5d", 42"#, "00042"),
        ])
        .await;
    }

    #[tokio::test]
    async fn commas_group_integer_digits_after_precision_and_before_field_padding() {
        check(&[
            (r#""%,d", 999"#, "999"),
            (r#""%,d", 1000"#, "1,000"),
            (r#""%,d", 123456789"#, "123,456,789"),
            (r#""%+,i", 1234"#, "+1,234"),
            (
                r#""%,d", -9223372036854775807 - 1"#,
                "-9,223,372,036,854,775,808",
            ),
            (r#""%,.7d", 42"#, "0,000,042"),
            (r#""%,010d", 1234"#, "000001,234"),
            (r#""%,-10d", 1234"#, "1,234     "),
            (r#""%,'.'10d", 1234"#, ".....1,234"),
            (r#""%+,.0d", 0"#, "+"),
            (
                r#""%,x %,X %,o", 0xabcdef, 0xabcdef, 0o123456"#,
                "abc,def ABC,DEF 123,456",
            ),
            (r#""%,.8B", 5"#, "00,000,101"),
            (r#""%@@,d", ({ 1000, 2000 })"#, "1,0002,000"),
            (r#""%,.2f", 1234.5"#, "1234.50"),
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
            (r#""%B", -5"#, "-101"),
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
            (r#""%8.2F", 3.5"#, "    3.50"),
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
    async fn percent_q_quotes_strings_and_escapes_control_characters() {
        check(&[
            (r#""%Q", "a\nb""#, r#""a\nb""#),
            (r#""%Q", "a\"b\\c""#, r#""a\"b\\c""#),
            (r#""%Q", "\a\b\t\n\v\f\r""#, r#""\a\b\t\n\v\f\r""#),
            (
                r#""%Q", sprintf("%c%c%c", 0, 27, 127)"#,
                r#""\x00\x1b\x7f""#,
            ),
            ("\"%Q\", \"界e\u{301}👩‍💻\"", "\"界e\u{301}👩‍💻\""),
            (r#""%8Q", "x""#, "     \"x\""),
            (r#""%.3Q", "abc""#, "\"ab"),
            (
                r#""%Q", to_bytes(({ 0, 34, 92, 255 }))"#,
                r#"b"\x00\"\\\xff""#,
            ),
        ])
        .await;
    }

    #[tokio::test]
    async fn debug_formats_quote_recursively_and_compact_collection_layout() {
        check(&[
            (r#""%Q", ({ "x", "y" })"#, "({\n  \"x\",\n  \"y\"\n})"),
            (r#""%Q", ([ "a": "b" ])"#, "([\n  \"a\": \"b\"\n])"),
            (r#""%#O", ({ 1, "two" })"#, "({1,two})"),
            (r#""%#O", ([ "a": 1 ])"#, "([a:1])"),
            (r#""%#Q %#O", ({}), ([])"#, "({}) ([])"),
            (
                r#""%#Q", ({ "a\nb", ([ "key": ({ "quoted", "x" }) ]) })"#,
                r#"({"a\nb",(["key":({"quoted","x"})])})"#,
            ),
            (r#""%10#Q", ({ 1, 2 })"#, "   ({1,2})"),
        ])
        .await;
    }

    #[tokio::test]
    async fn debug_formats_read_transactional_updates_and_bound_recursion() {
        let code = r#"
            string create() {
                mixed a = ({ ({ "old" }) });
                a[0][0] = "new";
                return sprintf("%#Q", a);
            }
        "#;
        assert_eq!(
            run_prog(code).await.result().unwrap().as_str(),
            Some(r#"({({"new"})})"#)
        );
        for conversion in ["Q", "#O", "#Q"] {
            for setup in ["mixed a = ({ 0 }); a[0] = a;", "mixed a = ([]); a[0] = a;"] {
                let code =
                    format!("string create() {{ {setup} return sprintf(\"%{conversion}\", a); }}");
                let err = try_run_prog(&code).await.unwrap_err().to_string();
                assert!(err.contains("Too deep recursion"), "{err}");
            }
        }
    }

    #[tokio::test]
    async fn justification_spreads_spaces_without_splitting_graphemes_or_controls() {
        check(&[
            (r#""%$9s", "aa bb cc""#, "aa  bb cc"),
            (r#""%$8s", "  aa   bb  ""#, "aa    bb"),
            (r#""%$8s", "aa""#, "aa      "),
            (r#""%$4s", "aa bb""#, "aa bb"),
            (r#""%$-8s", "aa bb""#, "aa bb   "),
            (r#""%-$8d", 12"#, "12      "),
            (r#""%$*s", -8, "aa bb""#, "aa bb   "),
            (r#""%$6s", "👩‍💻 x""#, "👩‍💻   x"),
            ("\"%$5s\", \"e\u{301} x\"", "e\u{301}   x"),
            (
                "\"%$8s\", \"\x1b[31m   界 aa \x1b[0m\"",
                "\x1b[31m界    aa\x1b[0m",
            ),
            ("\"%$3s\", \"\x1b[0m\"", "\x1b[0m   "),
        ])
        .await;
    }

    #[tokio::test]
    async fn justified_columns_leave_each_paragraphs_last_line_left_aligned() {
        check(&[
            (
                r#""%=$9s", "aa bb cc dd ee\nff gg\n""#,
                "aa  bb cc\ndd ee\nff gg\n",
            ),
            (
                r#""%=$9s|%=-4s\n", "aa bb cc dd", "x y z""#,
                "aa  bb cc|x y\ndd        z\n",
            ),
            (r#""%=$9.6s", "aa bb cc""#, "aa     bb\ncc"),
            (r#""%#$12.2s", "a b\nc d""#, "a    bc d"),
            (
                "\"%=$5s\", \"\x1b[31maa bb cc\x1b[0m\"",
                "\x1b[31maa bb\ncc\x1b[0m",
            ),
            (
                "\"%=$5s\", \"\x1b[31maa bb \x1b[0m\"",
                "\x1b[31maa bb\x1b[0m",
            ),
            (
                "\"%=-5s\", \"\x1b[31maa bb \x1b[0m\"",
                "\x1b[31maa bb\x1b[0m",
            ),
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
