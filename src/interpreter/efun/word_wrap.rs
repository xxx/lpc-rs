//! `break_string` and the word wrap behind it.

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// The most spaces an int indent may ask for.
const MAX_INDENT: i64 = 1000;

/// `break_string(str, width [, indent])`: `str` word-wrapped to lines of
/// at most `width` characters, `indent` — a count of spaces or a string —
/// in front of every line. A non-string `str` is 0.
pub fn break_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        context.return_efun_result(NULL);
        return Ok(());
    };
    let LpcRef::Int(width) = context.arg(1) else {
        return Err(context.runtime_error("break_string: the width must be an int"));
    };
    let width = usize::try_from(width.0).unwrap_or(0).max(1);
    let indent = match context.try_arg(2).filter(|r| !r.is_null()) {
        None => String::new(),
        Some(LpcRef::Int(n)) if (0..=MAX_INDENT).contains(&n.0) => {
            " ".repeat(usize::try_from(n.0).unwrap_or(0))
        }
        Some(LpcRef::Int(n)) => {
            return Err(context.runtime_error(format!(
                "break_string: the indent must be 0..={MAX_INDENT} spaces, got {}",
                n.0
            )));
        }
        Some(LpcRef::String(s)) => s.to_str().to_owned(),
        Some(_) => {
            return Err(
                context.runtime_error("break_string: the indent must be an int or a string")
            );
        }
    };
    let wrapped = wrap(text, width, &indent);
    context.return_efun_result(LpcRef::from(wrapped));
    Ok(())
}

/// `text` in lines of at most `width` characters, `indent` in front of
/// each: a line breaks at the last space that keeps it within the width,
/// a word longer than a line stands alone, and a newline already in
/// `text` ends a line. The indent counts toward the width, which is at
/// least one more than the indent.
pub(crate) fn wrap(text: &str, width: usize, indent: &str) -> String {
    if text.is_empty() {
        return String::new();
    }
    let indent_len = indent.chars().count();
    let room = width.max(indent_len + 1) - indent_len;
    let (body, trailing) = match text.strip_suffix('\n') {
        Some(body) => (body, "\n"),
        None => (text, ""),
    };
    let lines: Vec<String> = body
        .split('\n')
        .flat_map(|paragraph| break_paragraph(paragraph, room))
        .map(|line| format!("{indent}{line}"))
        .collect();
    lines.join("\n") + trailing
}

/// The lines `paragraph` (no newline in it) breaks into: at the last
/// space that keeps a line within `room` characters, a word longer than
/// that standing alone.
fn break_paragraph(paragraph: &str, room: usize) -> Vec<String> {
    let chars: Vec<char> = paragraph.chars().collect();
    let mut lines = Vec::new();
    let mut start = 0;
    let mut last_space = None;
    for (i, &c) in chars.iter().enumerate() {
        if c == ' ' {
            last_space = Some(i);
        }
        if i - start >= room
            && let Some(space) = last_space
        {
            lines.push(chars[start..space].iter().collect());
            start = space + 1;
            last_space = None;
        }
    }
    lines.push(chars[start..].iter().collect());
    lines
}

#[cfg(test)]
mod tests {
    use super::wrap;
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    #[test]
    fn breaks_at_the_last_space_within_the_width() {
        assert_eq!(
            wrap("ab cd ef gh ij kl mn op qr", 8, ""),
            "ab cd ef\ngh ij kl\nmn op qr"
        );
    }

    #[test]
    fn a_line_may_fill_the_width_exactly() {
        assert_eq!(wrap("aaaa bbbb", 4, ""), "aaaa\nbbbb");
        assert_eq!(wrap("aaaa bbbb", 5, ""), "aaaa\nbbbb");
        assert_eq!(wrap("aaaa bbbb", 9, ""), "aaaa bbbb");
    }

    #[test]
    fn a_word_longer_than_the_width_stands_alone() {
        assert_eq!(wrap("aaaaaaaaaa b", 4, ""), "aaaaaaaaaa\nb");
    }

    #[test]
    fn text_that_fits_is_unchanged() {
        assert_eq!(wrap("hello world", 76, ""), "hello world");
        assert_eq!(wrap("", 10, "  "), "");
    }

    #[test]
    fn the_indent_counts_toward_the_width() {
        assert_eq!(wrap("ab cd ef", 6, "  "), "  ab\n  cd\n  ef");
        assert_eq!(wrap("ab cd", 3, "> "), "> ab\n> cd");
    }

    #[test]
    fn a_width_below_one_is_one() {
        assert_eq!(wrap("a b", 0, ""), "a\nb");
    }

    #[test]
    fn an_existing_newline_ends_a_line() {
        assert_eq!(wrap("ab\ncd ef", 5, ""), "ab\ncd ef");
        assert_eq!(wrap("abc def\nghi jkl", 5, ""), "abc\ndef\nghi\njkl");
        assert_eq!(wrap("a\n\nb", 5, "  "), "  a\n  \n  b");
    }

    #[test]
    fn a_trailing_newline_is_kept_and_not_indented_after() {
        assert_eq!(wrap("ab cd\n", 3, "> "), "> ab\n> cd\n");
        assert_eq!(wrap("ab cd\n", 8, ""), "ab cd\n");
    }

    #[test]
    fn characters_are_counted_not_bytes() {
        assert_eq!(wrap("éé éé", 2, ""), "éé\néé");
    }

    #[tokio::test]
    async fn the_efun_wraps_with_an_int_or_string_indent() {
        let result = run_prog(
            r#"string create() { return break_string("ab cd", 3, 2) + "|" + break_string("ab cd", 3, "> ") + "|" + break_string("ab cd ef gh", 6); }"#,
        )
        .await
        .result();
        assert_eq!(
            result,
            Some(LpcRef::from("  ab\n  cd|> ab\n> cd|ab cd\nef gh"))
        );
    }

    #[tokio::test]
    async fn a_non_string_text_is_zero() {
        let result = run_prog(r#"mixed create() { mixed x = 0; return break_string(x, 8); }"#)
            .await
            .result();
        assert_eq!(result, Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn a_negative_indent_is_an_error() {
        let err = try_run_prog(r#"string create() { return break_string("a b", 8, -1); }"#)
            .await
            .expect_err("a negative indent")
            .to_string();
        assert!(
            err.contains("break_string: the indent must be 0..=1000 spaces, got -1"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_float_indent_is_an_error() {
        let err =
            try_run_prog(r#"string create() { mixed i = 1.5; return break_string("a b", 8, i); }"#)
                .await
                .expect_err("a float indent")
                .to_string();
        assert!(
            err.contains("break_string: the indent must be an int or a string"),
            "{err}"
        );
    }
}
