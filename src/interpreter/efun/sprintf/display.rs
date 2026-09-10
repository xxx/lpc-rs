//! Terminal columns and indivisible graphemes, with SGR bytes kept verbatim.

use std::borrow::Cow;

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

struct Escape<'a> {
    offset: usize,
    start: usize,
    text: &'a str,
}

pub(super) struct Text<'a> {
    raw: &'a str,
    plain: Cow<'a, str>,
    escapes: Vec<Escape<'a>>,
}

pub(super) struct Unit<'a> {
    pub raw: &'a str,
    pub width: usize,
}

impl<'a> Text<'a> {
    pub(super) fn new(raw: &'a str) -> Self {
        let mut plain = String::new();
        let mut escapes = Vec::new();
        let mut copied = 0;
        for (start, _) in raw.match_indices('\x1b') {
            let Some(len) = sgr_len(&raw[start..]) else {
                continue;
            };
            plain.push_str(&raw[copied..start]);
            escapes.push(Escape {
                offset: plain.len(),
                start,
                text: &raw[start..start + len],
            });
            copied = start + len;
        }
        let plain = if escapes.is_empty() {
            Cow::Borrowed(raw)
        } else {
            plain.push_str(&raw[copied..]);
            Cow::Owned(plain)
        };
        Self {
            raw,
            plain,
            escapes,
        }
    }

    pub(super) fn width(&self) -> usize {
        self.plain.graphemes(true).map(str::width).sum()
    }

    pub(super) fn units(&self) -> impl Iterator<Item = Unit<'_>> {
        let mut raw_start = 0;
        let mut skipped = 0;
        let mut escapes = self.escapes.iter().peekable();
        self.plain.grapheme_indices(true).map(move |(start, text)| {
            let end = start + text.len();
            while let Some(escape) = escapes.peek()
                && escape.offset < end
            {
                skipped += escape.text.len();
                escapes.next();
            }
            let raw_end = if end == self.plain.len() {
                self.raw.len()
            } else {
                end + skipped
            };
            let raw = &self.raw[raw_start..raw_end];
            raw_start = raw_end;
            Unit {
                raw,
                width: text.width(),
            }
        })
    }

    pub(super) fn truncate(&self, columns: usize) -> String {
        let mut width = 0;
        let mut end = 0;
        for unit in self.units() {
            if unit.width > columns.saturating_sub(width) {
                break;
            }
            width += unit.width;
            end += unit.raw.len();
        }
        let mut result = self.raw[..end].to_owned();
        // Keep suffix controls so truncation cannot discard the caller's reset.
        for escape in &self.escapes {
            if escape.start >= end {
                result.push_str(escape.text);
            }
        }
        result
    }
}

fn sgr_len(text: &str) -> Option<usize> {
    let params = text.strip_prefix("\x1b[")?;
    let end = params.find(|c: char| !c.is_ascii_digit() && !matches!(c, ';' | ':'))?;
    (params.as_bytes()[end] == b'm').then_some(end + 3)
}

pub(super) fn width(text: &str) -> usize {
    Text::new(text).width()
}

pub(super) fn padding(text: &str, columns: usize) -> String {
    let text = Text::new(text);
    let width = text.width();
    if width == 0 {
        return " ".repeat(columns);
    }
    let mut result = text.raw.repeat(columns / width);
    let remainder = columns % width;
    if remainder > 0 {
        let tail = text.truncate(remainder);
        let used = self::width(&tail);
        result.push_str(&tail);
        result.push_str(&" ".repeat(remainder - used));
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sgr_is_zero_width_and_other_text_is_literal() {
        assert_eq!(width("\x1b[1;38;2;255;0;0mred\x1b[0m"), 3);
        assert_eq!(width("\x1b[38:2::255:0:0mred\x1b[m"), 3);
        assert_eq!(width("%^RED%^red%^RESET%^"), 19);
        assert_eq!(width("\x1b[31"), 4);
    }

    #[test]
    fn escapes_inside_a_grapheme_do_not_split_it() {
        let raw = "e\x1b[31m\u{301}👩\x1b[0m\u{200d}💻";
        let text = Text::new(raw);
        let units: Vec<_> = text.units().collect();
        assert_eq!(units.iter().map(|u| u.width).collect::<Vec<_>>(), [1, 2]);
        assert_eq!(units.iter().map(|u| u.raw).collect::<String>(), raw);
        assert_eq!(text.truncate(2), "e\x1b[31m\u{301}\x1b[0m");
    }

    #[test]
    fn precision_keeps_resets_and_never_splits_a_wide_grapheme() {
        let text = Text::new("\x1b[31m界x\x1b[0m");
        assert_eq!(text.truncate(1), "\x1b[31m\x1b[0m");
        assert_eq!(text.truncate(2), "\x1b[31m界\x1b[0m");
        assert_eq!(text.truncate(3), text.raw);
        assert_eq!(Text::new("\x1b[0m").truncate(0), "\x1b[0m");
    }

    #[test]
    fn padding_fills_columns_even_when_a_grapheme_does_not_fit() {
        assert_eq!(padding("界", 5), "界界 ");
        assert_eq!(padding("e\u{301}", 2), "e\u{301}e\u{301}");
        assert_eq!(
            padding("\x1b[31m.\x1b[0m", 2),
            "\x1b[31m.\x1b[0m\x1b[31m.\x1b[0m"
        );
        assert_eq!(padding("\x1b[0m", 2), "  ");
    }
}
