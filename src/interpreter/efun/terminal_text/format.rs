use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_telnet::ColourDepth;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use super::colour::Style;

pub(in crate::interpreter::efun) const LIMIT: usize = 1024 * 1024;

pub(in crate::interpreter::efun) enum Part<'a> {
    Text(&'a str),
    Key(&'a str),
}

pub(in crate::interpreter::efun) fn next_part<'a>(
    text: &'a str,
    cursor: &mut usize,
) -> Option<Part<'a>> {
    let rest = text.get(*cursor..).filter(|s| !s.is_empty())?;
    #[cfg(debug_assertions)]
    let before = *cursor;
    if rest.starts_with("%%^^") {
        *cursor += 4;
        #[cfg(debug_assertions)]
        debug_assert!(
            before < *cursor && *cursor <= text.len(),
            "markup cursor did not advance"
        );
        return Some(Part::Text("%^"));
    }
    if let Some(key) = rest.strip_prefix("%^") {
        if let Some(end) = key.find("%^") {
            *cursor += end + 4;
            #[cfg(debug_assertions)]
            debug_assert!(
                before < *cursor && *cursor <= text.len(),
                "markup cursor did not advance"
            );
            return Some(Part::Key(&key[..end]));
        }
        *cursor = text.len();
        #[cfg(debug_assertions)]
        debug_assert!(
            before < *cursor && *cursor <= text.len(),
            "markup cursor did not advance"
        );
        return Some(Part::Text(rest));
    }
    let mut end = 0;
    while end < rest.len() {
        #[cfg(debug_assertions)]
        let previous_end = end;
        let tail = &rest[end..];
        if end > 0 && (tail.starts_with("%^") || tail.starts_with("%%^^")) {
            break;
        }
        if tail.starts_with('\x1b') {
            end += escape(tail).0;
        } else if let Some(c) = tail.chars().next() {
            end += c.len_utf8();
        }
        #[cfg(debug_assertions)]
        debug_assert!(end > previous_end, "markup scan did not advance");
    }
    *cursor += end;
    #[cfg(debug_assertions)]
    debug_assert!(
        before < *cursor && *cursor <= text.len(),
        "markup cursor did not advance"
    );
    Some(Part::Text(&rest[..end]))
}

#[derive(Debug, Clone)]
pub(in crate::interpreter::efun) struct Text {
    plain: String,
    changes: Vec<(usize, Style)>,
    style: Style,
    charged: usize,
    after_cr: bool,
    caller: &'static str,
}

impl Default for Text {
    fn default() -> Self {
        Self::new("terminal_colour")
    }
}

impl Text {
    pub(in crate::interpreter::efun) fn new(caller: &'static str) -> Self {
        Self {
            plain: String::new(),
            changes: Vec::new(),
            style: Style::default(),
            charged: 0,
            after_cr: false,
            caller,
        }
    }

    pub(in crate::interpreter::efun) fn plain(&self) -> &str {
        &self.plain
    }

    pub(in crate::interpreter::efun) fn set_style(&mut self, style: Style) {
        let previous = self.style;
        self.style = style;
        self.record_style(previous);
    }

    pub(in crate::interpreter::efun) fn extend(&mut self, text: &Self) -> Result<()> {
        self.charge(text.charged)?;
        self.set_style(Style::default());
        let base = self.plain.len();
        self.plain.push_str(&text.plain);
        self.changes.extend(
            text.changes
                .iter()
                .map(|&(offset, style)| (base + offset, style)),
        );
        self.style = text.style;
        self.after_cr = text.after_cr;
        Ok(())
    }

    fn charge(&mut self, len: usize) -> Result<()> {
        if len > LIMIT.saturating_sub(self.charged) {
            return Err(lpc_error!("{}: expansion exceeds 1 MiB", self.caller));
        }
        self.charged += len;
        Ok(())
    }

    fn record_style(&mut self, previous: Style) {
        if previous == self.style {
            return;
        }
        if let Some((offset, style)) = self.changes.last_mut()
            && *offset == self.plain.len()
        {
            *style = self.style;
        } else {
            self.changes.push((self.plain.len(), self.style));
        }
    }

    pub(in crate::interpreter::efun) fn token(&mut self, name: &str) -> Result<bool> {
        self.charge(name.len())?;
        let previous = self.style;
        let found = self.style.token(name);
        self.record_style(previous);
        Ok(found)
    }

    pub(in crate::interpreter::efun) fn builtins(&mut self, text: &str) -> Result<()> {
        if text.len() > LIMIT {
            return Err(lpc_error!("{}: replacement exceeds 1 MiB", self.caller));
        }
        let mut cursor = 0;
        while let Some(part) = next_part(text, &mut cursor) {
            match part {
                Part::Text(s) => self.literal(s)?,
                Part::Key(key) => {
                    self.token(key)?;
                }
            }
        }
        Ok(())
    }

    pub(in crate::interpreter::efun) fn literal(&mut self, text: &str) -> Result<()> {
        self.append(text, true)
    }

    pub(in crate::interpreter::efun) fn unstyled(&mut self, text: &str) -> Result<()> {
        self.append(text, false)
    }

    fn append(&mut self, text: &str, accept_sgr: bool) -> Result<()> {
        self.charge(text.len())?;
        let mut rest = text;
        while !rest.is_empty() {
            if rest.starts_with('\x1b') {
                let (len, sgr) = escape(rest);
                debug_assert!(len > 0, "escape did not consume input");
                if accept_sgr && let Some(params) = sgr {
                    let previous = self.style;
                    self.style.sgr(params);
                    self.record_style(previous);
                }
                rest = &rest[len..];
                continue;
            }
            let Some(c) = rest.chars().next() else { break };
            rest = &rest[c.len_utf8()..];
            match c {
                '\r' => {
                    self.plain.push('\n');
                    self.after_cr = true;
                }
                '\n' if self.after_cr => self.after_cr = false,
                '\n' | '\t' => {
                    self.plain.push(c);
                    self.after_cr = false;
                }
                _ => {
                    self.after_cr = false;
                    if !c.is_control() {
                        self.plain.push(c);
                    }
                }
            }
        }
        Ok(())
    }

    pub(in crate::interpreter::efun) fn render(
        &self,
        depth: ColourDepth,
        wrap: i64,
        indent: usize,
    ) -> Result<String> {
        let width = wrap.unsigned_abs() as usize;
        let atoms: Vec<_> = self
            .plain
            .grapheme_indices(true)
            .map(|(offset, text)| Atom {
                offset,
                text,
                width: text.width(),
            })
            .collect();
        let mut output = Output::new(depth, &self.changes, self.caller);
        let mut start = 0;
        let mut continuation = false;
        #[cfg(debug_assertions)]
        let mut previous_start = None;
        while start < atoms.len() {
            #[cfg(debug_assertions)]
            debug_assert!(
                previous_start.replace(start).is_none_or(|old| start > old),
                "wrapped line did not advance"
            );
            let padding = if continuation { indent } else { 0 };
            let mut col = padding;
            let mut end = start;
            let mut space = None;
            let mut soft = false;
            while end < atoms.len() && atoms[end].text != "\n" {
                #[cfg(debug_assertions)]
                let previous_end = end;
                let atom = &atoms[end];
                if width != 0 && col + atom.columns(col) > width {
                    if atom.is_space() {
                        soft = true;
                        break;
                    }
                    if let Some(at) = space {
                        end = at;
                    } else if end == start {
                        end += 1;
                    }
                    soft = true;
                    break;
                }
                if atom.is_space() {
                    space = Some(end);
                }
                col += atom.columns(col);
                end += 1;
                #[cfg(debug_assertions)]
                debug_assert!(end > previous_end, "line scan did not advance");
            }
            if soft {
                while end > start && atoms[end - 1].is_space() {
                    #[cfg(debug_assertions)]
                    let previous_end = end;
                    end -= 1;
                    #[cfg(debug_assertions)]
                    debug_assert!(end < previous_end, "space trimming did not advance");
                }
            }
            let mut next = end;
            if soft {
                while next < atoms.len() && atoms[next].is_space() {
                    #[cfg(debug_assertions)]
                    let previous_next = next;
                    next += 1;
                    #[cfg(debug_assertions)]
                    debug_assert!(next > previous_next, "space scan did not advance");
                }
            }
            let explicit = next < atoms.len() && atoms[next].text == "\n";
            if soft && end == start && !explicit {
                start = next;
                continue;
            }
            output.spaces(padding)?;
            col = padding;
            for atom in &atoms[start..end] {
                output.atom(atom, col)?;
                col += atom.columns(col);
            }
            output.style(Style::default())?;
            if wrap < 0 {
                output.spaces(width.saturating_sub(col))?;
            }
            if explicit || next < atoms.len() {
                output.push("\n")?;
            }
            start = next + usize::from(explicit);
            continuation = soft && !explicit;
        }
        Ok(output.text)
    }
}

/// Consume an escape atom; only SGR has a supported formatting effect.
fn escape(text: &str) -> (usize, Option<&str>) {
    let bytes = text.as_bytes();
    match bytes.get(1) {
        Some(b'[') => {
            if let Some(i) = bytes
                .iter()
                .enumerate()
                .skip(2)
                .find_map(|(i, b)| (0x40..=0x7e).contains(b).then_some(i))
            {
                (i + 1, (bytes[i] == b'm').then_some(&text[2..i]))
            } else {
                (text.len(), None)
            }
        }
        Some(b']' | b'P' | b'^' | b'_' | b'X') => {
            let mut i = 2;
            while i < bytes.len() {
                #[cfg(debug_assertions)]
                let previous_i = i;
                if bytes[i] == 7 {
                    return (i + 1, None);
                }
                if bytes[i..].starts_with(b"\x1b\\") {
                    return (i + 2, None);
                }
                i += 1;
                #[cfg(debug_assertions)]
                debug_assert!(i > previous_i, "escape scan did not advance");
            }
            (text.len(), None)
        }
        Some(b) if b.is_ascii() => (2, None),
        _ => (1, None),
    }
}

struct Atom<'a> {
    offset: usize,
    text: &'a str,
    width: usize,
}

impl Atom<'_> {
    fn is_space(&self) -> bool {
        matches!(self.text, " " | "\t")
    }
    fn columns(&self, col: usize) -> usize {
        if self.text == "\t" {
            8 - col % 8
        } else {
            self.width
        }
    }
}

struct Output<'a> {
    text: String,
    depth: ColourDepth,
    emitted: Style,
    source: Style,
    changes: &'a [(usize, Style)],
    caller: &'static str,
}

impl<'a> Output<'a> {
    fn new(depth: ColourDepth, changes: &'a [(usize, Style)], caller: &'static str) -> Self {
        Self {
            text: String::new(),
            depth,
            emitted: Style::default(),
            source: Style::default(),
            changes,
            caller,
        }
    }

    fn push(&mut self, text: &str) -> Result<()> {
        if text.len() > LIMIT.saturating_sub(self.text.len()) {
            return Err(lpc_error!("{}: output exceeds 1 MiB", self.caller));
        }
        self.text.push_str(text);
        Ok(())
    }

    fn spaces(&mut self, n: usize) -> Result<()> {
        if n > LIMIT.saturating_sub(self.text.len()) {
            return Err(lpc_error!("{}: output exceeds 1 MiB", self.caller));
        }
        self.text.extend(std::iter::repeat_n(' ', n));
        Ok(())
    }

    fn style(&mut self, style: Style) -> Result<()> {
        if self.depth == ColourDepth::Plain || self.emitted == style {
            return Ok(());
        }
        if self.emitted != Style::default() {
            self.push("\x1b[0m")?;
        }
        self.push(&style.sequence(self.depth))?;
        self.emitted = style;
        Ok(())
    }

    fn atom(&mut self, atom: &Atom<'_>, col: usize) -> Result<()> {
        let end = atom.offset + atom.text.len();
        while let Some(&(offset, style)) = self.changes.first() {
            if offset > atom.offset {
                break;
            }
            self.source = style;
            self.changes = &self.changes[1..];
        }
        self.style(self.source)?;
        if atom.text == "\t" {
            return self.spaces(atom.columns(col));
        }
        let mut start = atom.offset;
        while let Some(&(offset, style)) = self.changes.first() {
            if offset >= end {
                break;
            }
            self.push(&atom.text[start - atom.offset..offset - atom.offset])?;
            self.source = style;
            self.changes = &self.changes[1..];
            self.style(style)?;
            start = offset;
        }
        self.push(&atom.text[start - atom.offset..])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render(text: &str, wrap: i64, indent: usize) -> String {
        let mut parsed = Text::default();
        parsed.builtins(text).unwrap();
        parsed
            .render(ColourDepth::TrueColour, wrap, indent)
            .unwrap()
    }

    #[test]
    fn literal_chunk_boundaries_preserve_streamed_rendering() {
        let chunks = [
            "a",
            "\r",
            "\x1b[31m",
            "\n",
            "e",
            "́",
            "字",
            "\x1b[0m",
            "\t",
            "z",
            "\r",
            "\x1b]title\x07",
            "\n",
            "q",
        ];
        let mut whole = Text::default();
        whole.literal(&chunks.concat()).unwrap();
        let mut streamed = Text::default();
        for chunk in chunks {
            streamed.literal(chunk).unwrap();
        }

        assert_eq!(streamed.plain(), whole.plain());
        for depth in [
            ColourDepth::Plain,
            ColourDepth::Ansi8,
            ColourDepth::TrueColour,
        ] {
            for wrap in [0, 4, -4] {
                assert_eq!(
                    streamed.render(depth, wrap, 1).unwrap(),
                    whole.render(depth, wrap, 1).unwrap()
                );
            }
        }
    }

    #[test]
    fn extending_text_relocates_style_changes_after_the_existing_prefix() {
        let mut left = Text::default();
        left.builtins("%^BLUE%^prefix ").unwrap();
        let mut right = Text::default();
        right.builtins("x%^RED%^red%^RESET%^tail").unwrap();
        left.extend(&right).unwrap();
        let mut whole = Text::default();
        whole
            .builtins("%^BLUE%^prefix %^RESET%^x%^RED%^red%^RESET%^tail")
            .unwrap();

        assert_eq!(left.plain(), whole.plain());
        for depth in [
            ColourDepth::Plain,
            ColourDepth::Ansi8,
            ColourDepth::TrueColour,
        ] {
            for wrap in [0, 6, -6] {
                assert_eq!(
                    left.render(depth, wrap, 1).unwrap(),
                    whole.render(depth, wrap, 1).unwrap()
                );
            }
        }
    }

    #[test]
    fn replacement_limit_is_inclusive_and_rejects_larger_input_before_appending() {
        let exact = "x".repeat(LIMIT);
        let mut text = Text::default();
        text.builtins(&exact).unwrap();
        assert_eq!(text.plain().len(), LIMIT);

        let mut text = Text::default();
        let error = text.builtins(&format!("{exact}x")).unwrap_err();
        assert!(error.to_string().contains("replacement exceeds 1 MiB"));
        assert!(text.plain().is_empty());
    }

    #[test]
    fn an_explicit_newline_after_soft_wrapping_resets_continuation_indent() {
        assert_eq!(render("abc   \nx", 3, 1), "abc\nx");
    }

    #[test]
    fn unsupported_escapes_consume_ascii_controls_and_preserve_unicode_text() {
        for (input, expected) in [
            ("\x1b7x", "x"),
            ("\x1bcx", "x"),
            ("\x1béx", "éx"),
            ("\x1b界x", "界x"),
            ("\x1b🦀x", "🦀x"),
            ("\x1b", ""),
        ] {
            assert_eq!(render(input, 0, 0), expected);
        }
    }

    #[test]
    fn unterminated_control_strings_consume_the_remaining_input() {
        for kind in [']', 'P', '^', '_', 'X'] {
            let input = format!("before\x1b{kind}hidden%^RED%^");
            assert_eq!(render(&input, 0, 0), "before");
        }
    }

    #[test]
    fn opaque_escape_terminators_preserve_crlf_normalization() {
        for kind in [']', 'P', '^', '_', 'X'] {
            for terminator in ["\x07", "\x1b\\"] {
                let input = format!("a\r\x1b{kind}payload{terminator}\nb");
                assert_eq!(render(&input, 0, 0), "a\nb");
            }
        }
    }

    #[test]
    fn style_changes_inside_prefixed_graphemes_use_relative_offsets() {
        assert_eq!(render("p e%^RED%^́x", 0, 0), "p e\x1b[38;5;1ḿx\x1b[0m",);
    }

    #[test]
    fn markup_is_delimited_escaped_and_not_recursive() {
        assert_eq!(
            render("RED %%^^RED%%^^ a%^%^b %^UNKNOWN%^ %^RED", 0, 0),
            "RED %^RED%^ ab  %^RED"
        );
    }

    #[test]
    fn wrapping_resets_and_reapplies_style_around_indent() {
        assert_eq!(
            render("%^RED%^aaaa bbbb%^RESET%^", 6, 2),
            "\x1b[38;5;1maaaa\x1b[0m\n  \x1b[38;5;1mbbbb\x1b[0m"
        );
    }

    #[test]
    fn wraps_words_and_preserves_explicit_lines() {
        assert_eq!(render("  a  b", 1, 0), "a\nb");
        assert_eq!(render("aaa bbb ccc ddd", 8, 2), "aaa bbb\n  ccc\n  ddd");
        assert_eq!(render("abcdefghij", 4, 0), "abcd\nefgh\nij");
        assert_eq!(render("ab cd\n\nef gh\n", 3, 1), "ab\n cd\n\nef\n gh\n");
        assert_eq!(render("abc   \nnext", 3, 0), "abc\nnex\nt");
    }

    #[test]
    fn padding_is_plain_and_tabs_use_display_columns() {
        assert_eq!(render("a bb c", -4, 0), "a bb\nc   ");
        assert_eq!(render("字\tx", 0, 0), "字      x");
        assert_eq!(render("a\r\nb\rc", 0, 0), "a\nb\nc");
        assert_eq!(render("%^RED%^a", -3, 0), "\x1b[38;5;1ma\x1b[0m  ");
    }

    #[test]
    fn graphemes_remain_intact_even_with_embedded_style_changes() {
        assert_eq!(render("字字字", 4, 0), "字字\n字");
        assert_eq!(render("ééé", 2, 0), "éé\né");
        assert_eq!(render("👩🏽‍💻👩🏽‍💻", 2, 0), "👩🏽‍💻\n👩🏽‍💻");
        assert_eq!(render("🇺🇸🇨🇦", 1, 0), "🇺🇸\n🇨🇦");
        assert_eq!(
            render("e%^RED%^́x", 1, 0),
            "e\x1b[38;5;1ḿ\x1b[0m\n\x1b[38;5;1mx\x1b[0m"
        );
    }

    #[test]
    fn plain_mode_removes_sgr_osc_and_other_controls() {
        assert_eq!(
            render("\x1b]title %^RED%^hidden\x07visible", 0, 0),
            "visible"
        );
        let mut parsed = Text::default();
        parsed
            .builtins(
                "%^RED%^hi\x1b[0m\x1b]8;;https://example.test\x1b\\link\x1b]8;;\x1b\\\x1b[2J!\x07",
            )
            .unwrap();
        assert_eq!(parsed.render(ColourDepth::Plain, 0, 0).unwrap(), "hilink!");
        assert_eq!(
            render("x\x1b[38;2;255;0;0my", 0, 0),
            "x\x1b[38;2;255;0;0my\x1b[0m"
        );
    }

    #[test]
    fn cumulative_expansion_accepts_the_exact_budget_and_rejects_more() {
        let half = "x".repeat(LIMIT / 2);
        let mut text = Text::default();
        text.literal(&half).unwrap();
        text.literal(&half).unwrap();
        assert_eq!(text.plain().len(), LIMIT);

        assert!(text.literal("x").is_err());
        assert_eq!(text.plain().len(), LIMIT);
    }

    #[test]
    fn expansion_and_output_are_bounded() {
        let mut text = Text::default();
        assert!(text.literal(&"x".repeat(LIMIT + 1)).is_err());
        text.literal("x\nx").unwrap();
        assert!(text.render(ColourDepth::Plain, -(LIMIT as i64), 0).is_err());
    }
}
