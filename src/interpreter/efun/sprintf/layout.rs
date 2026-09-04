//! The output assembled row by row: a column or table field's later lines
//! continue under it on the rows that follow.

use std::collections::VecDeque;

/// What a field contributes: one line, or a column's or table's lines.
pub(super) struct Field {
    pub lines: Vec<String>,
    pub kind: Kind,
}

/// What the text at a row's end came from, for the trailing-space rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Kind {
    Literal,
    Plain,
    /// `strip`: padding at the row's end is dropped (a default pad only).
    Column {
        strip: bool,
    },
    Table,
}

/// A multi-line field's remaining lines, continued at `offset`.
struct Pending {
    offset: usize,
    lines: VecDeque<String>,
    kind: Kind,
}

#[derive(Default)]
pub(super) struct Layout {
    out: String,
    row: String,
    /// `row`'s width in characters.
    row_len: usize,
    last: Option<Kind>,
    pending: Vec<Pending>,
}

impl Layout {
    /// One character of the format's own text; a newline ends the row.
    pub(super) fn text_char(&mut self, c: char) {
        if c == '\n' {
            self.finish_row(true);
            return;
        }
        self.row.push(c);
        self.row_len += 1;
        self.last = Some(Kind::Literal);
    }

    /// A field's first line into the row; the rest wait for the rows that
    /// follow.
    pub(super) fn field(&mut self, field: Field) {
        let offset = self.row_len;
        let mut lines: VecDeque<String> = field.lines.into();
        let first = lines.pop_front().unwrap_or_default();
        self.row_len += first.chars().count();
        self.row.push_str(&first);
        self.last = Some(field.kind);
        if !lines.is_empty() {
            self.pending.push(Pending {
                offset,
                lines,
                kind: field.kind,
            });
        }
    }

    /// Close the row and the continuation rows under it, each followed by
    /// a newline when `newline`, else joined by one.
    fn finish_row(&mut self, newline: bool) {
        let mut rows = vec![Self::stripped(std::mem::take(&mut self.row), self.last)];
        self.row_len = 0;
        self.last = None;
        while self.pending.iter().any(|p| !p.lines.is_empty()) {
            let mut row = String::new();
            let mut len = 0;
            let mut last = None;
            for pending in &mut self.pending {
                let Some(line) = pending.lines.pop_front() else {
                    continue;
                };
                if pending.offset > len {
                    row.push_str(&" ".repeat(pending.offset - len));
                    len = pending.offset;
                }
                len += line.chars().count();
                row.push_str(&line);
                last = Some(pending.kind);
            }
            rows.push(Self::stripped(row, last));
        }
        self.pending.clear();
        self.out.push_str(&rows.join("\n"));
        if newline {
            self.out.push('\n');
        }
    }

    /// `row` without the padding a column left at its end.
    fn stripped(row: String, last: Option<Kind>) -> String {
        match last {
            Some(Kind::Column { strip: true }) => row.trim_end_matches(' ').to_owned(),
            _ => row,
        }
    }

    /// The whole output.
    pub(super) fn finish(mut self) -> String {
        if !self.row.is_empty() || !self.pending.is_empty() {
            self.finish_row(false);
        }
        self.out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn column(lines: &[&str]) -> Field {
        Field {
            lines: lines.iter().map(|s| (*s).to_owned()).collect(),
            kind: Kind::Column { strip: true },
        }
    }

    #[test]
    fn a_columns_later_lines_continue_under_its_offset() {
        let mut layout = Layout::default();
        layout.text_char('>');
        layout.field(column(&["aa ", "b  "]));
        layout.text_char('|');
        layout.text_char('\n');
        assert_eq!(layout.finish(), ">aa |\n b\n");
    }

    #[test]
    fn rows_with_no_format_newline_are_joined_by_one() {
        let mut layout = Layout::default();
        layout.field(column(&["x ", "y ", ""]));
        assert_eq!(layout.finish(), "x\ny\n");
    }

    #[test]
    fn literal_text_at_a_rows_end_keeps_its_spaces() {
        let mut layout = Layout::default();
        layout.text_char('a');
        layout.text_char(' ');
        assert_eq!(layout.finish(), "a ");
    }
}
