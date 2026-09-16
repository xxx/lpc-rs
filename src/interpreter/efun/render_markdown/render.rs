use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_telnet::ColourDepth;
use pulldown_cmark::{Alignment, CowStr, Event, Options, Parser, Tag, TagEnd, TextMergeStream};
use unicode_width::UnicodeWidthStr;

use super::super::terminal_text::{LIMIT, Style, Text};

const MAX_NESTING: usize = 128;

pub(super) fn render(source: &str, width: usize, depth: ColourDepth) -> Result<String> {
    if source.len() > LIMIT {
        return Err(lpc_error!("render_markdown: input exceeds 1 MiB"));
    }
    let mut clean = Text::new("render_markdown");
    clean.unstyled(source)?;
    let parser = Parser::new_ext(
        clean.plain(),
        Options::ENABLE_TABLES | Options::ENABLE_TASKLISTS,
    );
    let mut renderer = Renderer::new(width, depth);
    for (count, event) in TextMergeStream::new(parser).enumerate() {
        if count >= LIMIT {
            return Err(lpc_error!("render_markdown: too many parser events"));
        }
        renderer.event(event)?;
    }
    renderer.flush()?;
    Ok(renderer.output)
}

enum Container {
    Quote,
    Item { marker: String, used: bool },
}

impl Container {
    fn width(&self) -> usize {
        match self {
            Self::Quote => 2,
            Self::Item { marker, .. } => marker.len(),
        }
    }
}

struct Table {
    alignments: Vec<Alignment>,
    rows: Vec<Vec<Text>>,
}

struct List {
    next: Option<u64>,
    loose: bool,
}

struct Renderer<'a> {
    width: usize,
    depth: ColourDepth,
    output: String,
    block: Text,
    style: Style,
    styles: Vec<Style>,
    containers: Vec<Container>,
    lists: Vec<List>,
    links: Vec<(usize, CowStr<'a>)>,
    table: Option<Table>,
    code: bool,
    blank: bool,
    expanded: usize,
}

impl<'a> Renderer<'a> {
    fn new(width: usize, depth: ColourDepth) -> Self {
        Self {
            width,
            depth,
            output: String::new(),
            block: Text::new("render_markdown"),
            style: Style::default(),
            styles: Vec::new(),
            containers: Vec::new(),
            lists: Vec::new(),
            links: Vec::new(),
            table: None,
            code: false,
            blank: false,
            expanded: 0,
        }
    }

    fn event(&mut self, event: Event<'a>) -> Result<()> {
        match event {
            Event::Start(tag) => {
                if self.styles.len() >= MAX_NESTING {
                    return Err(lpc_error!("render_markdown: nesting exceeds 128"));
                }
                self.styles.push(self.style);
                self.start(tag)?;
            }
            Event::End(tag) => {
                self.end(tag)?;
                self.style = self.styles.pop().unwrap_or_default();
            }
            Event::Text(text) | Event::Html(text) | Event::InlineHtml(text) => self.text(&text)?,
            Event::Code(text) => {
                let previous = self.style;
                self.style.token("CYAN");
                self.text(&text)?;
                self.style = previous;
            }
            Event::SoftBreak => self.text(" ")?,
            Event::HardBreak => self.text("\n")?,
            Event::Rule => {
                self.flush()?;
                self.begin_block()?;
                self.line(&"-".repeat(self.available().min(40)), 0)?;
                self.blank = true;
            }
            Event::TaskListMarker(checked) => self.text(if checked { "[x] " } else { "[ ] " })?,
            // Extensions other than tables and task lists are disabled above.
            _ => {}
        }
        Ok(())
    }

    fn start(&mut self, tag: Tag<'a>) -> Result<()> {
        match tag {
            Tag::Paragraph | Tag::HtmlBlock => self.flush()?,
            Tag::Heading { level, .. } => {
                self.flush()?;
                self.style.token("BOLD");
                self.style.token("CYAN");
                self.text(&"#".repeat(level as usize))?;
                self.text(" ")?;
            }
            Tag::BlockQuote(_) => {
                self.flush()?;
                self.containers.push(Container::Quote);
            }
            Tag::List(start) => {
                self.flush()?;
                self.lists.push(List {
                    next: start,
                    loose: false,
                });
            }
            Tag::Item => {
                self.flush()?;
                let marker = match self.lists.last_mut().and_then(|list| list.next.as_mut()) {
                    Some(number) => {
                        let marker = format!("{number}. ");
                        *number = number.saturating_add(1);
                        marker
                    }
                    _ => "- ".to_owned(),
                };
                self.containers.push(Container::Item {
                    marker,
                    used: false,
                });
            }
            Tag::CodeBlock(_) => {
                self.flush()?;
                self.code = true;
                self.style.token("CYAN");
            }
            Tag::Emphasis => {
                self.style.token("ITALIC");
            }
            Tag::Strong => {
                self.style.token("BOLD");
            }
            Tag::Link { dest_url, .. } => {
                self.links.push((self.block.plain().len(), dest_url));
                self.style.token("UNDERLINE");
            }
            Tag::Image { .. } => self.text("[image: ")?,
            Tag::Table(alignments) => {
                self.flush()?;
                self.table = Some(Table {
                    alignments,
                    rows: Vec::new(),
                });
            }
            Tag::TableHead | Tag::TableRow => {
                if tag == Tag::TableHead {
                    self.style.token("BOLD");
                }
                if let Some(table) = &mut self.table {
                    table.rows.push(Vec::new());
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn end(&mut self, tag: TagEnd) -> Result<()> {
        match tag {
            TagEnd::Paragraph | TagEnd::Heading(_) | TagEnd::HtmlBlock => {
                self.flush()?;
                self.blank = true;
                if tag == TagEnd::Paragraph
                    && matches!(self.containers.last(), Some(Container::Item { .. }))
                    && let Some(list) = self.lists.last_mut()
                {
                    list.loose = true;
                }
            }
            TagEnd::BlockQuote(_) => {
                self.flush()?;
                self.containers.pop();
                self.blank = true;
            }
            TagEnd::List(_) => {
                self.flush()?;
                self.lists.pop();
                self.blank = true;
            }
            TagEnd::Item => {
                self.flush()?;
                if matches!(
                    self.containers.last(),
                    Some(Container::Item { used: false, .. })
                ) {
                    self.begin_block()?;
                    self.line("", 0)?;
                }
                self.containers.pop();
                self.blank = self.lists.last().is_some_and(|list| list.loose);
            }
            TagEnd::CodeBlock => {
                self.flush()?;
                self.code = false;
                self.blank = true;
            }
            TagEnd::Link => {
                if let Some((start, destination)) = self.links.pop()
                    && self.block.plain().get(start..) != Some(destination.as_ref())
                    && !destination.is_empty()
                {
                    self.text(" (")?;
                    self.text(&destination)?;
                    self.text(")")?;
                }
            }
            TagEnd::Image => self.text("]")?,
            TagEnd::TableCell => {
                let cell = self.take_block();
                if let Some(row) = self.table.as_mut().and_then(|table| table.rows.last_mut()) {
                    row.push(cell);
                }
            }
            TagEnd::Table => {
                if let Some(table) = self.table.take() {
                    self.render_table(table)?;
                }
                self.blank = true;
            }
            _ => {}
        }
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<()> {
        if text.len() > LIMIT.saturating_sub(self.expanded) {
            return Err(lpc_error!("render_markdown: expansion exceeds 1 MiB"));
        }
        self.expanded += text.len();
        self.block.set_style(self.style);
        self.block.unstyled(text)
    }

    fn take_block(&mut self) -> Text {
        std::mem::replace(&mut self.block, Text::new("render_markdown"))
    }

    fn available(&self) -> usize {
        self.width
            .saturating_sub(self.containers.iter().map(Container::width).sum())
            .max(1)
    }

    fn flush(&mut self) -> Result<()> {
        if self.block.plain().is_empty() {
            return Ok(());
        }
        let block = self.take_block();
        let width = if self.code {
            0
        } else {
            self.available() as i64
        };
        let rendered = block.render(self.depth, width, 0)?;
        self.begin_block()?;
        for line in rendered.split_terminator('\n') {
            self.line(line, if self.code { 4 } else { 0 })?;
        }
        Ok(())
    }

    fn begin_block(&mut self) -> Result<()> {
        if self.blank && !self.output.is_empty() {
            self.push("\n")?;
        }
        self.blank = false;
        Ok(())
    }

    fn line(&mut self, text: &str, margin: usize) -> Result<()> {
        let mut prefix = String::new();
        for container in &mut self.containers {
            match container {
                Container::Quote => prefix.push_str("> "),
                Container::Item { marker, used } => {
                    if *used {
                        prefix.extend(std::iter::repeat_n(' ', marker.len()));
                    } else {
                        prefix.push_str(marker);
                        *used = true;
                    }
                }
            }
        }
        prefix.extend(std::iter::repeat_n(' ', margin));
        self.push(&prefix)?;
        self.push(text)?;
        self.push("\n")
    }

    fn push(&mut self, text: &str) -> Result<()> {
        if text.len() > LIMIT.saturating_sub(self.output.len()) {
            return Err(lpc_error!("render_markdown: output exceeds 1 MiB"));
        }
        self.output.push_str(text);
        Ok(())
    }

    fn render_table(&mut self, table: Table) -> Result<()> {
        let columns = table.alignments.len();
        if columns == 0 || table.rows.is_empty() {
            return Ok(());
        }
        let mut widths = vec![1; columns];
        let mut plain_rows = Vec::with_capacity(table.rows.len());
        for row in &table.rows {
            let mut plain_row = Vec::with_capacity(row.len());
            for (column, cell) in row.iter().enumerate() {
                let plain = cell.render(ColourDepth::Plain, 0, 0)?;
                if let Some(width) = widths.get_mut(column) {
                    *width = (*width).max(plain.lines().map(str::width).max().unwrap_or(0));
                }
                plain_row.push(plain);
            }
            plain_rows.push(plain_row);
        }
        let table_width = widths.iter().sum::<usize>() + 3 * (columns - 1);
        if table_width > self.available() {
            return self.table_fields(&table, &plain_rows);
        }
        self.begin_block()?;
        for (row_index, row) in table.rows.iter().enumerate() {
            let cells = row
                .iter()
                .map(|cell| cell.render(self.depth, 0, 0))
                .collect::<Result<Vec<_>>>()?;
            let lines: Vec<Vec<_>> = cells
                .iter()
                .map(|cell| cell.split('\n').collect())
                .collect();
            let plain_lines: Vec<Vec<_>> = plain_rows[row_index]
                .iter()
                .map(|cell| cell.split('\n').collect())
                .collect();
            let height = lines.iter().map(Vec::len).max().unwrap_or(1);
            for line_index in 0..height {
                let mut line = String::new();
                for (column, &width) in widths.iter().enumerate() {
                    if column > 0 {
                        line.push_str(" | ");
                    }
                    let text = lines
                        .get(column)
                        .and_then(|cell| cell.get(line_index))
                        .copied()
                        .unwrap_or("");
                    let plain = plain_lines
                        .get(column)
                        .and_then(|cell| cell.get(line_index))
                        .copied()
                        .unwrap_or("");
                    let padding = width.saturating_sub(plain.width());
                    let left = match table.alignments[column] {
                        Alignment::Right => padding,
                        Alignment::Center => padding / 2,
                        _ => 0,
                    };
                    line.extend(std::iter::repeat_n(' ', left));
                    line.push_str(text);
                    line.extend(std::iter::repeat_n(' ', padding - left));
                }
                self.line(&line, 0)?;
            }
            if row_index == 0 {
                let separator = widths
                    .iter()
                    .map(|&width| "-".repeat(width))
                    .collect::<Vec<_>>()
                    .join("-+-");
                self.line(&separator, 0)?;
            }
        }
        Ok(())
    }

    fn table_fields(&mut self, table: &Table, plain_rows: &[Vec<String>]) -> Result<()> {
        let headers = &plain_rows[0];
        if table.rows.len() == 1 {
            for cell in &table.rows[0] {
                self.block.extend(cell)?;
                self.flush()?;
            }
            return Ok(());
        }
        for (row_index, row) in table.rows.iter().enumerate().skip(1) {
            if row_index > 1 {
                self.blank = true;
            }
            for column in 0..table.alignments.len() {
                let label = headers
                    .get(column)
                    .map(String::as_str)
                    .filter(|s| !s.is_empty());
                let mut heading = Style::default();
                heading.token("BOLD");
                self.block.set_style(heading);
                self.block.unstyled(&format!(
                    "{}: ",
                    label
                        .map(str::to_owned)
                        .unwrap_or_else(|| format!("Column {}", column + 1))
                ))?;
                if let Some(cell) = row.get(column) {
                    self.block.extend(cell)?;
                }
                self.flush()?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plain(source: &str, width: usize) -> String {
        render(source, width, ColourDepth::Plain).unwrap()
    }

    #[test]
    fn paragraphs_headings_and_breaks_have_readable_plain_structure() {
        assert_eq!(
            plain(
                "# Help\n\nFirst line\ncontinues.  \nHard break.\n\n---\n\n## More",
                20
            ),
            "# Help\n\nFirst line\ncontinues.\nHard break.\n\n--------------------\n\n## More\n"
        );
    }

    #[test]
    fn nested_lists_keep_markers_and_hanging_indentation() {
        assert_eq!(
            plain(
                "3. one two three four\n   - inner words wrap here\n4. last",
                16
            ),
            "3. one two three\n   four\n   - inner words\n     wrap here\n4. last\n"
        );
        assert_eq!(
            plain("- first\n\n- second\n\n  continuation", 80),
            "- first\n\n- second\n\n  continuation\n"
        );
        assert_eq!(plain("-\n- last", 80), "- \n- last\n");
    }

    #[test]
    fn quotes_and_checkboxes_survive_wrapping() {
        assert_eq!(
            plain(
                "> - [x] done with task\n> - [ ] remaining task\n\noutside",
                16
            ),
            "> - [x] done\n>   with task\n> - [ ]\n>   remaining\n>   task\n\noutside\n"
        );
        assert_eq!(plain("> outer\n>\n> > inner", 80), "> outer\n\n> > inner\n");
    }

    #[test]
    fn code_preserves_whitespace_and_literal_markup() {
        let source = "```lpc\n\twrite(\"%^RED%^**hi**\");  \n\n    return;\n```\n\n`%^RESET%^`";
        assert_eq!(
            plain(source, 8),
            "            write(\"%^RED%^**hi**\");  \n    \n        return;\n\n%^RESET%\n^\n"
        );
        assert_eq!(plain("    x  y\n    z", 3), "    x  y\n    z\n");
        assert_eq!(plain("` a\nb `", 80), "a b\n");
    }

    #[test]
    fn links_images_and_html_remain_readable_and_inert() {
        assert_eq!(
            plain(
                "[Guide](help.md#start) <https://example.org> ![A **map**](map.png) <b>raw</b> %^RED%^",
                100
            ),
            "Guide (help.md#start) https://example.org [image: A map] <b>raw</b> %^RED%^\n"
        );
        assert_eq!(
            plain("[manual][ref]\n\n[ref]: ../manual.md", 80),
            "manual (../manual.md)\n"
        );
        assert_eq!(plain("<div>\ntext\n</div>", 80), "<div>\ntext\n</div>\n");
    }

    #[test]
    fn nested_styles_restore_the_parent_and_reset_each_line() {
        assert_eq!(
            render("**bold *both* bold** plain", 80, ColourDepth::Ansi8).unwrap(),
            "\x1b[1mbold \x1b[0m\x1b[1;3mboth\x1b[0m\x1b[1m bold\x1b[0m plain\n"
        );
        assert_eq!(
            render("**one two**", 3, ColourDepth::Ansi8).unwrap(),
            "\x1b[1mone\x1b[0m\n\x1b[1mtwo\x1b[0m\n"
        );
        assert_eq!(
            render("# Hi", 80, ColourDepth::Indexed).unwrap(),
            "\x1b[1;38;5;6m# Hi\x1b[0m\n"
        );
    }

    #[test]
    fn control_sequences_are_discarded_even_when_created_by_entities() {
        let source = "a\x1b[31mb\x1b[0m\x1b]0;title\x07c\x07\n\n&#27;[31mred";
        for depth in [
            ColourDepth::Plain,
            ColourDepth::Ansi8,
            ColourDepth::TrueColour,
        ] {
            assert_eq!(render(source, 80, depth).unwrap(), "abc\n\nred\n");
        }
        assert_eq!(plain("a\r\nb\rc", 80), "a b c\n");
    }

    #[test]
    fn table_columns_follow_alignment_and_unicode_display_width() {
        assert_eq!(
            plain(
                "| Left | Center | Right |\n| :--- | :---: | ---: |\n| 界 | x | 3 |",
                30
            ),
            "Left | Center | Right\n-----+--------+------\n界   |   x    |     3\n"
        );
        assert_eq!(
            plain("| A | B |\n| - | - |\n| x |\n| y | z | ignored |", 20),
            "A | B\n--+--\nx |  \ny | z\n"
        );
    }

    #[test]
    fn narrow_tables_become_wrapped_labelled_fields() {
        assert_eq!(
            plain(
                "| Name | Description |\n| --- | --- |\n| sword | A sharp blade |\n| bow | Ranged |",
                18
            ),
            "Name: sword\nDescription: A\nsharp blade\n\nName: bow\nDescription:\nRanged\n"
        );
        assert_eq!(
            plain("| | Meaning |\n| - | - |\n| x | y |", 4),
            "Colu\nmn\n1: x\nMean\ning:\ny\n"
        );
    }

    #[test]
    fn table_headers_survive_without_body_rows() {
        assert_eq!(
            plain("| Alpha | Beta |\n| --- | --- |", 80),
            "Alpha | Beta\n------+-----\n"
        );
        assert_eq!(
            plain("| Alpha | Beta |\n| --- | --- |", 4),
            "Alph\na\nBeta\n"
        );
    }

    #[test]
    fn table_fallback_preserves_inline_styles() {
        let output = render(
            "| Header |\n| --- |\n| **very long value** |",
            12,
            ColourDepth::Ansi8,
        )
        .unwrap();
        assert_eq!(
            output,
            "\x1b[1mHeader: very\x1b[0m\n\x1b[1mlong value\x1b[0m\n"
        );
    }

    #[test]
    fn unicode_clusters_and_narrow_containers_are_never_split() {
        assert_eq!(plain("界界 e\u{301} 👩‍💻", 3), "界\n界\ne\u{301}\n👩‍💻\n");
        assert_eq!(plain("界e\u{301}", 1), "界\ne\u{301}\n");
        assert_eq!(plain("> - abc", 1), "> - a\n>   b\n>   c\n");
    }

    #[test]
    fn empty_and_malformed_documents_use_commonmark_behavior() {
        for source in ["", " \t\n\n", "[unused]: example.org"] {
            assert_eq!(plain(source, 80), "");
        }
        assert_eq!(
            plain("**unfinished [link](\n\n```\ncode", 80),
            "**unfinished [link](\n\n    code\n"
        );
    }

    #[test]
    fn input_output_expansion_and_nesting_are_bounded() {
        assert!(
            render(&"x".repeat(LIMIT + 1), 80, ColourDepth::Plain)
                .unwrap_err()
                .to_string()
                .contains("input exceeds")
        );
        assert!(
            render(&"x".repeat(LIMIT), 1, ColourDepth::Plain)
                .unwrap_err()
                .to_string()
                .contains("output exceeds")
        );
        let source = format!("{}x\n", "> ".repeat(MAX_NESTING));
        assert!(
            render(&source, 80, ColourDepth::Plain)
                .unwrap_err()
                .to_string()
                .contains("nesting exceeds")
        );
        let source = "![](x) ".repeat(LIMIT / 10 + 1);
        assert!(
            render(&source, 4096, ColourDepth::Plain)
                .expect_err("image labels exceed the expansion budget")
                .to_string()
                .contains("expansion exceeds 1 MiB")
        );
    }

    #[test]
    fn existing_efun_documentation_renders_at_terminal_and_phone_widths() {
        let source = include_str!("../../../../doc/efun/terminal_colour.md");
        for width in [24, 80, 120] {
            let output = plain(source, width);
            assert!(output.starts_with("# terminal_colour\n"));
            assert!(output.contains("%^BOLD%^%^RED%^"));
            assert!(!output.contains('\x1b'));
            assert!(output.contains("Compatibility"));
        }
    }
}
