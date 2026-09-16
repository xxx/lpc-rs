# render_markdown

```c
string render_markdown(string source, int width = 0, mixed terminal = -1)
```

Render Markdown as terminal text. This efun returns a string; the mudlib chooses
where documents live, who may read them, and how to send or page the result.

```c
string help = read_file("/doc/help/combat.md");
write(render_markdown(help));
```

## Width and terminal

Width counts visible terminal columns. The default, `0`, uses the recipient's
reported window width, with an 80-column fallback. Explicit widths range from
1 to 4096; detected widths are capped at 4096.

`terminal` accepts the same recipient/depth choices as `terminal_colour`:

| Value | Output |
| --- | --- |
| `-1` or omitted | Detect from `this_player()` |
| An object | Detect from that object's connection |
| `0` | Plain text |
| `3` | Eight ANSI colors |
| `4` | Sixteen ANSI colors |
| `8` | Xterm 256 colors |
| `24` | Truecolor |

A missing, disconnected, or destructed recipient, or unknown capabilities,
uses plain text. An explicit depth works without a connection. When width is
automatic and the terminal is an explicit depth, width comes from `this_player()`.
Pass a depth and width to apply stored player preferences.

```c
string plain = render_markdown("# Welcome\n\nRead **help** to begin.", 60, 0);
string colored = render_markdown("# Welcome", 80, 24);
tell_object(reader, render_markdown(help, 0, reader));
```

Format separately for each recipient. Send the result without another
`terminal_colour` pass, which would interpret literal Pinkfish examples.
If formatting inside the recipient's object, `write_socket` sends the string
directly. Output still obeys the normal transaction commit/rollback behavior.

## Documents

The renderer accepts CommonMark with pipe tables and task lists enabled.

- Headings retain their level markers (`#`, `##`, etc.) and use bold cyan.
- Paragraphs wrap at word boundaries; long words wrap by Unicode grapheme
  clusters. Source line breaks become spaces; Markdown hard breaks remain.
- Ordered lists retain their starting number. Nested lists use hanging
  indentation; quotes repeat `> ` on each content line. Task markers are
  `[ ]` and `[x]`.
- Strong text is bold, emphasis italic, code cyan, and links underlined.
  Style support depends on the client. Plain text retains document structure
  and contains no escape sequences. Every output line resets its styles.
- Code blocks preserve source lines and indentation with a four-space margin.
  They do not wrap or apply syntax highlighting, so long lines can exceed
  the selected width. Inline code participates in paragraph wrapping.
- Links show `label (destination)`, with no duplicate destination when the
  label already equals it. Relative links and anchors remain as written;
  nothing is followed or fetched. Images become `[image: alternative text]`.
- HTML is displayed literally. Pinkfish tokens such as `%^RED%^` remain
  literal, including in code. Source ANSI and nonprinting controls are removed.
  The result contains no generated HTML, MXP, or terminal hyperlink controls.

Tables use aligned columns when their natural widths fit. Alignment follows
the Markdown separator row; unspecified alignment is left. When a table is
too wide, each body row becomes a block of wrapped `Header: value` fields:

```text
Name: sword
Description: A sharp blade

Name: bow
Description: Ranged
```

Empty headers use `Column N` in this layout. Header-only tables retain their
headings. Missing cells are empty and extra cells beyond the header count are
ignored, following the Markdown parser's table rules.

Tabs expand at eight-column stops within the content area; CR and CRLF
normalize to LF. Unicode uses the default ambiguous-width convention, which
can differ from a client's display. A grapheme wider than the available space
stands alone, and container prefixes may exceed very narrow widths.

Empty or whitespace-only documents return `""`. Rendered documents end with
a newline. The renderer adds no leading or trailing blank blocks, and keeps
blank lines inside code blocks. Malformed Markdown follows CommonMark's
ordinary text fallback rules.

## Errors and limits

Invalid types, widths outside 0–4096, and unsupported depths raise runtime
errors. Input, expanded text, and output each have a 1 MiB limit. Nesting is
limited to 128 tags and rendering to 1,048,576 merged parser events. Exceeding a limit raises
an error rather than returning partial output.

See also: `read_file`, `terminal_colour`, `query_connection`, `write`,
`write_socket`, `tell_object`.
