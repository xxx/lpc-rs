# Markdown documents

## Summary

Mudlibs can render Markdown help, books, and announcements for terminal clients
using a string-returning efun. Authors can keep the same ordinary Markdown
sources for terminal help and web documentation.

## Behavior

1. `string render_markdown(string source, int width = 0, mixed terminal = -1)`
   returns formatted text without reading files, sending output, or paging.
   Existing output efuns retain their behavior.
2. `terminal` follows `terminal_colour`: an object selects its connection,
   `-1` or omission selects `this_player()`, and `0`, `3`, `4`, `8`, or `24`
   explicitly select plain text or a color depth. Unknown capabilities and
   disconnected or destructed recipients use plain text. Overrides allow
   formatting without a connection.
3. Width is measured in terminal columns. Zero or omission uses the selected
   recipient's reported width, falling back to 80 when unavailable. An explicit
   color depth uses `this_player()` for automatic width. Explicit widths from
   1 through 4096 are accepted; reported widths are capped at 4096. Other widths,
   depths, and argument types raise runtime errors.
4. CommonMark paragraphs, headings, emphasis, strong emphasis, nested ordered
   and unordered lists, quotes, code, links, images, and thematic breaks render
   as terminal text. Pipe tables and task lists are enabled extensions.
   Other extensions are treated according to ordinary CommonMark syntax.
5. Paragraph source line breaks become spaces; Markdown hard breaks stay line
   breaks. Headings retain their `#` level markers and use bold cyan styling.
   Lists use `- ` or incrementing numbers and aligned continuation lines; quotes
   prefix each content line with `> `. Task checkboxes use `[ ]` or `[x]`.
   Blank lines separate paragraphs and blocks; tight list items remain adjacent.
6. Strong and emphasis use bold and italic; code uses cyan and links underline.
   Plain output contains no escape sequences and retains document structure.
   Nested styles restore their enclosing style; each rendered line resets its
   styles so output and paging cannot leak styles into later messages.
7. Wrapping uses Unicode grapheme clusters and display columns. Long words wrap
   without splitting a cluster; a cluster wider than the available space stands
   alone. Container prefixes can exceed very narrow widths. Indentation and
   prefixes count toward available width. Tabs expand at eight-column stops
   within the content area. CRLF and CR normalize to LF.
8. Fenced and indented code blocks preserve indentation, spaces, and source
   line breaks and receive a four-space margin. They do not wrap or highlight
   syntax; long source lines may exceed width. Inline code follows CommonMark
   whitespace normalization and participates in paragraph wrapping.
9. Links display `label (destination)`, omitting the repeated destination when
   it equals the label. Relative paths and anchors remain visible as written;
   the renderer does not resolve or follow them. Images display `[image: alt]`.
   Raw HTML is displayed literally, including tags; it is never sent as MXP.
   Pinkfish tokens remain literal in all document text, including code examples.
   Source ANSI and other nonprinting controls are discarded.
10. Tables use ` | ` separators, a dashed header separator, and the declared
    left, center, or right alignment (default left). Columns use their natural
    display widths when the entire table fits. Otherwise each body row becomes
    a block of wrapped `Header: value` fields; an empty header uses `Column N`.
    Header-only tables retain their headings even when narrow. Empty cells stay
    empty. Ragged rows follow the parser's table rules: missing cells are empty
    and cells beyond the header count are ignored.
11. Empty or whitespace-only Markdown returns an empty string. Nonempty rendered
    documents end in a newline, with no added leading or trailing blank blocks;
    blank lines inside code blocks remain intact. Malformed Markdown is rendered
    according to CommonMark rather than rejected as a syntax error.
12. Source, expanded text, and final output are each bounded at 1 MiB. Nesting
    beyond 128 containers/inline tags and more than 1,048,576 merged parser events raise
    runtime errors. Limits fail the call rather than returning truncated text.
    Formatting has no external effects; output sent afterwards remains subject
    to the normal transaction commit/rollback behavior.
