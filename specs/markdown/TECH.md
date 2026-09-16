# Markdown rendering implementation

## Context

At `db67f7cd81bf6552ce31412b7beb37a404a18fdb`,
`src/interpreter/efun/terminal_colour/` owns concrete styling and Unicode layout.
`query_connection` reads the recipient's transactional connection binding and
its capability snapshot. Output already travels through deferred STM effects.
The consumer interface is defined in [PRODUCT.md](PRODUCT.md).

## Proposed changes

- Append `render_markdown` to the efun registry to preserve existing instruction
  indices. Its adapter validates arguments and snapshots width/depth once.
- Use `pulldown-cmark` with default features disabled and table/task-list options.
  Merge adjacent text events before filtering decoded controls, and consume
  events iteratively with bounded nesting and expansion. Keep document
  layout in the Markdown module; buffer only the current text block or table.
- Extract the existing color and text implementation into the private shared
  `terminal_text` module, retaining `terminal_colour` semantics and tests.
  Add explicit style assignment and literal unstyled input for Markdown so
  author text never becomes Pinkfish or ANSI instructions. Reuse its grapheme
  wrapping, color conversion, control filtering, and line-end resets.
- Render blocks with container prefixes and hanging list indentation. Buffer
  table cells as styled text, measure their plain display widths, and choose
  natural-width columns or labelled fields for the available width. Preserve
  code whitespace using unwrapped layout.
- Keep source loading, permissions, help discovery, themes, paging, and output
  in the mudlib. The efun returns an LPC string and introduces no socket path.

## Testing and validation

- Renderer tests exercise headings, nested styles and lists, quotes, soft/hard
  breaks, literal code/HTML/Pinkfish, links, images, tables at wide/narrow widths,
  Unicode, malformed/empty input, and limits (PRODUCT 4–12).
- Interpreter tests cover defaults, explicit widths/depths, detected recipient
  capabilities, function-pointer calls, invalid arguments, and commit/rollback
  of sent output (PRODUCT 1–3, 12).
- Render representative existing efun documentation as a real-document check.
- Preserve all terminal-colour regression tests after extracting shared code.
- Run workspace build/tests, clippy with all targets, formatting, and
  `RUSTDOCFLAGS="-D warnings" cargo doc --workspace`.
