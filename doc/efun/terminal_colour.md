# terminal_colour

```c
string terminal_colour(string text, mixed colours = 1,
                       int wrap = 0, int indent = 0,
                       mixed terminal = -1)
```

Convert `%^TOKEN%^` markup to terminal colors and styles, optionally wrapping
the result. This efun returns a string; it does not send output. `write`,
`tell_object`, and `write_socket` do not expand markup automatically.

## Selecting the terminal

`terminal` is an object whose connection supplies the detected color depth,
or one of these integers:

| Value | Output |
|---|---|
| `-1` (default) | Detect from `this_player()` |
| `0` | Plain text |
| `3` | Eight ANSI colors |
| `4` | Sixteen ANSI colors |
| `8` | Xterm 256 colors |
| `24` | 24-bit RGB |

Unknown capabilities, disconnected objects, and a missing `this_player()` use
plain text. An explicit depth works without a connection and overrides client
detection. The mudlib can implement user preferences by selecting a depth.
Each recipient should format separately when clients have different capabilities:

```c
void catch_tell(string text) {
    write_socket(terminal_colour(text, 1, 0, 0, this_object()));
}
```

## Tokens

| Tokens | Meaning |
|---|---|
| `BLACK`, `RED`, `GREEN`, `YELLOW`, `BLUE`, `MAGENTA`, `CYAN`, `WHITE` | Palette indices 0–7 |
| `ORANGE` | Palette index 208 |
| `B_` followed by a named color | Background variant |
| `FG_0` through `FG_255` | Indexed foreground |
| `BG_0` through `BG_255` | Indexed background |
| `#RRGGBB`, `#RGB` | RGB foreground; upper- or lowercase hex |
| `B_#RRGGBB`, `B_#RGB` | RGB background |
| `BOLD`, `FLASH`, `ITALIC`, `REVERSE`, `UNDERLINE` | Text styles |
| `RESET` | Reset colors and styles |
| `FG_-1`, `INITTERM`, `WINDOW`, `ENDTERM`, `STATUS` | Empty portability tokens |

Names are case sensitive. Adjacent tokens each need their own delimiters:
`%^BOLD%^%^RED%^`. `%%^^` emits a literal `%^`. Unknown enclosed tokens and
empty tokens disappear. An unfinished `%^TOKEN` remains literal text.

```c
string text = "%^#ff8800%^Orange foreground%^RESET%^";
string rgb = terminal_colour(text, 1, 0, 0, 24);
string indexed = terminal_colour(text, 1, 0, 0, 8);
string plain = terminal_colour(text, 0);
```

RGB output uses semicolon-form SGR (`38;2;r;g;b` / `48;2;r;g;b`). Indexed colors
remain indexed on 256-color and truecolor clients. RGB fallback chooses the
nearest xterm cube/grayscale entry, indices 16–255, by squared RGB distance;
ties choose the lower index. Basic-palette fallback uses the same distance
against this reference palette:

| Indices | RGB hex values in order |
|---|---|
| 0–7 | `000000`, `800000`, `008000`, `808000`, `000080`, `800080`, `008080`, `c0c0c0` |
| 8–15 | `808080`, `ff0000`, `00ff00`, `ffff00`, `0000ff`, `ff00ff`, `00ffff`, `ffffff` |

Clients can customize their palettes, so approximations may look different.
Eight colors use SGR 30–37/40–47; sixteen additionally use 90–97/100–107.
Styles depend on client support independently of color depth.

## Custom replacements

`colours` accepts:

- `1` (default): built-in tokens.
- `0`: strip every enclosed token and all ANSI controls, regardless of `terminal`.
- A mapping: string-valued entries override or supplement built-ins. Other
  individual values are ignored. Key `0` may supply a default string or function
  for unknown tokens; a zero default means no default.
- A function: called with each nonempty marked token name, including built-ins,
  and required to return a string.

Replacements may contain ordinary text, supported ANSI SGR, and built-in markup:

```c
mapping codes = ([ "NOTICE": "%^BOLD%^%^#f80%^" ]);
string out = terminal_colour("%^NOTICE%^Hello%^RESET%^", codes, 80, 2, 8);
```

Built-in markup in replacements expands once. Custom mappings and callbacks are
not recursively reapplied. Each callback runs once per token occurrence within
an attempt; transaction retries may run it again. Errors propagate normally,
and mappings are not modified by formatting.

To retain printable custom replacements while disabling color, pass the mapping
and select `terminal=0`. Passing `colours=0` instead removes the entire token.
The driver assigns no meaning to semantic names or themes; mudlib simul-efuns
can implement those separately using these generic facilities.

## Wrapping and controls

`wrap=0` disables wrapping and padding. Otherwise `abs(wrap)` is the line width
in terminal columns; a negative value also pads each line to that width. Indent
adds spaces after inserted wraps and counts toward the width. It does not indent
the first line or lines beginning at an explicit newline.

Wrapping prefers ASCII spaces/tabs, drops whitespace at inserted breaks, and
hard-wraps long words. Unicode grapheme clusters and complete escape sequences
are never split; a cluster wider than the available line stands alone. Widths
use Unicode's default ambiguous-width convention, which can differ from an
individual terminal. Printable replacement text counts toward the width.

Tabs advance to the next eight-column stop. CRLF and CR normalize to LF. Color
and style state resets at line ends, before padding, and is reapplied for the
next visible text. A trailing active style resets at the end of the result;
redundant or unused style changes may be omitted.

Raw semicolon-form SGR colors and the styles listed above are recognized,
including their reset forms. Other escape sequences and nonprinting controls
are discarded. Plain mode emits no escape sequences, including resets.
Control sequences must be complete within a literal text segment or replacement.

Invalid argument types, unknown depths, negative indentation, indentation at
least as wide as a nonzero wrap, and widths/indentation exceeding 1 MiB are runtime
errors. Input, expanded text, and output are each limited to 1 MiB; overflow
raises an error rather than returning truncated text.

## Compatibility

`sprintf` preserves `%^` in its format string and measures expanded ANSI colours
by visible width. Format plain arguments before expanding surrounding markup;
expand markup inside an argument before passing it to a width-sensitive field.
See `sprintf` for examples and precision behavior.

Unlike FluffOS, ordinary words are never looked up as tokens and input mappings are
not modified. Unlike LDMud, `0` strips tokens rather than selecting pure wrapping.
`break_string` remains available for ordinary text wrapping.

See also: `query_connection`, `break_string`, `sprintf`, `write_socket`.
