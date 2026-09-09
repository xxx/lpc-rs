# Terminal colour

## Summary

`terminal_colour` formats Pinkfish markup with up to 24-bit color and degrades
it for older clients. Themes are separate mudlib work: the driver has no theme
state, reserved theme names, theme defaults, or theme-specific efuns/hooks.

## Behavior

1. The efun is `string terminal_colour(string text, mixed colours = 1,
   int wrap = 0, int indent = 0, mixed terminal = -1)`. It returns a formatted
   string without sending output. Existing write/tell/socket operations do
   not automatically expand markup.
2. `colours` is `1` for built-ins, `0` to strip all marked tokens, a mapping of
   custom token names to replacement strings, or a function accepting a token
   name and returning a replacement string. Mappings supplement and override
   built-ins. Mapping key `0` may supply a default string or function for unknown
   tokens. A function resolver sees every marked token. Invalid return types
   are runtime errors; non-string individual mapping values are ignored.
3. Replacement strings may contain text, ANSI SGR, and built-in Pinkfish tokens.
   They receive the same depth conversion and wrapping as the original text.
   Their built-in tokens are expanded once; custom resolution is not recursive.
   Input mappings are not modified. Each callback runs once per token occurrence
   within an attempt; normal transaction retries may rerun callbacks.
4. Only enclosed `%^NAME%^` tokens are resolved. Unknown enclosed tokens are
   removed. `%%^^` emits a literal `%^`; empty tokens disappear. An unterminated
   token is preserved literally. Literal words are never looked up as keys.
5. Built-ins include BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE,
   ORANGE; their `B_` background variants; FG_0 through FG_255 and BG_0 through
   BG_255; RESET, BOLD, FLASH, ITALIC, REVERSE, UNDERLINE; and the empty
   portability tokens FG_-1, INITTERM, WINDOW, ENDTERM, STATUS. Names are case
   sensitive. No T_* names are built in.
6. RGB tokens are `#RRGGBB` and `B_#RRGGBB`, also accepting #RGB shorthand and
   either hex case. Colors degrade to the closest reference-palette color by
   squared RGB distance. Indexed fallback uses the fixed xterm cube and grayscale
   ramp (indices 16–255); basic colors use the documented reference palette.
7. `terminal` selects depth: 0 plain, 3 eight colors, 4 sixteen colors, 8 indexed
   256 colors, 24 truecolor. An object selects its connection's detected depth;
   -1 or omission selects this_player(). Unknown/unconnected targets use plain
   text. Explicit depths override detection and work without a connection.
   `colours=0` always forces plain output, even with a higher explicit depth.
8. Plain mode emits no escape sequences, including resets; ANSI controls in
   input/replacements are removed. Supported SGR colors/styles in color mode
   are normalized to the selected profile; unsupported controls are discarded.
   Formatting ends with a reset only if a supported style/color remains active.
9. Zero wrap disables wrapping/padding; otherwise absolute wrap is the terminal
   column limit, and a negative wrap also pads each line to that width. Indent
   applies only after inserted wraps, counts toward width, and must be smaller
   than the width. Existing newlines are preserved without adding indentation.
   Wrapping prefers ASCII spaces, drops spaces at a wrap, hard-wraps long words,
   and never splits a grapheme cluster or escape sequence. A single grapheme
   wider than the available line stands alone. Tabs expand to eight-column stops;
   CRLF and CR normalize to LF. Other nonprinting controls are discarded.
10. Line breaks reset active formatting before padding/indentation and
    reapply it before the next text. Grapheme widths use Unicode's default
    non-CJK ambiguous-width convention; actual terminal rendering can differ.
    Input, expanded text, and output are each limited to 1 MiB, as are width
    and indent; excessive expansion or invalid arguments raise LPC errors.
11. New connections request TTYPE without delaying login. Requests cycle until
    an answer repeats, valid MTTS arrives, or eight replies are received. Only
    solicited, negotiated IS responses containing 1–128 printable ASCII bytes
    are accepted. Disabling TTYPE clears reported capabilities.
12. `query_connection` adds client_name, terminal_type, mtts, and colour_depth.
    Unreported names are 0; unreported MTTS/depth is -1. Explicit MTTS wins over
    terminal-name hints, including an MTTS mask reporting no colors. Truecolor
    and 256-color suffixes are recognized; bare XTERM/SCREEN/TMUX select sixteen
    colors; ANSI/VT100/LINUX and MTTS ANSI alone select eight conservatively.
    DUMB selects plain. Unknown names do not imply color support.

## Compatibility

Unlike FluffOS, ordinary text segments are not token lookups and mappings are
never mutated.
Unlike LDMud, argument 0 strips markup; plain wrapping remains available through
break_string. Printable custom replacements count toward display width.

The research and local driver probes are in local/bench-drivers/colourprobe.
