# sprintf

`string sprintf(string fmt, mixed arg, ...)`

Return `fmt` with each conversion replaced by the next argument. Text
outside a conversion passes through; `%%` is one percent sign and `%^`
passes through literally for use with `terminal_colour`. Neither escape
consumes an argument. A conversion is `%`, then flags and sizes in any
order, then a type letter.

### Type letters

| Letter | Argument | Rendered as |
|---|---|---|
| `s` | string | the string |
| `d`, `i` | int | decimal |
| `b`, `B`, `o`, `x`, `X` | int | binary (`b`/`B`), octal, hex, upper-case hex |
| `c` | int | the character with that code point |
| `f`, `F` | int or float | fixed: `123.500000`, six decimals unless a precision is given; `F` capitalizes nonfinite values |
| `e`, `E` | int or float | scientific: `1.235000e+02` |
| `g`, `G` | int or float | the shorter of the two, trailing zeroes dropped: `123.5` |
| `O` | anything | as `dump` prints it; arrays and mappings one entry per line |
| `Q` | anything | debug output with quoted, escaped strings, including inside arrays and mappings |

An argument of the wrong type, a missing argument, an unknown letter, or a
`%` with no letter is an error. Extra arguments are ignored.

### Flags and sizes

| Form | Effect |
|---|---|
| `n` | the field width in terminal columns; a shorter text is padded to it |
| `.n` | the precision: a string is cut to at most `n` terminal columns, an integer gets at least `n` digits, a float gets `n` decimals (significant digits for `g`/`G`) |
| `:n` | width and precision both `n` |
| `*` | the width (or precision, after `.`) is the next int argument; a negative width aligns left, a negative precision is treated as omitted |
| `-` | left-aligned in the field (the default is right) |
| `\|` | centred, the odd space on the left |
| `0` | before the width: pad a number with zeroes behind its sign; ignored for integers with a precision |
| `'X'` | pad with the quoted text, repeated; `\'` is a quote |
| `+`, ` ` | prefix a non-negative number with `+`, or a space |
| `,` | group integer digits in threes with commas |
| `@` | apply the conversion to each element of an array |
| `$` | fully justify a string by distributing spaces between words |

Each `*` consumes an int argument in the order it occurs in the conversion,
before the value being formatted. With `:*`, one argument sets both width
and precision; a negative argument aligns left and its absolute value sets
both sizes. Later sizes replace earlier ones, but every `*` still consumes
its argument.

```c
sprintf("%7s|%-7s|%|7s", "foo", "foo", "foo")   // "    foo|foo    |  foo  "
sprintf("%05d %+d % d", -12, 3, 3)               // "-0012 +3  3"
sprintf("%'.'7s %-7'+-'s", "foo", "foo")         // "....foo foo+-+-"
sprintf("%6.3s|%*d", "foobar", 5, 42)            // "   foo|   42"
sprintf("%8.3f %12.4e %g", 123.5, 123.5, 123.5)  // " 123.500   1.2350e+02 123.5"
```

Integer precision applies to `d`, `i`, `b`, `B`, `o`, `x`, and `X`: leading zeroes
fill the minimum digit count, the sign does not count, and longer numbers
are never truncated. A zero value with zero precision emits no digits;
sign flags and field width still apply. Field padding is added around the
result, with the `0` flag ignored when a precision is present; an explicit
quoted pad string still applies.

```c
sprintf("%.5d|%.2d", 42, 1234)   // "00042|1234"
sprintf("%.5d", -42)            // "-00042"
sprintf("%08.5d", 42)           // "   00042"
sprintf("%.4x", 123)            // "007b"
sprintf("[%.0d]", 0)            // "[]"
```

### Integer grouping: `,`

Commas separate groups of three digits from the right in `d`, `i`, `b`,
`B`, `o`, `x`, and `X`. Precision adds any leading digits first, grouping
then adds commas, and field padding is applied last. The sign is outside
the groups, and commas count toward field width. Other conversions ignore
the grouping flag.

```c
sprintf("%,d", 1000000)         // "1,000,000"
sprintf("%+,d", 1234)           // "+1,234"
sprintf("%,.7d", 42)            // "0,000,042"
sprintf("%,010d", 1234)         // "000001,234"
sprintf("%,X", 0xabcdef)        // "ABC,DEF"
```

### Arrays: `@`

The value argument must be an array. Each element is formatted by the same
conversion, with width and precision shared across elements, and the results
are placed consecutively. Literal text outside the conversion is not repeated.
An empty array emits nothing, and repeating `@` does not traverse nested arrays.
Element type errors identify the argument and its zero-based element index.
Column and table elements continue side by side using the usual layout rules.

```c
sprintf("%@-4s", ({ "oak", "elm" }))           // "oak elm "
sprintf("%@*.*s", 4, 2, ({ "abcd", "xyz" }))   // "  ab  xy"
sprintf("%@:*s", 3, ({ "abcdef", "x" }))       // "abc  x"
```

### Debug output: `%O`, `%Q`, `%#O`, `%#Q`

`%Q` surrounds strings with double quotes and escapes quotes, backslashes,
and controls recursively. The familiar escapes `\a`, `\b`, `\t`, `\n`,
`\v`, `\f`, and `\r` name their controls; other byte-sized controls use
`\xNN`, and larger controls and Unicode line/paragraph separators use
`\uNNNN`. Printable Unicode is retained. Bytes retain their `b"..."` notation.
These are diagnostic representations, not a serialization format.

For `%O` and `%Q`, `#` requests compact collection output: no layout newlines,
indentation, or spaces around collection separators. `%#O` still includes
literal string contents, including any newlines within strings. `%#Q` escapes
those newlines. Field width and precision apply to the rendered representation;
precision can truncate it. Excessive nesting, including cycles, is an error.

```c
sprintf("%#O", ({ 1, 2 }))          // "({1,2})"
sprintf("%Q", "a\nb")              // "\"a\\nb\""
sprintf("%#Q", ({ "oak", "elm" })) // "({\"oak\",\"elm\"})"
```

### Colour and display width

Widths use Unicode grapheme clusters and terminal columns, with the same
default ambiguous-width convention as `terminal_colour`. Combining sequences
and emoji clusters stay intact; wide glyphs occupy two columns. Complete ANSI
SGR sequences (`ESC [` followed by digits, semicolons or colons, and `m`) have
zero width and are preserved verbatim. This applies to alignment, precision,
custom padding, column offsets, and table sizing, without a configuration flag.
Other controls and incomplete sequences are preserved as ordinary text.

String precision stops before a grapheme that would exceed the limit. SGR
sequences in the omitted suffix are retained, including any final reset.
Custom padding repeats to fill terminal columns; a remainder too narrow for
the next grapheme is filled with spaces. Empty or zero-width pad strings use
spaces.

`sprintf` does not expand colour tokens or select terminal capabilities.
Unexpanded `%^TOKEN%^` text counts toward display width. For plain arguments,
put the markup around the field and expand the formatted result:

```c
string line = terminal_colour(
    sprintf("%^RED%^%-10s%^RESET%^", "sword"), 1, 0, 0, 3);
```

When an argument itself contains markup, expand it before formatting the field:

```c
string name = terminal_colour("%^RED%^sword%^RESET%^", 1, 0, 0, 3);
string line = sprintf("|%-10s|", name);  // displays |sword     |
```

`sprintf` preserves supplied style changes without inserting resets or choosing
colours for padding. `terminal_colour` supplies resets for the strings it emits.

### Columns: `%=`

A string under `=` is word-wrapped into lines of the field width (or of the
precision, when given, each line then padded to the width), one line per
output row: the first line where the conversion sits, the rest on the rows
below it, under the same start column. Two such fields on one line make two
columns of text that continue side by side. Padding at the end of an output
row is dropped unless a `'X'` pad was given.
Long words are split only between complete graphemes; a grapheme wider than
the column is emitted alone.

```c
sprintf("%=-12s", "this is a very long sentence\n")
    // "this is a\nvery long\nsentence\n"
sprintf("%=-10s|%=-10s\n", "aa bb cc dd", "x y z")
    // "aa bb cc  |x y z\ndd\n"
```

### Justification: `$`

Within a string field, `$` normalizes runs of ASCII spaces between words,
removes leading and trailing ASCII spaces, and distributes the available
spaces evenly between words. Extra spaces go to the leftmost gaps. A single
word is left-aligned, and text wider than the field is not truncated unless
precision requests it. Without a field width, `$` has no effect.

With `=`, each paragraph's final line is left-aligned; earlier lines are fully
justified. Explicit newlines end paragraphs. Precision chooses the wrapping
width, capped at the field width. Graphemes and SGR controls stay intact.
The last of `-`, `|`, and `$` selects the string alignment, and a negative
dynamic width selects left alignment. Non-string conversions ignore `$`.

```c
sprintf("%$9s", "aa bb cc")                   // "aa  bb cc"
sprintf("%=$9s", "aa bb cc dd ee\nff gg\n")  // "aa  bb cc\ndd ee\nff gg\n"
```

### Tables: `%#`

A string under `#` is a list of words separated by newlines, laid out down
columns like `ls`: the precision is the number of columns, otherwise as
many as fit the longest word plus two into the width; each column is
`width / columns` wide, and the last column is not padded.
With `$`, each non-final column's cell is fully justified within that width.

```c
sprintf("%#-40.3s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n")
    // "one          five         nine\n"
    // "two          six          ten\n"
    // "three        seven        \n"
    // "four         eight        \n"
```

### See also

`write`, `dump`, `to_string`, `sscanf`, `implode`, `terminal_colour`
