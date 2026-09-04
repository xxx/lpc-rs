# sprintf

`string sprintf(string fmt, mixed arg, ...)`

Return `fmt` with each conversion replaced by the next argument. Text
outside a conversion passes through; `%%` is one percent sign. A
conversion is `%`, then flags and sizes in any order, then a type letter.

### Type letters

| Letter | Argument | Rendered as |
|---|---|---|
| `s` | string | the string |
| `d`, `i` | int | decimal |
| `b`, `o`, `x`, `X` | int | binary, octal, hex, upper-case hex |
| `c` | int | the character with that code point |
| `f` | int or float | fixed: `123.500000`, six decimals unless a precision is given |
| `e`, `E` | int or float | scientific: `1.235000e+02` |
| `g`, `G` | int or float | the shorter of the two, trailing zeroes dropped: `123.5` |
| `O` | anything | as `dump` prints it; arrays and mappings one entry per line |

An argument of the wrong type, a missing argument, an unknown letter, or a
`%` with no letter is an error. Extra arguments are ignored.

### Flags and sizes

| Form | Effect |
|---|---|
| `n` | the field width; a shorter text is padded to it |
| `.n` | the precision: a string is cut to `n` characters, a float gets `n` decimals |
| `:n` | width and precision both `n` |
| `*` | the width (or precision, after `.`) is the next int argument; a negative width aligns left |
| `-` | left-aligned in the field (the default is right) |
| `\|` | centred, the odd space on the left |
| `0` | before the width: pad a number with zeroes behind its sign |
| `'X'` | pad with the quoted text, repeated; `\'` is a quote |
| `+`, ` ` | prefix a non-negative number with `+`, or a space |

```c
sprintf("%7s|%-7s|%|7s", "foo", "foo", "foo")   // "    foo|foo    |  foo  "
sprintf("%05d %+d % d", -12, 3, 3)               // "-0012 +3  3"
sprintf("%'.'7s %-7'+-'s", "foo", "foo")         // "....foo foo+-+-"
sprintf("%6.3s|%*d", "foobar", 5, 42)            // "   foo|   42"
sprintf("%8.3f %12.4e %g", 123.5, 123.5, 123.5)  // " 123.500   1.2350e+02 123.5"
```

### Columns: `%=`

A string under `=` is word-wrapped into lines of the field width (or of the
precision, when given, each line then padded to the width), one line per
output row: the first line where the conversion sits, the rest on the rows
below it, under the same start column. Two such fields on one line make two
columns of text that continue side by side. Padding at the end of an output
row is dropped unless a `'X'` pad was given.

```c
sprintf("%=-12s", "this is a very long sentence\n")
    // "this is a\nvery long\nsentence\n"
sprintf("%=-10s|%=-10s\n", "aa bb cc dd", "x y z")
    // "aa bb cc  |x y z\ndd\n"
```

### Tables: `%#`

A string under `#` is a list of words separated by newlines, laid out down
columns like `ls`: the precision is the number of columns, otherwise as
many as fit the longest word plus two into the width; each column is
`width / columns` wide, and the last column is not padded.

```c
sprintf("%#-40.3s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n")
    // "one          five         nine\n"
    // "two          six          ten\n"
    // "three        seven        \n"
    // "four         eight        \n"
```

### See also

`write`, `dump`, `to_string`, `sscanf`, `implode`
