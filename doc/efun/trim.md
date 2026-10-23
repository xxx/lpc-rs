# trim

```c
string trim(string text)
string trim(string text, int where)
string trim(string text, int where, string | int chars)
```

Return `text` with matching characters removed from the selected ends. The
original string and all characters between those ends remain unchanged.

| `where` | Meaning |
| --- | --- |
| `0` (default) | Both ends |
| `1` | Left end |
| `2` | Right end |

Other values, including `3`, raise an error. The shipped ulib header
[`<strings.h>`](../../ulib/include/strings.h) defines `TRIM_BOTH`, `TRIM_LEFT`
and `TRIM_RIGHT` with these values.

Without `chars`, trim space, tab, LF, CR, vertical tab and form feed.
Other Unicode whitespace, such as nonbreaking and ideographic spaces, remains.

A string `chars` supplies a set of Unicode scalar values, replacing the default
set. Order and duplicates do not matter; an empty set removes nothing. An
integer supplies one Unicode scalar, so character literals work too. Negative
integers, surrogates and values above `0x10ffff` raise errors. Matching does
not normalize text or group combining marks into graphemes.

Embedded NUL is ordinary data and is preserved by default. Pass integer zero
or a string containing NUL to trim it from the selected ends.

```c
#include <strings.h>

trim("  hello\r\n")                   // "hello"
trim("  hello  ", TRIM_LEFT)          // "hello  "
trim("  hello  ", TRIM_RIGHT)         // "  hello"
trim("..hello__", TRIM_BOTH, "._")   // "hello"
trim("--hello--", TRIM_LEFT, '-')     // "hello--"
trim("  hello  ", TRIM_BOTH, "")     // "  hello  "
trim("　hello　", TRIM_BOTH, "　")     // "hello"

string *lines = ({ "  first  ", "  second\n" });
map(lines, trim, TRIM_RIGHT)          // ({ "  first", "  second" })
```

Empty input and all-matching input produce an empty string. Supplied arguments
are validated even when the input is empty or no characters would be removed.
Wrong types raise errors; distinct nil is not an omitted argument.

A changed result exceeding **8,192 UTF-8 bytes** raises an error before
allocating the result. Unchanged strings are returned as-is, including existing
strings above that limit. A larger existing string can be reduced to fit.

See also: [strindex](strindex.md), [replace_string](replace_string.md),
[map](map.md).
