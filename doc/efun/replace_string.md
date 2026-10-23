# replace_string

```c
string replace_string(string text, string pattern, string replacement)
string replace_string(string text, string pattern, string replacement, int max)
string replace_string(string text, string pattern, string replacement, int first, int last)
```

Return `text` with selected occurrences of `pattern` replaced by `replacement`.
The original string is unchanged. Matching is literal and case-sensitive;
replacement text has no special escape or capture syntax.

Matches are counted from left to right, starting at one, and do not overlap.
Inserted text is not searched again. An empty replacement deletes matches;
an empty pattern leaves the text unchanged. Embedded NULs are ordinary data.
Unicode sequences match exactly, without normalization or grapheme grouping.

With three arguments, replace all matches. With four, replace at most `max`
matches: `0` means all, and a negative maximum makes no replacements.

With five arguments, replace occurrences `first` through `last`, inclusive:

- A `first` less than one starts with the first match.
- A `last` of zero includes all remaining matches.
- A negative `last`, a positive `last` before `first`, or a `first` beyond the
  available matches makes no replacements.
- A `last` beyond the available matches includes all remaining matches.

These bounds are occurrence numbers, not character positions.

```c
replace_string("aaa", "a", "x")        // "xxx"
replace_string("aaa", "a", "x", 2)     // "xxa"
replace_string("aaa", "a", "x", 2, 2)  // "axa"
replace_string("aaa", "a", "x", 2, 0)  // "axx"
replace_string("a-b-c", "-", "")       // "abc"
replace_string("aaaaa", "aa", "x")     // "xxa"
replace_string("a", "a", "aa")         // "aa"
replace_string("a.b", ".", "$1")       // "a$1b"
```

Wrong argument types raise errors, even when no matches would be replaced.
A changed result exceeding the native string limit of **8,192 UTF-8 bytes**
raises an error before allocating the result. Unchanged strings are returned
as-is, including existing strings above that limit. A larger existing string
can be reduced to fit within the limit.

See also: [strindex](strindex.md), [explode](explode.md), [implode](implode.md).
