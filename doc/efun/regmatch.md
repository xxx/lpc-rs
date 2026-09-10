# regmatch

`mixed regmatch(string text, string pattern, int flags = 0, int start = 0)`

Return the first substring matching `pattern` at or after `start`, or integer
`0` when there is no match. A successful empty match returns `""`.

With `RE_MATCH_SUBS`, return an array containing the full match, each capture
group in opening-parenthesis order, and an integer continuation index:
`({ match, group1, group2, ..., next_start })`. A group that did not participate
is `0`; a group that matched an empty string is `""`.

`start` and `next_start` are zero-based Unicode character offsets in the original
text. `start` must be between `0` and `sizeof(text)`, inclusive. Searching from an
offset preserves the original text's anchors and word boundaries. The end of the
text can match an empty pattern or an end anchor.

`next_start` is the absolute end of the match; for an empty match it is one
character past the match position. Stop iterating when it exceeds `sizeof(text)`.

### Flags

Define these constants in your mudlib's `regexp.h` and combine flags with `|`.
These flags apply to `regmatch`; `regexp` has its own flag meanings.

| Flag | Value | Effect |
| --- | --- | --- |
| `RE_CASELESS` | `0x0004` | Ignore case, including Unicode case folding. |
| `RE_MULTILINE` | `0x0008` | Let `^` and `$` match line boundaries. |
| `RE_DOTALL` | `0x0010` | Let `.` match newlines. |
| `RE_EXTENDED` | `0x0020` | Ignore pattern whitespace and `#` comments. |
| `RE_ANCHORED` | `0x0040` | Require the match to begin exactly at `start`. |
| `RE_UNGREEDY` | `0x0400` | Swap greedy and lazy repetition. |
| `RE_MATCH_SUBS` | `0x1000` | Return captures and the continuation index. |

Patterns use the same Rust `regex` dialect as [regexp](regexp.md), including inline flags.
Back-references, look-around, and selection of other regex engines are unsupported.
Invalid patterns, unsupported flag bits, wrong argument types, and out-of-range
start indices raise runtime errors. The pattern is compiled once per call.

### Examples

```c
#define RE_MATCH_SUBS 0x1000

regmatch("abcdefcdf", "cd")                       /* "cd" */
regmatch("abcdefcdf", "cd(e)", RE_MATCH_SUBS)      /* ({ "cde", "e", 5 }) */
regmatch("b", "(a)?(b)", RE_MATCH_SUBS)            /* ({ "b", 0, "b", 1 }) */
regmatch("abcdefcdf", "cd", RE_MATCH_SUBS, 4)      /* ({ "cd", 8 }) */
regmatch("café猫", "(猫)", RE_MATCH_SUBS, 4)       /* ({ "猫", "猫", 5 }) */
regmatch("abc", "z")                             /* 0 */
```

### See also

[regexp](regexp.md), `sscanf`, `wildmatch`
