# regexp

`mixed *regexp(mixed *strings, string pattern, int flag = 0)`

Return the strings in `strings` that `pattern` matches, in their order.
Elements that are not strings are ignored. `flag` is a bit set:

- Bit 2 (`2`): return the strings that do not match instead.
- Bit 1 (`1`): put each string's one-based index in `strings` before it, so
  the result is `({ index1, match1, index2, match2, ... })`.

The pattern is compiled once per call. An invalid pattern is an error.

### Dialect

Patterns are the Rust `regex` crate's: Perl-style classes (`\d`, `\w`,
`\s`, `\b`), POSIX classes (`[[:alpha:]]`), counted repeats (`a{2,5}`),
lazy repeats (`a*?`), alternation, non-capturing groups (`(?:...)`), flags
(`(?i)`), and Unicode by default. It has no back-references and no
look-around, so `\1` and `(?=...)` are errors.

CD uses a traditional regex engine. LDMud can select traditional or optional
PCRE engines; FluffOS provides traditional `regexp` and separate `pcre_*` efuns.
Patterns and option flags need review when porting; see the public sources in
the [driver comparison](../driver-comparison.md#differences-when-porting).

### Examples

```c
regexp(({ "d12", "m3", "x" }), "^d[0-9]+$")   /* ({ "d12" }) */
regexp(({ "a", "b", "ab" }), "a", 2)          /* ({ "b" }) */
regexp(({ "x", "ay", "az" }), "a", 1)         /* ({ 2, "ay", 3, "az" }) */
```

### See also

[regmatch](regmatch.md), `filter`, `explode`, `sscanf`
