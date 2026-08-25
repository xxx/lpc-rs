# sscanf

`int sscanf(string str, string fmt, mixed var...)`

Scans `str` against `fmt`, assigning each conversion to the next `var` in
turn. The variables are passed by reference implicitly — write `n`, not
`ref n` — and must be variables. Returns the number of conversions matched.

| in `fmt` | matches | assigns |
|---|---|---|
| literal text | itself, exactly (case-sensitive) | — |
| `%d` | an integer: optional leading whitespace, optional sign, digits | int |
| `%x` | a hexadecimal integer, `0x` prefix optional | int |
| `%f` | a float, as `4.43e-2` | float |
| `%s` | see below | string |
| `%%` | a literal `%` | — |
| `%*d`, `%*x`, `%*f`, `%*s` | as the conversion, assigning nothing | — |

`%s` at the end of `fmt` takes the rest of `str`. Before literal text it
takes the shortest prefix after which the literal matches, so `"%s %s"`
splits at the first space: the first word, then the remainder. Before `%d`
it stops at the first digit; before `%f` at the first digit or `.`digit;
before `%x` at the first `0x` followed by a hex digit (without that prefix
the `%s` takes the rest and the `%x` fails); before `%%` at the first `%`.
Two `%s` with nothing between them are an error.

Matching stops at the first conversion or literal that fails; the count so
far is returned, variables already assigned keep their new values, and the
rest keep their old ones. Skips count as matches. If `fmt` is exhausted with
input left over and an unused variable remains, the leftover is assigned to
it and counts. A `%d` too large for an int does not match.

Runtime errors: two adjacent `%s`; `fmt` ending in `%`; an unknown
conversion letter; fewer variables than non-skip conversions.

Unlike other drivers, `%(regex)` is not supported.

### Examples

```c
int n; string what;
if (sscanf(arg, "%d %s", n, what) == 2) { ... }

string who, msg;
sscanf("tell bob hello there", "tell %s %s", who, msg);
// who == "bob", msg == "hello there"
```

### See also

`explode`, `implode`, [references](../lpc/references.md)
