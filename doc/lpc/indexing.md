# Indexing and slices

Arrays, strings and bytes support ordinary and reverse indices. String positions
count Unicode scalar values; bytes positions count individual bytes. Mappings
use keys and do not support reverse indices or slices.

## Element reads

`value[i]` starts at zero. A negative ordinary index counts from the end.
`value[<n]` explicitly selects the position `length - n`:

```c
int *items = ({ 10, 20, 30 });
int last = items[<1];       // 30
int first = items[<3];      // 10
int same_last = items[-1];  // 30
int distance = 2;
int middle = items[<distance]; // 20
```

The distance can be an integer expression, including `items[<distance + 1]`.
`<` is a marker at the start of a subscript bound, not a general unary operator.
Comparisons inside bounds retain their ordinary meaning.

`<0` is one past the end. Negative distances point further past the end; they
do not wrap around to the start. A distance greater than the length points
before the start. Out-of-bounds array reads raise an error; out-of-bounds string
and bytes reads return `0`. These rules also apply to empty values. Distances
must be integers, including when the receiver is empty.

## Slices

Slice endpoints are inclusive and can independently use reverse indexing:

| Expression with `text = "abcd"` | Result |
|---|---|
| `text[1..<2]` | `"bc"` |
| `text[<3..2]` | `"bc"` |
| `text[<3..<2]` | `"bc"` |
| `text[<2..]` | `"cd"` |
| `text[..<2]` | `"abc"` |
| `text[0.. < 2]` | `"abc"` |

Omitting the start means zero; omitting the end means the last element.
Reverse endpoints are resolved once as `length - distance`, then clipped to
the collection's bounds. A start before the beginning clips to zero; an end
past the last element clips to the last element. An end before the beginning,
a start past the last element, or a start after the end gives an empty result.
Thus `text[<99..<1]` and `text[..<0]` both return the whole string, while
`text[..<99]` and `text[<0..]` return `""`.

Slices preserve the collection type. Array slices create a new array containing
the selected references. An empty receiver produces an empty slice, but invalid
bound types in reverse slices still raise errors.

Ordinary negative bounds keep their existing behavior even alongside a reverse
bound: `text[-2..<1]` returns `"cd"`. Ordinary bounds are not reinterpreted using
another driver's range mode. In particular, the existing handling of a negative
bound before the beginning remains unchanged: `text[-99..]` is empty and
`text[0..-99]` is the whole string. Use explicit reverse bounds for clipping
without that legacy behavior.

## Writes

Reverse indices support array element assignment, compound assignment, `++`,
`--`, and implicit efun output arguments such as `sscanf`:

```c
items[<1] = 40;
items[<2] += 5;
items[<1]++;
sscanf("42", "%d", items[<1]);
```

Out-of-bounds writes raise an error and do not change an element. Writes use
the same transactional array storage as ordinary indexing, including shared
array references. Indexed string and bytes writes are unsupported. A slice is
a value, not an assignable location; slice assignment, stepping a slice, and
using a slice as an efun output argument are rejected.
