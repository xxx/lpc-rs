# Indexing and slices

Arrays, strings and bytes support zero-based indices, negative indices, and
explicit reverse indices using `<`. String positions count Unicode scalar
values; bytes positions count individual bytes. Mappings use keys and do not
support reverse indices or slices; a negative mapping key is just a key.

## Element reads

`value[i]` counts from the beginning when `i` is zero or positive. A negative
index counts from the end: `value[-1]` is the last element, `value[-2]` is the
second-to-last, and so on.

`value[<n]` explicitly selects the position `length - n`. For a positive
distance within the collection, **`value[-n]` and `value[<n]` select the same
element**, for both reads and array writes. You can use negative indices for
ordinary access from the end; `<` is another supported spelling.

For `items = ({ 10, 20, 30 })`:

| From the beginning | Negative index | Explicit reverse index | Value |
|---|---|---|---|
| `items[0]` | `items[-3]` | `items[<3]` | `10` |
| `items[1]` | `items[-2]` | `items[<2]` | `20` |
| `items[2]` | `items[-1]` | `items[<1]` | `30` |

Both forms accept integer expressions:

```c
int *items = ({ 10, 20, 30 });
int distance = 2;
int middle = items[-distance];        // 20
int same_middle = items[<distance];   // 20
int first = items[-(distance + 1)];   // 10
int same_first = items[<distance + 1]; // 10
```

`<` is a marker at the start of a subscript bound, not a general unary operator.
Comparisons inside bounds retain their ordinary meaning.

### Zero and negative distances

The two forms differ when the distance is zero or negative. Ordinary indexing
counts from the end only when the resulting index is negative. The `<` form
always subtracts its distance from the length:

| Distance `n`, with the three-element `items` above | `items[-n]` | `items[<n]` |
|---|---|---|
| `0` | `items[0]`, the first element (`10`) | Position `3`, one past the end: error |
| `-1` | `items[1]`, the second element (`20`) | Position `4`, past the end: error |
| `4` | Before the beginning: error | Before the beginning: error |

In particular, `-0` is just `0`; it cannot express the position selected by
`<0`. A negative distance after `<` points further past the end and never
wraps around to the beginning.

Out-of-bounds array reads raise an error; out-of-bounds string and bytes reads
return `0`. These rules also apply to empty values. Reverse distances must be
integers, including when the receiver is empty.

## Slices

Slice endpoints are inclusive. Either endpoint can independently use a
zero-based index, a negative index, or an explicit reverse index. For in-range
positive distances, the negative and `<` forms select the same slice:

| Negative bounds, with `text = "abcd"` | Explicit reverse bounds | Result |
|---|---|---|
| `text[1..-2]` | `text[1..<2]` | `"bc"` |
| `text[-3..2]` | `text[<3..2]` | `"bc"` |
| `text[-3..-2]` | `text[<3..<2]` | `"bc"` |
| `text[-2..]` | `text[<2..]` | `"cd"` |
| `text[..-2]` | `text[..<2]` | `"abc"` |

Omitting the start means zero; omitting the end means the last element. The
forms can be mixed: `text[-2..<1]` and `text[<2..-1]` both return `"cd"`.
Whitespace around the marker is allowed: `text[0.. < 2]` returns `"abc"`.

Slices preserve the collection type. Array slices create a new array containing
the selected references. An empty receiver produces an empty slice, but invalid
bound types in reverse slices still raise errors.

### Slice boundaries and clipping

Unlike individual element access, a slice can extend beyond the collection.
Explicit reverse bounds are resolved once as `length - distance`. A start
before the beginning clips to zero; an end past the last element clips to the
last element. An end before the beginning, a start past the last element, or
a start after the end produces an empty slice.

Ordinary negative bounds have different existing behavior when they reach
before the beginning: such a start produces an empty slice, while such an end
clips to the last element. They keep that behavior even when the other endpoint
uses `<`.

| Ordinary bounds, with `text = "abcd"` | Result | Explicit reverse bounds | Result |
|---|---|---|---|
| `text[-99..]` | `""` | `text[<99..]` | `"abcd"` |
| `text[..-99]` | `"abcd"` | `text[..<99]` | `""` |
| `text[-0..]` | `"abcd"` | `text[<0..]` | `""` |
| `text[..-0]` | `"a"` | `text[..<0]` | `"abcd"` |

The same rules apply to arrays and bytes. For example, `items[-99..]` returns
`({})`, while `items[<99..]` returns `({ 10, 20, 30 })`. Use explicit reverse
bounds when a distance may extend before the beginning and you want clipping
at that boundary.

## Writes

Zero-based, negative, and explicit reverse indices all support array element
assignment, compound assignment, `++`, `--`, and implicit efun output arguments
such as `sscanf`:

```c
items[-1] = 40;                 // same element as items[<1]
items[-2] += 5;                 // same element as items[<2]
items[<1]++;
sscanf("42", "%d", items[-1]); // items[<1] also works here
```

Out-of-bounds writes raise an error and do not change an element. Writes use
transactional array storage with shared array references. Indexed string and
bytes writes are unsupported. A slice is a value, not an assignable location;
slice assignment, stepping a slice, and using a slice as an efun output argument
are rejected.
