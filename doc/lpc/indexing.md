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
bytes writes are unsupported.

### Array slice assignment

`items[start..end] = replacement` replaces the inclusive range with the elements
of another array. The replacement may be shorter, longer, or empty. Both operands
must be arrays; a `mixed` value is checked at runtime. Known array types follow
the usual assignment checks.

Every array slice assignment constructs a **new shallow array** and assigns it back
to the target, including equal-length replacements and empty operations. Other
references to the old outer array retain its contents. Nested arrays, mappings,
objects, and class instances remain shared.

```c
int *items = ({ 10, 20, 30 });
int *old = items;
items[1..1] = ({ 40, 50 }); // items is ({ 10, 40, 50, 30 })
                         // old is still ({ 10, 20, 30 })
items[0..0] = ({});        // delete the first element
items[<1..] = ({ 60 });    // replace the last element
```

### String slice assignment

`text[start..end] = replacement` replaces an inclusive range of Unicode scalars
with another string. Replacement constructs a new string and rebinds its owner;
other references to the old string keep their contents. The replacement may be
shorter, longer, or empty, and both operands must be strings. A `mixed` value is
checked at runtime; arrays and strings cannot be mixed in one slice assignment.

```c
string text = "aé😀z";
string old = text;
text[1..2] = "中";             // text is "a中z"; old is still "aé😀z"
text[<1..] = "!";             // replace the last scalar
text[sizeof(text)..] = "?";   // append
```

Positions follow string reads: a combining mark occupies its own position, and
a joined emoji may occupy several positions. Embedded NULs are preserved in the
source, replacement and result. Replacement never splits a UTF-8 encoding.

The result must fit within **8,192 UTF-8 bytes**, regardless of its scalar count.
An oversized result raises an error before writeback. A longer input may be
shortened into the limit; an edit whose result remains oversized, including an
empty insertion, raises an error. Strings are not converted from integers,
arrays, bytes, zero or nil.

### Slice owners and results

The owner must be an assignable variable, class member, or indexed slot:
`history[0..0] = ({})`, `state.history[0..0] = ({})`, and
`records["history"][0..0] = ({})` are supported. These owner rules apply to arrays
and strings. A slice of a temporary value,
such as `make_array()[0..0] = ({})` or `make_text()[0..0] = "x"`, has no owner
to rebind and is rejected. The assignment expression returns the RHS value,
not the complete new array or string.

### Write bounds

Slice writes validate their bounds; they do not clip like slice reads. Resolve
negative indices and `<` distances using the original value's length, then
require `0 <= start <= end + 1 <= length`. The end remains inclusive. Omitting
the start means zero; omitting the end means `length - 1`.

When `start == end + 1`, the range is empty and the replacement is inserted at
`start`. Larger reversed ranges and endpoints outside the permitted interval
raise errors. Bounds must be integers, even when the array or string is empty.
String lengths and endpoints count Unicode scalars.

```c
int *items = ({ 10, 30 });
items[1..0] = ({ 20 });                  // insert between 10 and 30
items[sizeof(items)..] = ({ 40 });       // append; items[<0..] also works
items[0..<sizeof(items) + 1] = ({ 5 });   // insert before the first element
int *empty = ({});
empty[0..] = ({ 10 });                   // insert into an empty array
```

`items[0..-1]` replaces the whole array: ordinary `-1` denotes the last element.
It does not describe an empty prefix. An explicit reverse end of
`<sizeof(items) + 1` resolves to the position just before the first element.
An explicit reverse end of `<0` is past the last element and is invalid for a
write, even though a read slice clips it.

### Evaluation and cost

Slice assignment evaluates the RHS first, captures the owner and its value,
then evaluates the start and end from left to right. Each expression runs once.
Receiver, index, and RHS values are retained if a later expression reassigns the
variables that supplied them. Array contents are read after operand evaluation,
so element mutations performed by those expressions are visible to the replacement.
Captured strings are immutable and retain their original contents.

The new value is written to the captured owner. Reassigning the owner's variable
during bound evaluation does not change which original value supplies the prefix
and suffix; the final write replaces that variable's value. For indexed slots and
class members, writeback uses the captured container or instance.

Invalid operands or bounds leave the target unchanged by the slice operation;
side effects from evaluating its operands still follow normal `catch` behavior.
Publication and retries use the ordinary transaction rules. Constructing the
array takes linear time and space in its length. String replacement scans the
source to resolve positions and constructs the result, taking time linear in
source plus result bytes and additional space linear in result bytes. Repeated
front deletion or repeated single-position replacements can take quadratic time;
use `map` for string transformations that can build the result once.

Only plain array and string slice `=` is supported. Compound slice assignments,
`++`, `--`, slice output arguments, indexed string writes, and bytes writes are
unsupported.
