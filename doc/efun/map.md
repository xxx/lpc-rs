# map

`mixed *map(mixed *arr, function f, mixed extra...)`

`mapping map(mapping m, function f, mixed extra...)`

`string map(string text, function f, mixed extra...)`

Return a new collection with each entry of the first argument replaced by
what `f` returns for it. For an array, `f` is called as
`f(item, extra...)` for each item in order, and the results form the new
array. For a mapping, `f` is called as `f(key, value, extra...)` and the
new mapping has the same keys, in the same order, each with `f`'s result as
its value.

For a string, `f` is called as `f(codepoint, extra...)` for each Unicode scalar
value in order, matching string [indexing](../lpc/indexing.md) and
[`foreach`](../lpc/foreach.md). Each result must be an integer scalar value;
the results form a new string. Combining marks are separate scalars, and no
normalization is performed. A scalar can change its UTF-8 byte width.

Zero produces a NUL character, and embedded NULs in the input are visited like
any other scalar. To preserve a character, return its original code point.
Strings, floats and other non-integer results raise errors, as do negative
integers, surrogates, and values above `0x10ffff`. Each result is validated before
the next callback runs. Results larger than 8,192 UTF-8 bytes raise an error;
the caller receives no partial string.

The [null mode](../lpc/null.md) also applies to callback results. A void callback
returns zero in zero mode, producing NUL, or nil in distinct mode, which raises
an error. For character callbacks used only for side effects, use `foreach`.

The original is not changed. `f` runs as a call from the object calling
`map`, so `previous_object()` inside it is that object and `origin()` is `"efun"`.
An error it throws propagates to `map`'s caller. `f` runs on the calling task's
own call stack, so nesting is bounded by the call stack rather than the task chain. A
first argument that is not an array, mapping or string, or an `f` that is
not a function, is an error. Empty inputs return an empty value of the same kind
without calling `f`, but still require a function argument.

With a dynamic pointer such as `&->query_name()`, each array item is the
receiver. A missing method contributes the configured null value to the result,
just as with `(: $1->query_name() :)`.

### Examples

```c
int *tens = map(({ 1, 2 }), (: $1 * 10 :));                 // ({ 10, 20 })
string *names = map(all_inventory(room), (: $1->query_name() :));
mapping doubled = map(([ "a": 1 ]), (: $2 * 2 :));           // ([ "a": 2 ])
string shifted = map("abc", (: $1 + 1 :));                // "bcd"
string unchanged = map("é猫🐉", (: $1 :));                  // "é猫🐉"
string zeros = map("ab", (: 0 :));                       // two NUL characters
```

### See also

`filter`, `reduce`, `sort_array`
