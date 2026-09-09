# reduce

`mixed reduce(mixed *items, function f)`
`mixed reduce(mixed *items, function f, mixed initial)`

Fold an array from left to right by calling `f(accumulator, element)` and
using each return value as the next accumulator. The accumulator and
elements may have any type, and the accumulator's type may change between
calls. Return the final accumulator.

When `initial` is omitted, use the first element as the accumulator and
start with the second element. An empty array returns `0`; a singleton
returns its element unchanged. Neither case calls `f`.

When `initial` is supplied, including explicit `0`, start with that value
and call `f` for every element. An empty array returns `initial` unchanged.

`f` receives exactly two arguments. Bind additional context with a closure
or a partially applied function pointer. Callbacks run as calls from the
object calling `reduce`, and their errors propagate to its caller.

The array's elements are captured before the first callback. Replacing an
array element in a callback does not change the remaining values to visit;
referenced arrays, mappings, and objects are still shared. `reduce` itself
does not modify the input array.

A first argument that is not an array, or a second argument that is not a
function, is an error even when no callback would be needed.

### Examples

```c
reduce(({ 2, 3, 4 }), &operator(*)());       // 24
reduce(({ 20, 3, 2 }), &operator(-)());      // 15: (20 - 3) - 2
reduce(({ 2, 3, 4 }), &operator(*)(), 0);    // 0
reduce(({}), &operator(*)(), 1);            // 1
reduce(({}), &operator(+)());               // 0
reduce(({ "a", "b" }), (: $1 + $2 :), ">"); // ">ab"

map(({ ({ 1, 2 }), ({ 3, 4 }) }), &reduce(, &operator(+)())); // ({ 3, 7 })
```

### Compatibility

The collection comes first, as in `map`, `filter`, and `sort_array`.
CD's `reduce` takes the callback first; swap its first two arguments when
porting a call, including the bound arguments in `&reduce(...)` pointers.

### See also

`map`, `filter`, `sort_array`, `papplyv`
