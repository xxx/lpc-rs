# filter

`mixed *filter(mixed *arr, function f, mixed extra...)`
`mapping filter(mapping m, function f, mixed extra...)`

Return a new collection holding the entries of the first argument that `f`
accepts. For an array, `f` is called as `f(item, extra...)` for each item
in order, and the items for which it returns a true value are kept, in
their order. For a mapping, `f` is called as `f(key, value, extra...)` and
the entries it accepts are kept, in their order.

The original is not changed. `f` runs as a call from the object calling
`filter`, so `previous_object()` inside it is that object, and an error it
throws is `filter`'s caller's. `f` runs on the calling task's own call
stack, so nesting is bounded by the call stack rather than the task chain.
A first argument that is neither an array nor a mapping, or an `f` that is
not a function, is an error.

### Examples

```c
int *evens = filter(({ 1, 2, 3, 4 }), (: $1 % 2 == 0 :));       // ({ 2, 4 })
mapping big = filter(([ "a": 1, "b": 20 ]), (: $2 > 10 :));      // ([ "b": 20 ])
object *living = filter(all_inventory(room), (: living($1) :));
```

### See also

`map`, `sort_array`, `member_array`
