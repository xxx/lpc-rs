# filter_map

`mixed *filter_map(mixed *arr, function f, mixed extra...)`
`mapping filter_map(mapping m, function f, mixed extra...)`

Return a new collection holding what `f` returns for each entry of the
first argument, keeping only the results that are true. For an array, `f`
is called as `f(item, extra...)` for each item in order, and the true
results form the new array, in their order. For a mapping, `f` is called
as `f(key, value, extra...)` and the new mapping has the keys whose result
was true, in their order, each with that result as its value. A result is
false when it is `0`, `0.0`, or a destructed object; anything else,
including `""` and `({ })`, is true.

This is `map` followed by `filter` in one pass, with one call of `f` per
entry and no intermediate collection.

The original is not changed. `f` runs as a call from the object calling
`filter_map`, so `previous_object()` inside it is that object, and an
error it throws is `filter_map`'s caller's. `f` runs on the calling task's
own call stack, so nesting is bounded by the call stack rather than the
task chain. A first argument that is neither an array nor a mapping, or an
`f` that is not a function, is an error.

### Examples

```c
int *tens = filter_map(({ 1, 2, 3 }), (: $1 % 2 ? $1 * 10 : 0 :));      // ({ 10, 30 })
string *names = filter_map(all_inventory(room), (: $1->query_name() :)); // the named ones
mapping big = filter_map(([ "a": 1, "b": 20 ]), (: $2 > 10 ? $2 * 2 : 0 :)); // ([ "b": 40 ])
```

### See also

`filter`, `map`, `sort_array`
