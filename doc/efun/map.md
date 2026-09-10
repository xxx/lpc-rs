# map

`mixed *map(mixed *arr, function f, mixed extra...)`
`mapping map(mapping m, function f, mixed extra...)`

Return a new collection with each entry of the first argument replaced by
what `f` returns for it. For an array, `f` is called as
`f(item, extra...)` for each item in order, and the results form the new
array. For a mapping, `f` is called as `f(key, value, extra...)` and the
new mapping has the same keys, in the same order, each with `f`'s result as
its value.

The original is not changed. `f` runs as a call from the object calling
`map`, so `previous_object()` inside it is that object, and an error it
throws is `map`'s caller's. `f` runs on the calling task's own call stack,
so nesting is bounded by the call stack rather than the task chain. A
first argument that is neither an array nor a mapping, or an `f` that is
not a function, is an error.

With a dynamic pointer such as `&->query_name()`, each array item is the
receiver. A missing method contributes `0` to the result, just as with
`(: $1->query_name() :)`.

### Examples

```c
int *tens = map(({ 1, 2 }), (: $1 * 10 :));                 // ({ 10, 20 })
string *names = map(all_inventory(room), (: $1->query_name() :));
mapping doubled = map(([ "a": 1 ]), (: $2 * 2 :));           // ([ "a": 2 ])
```

### See also

`filter`, `reduce`, `sort_array`
