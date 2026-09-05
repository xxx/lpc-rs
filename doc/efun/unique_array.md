# unique_array

`mixed **unique_array(mixed *arr, function f, mixed skip = 0)`

Group the elements of `arr` by what `f(element)` returns: the result is an
array of groups, each group an array of the elements for which `f` returned
the same value. Groups come in the order their key was first seen, and the
elements of a group in their order in `arr`. The group whose key equals
`skip` is left out; with no `skip`, that is the group with key `0`.

`f` is called once per element, with that element as its only argument. An
error in `f` is the caller's. An empty `arr` calls nothing and returns
`({ })`.

### Examples

```c
/* Livings grouped by level; level 0 (the guests) left out. */
unique_array(users(), (: $1->query_level() :))

/* Every group, including key 0: pick a skip value no key can take. */
unique_array(({ 1, 2, 3, 4, 5, 6 }), (: $1 % 3 :), -1)
/* ({ ({ 1, 4 }), ({ 2, 5 }), ({ 3, 6 }) }) */
```

### See also

`filter`, `map`, `sort_array`
