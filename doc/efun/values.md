# values

`mixed *values(mapping m)`

Return the values of `m` as an array, in the order their keys were
inserted, so that `keys(m)[i]` and `values(m)[i]` are one entry. An entry
whose key names a destructed object is dropped from the mapping first, as
`sizeof` drops it. A non-mapping is an error.

### Examples

```c
mapping m = ([ "b": 1, "a": 2 ]);
values(m);    // ({ 1, 2 })
```

### See also

`keys`, `m_delete`, `sizeof`
