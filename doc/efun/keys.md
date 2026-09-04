# keys

`mixed *keys(mapping m)`

Return the keys of `m` as an array, in the order they were inserted, so
that `keys(m)[i]` and `values(m)[i]` are one entry. A key that names a
destructed object is dropped from the mapping first, as `sizeof` drops it.
A non-mapping is an error.

### Examples

```c
mapping m = ([ "b": 1, "a": 2 ]);
keys(m);    // ({ "b", "a" })
```

### See also

`values`, `m_delete`, `sizeof`
