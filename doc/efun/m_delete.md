# m_delete

`mapping m_delete(mapping m, mixed key)`

Remove `key` and its value from `m`, and return `m` itself: the mapping is
changed in place, so every variable holding it sees the removal. A `key`
that is not present changes nothing. The remaining entries keep their
order. A `key` naming a destructed object names the selected
[null value](../lpc/null.md), as it would when indexing. A non-mapping is an error.

### Examples

```c
mapping m = ([ "a": 1, "b": 2 ]);
m_delete(m, "a");
keys(m);    // ({ "b" })
```

### See also

[`has_key`](has_key.md), `keys`, `values`, `sizeof`
