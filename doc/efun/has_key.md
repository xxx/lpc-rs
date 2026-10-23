# has_key

`int has_key(mapping map, mixed key)`

Return `1` if `map` contains `key`, or `0` if it does not. A present entry
counts even when its value is `0` or `nil`. A non-mapping argument is an error.

The lookup uses the same key rules as `map[key]`. Strings and bytes match by
content; mutable containers, objects, and function values match by identity.
In distinct null mode, `nil` and integer `0` are separate keys.

A destructed object supplied as `key` names the selected
[null value](../lpc/null.md): `0` in zero mode, `nil` in distinct mode. An entry
stored under that object while it was live does not become a null-key entry.

`has_key` does not modify the mapping or allocate a keys array. It observes the
current transaction's reads and writes, including insertions and deletions
already made by the calling task.

### Examples

```c
mapping settings = ([ "enabled": 0, "owner": nil ]);

has_key(settings, "enabled"); // 1
has_key(settings, "owner");   // 1
has_key(settings, "missing"); // 0

m_delete(settings, "owner");
has_key(settings, "owner");   // 0
```

### See also

[`keys`](keys.md), [`values`](values.md), [`m_delete`](m_delete.md),
[`member_array`](member_array.md)
