# nilp

`int nilp(mixed value)`

Returns `1` if `value` is the configured null value, otherwise `0`.
A destructed object also returns `1`, including destruction within the current
transaction.

| Value | `NULL_MODE=zero` | `NULL_MODE=distinct` |
|---|---|---|
| `nil` | `1` | `1` |
| Integer `0` | `1` | `0` |
| Float `0.0` | `0` | `0` |
| Destructed object | `1` | `1` |
| Live object or empty string, array, mapping, bytes, or class | `0` | `0` |

This tests the value, not whether a mapping key exists or an argument was
supplied. A present mapping entry can store nil; use [`has_key`](has_key.md)
to test membership. Use declared defaults or a variadic collector to handle
omitted arguments.

```c
object target = find_object("/rooms/hall");
if (nilp(target)) {
    write("The hall is not loaded.\n");
}
```

### See also

[Null values](../lpc/null.md), [`typeof`](typeof.md), [`intp`](intp.md),
[`objectp`](objectp.md), [`has_key`](has_key.md)
