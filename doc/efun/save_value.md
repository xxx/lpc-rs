# save_value

`string save_value(mixed value)`

Serialize one value into a string using the native [save format](../save-format.md).
The result contains only the encoded value, without a variable name or trailing
newline. The call performs no file I/O and requires no master permission apply.

Integers, finite floats, strings, bytes, arrays, mappings and classes are supported.
Containers include their current transactional contents; mapping keys can be any
serializable value. Class records retain their nominal identity, typed fields and
materialized views without requiring visible class definitions.

Live objects are encoded by name and creation time. Destructed objects and function
pointers, including a function passed directly as `value`, become the selected null:
`0` in zero mode or `nil` in distinct mode. See [null values](../lpc/null.md).

Repeated occurrences of a container are encoded independently. Shared identity is
not preserved on restoration. Cycles, nesting deeper than 128 levels and non-finite
floats raise runtime errors that `catch()` can handle.

### Examples

```c
string text = save_value(([ "hp": 25, "tags": ({ "awake" }) ]));
mapping restored = restore_value(text);
// restored["hp"] == 25

save_value(42);   // "42"
save_value(3.25); // "#0x1.ap+1#"
```

### See also

[restore_value](restore_value.md), [save_object](save_object.md),
[save_map](save_map.md), [json_encode](json_encode.md), [deep_copy](deep_copy.md)
