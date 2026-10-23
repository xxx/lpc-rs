# restore_value

`mixed restore_value(string text)`

Parse one value from a string in the native [save format](../save-format.md), such
as a result of `save_value()`. There is no variable name, file header or newline.
Trailing spaces are accepted; leading whitespace, trailing tabs or newlines,
extra values, malformed input and nesting deeper than 128 levels raise runtime
errors that `catch()` can handle. The complete input must parse before a value is
returned.

Arrays, mappings and classes are new mutable instances. Repeated containers do not
share identity. Class records restore without visible definitions or source files;
invalid schemas and incompatible field values are rejected. Decimal floats are
accepted as well as the writer's hexadecimal form; integers beyond the 64-bit
range clamp to its limits.

Object references resolve only to currently live objects with the same name and
creation second; otherwise they become the selected null value. No objects are
loaded. The call performs no file I/O and requires no master permission apply.

Saved `0` always remains integer zero. Saved `nil` remains nil in distinct mode;
zero mode converts it to `0` and emits one warning per successful call at commit,
counting converted nil values, including keys and nested values. A failed restore
or aborted task emits no conversion warning. If conversion merges mapping keys,
the later entry wins. See [null values](../lpc/null.md).

### Examples

```c
mapping data = restore_value(save_value(([ "hp": 25 ])));
mixed value;
mixed error = catch(value = restore_value("({1,2,3,})"));
// On success, value is ({ 1, 2, 3 }).
```

### See also

[save_value](save_value.md), [restore_object](restore_object.md),
[restore_map](restore_map.md), [json_decode](json_decode.md)
