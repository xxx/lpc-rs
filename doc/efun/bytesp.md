# bytesp

`int bytesp(mixed value)`

Return 1 if `value` is a `bytes`, 0 otherwise. A string is not a `bytes`,
however it was spelled: the two types never mix, and `to_bytes` and
`to_text` are what convert between them.

### Examples

```c
if (bytesp(x)) {
    write(to_text(x, "UTF-8"));
}
```

### See also

`stringp`, `intp`, `floatp`, `objectp`, `arrayp`, `mappingp`, `functionp`,
`to_bytes`, `to_text`
