# typeof

`int typeof(mixed value)`

Return the tag of `value`'s type:

| Tag | Value's type |
|---|---|
| `1` | an int, including `0` |
| `2` | a float |
| `3` | a string |
| `4` | a live object |
| `5` | an array |
| `6` | a mapping |
| `7` | a function pointer or closure |
| `8` | a `bytes` |
| `9` | a class instance |
| `10` | distinct nil |

A destructed object has the selected [null type](../lpc/null.md): `1` in
zero mode, `10` in distinct mode.

The driver ships no header; a mudlib names the tags in one of its own:

```c
#define T_INT 1
#define T_FLOAT 2
#define T_STRING 3
#define T_OBJECT 4
#define T_ARRAY 5
#define T_MAPPING 6
#define T_FUNCTION 7
#define T_BYTES 8
#define T_CLASS 9
#define T_NIL 10
```

### Examples

```c
if (typeof(x) == T_STRING) {
    write(x);
}
```

### See also

`intp`, `floatp`, `stringp`, `objectp`, `arrayp`, `mappingp`, `functionp`, `bytesp`,
[`classp`](classp.md), [`nilp`](nilp.md)
