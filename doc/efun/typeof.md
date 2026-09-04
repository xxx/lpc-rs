# typeof

`int typeof(mixed value)`

Return the tag of `value`'s type:

| Tag | Value's type |
|---|---|
| `1` | an int, including `0` and a destructed object |
| `2` | a float |
| `3` | a string |
| `4` | a live object |
| `5` | an array |
| `6` | a mapping |
| `7` | a function pointer or closure |

The driver ships no header; a mudlib names the tags in one of its own:

```c
#define T_INT 1
#define T_FLOAT 2
#define T_STRING 3
#define T_OBJECT 4
#define T_ARRAY 5
#define T_MAPPING 6
#define T_FUNCTION 7
```

### Examples

```c
if (typeof(x) == T_STRING) {
    write(x);
}
```

### See also

`intp`, `floatp`, `stringp`, `objectp`, `arrayp`, `mappingp`, `functionp`
