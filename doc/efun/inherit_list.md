# inherit_list

`string *inherit_list(object ob)`

Return `ob`'s direct parent program paths in inheritance declaration order.
Paths include `.c`; automatically inherited programs are included. The object's
own program is excluded. Clones report their compiled program's parents.

For `child` inheriting `left` and `right`, both of which inherit `base`, the
result is `({ "/left.c", "/right.c" })`.

The array is fresh, requires no authorization, and does not load any programs.
Zero or a destructed object returns an empty array. Other argument types raise
an error.

```c
dump(inherit_list(this_object()));
```

## See also

`deep_inherit_list`, `variable_info`
