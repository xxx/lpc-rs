# deep_inherit_list

`string *deep_inherit_list(object ob)`

Return all ancestor program paths, once each, in initialization order: ancestors
before descendants, following inheritance declaration order. The object's own
program is excluded. Paths include `.c`. Automatically inherited programs and
programs without globals are included. Clones report their compiled program's
ancestors.

For `child` inheriting `left` and `right`, both of which inherit `base`, the
result is `({ "/base.c", "/left.c", "/right.c" })`.

The array is fresh, requires no authorization, and does not load any programs.
Zero or a destructed object returns an empty array. Other argument types raise
an error.

```c
dump(deep_inherit_list(this_object()));
```

## See also

`inherit_list`, `variable_info`
