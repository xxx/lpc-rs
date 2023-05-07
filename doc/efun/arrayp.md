# arrayp

`int arrayp(mixed)`

Returns 1 if the argument is an array, 0 otherwise. The type of the items
contained in the array is not considered.

Some drivers name this function `pointerp`. If porting code from a driver 
that does so, you can define a simul efun to make porting easier:

```c
int pointerp(mixed arg) {
    return arrayp(arg);
}
```

### See also:

`intp`, `floatp`, `stringp`, `objectp`, `mappingp`, `functionp`