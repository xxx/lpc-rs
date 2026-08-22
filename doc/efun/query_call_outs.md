# query_call_outs

`mixed *query_call_outs(object ob = this_object())`

Get information about all call outs of an object, defaulting to the object it's called within.
Call outs scheduled by the same transaction are included.

The result is an array of arrays. The inner arrays are the same as the result of `query_call_out`.

### Examples

```c
dump(query_call_outs());
```

### See Also

`call_out`, `remove_call_out`, `query_call_out`