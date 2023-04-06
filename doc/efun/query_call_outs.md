# query_call_out

`mixed *query_call_outs(object ob = this_object())`

Get information about all call outs of an object, defaulting to object it's called within.

The result is an array of arrays. The inner arrays are the same as the result of `query_call_out`.

### Examples

```c
int call_out_id = call_out(&dump("repeats!"), 0, 0.1);

dump(query_call_out(call_out_id));
```

### See Also

`call_out`, `remove_call_out`, `query_call_out`