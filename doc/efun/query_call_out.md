# query_call_out

`mixed *query_call_out(int id)`

Get information about a call out by its ID.

The information returned is an array, with the following members:

```c
({
  object - The object that the call out was defined in (i.e. where 'call_out' was called),
  function - The function to be called,
  int - The number of milliseconds remaining until the call out will be made.
  int - The number of milliseconds between repeats, or 0 if the call out is not repeating.
})
```

If the call out was not found, 0 is returned.

### Examples

```c
int call_out_id = call_out(&dump("repeats!"), 0, 0.1);

dump(query_call_out(call_out_id));
```

### See Also

`call_out`, `remove_call_out`, `query_call_outs`