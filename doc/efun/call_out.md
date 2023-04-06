# call_out

`mixed call_out(function f, int | float seconds_delay, [int | float seconds_repeat = 0])`

Call a function after a delay, and possibly repeating on a regular interval.

If `seconds_delay` is 0 or negative, the function will be immediately prioritized
for calling when the current Task yields.

If `seconds_repeat` is positive, the function will be called repeatedly at that
interval, after the initial delay.

If `seconds_repeat` is 0 or negative, the function will only be called once.

When `seconds_delay` or `seconds_repeat` are floating point numbers, they will be
accurate to millisecond precision.

### Examples

```c
int call_out_id = call_out(&dump("i'm on a delay!", 5));

object ob = clone_object("/lib/test/test_object");

// Any function pointer (with a known receiver) can be passed to call_out, including closures.
int call_out_id1 = call_out(&ob->add_two(4, 6), 1.5));
int call_out_id3 = call_out(&call_other(ob, "add_two", 4, 6), 1.5));
int call_out_id2 = call_out((: ob->add_two(4, 6) :), 1.5));

// Composed functions work as well.
int call_out_combo_id = call_out(dump @ (: ob->add_two(4, 6) :), 2.45));
```

Note that function pointers will evaluate their arguments immediately, while functions
called within closures will not. Make sure that values of things like this_player()
are what you expect!

### See Also

`remove_call_out`, `query_call_out`, `query_call_outs`