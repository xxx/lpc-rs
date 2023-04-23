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

Because call outs are executed outside the user's REPL, `this_player()` will return
0 when called from within a call out. If you need access to the player, you can either
freeze it with a partial application, or capture it in a closure.

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

// The wrong way to access this_player() in a call out:
void my_bad_call_out() {
    this_player()->catch_msg("boo!"); // Wrong! this_player() is not set in call_outs!
}
call_out(my_bad_call_out, 1.5);

// The right way:
void my_good_call_out(object player) {
    player->catch_msg("boo!");
}
call_out(&my_good_call_out(this_player()), 1.5);
// -- OR --
object player = this_player();
call_out((: my_good_call_out(player) :), 1.5);
```

Note that function pointers will evaluate their arguments immediately, while functions
called within closures will not. Make sure that values of things like this_player()
are what you expect!

### See Also

`remove_call_out`, `query_call_out`, `query_call_outs`