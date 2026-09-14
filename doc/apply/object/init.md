# init

`void init()`

The driver applies `init` when an object and a living come into each other's
presence through `move_object`, with `this_player()` set to that living: a
living entering a room triggers `init` in the room, then in each object there,
then in the living once per other living present (and once for the room if it
is living); a non-living object entering triggers its own `init` once per
living present, the room itself included when the room is living. `init` is
where `add_action` and `add_rule` register the commands that object offers.

Participants and their order are selected before the first hook. Before each
call, the driver checks that its target and command giver are still present and
live, and that the command giver still has commands enabled. If the mover is no
longer in the destination or has been destructed, the remaining arrival calls
stop. An object that becomes living during an earlier hook is not added to the
already selected command givers.

An uncaught runtime error from `init` aborts the owning task, including the move
and all its rule changes. If LPC code catches the error, the move and earlier
writes remain in that task's transaction and can commit; movement does not
introduce a separate rollback point.

In `init`, `previous_object()` is that living, and the chain behind it is
where `move_object` was called.

### See also

`add_action`, `add_rule`, `move_object`, `previous_object`
