# init

`void init()`

The driver applies `init` when an object and a living come into each other's
presence through `move_object`, with `this_player()` set to that living: a
living entering a room triggers `init` in the room, then in each object there,
then in the living once per other living present (and once for the room if it
is living); a non-living object entering triggers its own `init` once per
living present, the room itself included when the room is living. `init` is
where `add_action` registers the verbs that object offers.

An `init` that raises a runtime error aborts the move and the task that caused
it, as any runtime error does: nothing it wrote is committed.

### See also

`add_action`, `move_object`
