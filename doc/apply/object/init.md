# init

`void init()`

The driver applies `init` when an object and a living come into each other's
presence through `move_object`, with `this_player()` set to that living: a
living entering a room triggers `init` in the room, then in each object there,
then in the living once per other living present (and once for the room if it
is living); a non-living object entering triggers its own `init` once per
living present. `init` is where `add_action` registers the verbs that object
offers.

### See also

`add_action`, `move_object`
