# remove_rule

`int remove_rule(int id)`

Unregisters the calling object's rule with `id` — every verb alternative of
it — from `this_player()`. Returns 1 when such a rule was there, 0 otherwise
(an unknown id, or a rule another object registered). No `this_player()` is a
runtime error.

`remove_action(verb)` also reaches native rules by verb.

### See also

`add_rule`, `remove_action`
