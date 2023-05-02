# interactive

`int interactive(object ob = this_object())`

Returns `1` if `ob` is an interactive object, `0` otherwise.

An interactive object is one with an active connection to a user, either via
login, or via the `exec` efun.

NPCs are not interactive, and neither are link-dead player bodies.

Being interactive does not mean that the object is `living`, though that's
typically the case once the login process is complete.

### See also:

`living`, `exec`