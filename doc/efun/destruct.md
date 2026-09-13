# destruct

`void destruct(object ob | object *obs)`

`destruct` removes the object `ob` or all objects in the array `obs` from the game.
Each live target requires the master's
[`valid_destruct(caller, target, program)`](../apply/master/valid_destruct.md)
to return a truthy value, including when an object destroys itself or the master
calls `destruct`. A missing master/apply or a false result raises
`destruct: permission denied`; errors from the apply propagate.

For an array, the driver captures the distinct live targets in array order and
authorizes all of them before removing any. A refused or failed authorization
therefore causes no removals by this call, even if its error is caught. Changes
made by the apply itself are ordinary transaction writes and are not undone by
`catch`. Mutating the input array during authorization does not change the targets.

Any variable that holds a reference to the object will be set to 0, and any
pointers to functions within them will throw an error when called. All call outs
owned by them will be canceled.

If `ob` holds a connection, the connection is closed once the task commits;
anything written to it earlier in the task is delivered first.

Null and already destructed objects are ignored without calling the apply.
Destruction and the apply share the caller's transaction: an uncaught error
rolls them back, and a transaction conflict retries them together.

## See also

`valid_destruct` (apply), `call_out`, `clone_object`
