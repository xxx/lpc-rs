# destruct

`void destruct(object ob | object *obs)`

`destruct` removes the object `ob` or all objects in the array `obs` from the game.
Any variable that holds a reference to the object will be set to 0, and any
pointers to functions within them will throw an error when called. All call outs
owned by them will be canceled.

If `ob` holds a connection, the connection is closed once the task commits;
anything written to it earlier in the task is delivered first.

Calling `destruct` on an already destructed object is a no-op.

## See also

`call_out`, `clone_object`