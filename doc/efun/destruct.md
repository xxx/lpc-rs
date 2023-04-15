# destruct

`void destruct(object ob | object *obs)`

`destruct` removes the object `ob` or all objects in the array `obs` from the game.
Any variable that holds a reference to the object will be set to 0, and any
pointers to functions within them will throw an error when called. All call outs
owned by them will be canceled.

Calling `destruct` on an already destructed object is a no-op.

## See also

`call_out`, `clone_object`