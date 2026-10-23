# all_inventory

`object *all_inventory(object ob = this_object())`

Returns the array of live objects in `ob`'s inventory, in arrival order. This will _not_ include the
inventories of the objects in `ob`'s inventory, however. (See `deep_inventory`
for that.)

Objects destroyed in the current transaction are excluded immediately. An
uncaught error rolls back the destruction; catching the error preserves it.

### See also:

`deep_inventory`, `environment`, `all_environment`
