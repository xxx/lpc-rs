# all_inventory

`object *all_inventory(object ob = this_object())`

Returns the array of objects in `ob`'s inventory. This will _not_ include the
inventories of the objects in `ob`'s inventory, however. (See `deep_inventory`
for that.)

### See also:

`environment`, `all_environment`, `deep_inventory`