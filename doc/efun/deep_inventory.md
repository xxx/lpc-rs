# deep_inventory

`object *deep_inventory(object ob = this_object())`

Returns the array of objects in `ob`'s inventory, recursively entering containers to
find the objects therein — depth-first, each container's contents right behind it, each
inventory in arrival order.

### See also:

`all_inventory`, `environment`, `all_environment`