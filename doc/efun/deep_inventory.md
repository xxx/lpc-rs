# deep_inventory

`object *deep_inventory(object ob = this_object())`

Returns the array of live objects in `ob`'s inventory, recursively entering containers to
find the objects therein — depth-first, each container's contents right behind it, each
inventory in arrival order. Each object is listed once; `ob` itself never is, even when a
containment cycle leads back to it.

Objects destroyed in the current transaction are excluded immediately, and
destroyed containers are not traversed. An uncaught error rolls back the
destruction; catching the error preserves it.

### See also:

`all_inventory`, `environment`, `all_environment`
