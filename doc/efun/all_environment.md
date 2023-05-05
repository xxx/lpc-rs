# all_environment

`object *all_environment(object ob = this_object())`

Returns the array of environments, starting with the `environment` of `ob`,
adding the environment of `ob`'s environment, etc., and
ending with the outermost environment. If `ob` is not contained by anything,
returns an empty array.

### See also:

`environment`, `all_inventory`, `deep_inventory`