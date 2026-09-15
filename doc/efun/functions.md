# functions

`string *functions(object | string ob = this_object(), int local_only = 0)`

Return an alphabetically sorted array of the function names defined in `ob`,
including inherited functions. Each name appears once, using the definition
that wins normal name lookup; overridden ancestor definitions are omitted.

Pass a nonzero `local_only` to exclude inherited functions. An override defined
in `ob` counts as local. Clones use their shared compiled program's definitions.

Omitting `ob` or passing `0` inspects the calling object. A string `ob` loads
the named object first, as `function_exists` does; normal load authorization
applies and load errors propagate.
An empty or destructed object returns an empty array. Other target types and
non-integer `local_only` arguments raise an error.

When inspecting another object, its private and protected functions are hidden.
The calling object can list its own private and protected functions. Globally
available efuns and simul-efuns are not added to the list. Anonymous closures and
compiler-generated initializers are excluded. Each call returns a fresh array
that can be modified independently.

## Examples

```c
string *all_names = functions();
string *local_names = functions(this_object(), 1);
string *public_names = functions(ob);
```

## See also

`function_exists`, `function_info`, `inherit_list`, `deep_inherit_list`
