# query_allow_shadow

`int query_allow_shadow(object target)`

The driver applies `query_allow_shadow` in the master before every attach
by `shadow(target, 1)`, with `previous_object()` the object that wants to
become the shadow. A non-zero return allows the attach; zero, or any
false value, refuses it, and `shadow` raises "The master refused the
shadow." in the caller.

`shadow` raises "Shadowing is disabled: the master defines no
query_allow_shadow." when the apply is absent, so a master that never
defines it has no shadowing at all. The driver's own checks — a shadow that
already shadows or is shadowed or has an environment, a target that is a
shadow, the master, the simul-efun object, a `no_shadow` program or one
whose `nomask` function the shadow defines — run before this apply and
refuse on their own.

The apply runs inside the caller's task: `this_object` is the master and an
error thrown here is the caller's error.

### Examples

```c
int query_allow_shadow(object target) {
    // A target may opt out by defining query_prevent_shadow().
    return !target->query_prevent_shadow(previous_object());
}
```

### See also

`shadow`
