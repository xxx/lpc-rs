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

After an allow, the driver checks that both objects are still live and repeats
the structural checks before attaching. Moving or destructing an object, or
changing its shadow relationships during this apply, can therefore cause the
attach to fail even after a non-zero return.

The apply joins the caller's transaction: `this_object()` is the master,
`previous_object()` is the prospective shadow, and `this_player()` retains the
requesting context. An error thrown here is the caller's error. If the caller
catches a refusal after the apply returns, changes made by the apply remain in
that transaction.

### Examples

```c
int query_allow_shadow(object target) {
    // A target may opt out by defining query_prevent_shadow().
    return !target->query_prevent_shadow(previous_object());
}
```

### See also

`shadow`
