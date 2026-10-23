# store_variable

`void store_variable(object target, string name, mixed value, string declaring_program = nil)`

Replace a named global in a live object. `target` is required: nil, zero,
destructed objects, and filenames are not accepted, and no object is loaded.
The operation returns no value.

The search includes every declaration reported by
[`variable_info`](variable_info.md), including inherited, hidden, private,
protected, `nosave`/`static`, and `nomask` globals. `name` must match the declared
name exactly. With an omitted or nil `declaring_program`, exactly one declaration
must have that name. Otherwise the selector must exactly match the canonical
source path in the entry's `program` field, including its leading slash and `.c`.
Basenames, inherit aliases, and alternative path spellings are not resolved.
A missing or ambiguous declaration raises an error without assigning it.

The replacement must pass the declared type's runtime cast check, regardless of
the global's current value. This follows the configured [nil rules](../lpc/null.md),
checks a scalar class's nominal identity, and checks arrays shallowly without
validating their elements. Values are not converted or copied deeply: assigned
arrays, mappings, classes, objects, and functions keep their normal sharing.

Writing one's own globals is allowed without a master apply, including from
inherited code. Writing another object's globals requires
[`valid_store_variable`](../apply/master/valid_store_variable.md) to return a
truthy value, even for public declarations. Refusal or a missing master/apply
raises `store_variable: permission denied`. Permission to inspect globals does
not grant permission to replace them.

Argument shapes are checked before authorization; declaration lookup and type
checking happen after it. If the apply destroys the target, the write fails.
Errors from the apply propagate. A simul-efun wrapper executes as the simul-efun
object, so writing its caller's globals requires authorization too.

The write uses the target's current transactional program image, so a saved
setter function still resolves the current declaration after recompilation.
The value is visible to subsequent reads in the same transaction. Commit makes
the write persistent in memory; abort discards it, and conflict retries repeat
the operation and its authorization. A failed setter leaves the destination
unchanged by the setter, but side effects made by the policy remain pending if
the error is caught. A policy may also mutate a shared proposed value.

```c
private int score;

void reset_score() {
    store_variable(this_object(), "score", 0);
}

// Select one declaration when multiple inherited programs declare "state".
void restore_state(mapping saved) {
    store_variable(this_object(), "state", saved, "/std/state.c");
}
```

Use `variable_info` for named reads. Replacing an entry in its result does not
replace the corresponding global; use this efun for that operation.

## See also

`variable_info`, `valid_store_variable`, `save_value`, `restore_value`
