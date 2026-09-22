# evaluate

`mixed evaluate(mixed value, ...)`

If `value` is a function, call it with the remaining arguments and return its
result. Otherwise, return `value` unchanged. This lets one setting hold either a
value or a callback that computes it:

```c
evaluate("Name: ");                     // "Name: "
evaluate((: "Hello, " + $1 :), "Ada");  // "Hello, Ada"
evaluate(strindex, "hello", "ll");      // 2
evaluate(0);                            // 0
```

Non-function values keep their type and identity, including arrays, mappings,
classes, and objects. Extra arguments are ignored in this case, but their
expressions still run in the usual left-to-right order before the call.

Only the supplied function is invoked. If it returns another function, that
function is returned unchanged. A callback returning `void` produces `0`.

## Function calls

The call follows the same rules as invoking a function variable directly:

- Bound arguments and holes are filled normally; default and variadic
  parameters retain their usual behavior.
- Closures retain their captures, and callbacks run with the same object,
  player, and caller context as a direct pointer call at that location.
- Named functions, closures, simul-efuns, efuns, and dynamic receiver pointers
  are supported. A missing dynamic method returns `0`.
- Argument-count, type, visibility, and object-liveness checks follow ordinary
  pointer-call rules. Reference parameters cannot be supplied through this call.

```c
int add(int a, int b) { return a + b; }

int example() {
    function add_ten = &add(10);
    return evaluate(add_ten, 5);  // 15
}
```

Callbacks execute on the caller's task stack and share its execution budget
and transaction. They can suspend for operations such as object loading. Their
errors propagate to the caller and can be caught with [`catch`](catch.md).
Catching an error retains earlier transaction writes; an uncaught error aborts
the attempt. A conflict retries the whole attempt, including the callback.

At least one argument is required, including when arguments arrive through a
spread or a pointer to `evaluate`.

See also: [function values](../lpc/function_type.md), [map](map.md),
[reduce](reduce.md).
