# compose

`function compose(function a, function b)`

#### Alternate Syntax
* `a @ b`

Create a new function that is the composition of two functions. The new function
takes the same arguments as `b`, and calls `a` with the result of `b`. The
result of `a` is the result of the new function, i.e. `compose(a, b)(x)` is
equivalent to `a(b(x))`. Function `a` receives one result argument, so any other
needed arguments should be partially applied beforehand.

If `a` has an unbound dynamic receiver, as in `&->move()`, the composed function
keeps that receiver open. Its first call argument supplies the receiver, and the
remaining arguments go to `b`: `compose(a, b)(ob, x)` calls `a(ob, b(x))`.
The result of `b` fills the first method-argument hole, or follows the bound
arguments if no holes remain. A receiver already bound in `a` stays bound, and
all call arguments go to `b`.
The receiver can also be bound on the composed function with `papplyv`.

When using the alternate `@` syntax, chained uses of it are _right associative_,
so `a @ b @ c` is equivalent to `a @ (b @ c)`.

### Examples

```c
int a(int x) { return x + 1; }
int b(int x) { return x * 2; }

function h = compose(a, b);
function i = a @ b;

int j = h(6); // 13
int k = i(6); // 13
```

```c
function move_to_quay = &->move() @ &present(NAME_WAFERQUAY);
move_to_quay(actor); // actor->move(present(NAME_WAFERQUAY))

function move_actor = papplyv(move_to_quay, ({ actor }));
move_actor(); // looks up the destination again, then moves actor
```

The inner function runs on each invocation of the composed function, so the
`present()` lookup above happens when the callback is called.
