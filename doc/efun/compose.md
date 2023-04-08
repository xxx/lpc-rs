# compose

`function compose(function a, function b)`

#### Alternate Syntax
* `a @ b`

Create a new function that is the composition of two functions. The new function
takes the same arguments as `b`, and calls `a` with the result of `b`. The
result of `a` is the result of the new function, i.e. `compose(a, b)(x)` is
equivalent to `a(b(x))`. Function `a` will always receive exactly one argument,
, so any other needed arguments should be partially-applied beforehand.

When using the alternate `@` syntax, chained uses of it are _right associative_,
so `a @ b @ c` is equivalent to `a @ (b @ c)`.

### Examples

```c
int a(int x) { return x + 1; }
int b(int x) { return x * 2; }

int h = compose(a, b);
int i = a @ b;

int j = h(6); // 13
int k = i(6); // 13
```
