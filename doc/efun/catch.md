# catch

```c
mixed catch(expression)
mixed catch { statements }
```

Evaluate the expression or execute the block, and return 0 if no error was thrown.
If an error was thrown, skip the rest of the body and return the error message.
`throw(value)` supplies the stringified value as that message.
The body's value is discarded. Both forms can be used wherever an expression is
allowed; a standalone catch statement needs a trailing semicolon.

```c
mixed err = catch {
    object item = load_object("/obj/item");
    item->setup();
};
if (err) {
    write(err);
}
```

Blocks may contain declarations and statements, including loops and nested
catches. Their local variables have ordinary block scope and can be captured by
closures. An empty block succeeds with 0. `catch` does not undo writes or effects
that precede the error: they remain part of the current transaction, subject to
its normal commit or retry behavior.

`return` inside a catch block returns from the enclosing function or closure;
an error while evaluating its return expression is still caught. `break` and
`continue` use their ordinary enclosing loop or switch targets. Leaving a catch
removes its handler; jumping within it keeps the handler active. A nested
closure has its own control-flow context. A switch outside a catch cannot jump
to a `case` or `default` inside that catch, but switches inside a catch work
normally.

A `for` initializer, a `foreach` collection, and a `switch` expression run before
that construct establishes its jump targets. Jumps in catch blocks there belong
to an enclosing loop or switch, if one exists. `return` in a global initializer
is an error.

When loading an object fails to compile, the returned string includes the
diagnostic: source file, line, column, highlighted code, and any related locations
or additional diagnostics. Reports over 4 KiB are shortened with a notice, leaving
room under the 8 KiB string limit for surrounding text. The complete report goes
to the debug log when the catching task commits. Runtime errors and explicit
`throw()` values return their messages unchanged.

`catch` is a keyword, not a function: `&catch()` is a compile error.

For FluffOS porting differences, including jumps out of catch blocks and thrown
values, see the [driver comparison](../driver-comparison.md#fluffos).
