# Learn LPC

LPC lets you write the rooms, objects, and rules of a multiplayer text game.
The **driver** runs that code and handles player connections. A **mudlib** is
the collection of LPC source files that defines a game.

These lessons assume you have used variables and functions in another
language. You do not need previous LPC or MUD programming experience. They
teach the dialect implemented by lpc-rs; other drivers have differences.

## Start here

Work through these in order. Each lesson gives you code to type, an observable
result, and a small change to try yourself.

1. [Write your first LPC program](first_program.md): run a file, define
   functions, store state, make decisions, and read a compiler error.
2. [Make objects players can use](first_objects.md): put two lanterns in a
   running game, give each its own state, and add commands that use them.
3. [Add a wave command](../../ulib/doc/add-a-command.md): use the current
   player's identity and send different messages to different people.

The first lesson uses the command-line runner. The second uses a copy of
[ulib](../../ulib/README.md), the small example mudlib shipped with the driver,
so you can concentrate on game code while its existing code handles login.

## Look things up as you need them

| When you need to… | Read |
|---|---|
| Store a list of items or a lookup table | [Types](types.md), especially arrays and mappings |
| Handle errors from an expression or a block | [`catch`](../efun/catch.md), [`throw`](../efun/throw.md) |
| Iterate over a collection | [`foreach`](foreach.md) |
| Exclude a cache or temporary global from object saves | [`nosave` globals](nosave.md) |
| Read elements or slices, including positions counted from the end | [Indexing and slices](indexing.md) |
| Pass a function to another function or schedule a callback | [Functions and closures](function_type.md), [`call_out`](../efun/call_out.md) |
| Collect or forward a variable number of arguments | [Variadic parameters](variadic_parameters.md), [argument spreading](argument_spreading.md) |
| Understand a word such as *efun*, *apply*, or *prototype* | [Glossary](../glossary.md) |
| Check a built-in function's arguments | [Efun reference](../efun) |
| Find a hook the driver calls in your object | [Object hooks](../apply/object), [other apply categories](../../ulib/doc/applies.md) |
| Import parent programs or restrict inherited visibility | [Inheritance](inheritance.md) |
| Use source-path and driver-version macros | [Predefined macros](macros.md#predefined-macros) |
| Control compiler pragmas and warnings | [Pragmas](pragmas.md) |
| Define macros or turn arguments into strings | [Macros](macros.md) |
| Understand ulib's login and command flow | [How ulib works](../../ulib/doc/how-it-works.md) |

You can return to these references after running the lessons; they are not
prerequisites for your first program.
