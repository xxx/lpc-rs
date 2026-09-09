# catch

`mixed catch(mixed | void)`

Evaluate the argument, and return 0 if no error was thrown. If an error was thrown,
return the error message.

When loading an object fails to compile, the returned string includes the
diagnostic: source file, line, column, highlighted code, and any related locations
or additional diagnostics. Reports over 4 KiB are shortened with a notice, leaving
room under the 8 KiB string limit for surrounding text. The complete report goes
to the debug log when the catching task commits. Runtime errors and explicit
`throw()` values return their messages unchanged.

`catch` is a keyword, not a function: `&catch()` is a compile error.
