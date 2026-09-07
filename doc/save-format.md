# The save-file format

`save_object`, `restore_object`, `save_map` and `restore_map` share one
text format, the one CD-lineage libs' `.o` files are written in, so a file
from such a lib loads unchanged and a file written here loads there.

A file is a sequence of lines, each `name value` with exactly one space
between: `name` is an identifier (a variable name, or a mapping key for
`save_map`), `value` one of the forms below. No header line, no blank
lines, no comments.

| value | form | example |
|---|---|---|
| int | decimal | `42`, `-17` |
| float | `#` + C99 hex-float + `#` | `#0x1.ap+1#` (3.25), `#0x0p+0#` |
| string | double-quoted; `"` `\` and newline as `\"` `\\` `\n`, every other byte raw | `"q\"b\\n\nt	uéé"` |
| array | `({` then each element followed by `,` then `})` | `({1,"two",({}),})` |
| mapping | `([` then each `key:value` followed by `,` then `])` | `(["k":4,7:"i",])` |
| object | `$` creation time `@` object name `$` | `$1788811132@/std/player#12$` |
| zero | `0` | `0` |

Reading is strict: a missing trailing comma, a space inside a container, a
tab or double space after the name, or a blank line is an error. Ints
beyond the 64-bit range clamp. An escape other than the three listed is
the bare character. A decimal float such as `3.25` is accepted and read as
a float. A file that is not valid UTF-8 is read as Latin-1.

An object reference restores as the live object of that name when it
exists and was created in the same second, else as 0. A function pointer
is never written: a variable holding one is left out of the file, and one
inside an array or mapping is written as `0`.

Files are written whole, through a temporary file renamed over the target,
when the writing task commits.
