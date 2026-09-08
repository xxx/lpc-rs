# The save-file format

`save_object`, `restore_object`, `save_map` and `restore_map` share one
text format, the one CD-lineage libs' `.o` files are written in, so a file
from such a lib loads unchanged and a file written here loads there.

A file is a sequence of lines, each `name value` with exactly one space
between: `name` is the variable name, or for `save_map` the key: any text
up to the first space, possibly empty. `value` is one of the forms below.
No header line, no blank lines, no comments. A `save_map` key may be any
string except one containing a space, tab, newline or carriage return; a
key with any of those is refused before anything is written.

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
tab or double space after the name, a line with no space at all, or a
blank line is an error. `restore_map` reads the name back as the key
verbatim, including empty; `restore_object` looks the name up as a
variable and ignores it when nothing matches, whether or not a prefix of
it happens to be a real variable name. Ints beyond the 64-bit range clamp.
An escape other than the three listed is the bare character. A decimal
float such as `3.25` is accepted and read as a float. A `\r` right before
a line's newline is ignored, so a CRLF file reads the same as an LF one. A
file that is not valid UTF-8 is read as Latin-1. A file is always written
as UTF-8, so re-saving a Latin-1 file changes its bytes.

A float that is not finite (infinity or NaN) cannot be saved: the writer
raises a runtime error instead of writing it.

An object reference restores as the live object of that name when it
exists and was created in the same second, else as 0. A function pointer
is never written: a variable holding one is left out of the file, and one
inside an array or mapping is written as `0`.

Files are written whole, through a temporary file renamed over the target,
when the writing task commits.
