# The save format

`save_object`, `restore_object`, `save_map` and `restore_map` share one
line-oriented text format based on CD-lineage `.o` files. The grammar and
extensions below define what this driver reads and writes. Cross-driver
interchange depends on the value types and encodings used; in particular,
the `bytes` and class forms are native extensions.

[`save_value`](efun/save_value.md) and [`restore_value`](efun/restore_value.md)
use the same value encodings directly in strings, without a name, file header or
trailing newline. They perform no file I/O. The value reader accepts trailing
spaces, but no leading whitespace, trailing tabs or newlines, or extra text.

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
| bytes | `b"` then each byte, printable ASCII as itself, `"` `\` as `\"` `\\`, every other byte `\xNN`, then `"` | `b"map\x00\xff"` |
| array | `({` then each element followed by `,` then `})` | `({1,"two",({}),})` |
| mapping | `([` then each `key:value` followed by `,` then `])` | `(["k":4,7:"i",])` |
| object | `$` creation time `@` object name `$` | `$1788811132@/std/player#12$` |
| class | versioned identity, typed slots and views | `c(1,"/record.h","record",({}),({({}),}))` (empty class) |
| zero | `0` | `0` |
| nil | `nil` | `nil` |

Saved `0` always reads as numeric zero. Saved `nil` remains nil in
`NULL_MODE=distinct`; in zero mode it becomes `0`. `restore_object` and
`restore_map` warn once per call, naming the file and counting converted nil
values, including nested values and keys. The warning is delivered at commit;
a caught partial `restore_object` also warns for the earlier lines it applied.
`restore_value` warns with the same count after a successful parse, without a
file path. Failed calls and aborted tasks do not deliver its warning.
See [null values](lpc/null.md) for mode selection and mapping-key collisions.

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
exists and was created in the same second, else as the selected null value. A function pointer
is never written: a top-level variable or `save_map` entry holding one is left
out of the file. In `save_value`, or inside an array, mapping or class field, a
function pointer is written as the selected null value.

Repeated occurrences of the same array, mapping or class restore as independent
instances. References between occurrences are not preserved. Cycles cannot be
saved; reading and writing reject value nesting deeper than 128 levels.

Files are written whole, through a temporary file renamed over the target,
when the writing task commits.

## Class records

[Classes](lpc/classes.md) use this version-one grammar. Spaces below are for
readability; the serialized form has none outside quoted strings.

```text
c(1, source_path, class_name, slots, views)
slots = ({ ({ field_name, type_descriptor, value, }), ... })
views = ({ ({ slot_index, ... }), ... })
primitive_type = ({ kind_name, array_flag, })
class_type = ({ "class", array_flag, source_path, class_name, })
```

Names and source paths are quoted strings with the ordinary string escapes.
The class identity is the canonical absolute in-game source path and class
name. The source path has no empty, `.` or `..` components or trailing slash;
the root alone is not a source file. Class and field names follow LPC's Unicode
identifier syntax. Line numbers, storage IDs and compilation counters are absent.

Primitive kinds are `int`, `float`, `string`, `bytes`, `object`, `mapping`,
`function` and `mixed`. An array flag is 0 or 1. Class field descriptors include
the full nominal identity of their declared type, including for arrays. Values
use the ordinary save grammar recursively, so nested records are allowed.

For `class record { int n; }` with `n == 7`, the value is:

```text
c(1,"/record.h","record",({({"n",({"int",0,}),7,}),}),({({0,}),}))
```

Each slot holds one field name, declared type and value. A view lists zero-based
slot indexes in declaration order. All materialized views and their retained
slots are saved; serialization does not create views. Views that select the same
slot share its storage after restoration. Different slots may have the same
name with different declared types across views.

Restoration rejects unknown versions and field kinds, invalid flags or identities,
duplicate slot keys, duplicate views, duplicate field names within a view,
out-of-range indexes, slots unused by every view, and incompatible field values.
At least one view is required; an empty class has an empty view. In zero mode,
integer zero is valid for any field type. In distinct mode, nil fits reference
fields and mixed, while zero remains an integer. Array checks remain shallow. Metadata integers
must be nonnegative and representable; they do not use value-integer clamping.
Metadata arrays describe schemas and do not count as nested LPC values.

Saving and restoring records requires neither visible class definitions nor
source files. Restoration loads no source objects. Later member access uses the
executing code's definitions and the normal field-view adaptation rules.
`nosave` globals remain excluded before serialization or restoration. A malformed
record is rejected before it becomes a usable class instance; a caught
`restore_object` error still leaves earlier successfully restored globals applied.

This is a native format extension. Foreign-driver class-save import and
graph-preserving serialization are not supported.
