# Non-persistent globals

Declare an object global `nosave` to exclude it from `save_object` and
`restore_object`:

```c
int score;
private nosave mapping lookup_cache = ([]);
protected nosave int last_input;
```

`save_object` writes `score` but omits `lookup_cache` and `last_input`.
`restore_object` ignores entries naming either excluded variable, including
entries in an older save file. Their current values remain intact; restoration
does not reset them to their initial values.

## Modifiers and scope

`nosave` and `static` have the same meaning on globals in lpc-rs. Both spellings
remain supported, and using them together is redundant but valid. The flag
applies to every variable in a comma-separated declaration and survives
inheritance.

Persistence and visibility are independent. A `nosave` global is public unless
declared `private` or `protected`; a private global is saved unless it is also
`nosave` or `static`. `nomask` can be combined with `nosave` to prevent an
inheritor from redeclaring that variable. A permitted redeclaration uses its own
storage flag, following the usual save-file name resolution.

`nosave` is only valid on globals. Functions, function prototypes, parameters,
locals, and inherit statements cannot use it. It is a reserved keyword, though
the preprocessor can still define aliases such as `#define nosave static`.
On functions, the existing `static` modifier controls visibility; it does not
make `nosave` a valid function modifier.

## Runtime behavior

These globals still initialize, retain values between calls, and participate
in transactions and garbage collection normally. The exclusion belongs to the
variable declaration: if another saved global refers to the same array or
mapping, that other global can still save its contents.

[`variable_info`](../efun/variable_info.md) includes `nosave` globals and their
values. Its `flags` string uses the canonical spelling `static` for both forms.

See [`save_object`](../efun/save_object.md),
[`restore_object`](../efun/restore_object.md), and the
[driver comparison](../driver-comparison.md#fluffos) for porting differences.
