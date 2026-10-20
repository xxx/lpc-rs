# Pragmas

Pragmas control preprocessing, declaration requirements, and how objects made
from a program may be used. This reference lists every pragma accepted by
lpc-rs.

## Syntax

Put `#pragma` on its own line, followed by one or more comma-separated names:

```lpc
#pragma strict_types
#pragma no_clone, no_inherit, no_shadow
```

Names are case-sensitive and are not expanded as macros. Whitespace around
commas and trailing comments are allowed. There is no semicolon, argument,
or trailing comma. Separate directives and a comma-separated list have the
same effect; names in a list are processed from left to right.

Warnings start enabled for each program compilation; use `no_warnings` to
disable them and `warnings` to re-enable them. Function type information is
always retained, with or without `save_types`. Other settings start disabled,
and have no directive to turn them off. Repeating a setting is harmless.
Pragmas in an inactive conditional branch, such as `#if 0`, have no effect and
produce no pragma warnings or errors.

## Recognized pragmas

| Pragma | Effect | Scope |
|---|---|---|
| [`strict_types`](#strict_types) | Require explicit function return types and parameter types. | From the directive onward in the file containing it. |
| [`warnings`, `no_warnings`](#warnings-and-no_warnings) | Enable or suppress compiler warnings. | From the directive onward, across textual includes, until the next switch. |
| [`save_types`](#save_types) | Accepted silently; function type information is always retained. | Compatibility spelling only. |
| [`once`](#once) | Skip later includes of the same file. | The file containing it, for the current compilation. |
| [`no_include`](#no_include) | Reject textual inclusion of this file. | The file containing it, including when used as the configured auto-include. |
| [`no_clone`](#no_clone) | Reject cloning and virtual instantiation of this program. | The compiled program; not propagated to inheriting programs. |
| [`no_inherit`](#no_inherit) | Reject explicit inheritance of this program. | The compiled program. |
| [`no_shadow`](#no_shadow) | Reject attaching a shadow to this program's objects. | The compiled program and programs that inherit it. |
| [`resident`](#resident) | Exclude this program's objects from automatic cleanup. | The compiled program, including its clones; not propagated to inheriting programs. |
| [`save_binary`](#save_binary) | Accepted with a warning; no effect. | Compatibility spelling only. |

`no_clone`, `no_inherit`, `no_shadow`, and `resident` apply to the whole
compiled program, regardless of where the directive appears. Declaring one
in an included header also enables it for the program including that header.
`strict_types`, `once`, and `no_include` instead apply to the file containing
the directive.

An `inherit` compiles the parent separately. Of the program flags, only
`no_shadow` propagates to the child; including a header and inheriting a
program therefore have different effects.

## strict_types

```lpc
#pragma strict_types

int add(int left, int right) {
    return left + right;
}

mixed echo(mixed value) {
    return value;
}
```

After this directive, every function definition in the same file needs an
explicit return type, and every parameter needs an explicit type. This also
applies to named closure parameters. Use `void` for a function that returns
no value and `mixed` when a value may have any type.

Without the pragma, omitted function return types and parameter types default
to `mixed`. Normal checking of declared types still takes place without it.
The pragma adds a requirement to write those types explicitly.

The requirement begins at the pragma's source position; earlier declarations
are unaffected. Each file has its own setting:

- An included header does not inherit the including file's `strict_types`.
- A header can enable `strict_types` for its own declarations, without enabling
  it in the including file or in other headers it includes.
- An inherited program's setting does not enable it in the child program.

For example, in a file with no earlier `strict_types` directive:

```lpc
echo(value) { return value; }

#pragma strict_types

mixed typed_echo(mixed value) { return value; }
```

Both definitions compile. Omitting the return type or parameter type from
`typed_echo` produces `Missing return type` or `Missing type for argument`,
respectively.

See also [types](types.md) and [functions and closures](function_type.md).

## warnings and no_warnings

```lpc
void first() { int unused; } // Warns.

#pragma no_warnings
void quiet() { int unused; }

#pragma warnings
void last() { int unused; } // Warns.
```

Warnings are enabled by default. Each switch controls warnings whose primary
source location follows it, even when the compiler detects the problem later.
For example, an unused local follows the setting at its declaration. Earlier
diagnostics are unaffected, and switches can appear inside a function. Macro
expansions use the setting at the invocation.

The setting follows preprocessing order through `#include` and the configured
auto-include. A header inherits the current setting, and any switch inside it
continues to affect the including file after the include returns. Repeated
includes preserve the setting for each occurrence. An `inherit` starts a
separate compilation with warnings enabled; a child's switch does not silence
warnings from its parents, and a parent's switch does not affect the child.

Only compiler warnings are suppressed. Syntax errors, type errors, unknown
pragmas, and internal compiler errors remain errors. Runtime errors and driver
logs are unaffected. Warnings without a source location remain visible; for
example, a root file's Latin-1 decoding warning is reported before any pragma
can be processed. An included file's decoding warning follows the setting at
its `#include` directive.

## save_types

```lpc
#pragma save_types
```

Accepted silently for source compatibility. lpc-rs always retains function
return types and parameter types in compiled programs, including for checking
calls to inherited functions. The pragma enables no additional checking and
does not imply `strict_types`: omitted types still default to `mixed` unless
that separate pragma requires explicit types.

This pragma does not change object save files. There is no `no_save_types`
pragma; type retention cannot be disabled.

## once

```lpc
#pragma once

#define MAX_ITEMS 20
```

Mark the current file as already included. Later `#include` directives that
resolve to the same file skip its contents for the rest of this compilation,
including when different include path spellings resolve to that file.
Compiling another program starts a fresh include-once set.

Place it near the start of a header, before any nested includes. Once the
directive has been reached, a recursive include of that file is skipped;
recursion that reaches the file again before the directive is processed is
still an include-cycle error. The pragma also works in the root source file.

Without `once`, every include processes the file's contents again unless
conditional include guards suppress them. During in-game compilation, the
pragma does not bypass the master's include-read policy: each explicit include
still passes through [`valid_read`](../apply/master/valid_read.md), even when
its contents will be skipped.

## no_clone

```lpc
#pragma no_clone
```

[`clone_object`](../efun/clone_object.md) raises a runtime error when its
blueprint's program has this flag. The driver also refuses to instantiate a
virtual object when the master's
[`compile_object`](../apply/master/compile_object.md) returns a blueprint with
`no_clone`.

The original object can still be loaded normally with
[`load_object`](../efun/load_object.md). This flag also allows inheritance;
the inheriting program can be cloned unless it declares `no_clone` itself.
Use `no_inherit` as well when neither use should be allowed.

Without the pragma, cloning and virtual instantiation are permitted subject
to the usual loading rules and runtime limits.

## no_inherit

```lpc
#pragma no_inherit
```

An explicit `inherit` of this program is a compile error, even if the master's
[`valid_inherit`](../apply/master/valid_inherit.md) would allow it. Loading the
object and cloning it remain possible unless separately restricted.

The driver's automatically injected inherit of the configured
`AUTO_INHERIT_FILE` is exempt. This lets an auto-inherit file forbid explicit
inheritance while still being supplied by the driver. An explicit `inherit`
naming that same file is still rejected. See [configuration](../../default.env).

Without the pragma, explicit inheritance is subject to the normal compiler
and master-policy checks.

## no_shadow

```lpc
#pragma no_shadow
```

[`shadow`](../efun/shadow.md) raises a runtime error when asked to attach a
shadow to an object whose program has this flag. The protection applies to
clones and propagates through inheritance, including indirect inheritance.
The master's [`query_allow_shadow`](../apply/master/query_allow_shadow.md)
cannot override it.

The flag protects the object as a shadow target. It does not prevent that
object from shadowing an otherwise eligible target. Function overrides in
inheriting programs remain allowed; use the `nomask` function modifier to
forbid an override.

Without the pragma, shadowing still requires the master's permission and
must satisfy the other restrictions documented for `shadow`.

## resident

```lpc
#pragma resident
```

Exclude objects made from this program, including clones and virtual objects,
from the driver's automatic idle
[`clean_up`](../apply/object/clean_up.md) queries.
[`request_clean_up`](../efun/request_clean_up.md) returns zero for these objects
and cannot re-enable automatic queries.

Explicit calls to `clean_up()` still work, and authorized
[`destruct`](../efun/destruct.md) calls can still destroy the object. Memory
garbage collection is separate from automatic cleanup; `resident` does not
disable memory collection. The flag does not propagate to programs that
inherit this one.

Without the pragma, initialized objects defining `clean_up` are eligible for
automatic queries under the rules in the apply reference, including the
configured interval and exemptions for the master and simul-efun objects.

Live [object recompilation](../efun/request_object_recompile.md) rejects
changes that add or remove cleanup eligibility. Adding or removing `resident`
on a program with a `clean_up` hook changes that eligibility.

## no_include

```lpc
#pragma no_include
```

The file may be compiled as a program or inherited, but cannot be textually
included. Encountering this pragma while processing a quoted, system, nested,
or configured auto-include fails compilation:

```text
`#pragma no_include` forbids including this file
```

The diagnostic points to the pragma and shows the explicit include chain.
The marked file can still include other headers. The restriction applies only
to the file containing the directive; it does not propagate to those headers
or to inheriting programs. An inactive conditional branch ignores the pragma.
Compiling the marked file normally emits no warning for this pragma.

## save_binary

```lpc
#pragma save_binary
```

Accepted for source compatibility, but has no effect. It does not enable
saving a compiled program to disk. Each occurrence emits the following warning
unless `no_warnings` is active:

```text
pragma `save_binary` has no effect in lpc-rs
```

## Errors

In active source, an unrecognized name is a compile error, reported as
``unknown pragma `name` ``. Only the names listed above are accepted;
alternative spellings and pragmas from other drivers are not silently ignored.

A bare `#pragma`, a trailing comma, space-separated names without a comma,
or additional tokens such as `#pragma strict_types off` are syntax errors.
