# Macros

`#define` substitutes tokens before LPC is parsed. Definitions are available
from their declaration onwards, including definitions in included headers.
`#undef NAME` removes a definition.
Repeating an identical definition is allowed; changing parameter names, token
spelling, or the positions of whitespace requires `#undef` first. Whitespace
amounts and comment text do not affect this comparison.

LPC keywords can be macro names or parameters: `#define nosave static` is
valid, including in included headers. These names expand in ordinary source,
macro bodies and arguments, and work in `#ifdef`, `#ifndef`, `defined()` and
`#if` expressions. Quoted text is unaffected.

```c
#define MAX_ITEMS 20
#define DOUBLE(x) ((x) * 2)
```

The `(` in a function macro's definition must immediately follow its name.
`#define DOUBLE (x)` instead defines an object macro whose replacement is `(x)`.
A function macro expands only when its name is followed by `(` at the use site;
whitespace between that name and `(` is allowed.

Arguments are separated by commas outside nested parentheses. Empty arguments
are permitted. Ordinary parameter uses are macro-expanded before substitution;
unused arguments are not expanded. A macro cannot keep expanding itself on the
same expansion path.

## Predefined macros

These reserved names expand to literals:

| Name | Type | Value |
|---|---|---|
| `__FILE__` | `string` | The current source's absolute in-game filename, including its extension. |
| `__DIR__` | `string` | That file's in-game directory, including the trailing `/`; a file at the mudlib root yields `/`. |
| `__VERSION__` | `string` | `lpc-rs ` followed by the compiler package version, for example `lpc-rs 0.1.0`. |
| `__PORT__` | `int` | The compilation configuration's primary plaintext listener port, default `24960`. |

For `/areas/example/room.c`:

```c
inherit __DIR__ "base"; // Inherits /areas/example/base.c.

string source_file() { return __FILE__; } // "/areas/example/room.c"
string driver_version() { return __VERSION__; }
int service_port() { return __PORT__ + 3; }
```

`__FILE__` and `__DIR__` follow the source currently being preprocessed.
Inside an included header they name the header; after it ends they again name
the including file. A macro defined in a header evaluates these names at its
invocation site, even through aliases and nested macro calls. Inherited code
keeps the values from its own compilation.

Paths use the same normalized in-game names as compiler diagnostics. For a
host-facing compile outside the configured mudlib, both path macros yield the
opaque label `<outside mudlib>`.

All four names are defined from the start, including in auto-includes.
`#ifdef`, `#ifndef`, and `defined(NAME)` recognize them. Redefining or undefining
them in active source is a compile error. In `#if` and `#elif`, the string macros
follow the existing string-expression rules: truthy in boolean positions, with
no integer arithmetic or string comparison support. `__PORT__` supports integer
arithmetic and comparisons; a configured zero is defined but false in boolean
positions.

`__PORT__` reads `LPC_PORT` (or its `PORT` alias) from the compiler configuration;
`LPC_PORT` takes precedence when both are set. It remains available when plaintext
Telnet is disabled and does not report a TLS port or an actual bound socket.
A configured zero expands to zero, even if a later listener binds an ephemeral
port. Each compiled object retains its literal value until recompiled, including
when executed with a different runtime configuration.

They expand in LPC source and macro replacements, excluding quoted literals,
comments, and inactive conditional branches. `#include` still requires a
literal quoted or angle-bracket path. `__VERSION__` identifies the
native driver build; mudlibs must assess required driver features separately.

## Stringification

Inside a function macro's replacement, `#parameter` converts that argument's
unexpanded tokens into one string literal.

```c
#define STR(x) #x
#define STRINGIZE(x) STR(x)
#define VALUE 0x2A

string name = STR(VALUE);          // "VALUE"
string spelling = STRINGIZE(VALUE); // "0x2A"
string expression = STR(a + b);    // "a + b"
```

The wrapper expands `VALUE` before passing it to `STR`. Direct `STR(VALUE)`
preserves the name. A parameter used both normally and with `#` supplies its
expanded tokens to the ordinary use and its original text to the string use.
The same rule applies to predefined macros: `STR(__FILE__)` produces
`"__FILE__"`, while `STRINGIZE(__FILE__)` produces the quoted filename as text.

Leading and trailing whitespace is removed. Internal whitespace and comments
between tokens become a single space; adjacent tokens remain adjacent.
`STR(a+b)` therefore produces `"a+b"`, while `STR(a /* comment */ b)` produces
`"a b"`. `STR()` produces the empty string.

Literal spelling is preserved: hexadecimal numbers stay hexadecimal, and
quoted literals keep their quotes and escape sequences as text in the result.
For example, `STR("a\n")` preserves both quotes and the literal backslash
followed by `n` in the result.

Adjacent string literals join normally, allowing macros to build paths:

```c
#define MODULE(group, name) "/std/combat/" group "/" #name
string module = MODULE("death", corpse); // "/std/combat/death/corpse"
```

`#` must be followed by one of the macro's parameter names; invalid operands
are rejected at definition time. Token pasting with `##` is not supported.

## Expansion limits

One top-level expansion has a nesting limit of 256 and a token-work limit of
65,536. Replacement construction and rescanning both count toward token work.
The total text generated by stringification within that expansion is limited
to 1,048,576 bytes, including intermediate strings. Exceeding a limit reports
a compilation error at the macro use.
