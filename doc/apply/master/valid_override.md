# valid_override

`int valid_override(string source, string name, string main_file)`

The driver asks this optional master apply before compiling an explicit
`efun::name(...)` call on LPC's behalf. A truthy result permits the call; a
false result fails compilation with `efun::<name>: permission denied` at the
call's source location. An error thrown by the hook also fails compilation.
Without a master or without this hook, explicit efun calls remain allowed.

- `source` is the canonical in-game file containing the explicit call,
  including its extension. A call written in a header names that header.
  Macro replacement text names the macro's invocation site; expressions passed
  as macro arguments retain their own source locations.
- `name` is the efun's unqualified name, such as `"input_to"`.
- `main_file` is the canonical in-game source being compiled, including `.c`.
  Included text shares that main file. Each inherited program, including a
  configured auto-inherit, is compiled with its own main file.

The check covers known explicit efun calls in function bodies, closures,
initializers and nested expressions, including unreachable code that survives
preprocessing. It applies even when no simul-efun shadows the efun. Ordinary
calls and bare function names do not ask; unknown efuns remain compile errors.
Permission to include or automatically include a header does not exempt the
explicit efun calls in that header.

LPC-triggered loads, `compile_string`, and in-place recompilation use the hook.
Sharing identical compiled code does not skip checks in a new compilation.
Bootstrap compilation of the master and simul-efuns, and standalone host
compilation such as `lpcc`, run without the master compilation gate. Runtime
loads requested during startup, including preloads, use the ordinary gate.
System recompilation consults the old master policy; staged code cannot
authorize itself.

The hook joins the requesting operation's transaction. `this_object()` is the
master, `previous_object()` follows the loading or recompiling caller chain,
and `this_player()` is preserved. Entry reports `origin() == "driver"`.
There is no new object representing the source being compiled yet. Inspect
the source arguments when deciding which code may contain a bypass.

Retries can invoke the hook again. Its writes and deferred output commit with
the caller; an uncaught error discards them. If the caller catches a refusal,
the hook's writes remain in that transaction. `load_object` retains its normal
failure behavior of returning nil instead of throwing a compilation error.

This is compilation policy. Changing it does not revoke calls already compiled
into resident objects or retained closures. Runtime permissions such as
`valid_destruct` still apply when authorized code executes. Control inheritance
of privileged code with `valid_inherit` as well.

For example, allow explicit efuns only in one wrapper program:

```c
int valid_override(string source, string name, string main_file) {
    return source == "/secure/simul_efuns.c"
        && main_file == "/secure/simul_efuns.c";
}
```

### See also

`valid_inherit`, `valid_read`, `valid_load`, `valid_recompile`
