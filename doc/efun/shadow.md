# shadow

`object shadow(object ob, int flag = 1)`

With `flag` true, the calling object becomes the outermost shadow of `ob`
and `ob` is returned. From then on every external call by name to `ob` —
`ob->f()`, a collection call, a `&->f()` pointer, `present`'s `id` probe, a
driver hook such as `catch_tell` — starts at the outermost shadow and falls
inward to the first object defining a public `f`, ending at `ob` itself.
The applies the connection makes on its body — `net_dead`, `gmcp`,
`window_size`, `write_prompt` — enter it the same way.
Calls made inside `ob`'s own code stay inside `ob`; `this_object()->f()`
does not. `this_object()` in the function that runs is the object it
belongs to and `previous_object()` is the real caller.

With `flag` 0, the object directly outside `ob` in its chain: for a
shadowed object its innermost shadow, for a shadow the next one out, 0 when
there is none — so `while (ob = shadow(ob, 0))` walks a chain inner to
outer.

A shadow is one object attached to one target. Every refusal is a runtime
error, `shadow '<caller>' on '<ob>': <reason>`:

- the caller already shadows something, or is itself shadowed;
- the caller has an environment (and a shadowing object cannot be moved);
- `ob` is a shadow, or the caller itself;
- `ob` is the master or the simul-efun object;
- `ob`'s program declares `#pragma no_shadow`, or inherits one that does;
- the caller defines a function `ob` marks `nomask`;
- the master defines no `query_allow_shadow` — shadowing is off until it
  does;
- `query_allow_shadow(ob)` answers a false value.

A lib that wants a refusal as an outcome rather than an error asks its own
question first, or wraps the call in `catch`.

Destructing a shadow closes the chain around it. Destructing the target
leaves its shadows alive, shadowing nothing. Function pointers bound to a
function (`&ob->f()`, `papplyv`, `call_out` and `add_action` handlers) keep
that function and never enter a chain.

### See also

`query_allow_shadow`, `previous_object`, `nomask`
