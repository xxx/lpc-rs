# parse_string

`mixed *parse_string(string grammar, string str, int alternatives = 0)`

Parses `str` under `grammar` and returns the parse as a flat array, or 0 when
`str` is not a sentence of the grammar — including when it cannot be broken
into tokens. `alternatives` must be 0; any other value is a runtime error (see
Departures).

### The grammar

Token rules and production rules in any order, whitespace-separated; a rule
ends where the next `name =` or `name :` begins, so newlines carry no meaning.
Names are identifiers as in LPC.

A token rule is `name = /regexp/` or `name = nomatch`. The regexp dialect:

| syntax | matches |
|---|---|
| `c` | the character `c`, for any `c` but `. [ \ * + ? ( ) \| /` |
| `\c` | the character `c`, including those |
| `.` | any single character, newline included |
| `[set]`, `[^set]` | a character in, or not in, the set: single characters and ranges like `a-z` (ascending); `\` escapes `]`, `^`, `-`, `\` |
| `a*` `a?` `a+` | zero or more, zero or one, one or more `a` |
| `ab` | `a` then `b` |
| `a\|b` | `a` or `b` |
| `(a)` | grouping |

`]` outside a set is a literal.

At each position the longest match across every rule wins; on a tie the rule
written first does. The name `whitespace` is special: its matches are dropped
from the token stream. Several rules may share a name, `whitespace` included.
A `nomatch` rule takes every run of characters from a position no other rule
matches up to the next position one does; without it, such a position makes
the parse fail. Input is matched character by character, so `.` and `[^x]`
consume a whole UTF-8 character.

A production rule is `name : rhs`, `rhs` zero or more symbols — token names,
production names — and `'string constants'` (`\'` for a quote inside). A string
constant matches that text exactly and takes precedence over any regexp rule of
the same length. The first production rule's name is the start symbol;
production rules unreachable from it are ignored, though their string
constants still take part in tokenization, and every symbol a reachable
rule uses must be defined. Matching is case-sensitive.

A production may end in `? func`: a semantic action, described below. A
`< func` before it is accepted and has no effect.

### The result

A token contributes its text as a string. A production without an action
contributes its children's values, in order, in place; so with no actions at
all the result is the list of tokens `str` was broken into, `whitespace` left
out.

A production with `? func` calls `func(mixed *tree)` in the object that called
`parse_string`, with `tree` the values the production's right-hand side
produced. Actions run bottom-up, left to right, and each distinct subtree runs
its action once per call. If `func` returns an array, its elements take the
production's place in the parent's values (an array inside it stays nested).
If it returns anything else, or the object has no such function, that
derivation is rejected and the next one is tried; the parse fails only when
every derivation is rejected. A runtime error inside an action ends the call
with that error. An action may itself call `parse_string`.

An ambiguous grammar's derivations are tried in the engine's order: for one
symbol, its productions in the order written; for a symbol inside a rule, the
longest span first.

### Example

```c
// An arithmetic evaluator: actions fold each subtree to its value.
string grammar = "
    whitespace = /[ \t]+/
    number = /[0-9]+/
    Expr: Term
    Expr: Expr '+' Term ? add
    Expr: Expr '-' Term ? subtract
    Term: Factor
    Term: Term '*' Factor ? multiply
    Factor: number ? value
    Factor: '(' Expr ')' ? group
";

mixed *value(mixed *tree) { int n; sscanf(tree[0], "%d", n); return ({ n }); }
mixed *add(mixed *tree) { return ({ tree[0] + tree[2] }); }
mixed *subtract(mixed *tree) { return ({ tree[0] - tree[2] }); }
mixed *multiply(mixed *tree) { return ({ tree[0] * tree[2] }); }
mixed *group(mixed *tree) { return ({ tree[1] }); }

mixed *evaluate(string s) { return parse_string(grammar, s); }   // "2 + 3 * (4 - 1)" -> ({ 11 })
```

### Errors

All raised as runtime errors prefixed `parse_string: `:

| condition | text |
|---|---|
| `alternatives` not 0 | `alternatives are not supported` |
| a fault in rule N | `Rule N: regular expression expected`, `Rule N: malformed regular expression`, `Rule N: malformed string constant`, `Rule N: function name expected`, `Rule N: extra nomatch rule`, `Rule N: unexpected token`, `Rule N: bad token` |
| a name used both ways | `Rule N previously defined as token rule`, `Rule N previously defined as production rule` |
| no token rule / no production rule | `No tokens` / `No starting rule` |
| a reachable symbol defined nowhere | ``nonterminal `name` has no production`` |
| a regexp the engine cannot compile | the engine's message |
| the parse ran past its budget | `parse budget exhausted` |

An error raised inside an action is that action's error, unprefixed.

### Limits

Each call pulls at most 64 derivations before giving up, and spends at most
2²⁰ steps (chart items plus derivation nodes) on the parse itself — the tick
limit bounds the actions, but not the parse. The 64 most recently used grammar
texts stay compiled; a text not among them is compiled again on its next use.

### Departures from DGD

| DGD | here |
|---|---|
| `nil` when nothing parses | 0 |
| `alternatives` keeps N extra alternatives per branch point, nested as sub-arrays | 0 only; anything else is an error |
| alternatives ordered by the topmost differing rule | the engine's order: productions as written, longest span first |
| the grammar is compiled per object, one at a time | one driver-wide cache of 64 texts |
| automata built incrementally per sentence seen | built whole, once per text |
| `Rule N is too long`, `Grammar too large`, `regular expression too large` | no text-size limits (groups nest at most 250 deep); the parse budget bounds the work |
| a runaway parse is bounded by ticks | the step budget |
| bytes | UTF-8 characters |
| `< func` stored | accepted, ignored |
| not callable from a special-purpose object | no such objects |

### See also

`parse_command`, `sscanf`, `explode`
