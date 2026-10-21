# Porting Lima to lpc-rs

Lima is an LPC mudlib. This guide describes its port from the reviewed FluffOS
configuration to lpc-rs and the configuration covered by that work. For differences
between driver APIs, use the [driver comparison](../driver-comparison.md).

The port is based on Lima revision
[`eefaff66adcae9b01e16059574c925b0d63e70a1`](https://github.com/limalib/lima/tree/eefaff66adcae9b01e16059574c925b0d63e70a1).
Source changes live in a dedicated Lima checkout on `lpc-rs-port`, paired with
the native driver's `lima-support` branch. The port checkout's
`README.lpc-rs.md` contains build, startup and test instructions.

## Supported profile

The agreed target is the configured fresh-install profile. Other combat models,
contributed packages and unfinished features remain explicit follow-up work.
The port does not claim that every optional Lima feature works.

| Setting | Lima port value |
|---|---|
| Null behavior | `LPC_NULL_MODE=distinct` |
| Call-out player context | `LPC_CALL_OUT_PRESERVE_PLAYER=true` |
| Task execution allowance | `LPC_MAX_EXECUTION_TIME=4000` |
| Object sizing | `USE_MASS` |
| Statistics | `BODY_STATS_LIMA` |
| Death and health | `DEATH_CORPSES`, `HEALTH_LIMBS` |
| Equipment | `WIELD_LIMBS`, `ARMOUR_LIMBS` |
| Combat pulse and damage | `PULSE_HEART_BEAT`, `BLOW_TYPES` |
| Formula and advancement | `FORMULA_SKILLS`, `ADVANCEMENT_SIMPLE` |

The committed `lpc-rs.env` selects `/secure/master.c`, `/secure/simul_efun.c`
and `/include/audit.h`, with no auto-inherit. Run from the port checkout root:
`LPC_LIB_DIR=lib` is relative to the working directory. The compiler loads the
profile with `-c`; the driver uses `-e`. Exported environment values override
profile values. The upstream `lpc-config.json` is editor metadata.

The driver supports other null modes for other mudlibs. Lima's active port
validation uses distinct nil; historical zero-mode runs are not an additional
Lima support requirement. See the native [null reference](../lpc/null.md).

## Adaptation areas

The native language, efun and apply references describe lpc-rs contracts. The
[FluffOS comparison](../driver-comparison.md#fluffos) explains driver-level
migration differences. Lima's adaptations apply those contracts to its own
master, simuls, object model, commands and content.

| Area | Lima port work and references |
|---|---|
| Language and compilation | Typed reference defaults use nil; variadic collectors retain the native type rules; class fields and callbacks use native syntax. See [types](../lpc/types.md), [classes](../lpc/classes.md), [functions](../lpc/function_type.md) and [pragmas](../lpc/pragmas.md). |
| Callback scheduling | Bound functions carry callback data; timer IDs support cancellation; player context follows the configured mode. See [`call_out`](../efun/call_out.md) and the [timer adaptations](lima-port-history.md#lima-event-and-help-timers). |
| Master and simul integration | Source authorization, object creation, command dispatch, input and persistence use the native hook and reflection APIs. See the [master records](lima-port-history.md#lima-master-startup) and [command records](lima-port-history.md#lima-command-dispatch-and-stdio). |
| Object and gameplay foundations | Containers, messages, effects, combat, equipment, skills and commands have coordinated mudlib adapters. Their [dated records](lima-port-history.md#lima-object-foundation) retain individual scope and test results. |
| Networking and bytes | The selected socket wrapper and HTTP/NNTP consumers use native socket and byte APIs. See [TCP sockets](../tcp-sockets.md), the [socket port](lima-port-history.md#lima-tcp-wrapper-and-i3-adaptation) and [HTTP records](lima-port-history.md#lima-http-request-framing-fix). |
| Native saves | The port has native account/body/save-and-restart qualifications. Existing-world FluffOS save/password import is not selected. See the [save format](../save-format.md) and [configured acceptance](lima-port-history.md#lima-configured-acceptance-and-coin-parser-integration). |

### Parser scope

The driver can only parse the objects supplied by the mudlib's scope helper.
Lima's reviewed `deep_useful_inv_parser_formatted` stops at inaccessible
containers. A derived container that exposes visible but inaccessible contents
needs a helper that includes them for `:v` rules. This is a mudlib scope contract;
the driver cannot restore objects omitted from its input. See the
[reviewed helper and container](lima-port-history.md#lima-parser-scope-visibility).

## Recorded validation

These are dated results, not checks automatically rerun when this guide changes.
The [history](lima-port-history.md) retains source revisions, test scope and
evidence locations. Runtime acceptance and standalone compilation measure
different things.

| Qualification | Recorded result | Scope |
|---|---|---|
| Configured lifecycle, 20 October 2026 | 30 lifecycle checkpoints and 69 coin/parser/native-save checks; 151 clean preloads per boot | Fresh account and character creation, ordinary commands, native saves, saved-password restart and wallet restoration across two processes. |
| Shipped suites in that acceptance | Items 26, limbs 16, liquids 7, race 5, salvage 5; normal and quiet combined runs each 59/59 | Per-suite commands first, with no manual program loads. |
| Whole-source inventory, 21 October 2026 | 1021/1182 compile; 161 standalone failures; no regressions, timeouts or crashes | Compile-only sweep after the handoff's two EOF blank-line removals; target constructors omitted. |
| Native implementation at `c0c7fe8b` | 5198 serial workspace tests pass, two ignored; ten doctests and build/Clippy/docs/fmt pass | Later native commits through the handoff change comparison documentation; release binary hashes match this record. |

The inventory's 161 failures are all classified:

- **113 host-dependent sources** have **47 complete shipped hosts** passing in
  that inventory. A fragment's isolated failure does not mean its complete
  object is unusable.
- **48 residual files** have explicit optional-provider, integration,
  unfinished-feature or deferred-command decisions. They are outside the agreed
  configured-profile target.

The inventory is not a port-completion percentage. See the
[remaining-source ledger](lima-port-history.md#lima-current-remaining-source-ledger)
and [review handoff](lima-port-history.md#lima-configured-profile-review-handoff).

## Optional follow-ups

| Residual disposition | Files |
|---|---:|
| Qualified optional complete-host compositions | 3 |
| Alternative providers with bounded adaptations | 7 |
| Alternative combat providers needing integration | 7 |
| Retained older provider choices | 6 |
| Unselected board packages | 10 |
| Contributed body/schema/shared-state integration | 6 |
| Unfinished subsystems/dependent examples | 5 |
| Deferred or upstream-disabled commands | 2 |
| Unselected incomplete legacy backends | 2 |
| Total | 48 |

Bounded optional adaptations keep their individual limits. NPC vaporization
does not establish player ghost routing; scalar blows do not qualify every
type-specific consumer; Rifts advancement retains its PE/stat-pairing and
original lower-level HP-cache issues. Alternative HP/equipment and formula
models require coordinated mudlib integration. Marriage, shared-state wrappers,
older backends and unfinished contributions are not selected by this port.

Any future expansion should name its model or feature and acceptance contract.
Existing-world import and the inherited-method signature/cache design discussion
remain separate. The residual list is not an automatic audit queue.

## Port record and maintenance

The [port history](lima-port-history.md) preserves the investigations and
qualifications previously embedded in the driver comparison. Its old pending
actions, branch heads and test counts remain historical; this guide records the
agreed target and current follow-up boundary.

Keep this guide focused on the supported configuration, required adaptations,
qualification and limitations. Personal investigation logs and handoffs belong
under `local/`; disposable scripts and diagnostic output belong under `/tmp/`.
Driver-specific comparisons stay in the driver comparison, with canonical API
details in the efun, apply and language references.
