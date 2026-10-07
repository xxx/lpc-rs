# Comparing lpc-rs with other LPC drivers

This reference describes differences relevant to choosing a driver or estimating
work on a mudlib port. It covers lpc-rs, CD, LDMud, FluffOS and DGD. Shared LPC
syntax and efun names do not establish compatibility: argument order, callback
context, return values and side effects can differ.

Start with the overview, then the notes for the driver you are coming from:
[CD](#cd), [LDMud](#ldmud), [FluffOS](#fluffos) or [DGD](#dgd).
The shared [`explode` examples](#explode) cover a string-handling gotcha across
all four. [Stored password migration](#stored-passwords) applies to any source
using the supported hash formats. This page is the home for porting advice;
the linked efun, apply, and language references describe native contracts.

## Scope and evidence

The lpc-rs column describes the accompanying code. Upstream sources were reviewed
on **19 September 2026**, against these public snapshots. The revisions identify
the source examined; they are not claims about every release or downstream fork.
Timer ID allocation in CD and FluffOS was additionally reviewed on
**21 September 2026** at the same revisions; it is independent of their optional
callback-player-context settings.
Global timer inspection in FluffOS was source-reviewed on **22 and 24 September 2026**
at the listed revision, with its core call-out efuns enabled. The inspected
implementation returns three fields per timer; no cross-driver execution was
performed. [Implementation][fl-callouts], [declaration][fl-core-spec].
Call-out player preservation was source-reviewed on **28 September 2026** at
these same revisions: FluffOS with its core call-out efuns and `this_player in
call_out` enabled (the runtime default), LDMud's call-out reference, and DGD's
`this_user` reference. FluffOS's shadow traversal is conditional on `NO_SHADOWS`;
its fallback requires an `O_LISTENER` owner. No upstream execution is claimed.
[FluffOS implementation][fl-callouts], [runtime default][fl-runtime-settings],
[LDMud reference][ld-callout-context], [DGD reference][dgd-callout-user].
Timer argument forwarding and lookup were additionally reviewed on
**26 September 2026** at the listed FluffOS revision. These are core efuns;
[Lima's configuration][lima-timer-config] enables `this_player in call_out`.
The event/help adaptations below were exercised on lpc-rs,
without FluffOS execution or a timing comparison.
[Scheduling][fl-callout-reference], [lookup][fl-find-callout].
FluffOS snooping was source-reviewed on **24 September 2026** at the listed
revision, with `NO_SNOOP` undefined and `receive snoop` enabled (as in the
reviewed Lima configuration). Callback and relationship behavior below comes
from source review, not cross-driver execution. [Connection code][fl-snoop].
FluffOS whole-object enumeration was source-reviewed on **26 September 2026**
at the listed revision, with core `F_OBJECTS` enabled. Visibility additionally
depends on `F_SET_HIDE`, and result size on runtime `__MAX_ARRAY_SIZE__`. These
are source findings, not cross-driver execution. [Declaration][fl-core-spec],
[implementation][fl-objects-implementation], [visibility][fl-object-enumeration].
The FluffOS `clonep` reference was reviewed on **26 September 2026** at the
same revision, with the core efun enabled; no cross-driver execution was
performed. [Reference][fl-clonep], [declaration][fl-core-spec].
The Lima object-caller and state-scheduling adaptations below were reviewed on
**26 September 2026** at its listed revision and exercised on lpc-rs.
No FluffOS execution was performed.
Lima's command base and stdio adaptations were also reviewed on that date at
its listed revision, with `SENSIBLE_MODIFIERS` enabled and function profiling
disabled. The four object commands were exercised with the adapted master and
simul-efuns, using fixture shell/user objects. This is not a
complete shell or Lima boot. The coordinated pipeline adaptation was exercised
on the same date using its actual parser module and fixture command lookup.
[Command base][lima-command-base], [stdio][lima-command-stdio],
[shell][lima-wish], [tokenizer][lima-shell-getopt].
FluffOS clone enumeration was source-reviewed on **24 September 2026** at the
listed revision. The core efun has no optional-package requirement. Filename
grouping and insertion order come from source review, with no cross-driver
execution. [Declaration][fl-core-spec], [object table][fl-children-table].
FluffOS master lookup and replacement during destruction were source-reviewed on
**24 September 2026** at the listed revision. These core paths require no optional
package; no cross-driver execution was performed. [Getter][fl-master-getter],
[destruction][fl-master-destruct].

FluffOS object restoration and saved-string encoding were source-reviewed on
**26 September 2026** at the listed revision. These are core persistence paths;
compressed saves additionally require `HAVE_ZLIB`. Lima's shared daemon module
uses uncompressed saves. The port was exercised on lpc-rs;
no FluffOS execution was performed. [Restore reference][fl-restore-object],
[persistence implementation][fl-object-persistence].

FluffOS file metadata and line counting were source-reviewed on
**26 September 2026** at the listed revision. `stat` is core; `file_length`
requires `PACKAGE_CONTRIB`, enabled by default in the reviewed build. Native
tests exercise adapted Lima callers and extracted functions; no FluffOS
execution was performed. [Metadata][fl-stat-source], [line counting][fl-file-length],
[directory results][fl-get-dir-source].

FluffOS stack inspection and function-pointer owner frames were source-reviewed
on **24 September 2026** at the listed revision. These are core paths without
optional package requirements; no cross-driver execution was performed.
[Stack reference][fl-call-stack], [pointer dispatch][fl-pointer-dispatch].

FluffOS class assembly/disassembly and byte-file reads were source-reviewed on
**24 September 2026** at the listed revision. Class conversion requires
`PACKAGE_CONTRIB`; `read_bytes` is a core efun and checks the configured maximum
byte transfer. No cross-driver execution was performed.
[Class conversion][fl-class-assembly], [file reads][fl-byte-read-range].

FluffOS text encoding was source-reviewed on **24 September 2026** at the listed
revision, with core `string_encode` / `string_decode` and ICU converters.
Lima's JSON simul helpers were reviewed at its pinned revision below; they are
mudlib code, not the FluffOS JSON package. Runtime comparisons exercised the
adapted Lima helpers and native efuns on lpc-rs, without cross-driver execution.
[Encoding source][fl-text-encoding], [Lima helpers][lima-json].

Mixed numeric comparisons were source-reviewed on **24 September 2026** at
that FluffOS revision. The core operators use native C++ int/float comparison,
which promotes the integer; no optional package is required. This is source
review, with no cross-driver execution. [Operators][fl-numeric-comparisons].

FluffOS rounding was source-reviewed on **24 September 2026** at the listed
revision with `PACKAGE_MATH` enabled. The declaration and implementation were
reviewed without cross-driver execution. [Declaration][fl-math-spec],
[implementation][fl-round].
FluffOS constructor argument forwarding was also reviewed on **21 September 2026**
at the listed revision; its reference specifies no optional build requirement.
FluffOS warning and type-retention pragmas were reviewed on **21 September 2026**
at the listed revision, with the checked-in `DEFAULT_PRAGMAS` enabling warnings
and saved types. [Build defaults][fl-options].
FluffOS's predefined source macros were also reviewed on **21 September 2026**:
the path macros are unconditional, and `__VERSION__` comes from the build's
`PROJECT_VERSION`. [Expansion][fl-predefined], [registration][fl-pragma-lexer].
The configured `__PORT__` macro was reviewed on **4 October 2026** at
`44532e0228f34be2184c701a361df4079371b8b0`: it expands numerically from
`external_port[0].port`, without an optional-package guard. This is source review,
with no FluffOS execution. [Registration][fl-configured-port].

Inheritance visibility was reviewed on **21 September 2026** against the same
FluffOS revision, with `SENSIBLE_MODIFIERS` enabled. This is source review;
inheritance execution tests in this change exercise lpc-rs only. Basename
qualification was also reviewed on **21 September 2026** at that revision;
`arrange_call_inherited` has no optional build requirement. [Source][fl-pragma-compiler].

Reverse indexing was source-reviewed on **21 September 2026** at the same
FluffOS revision, with `old range behavior` set to `0`, as in the reviewed Lima
configuration. The setting affects ordinary negative string/buffer bounds;
explicit reverse bounds have their own handling. No cross-driver execution
was performed. [Range implementation][fl-ranges], [index implementation][fl-indexing].

Array slice assignment was source-reviewed on **23 September 2026** at the same
FluffOS revision, with no optional build requirement. The review covers the two
array assignment paths; tests here execute lpc-rs only. [Implementation][fl-slice-assignment].

String slice assignment was source-reviewed on **23 September 2026** at the same
revision, using core UTF-8/ICU string handling without an optional efun package.
The review covers string detachment, range lvalues and replacement; no FluffOS
execution was performed. [Detachment][fl-string-unlink], [ranges][fl-string-write-ranges].

Indexed string assignment was source-reviewed on **23 September 2026** at the
same revision, using core UTF-8/ICU handling without an optional efun package.
The review covers character lvalues and replacement; no FluffOS execution was
performed. [Lvalues][fl-string-index-lvalue], [replacement][fl-string-index-replacement].

Named variadic parameters were source-reviewed on **21 September 2026** at
the same FluffOS revision. The declaration and argument packing paths have no
optional build requirement. No cross-driver execution was performed.
[Declarations][fl-variadic-declarations], [flags][fl-inherit-grammar],
[argument packing][fl-variadic-runtime].

Class visibility modifiers were source-reviewed on **23 September 2026** at
the listed FluffOS revision. The grammar accepts the modifiers but does not pass
them to class creation; inherited class copying does not filter visibility.
These paths have no optional build requirement. No cross-driver execution was
performed. [Grammar][fl-class-grammar], [declarations][fl-class-declarations],
[inheritance][fl-class-inheritance].

Array literal spreading was source-reviewed on **23 September 2026** at the
listed FluffOS revision. The grammar and expression-list generation have no
optional build requirement. No cross-driver execution was performed.
[Grammar][fl-parameter-grammar], [code generation][fl-array-spread-codegen].

Trailing call commas were source-reviewed on **23 September 2026** at the listed
FluffOS revision. The argument-list grammar permits one after a nonempty list,
with no optional build requirement. No cross-driver execution was performed.
[Grammar][fl-call-arguments].

`nosave` was source-reviewed on **21 September 2026** at the same FluffOS
revision with `SENSIBLE_MODIFIERS` enabled, as in Lima. That option enables the
keyword; the legacy `static` spelling is reserved in the opposite build.
This is source review, with no cross-driver execution. [Keywords][fl-nosave-keywords],
[storage][fl-nosave-storage], [function modifiers][fl-inherit-grammar].

Explicit closure captures were reviewed on **21 September 2026** against the
listed FluffOS function reference; it states no optional build requirement for
`$(expression)`. The evaluation-time comparison is source-reviewed, with no
cross-driver execution. [Function reference][fl-functions].

Unnamed prototype parameters were source-reviewed on **21 September 2026** at
the listed FluffOS revision. The grammar accepts a type with an optional array
marker and no name, without an optional build requirement. No cross-driver
execution was performed. [Grammar][fl-parameter-grammar],
[parameter declarations][fl-variadic-declarations].

The `foreach` separator was source-reviewed on **22 September 2026** at the
listed FluffOS revision. Its reference uses `in`, and the corresponding grammar
rule has no optional build requirement. No cross-driver execution was performed.
[Reference][fl-foreach], [grammar][fl-parameter-grammar].

Block-form `catch` was source-reviewed on **22 September 2026** at the listed
FluffOS revision. Its grammar and jump restrictions have no optional build
requirement. No cross-driver execution was performed.
[Reference][fl-catch], [grammar][fl-parameter-grammar], [jumps][fl-catch-jumps].

Deep copying was source-reviewed on **22 September 2026** at the listed FluffOS
revision, with `PACKAGE_CONTRIB` enabled as in the reviewed build defaults.
No cross-driver execution was performed. [Implementation][fl-copy],
[declaration][fl-contrib-spec], [build options][fl-packages].

Array and mapping predicates were source-reviewed on **24 September 2026**
at the listed FluffOS revision. Both are core efuns without optional package
requirements; no cross-driver execution was performed.
[Declarations][fl-core-spec], [implementations][fl-type-predicates].

Mapping deletion and mapping callbacks were source-reviewed on
**26 September 2026** at the listed FluffOS revision. They are core efuns
without optional package requirements. Lima adaptations were exercised on
lpc-rs; no cross-driver execution was performed.
[Deletion][fl-map-delete], [callbacks][fl-mapping-transform].

Type inspection was source-reviewed on **24 September 2026** at the listed
FluffOS revision. `typeof` is a core efun without an optional-package
requirement; its string result was reviewed without cross-driver execution.
[Reference][fl-typeof], [implementation][fl-typeof-source].

Regex association was source-reviewed on **24 September 2026** at the listed
FluffOS revision. `reg_assoc` is core; `pcre_assoc` requires `PACKAGE_PCRE`.
The result layout, pattern selection and suffix searches were source-reviewed;
no cross-driver execution was performed. [Reference][fl-reg-assoc],
[core implementation][fl-reg-assoc-source], [PCRE implementation][fl-pcre-assoc].

Array shuffling was source-reviewed on **24 September 2026** at the listed
FluffOS revision, with `PACKAGE_CONTRIB` enabled. The in-place permutation,
array identity and underlying random bounds were reviewed without cross-driver
execution. [Reference][fl-shuffle], [implementation][fl-shuffle-source],
[declaration][fl-shuffle-spec], [random entry point][fl-random].

The core indexing and length operations used by random-selection helpers were
source-reviewed on **24 September 2026** at the listed FluffOS revision, using
its UTF-8/ICU handling without an optional efun package. Empty-input and string
result behavior below comes from source review, not cross-driver execution.
[Length][fl-grapheme-length], [indexing][fl-string-read],
[UTF-8 helper][fl-string-read-scalar].

Literal search and replacement were source-reviewed on **22 September 2026**
at the listed FluffOS revision. Both are core efuns without optional package
requirements; replacement uses the configured maximum string length. The review
includes the Unicode search helper and upstream tests, with no FluffOS execution.
[Search][fl-strsrch-source], [replacement][fl-replace-string-source],
[Unicode helper][fl-string-search], [declarations][fl-core-spec].

String overloads of `member_array` and `map` were source-reviewed on
**23 September 2026** at the listed FluffOS revision, with core efuns enabled
and no optional package requirement. These paths operate on bytes despite the
driver's grapheme-aware indexing. No cross-driver execution was performed.
[Search][fl-member-string], [mapping][fl-map-string], [declarations][fl-core-spec],
[search tests][fl-member-tests], [mapping tests][fl-map-tests].

Array `map_array` / `filter_array` aliases and callback behavior were
source-reviewed on **24 September 2026** at the listed FluffOS revision.
These are core efuns without optional package requirements. The review covers
argument forwarding, empty arrays, predicate results and input mutation;
no cross-driver execution was performed. [Declarations][fl-core-spec],
[map loop][fl-map-array], [filter loop][fl-filter-array],
[callback dispatch][fl-array-callbacks], [zero test][fl-filter-zero].

String repetition was source-reviewed on **24 September 2026** at the listed
FluffOS revision, with `PACKAGE_CONTRIB` enabled. Its output cap uses the
configured maximum string length. No cross-driver execution was performed.
[Implementation][fl-repeat-string], [declaration][fl-repeat-spec],
[build options][fl-packages].

`evaluate` was source-reviewed on **22 September 2026** at the listed FluffOS
revision. Its core declaration and implementation require no optional package.
No cross-driver execution was performed. [Reference][fl-evaluate],
[implementation][fl-evaluate-source], [declaration][fl-core-spec].

`origin` and pointer dispatch were source-reviewed on **22 September 2026** at
the listed FluffOS revision. The core efun requires no optional package.
This is source review, with no cross-driver execution.
[Reference][fl-origin], [result strings][fl-origin-source],
[pointer dispatch][fl-pointer-dispatch], [declaration][fl-core-spec].

Null semantics were source-reviewed on **22 September 2026** at the listed
FluffOS and DGD revisions. FluffOS's core `undefinedp`/`nullp` uses its
undefined-number marker; DGD's distinct nil depends on runtime `typechecking=2`.
No cross-driver execution was performed. [FluffOS predicate][fl-undefined],
[DGD configuration][dgd-nil-config], [DGD values][dgd-nil-values].

Value serialization was source-reviewed on **22 September 2026** at the listed
FluffOS revision. `save_variable` and `restore_variable` are core efuns without
optional package requirements. This review covers their documented string API
and declarations; cross-driver save-data interchange was not tested.
[Save reference][fl-save-variable], [restore reference][fl-restore-variable],
[declarations][fl-core-spec].

Named global access was source-reviewed on **22 September 2026** at the listed
FluffOS revision, using its `PACKAGE_CONTRIB` implementation and
`SENSIBLE_MODIFIERS` visibility flags. The reference's public-only description
is narrower than the implementation's private-only foreign-access restriction.
No FluffOS execution or general `bind` compatibility test was performed.
[Variable operations][fl-named-globals], [lookup][fl-global-lookup],
[modifiers][fl-global-flags].

Character input was source-reviewed on **22 September 2026** at the listed
FluffOS revision. `get_char` is a core efun without an optional package
requirement. The review covers its reference, declaration and classic SGA
mode negotiation; RFC LINEMODE behavior and cross-driver execution were not
tested. [Reference][fl-get-char], [declaration][fl-core-spec],
[mode negotiation][fl-input-mode].

Explicit efun authorization was source-reviewed on **22 September 2026** at
the listed FluffOS revision. The compiler hook has no optional build requirement;
individual efun availability still depends on the build. Source/header identity
and denial behavior were reviewed, without cross-driver execution or a claim
that all bootstrap and missing-hook defaults match. [Reference][fl-valid-override],
[compiler check][fl-valid-override-source].

The consolidated parser, array-predicate, `sscanf`, and colour-expansion notes
were reviewed on **22 September 2026** at these same revisions. CD's parser
notes use the checked-in English implementation without `PARSE_FOREIGN`.
FluffOS parser notes require `PACKAGE_PARSER`; colour expansion requires
`PACKAGE_CONTRIB`. Parser debugging additionally needs `DEBUG` or `PARSE_DEBUG`.
Successful rule selection, failure reporting, and two-object permission checks
were also reviewed against that FluffOS parser revision on **22 September 2026**,
with `PACKAGE_PARSER`.
The DGD parser review uses its source and separately pinned LPC documentation.
Unverified details are marked below; no cross-driver execution was performed.

| Driver | Reviewed revision |
|---|---|
| CD | [03a98d78](https://github.com/cotillion/cd-gamedriver/tree/03a98d78db215e40d33642828aad7a43a65e35f7) |
| LDMud | [e1cbe804](https://github.com/ldmud/ldmud/tree/e1cbe804944c1f5865ad1beeeff220b43c48c7e3) |
| FluffOS | [2c272875](https://github.com/fluffos/fluffos/tree/2c27287500daf48a87a1f89d69a97d8b9603028d) |
| DGD | [733ea01e](https://github.com/dworkin/dgd/tree/733ea01eeeb571721c35511cb7f51a7f9d32fb6c), with its LPC documentation at [4bf768ac](https://github.com/dworkin/lpc-doc/tree/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4) |

Unless labelled not checked, entries below are **source-reviewed**: compared with
documentation or implementation, with links to the evidence. No cross-driver execution tests
were run for this page. **Not checked** means no conclusion has been reached;
**not implemented** describes a known absence in the reviewed implementation.
Optional packages, compiler defines and runtime modes can change an upstream
build's behaviour; relevant conditions are stated alongside the feature.
Support implemented in a mudlib or optional package counts too, with that
requirement stated. Disabled by default does not mean unsupported, and a feature
omitted from a selected inventory should not be read as absent.

FluffOS is compared at its own revision, rather than treated as interchangeable
with historical MudOS. DGD's separately distributed multicore relative Hydra is
outside this comparison; the [DGD README][dgd-readme] distinguishes them.

String trimming was source-reviewed on **26 September 2026** at the listed
revisions: FluffOS's `PACKAGE_TRIM` (enabled by default), and LDMud's reference
and header. No cross-driver execution was performed. [FluffOS wrapper][fl-trim],
[Unicode helper][fl-trim-helper], [build option][fl-trim-build],
[LDMud reference][ld-trim], [header][ld-trim-header].

## Overview

These cells describe selected capabilities, not complete feature inventories.
A source link supports each reviewed upstream claim. The
[protocol table](#client-protocols-and-transports) compares individual protocols
across all five drivers; the broader rows describe differences within each area.

| Area | lpc-rs | CD | LDMud | FluffOS | DGD |
|---|---|---|---|---|---|
| Execution | Parallel LPC tasks; snapshot reads, atomic commits and conflict retries. [Runtime](../README.md#how-execution-works); [monotonic elapsed VM uptime](efun/uptime.md). | Tasks evaluated serially by the [backend loop][cd-backend]. | Sequential [backend evaluation][ld-backend]; explicit [coroutines][ld-async]. | Event-driven LPC; [async functions and promises][fl-async], with worker-thread I/O in the async package. | Explicit [atomic functions][dgd-lpc] roll back on errors; multicore acceleration is a separate [Hydra offering][dgd-readme]. |
| Saving and restarting | [Object and mapping save files, including class records](save-format.md), with [`nosave` globals](lpc/nosave.md); [`save_value`](efun/save_value.md) / [`restore_value`](efun/restore_value.md) serialize individual values as strings. Whole-world snapshot/restart is not implemented. | [Object variable saves][cd-save]. | [Object saves][ld-save] with selectable formats and a serialised-string form. | [Object saves][fl-save] with flags, optional compression and a serialised-string form; [`nosave` globals][fl-nosave-keywords] with `SENSIBLE_MODIFIERS`; individual values through [`save_variable`][fl-save-variable] / [`restore_variable`][fl-restore-variable]. | Object saves plus [whole-system snapshots][dgd-dump] and [restart from a snapshot][dgd-intro]. |
| Language features | [Caller inspection](efun/calling_object.md); [Exact int/float comparisons](lpc/comparisons.md); [Configurable zero or distinct nil](lpc/null.md) with [`nilp`](efun/nilp.md); [`has_key` for mapping membership](efun/has_key.md); [UTF-8 strings, immutable bytes, function values and closures](lpc/types.md); [`evaluate` for values or callbacks](efun/evaluate.md); explicit [value captures](lpc/function_type.md#explicit-value-captures) and [reference parameters](lpc/references.md); [macro argument stringification](lpc/macros.md#stringification) and [predefined source macros](lpc/macros.md#predefined-macros); [warning pragmas and retained function types](lpc/pragmas.md); [inheritance visibility, basename calls, and checked sibling/child function resolution](lpc/inheritance.md); [reverse indices, slices and array/string writes](lpc/indexing.md); [array literal spreading](lpc/array_spreading.md) and [trailing call commas](lpc/anatomy_of_a_program.md#function-calls); [`foreach` with `in` or `:`](lpc/foreach.md); [expression and block catches](efun/catch.md); [named variadic parameters with `mixed` or array collectors](lpc/variadic_parameters.md) and [unnamed prototype parameters](lpc/anatomy_of_a_program.md#function-prototypes); [nominal classes with declaration visibility, shared instances, guarded members, shallow copying and native persistence](lpc/classes.md); [`deep_copy` of mutable container graphs](efun/deep_copy.md); literal [`strindex`](efun/strindex.md), [`replace_string`](efun/replace_string.md) and [`trim`](efun/trim.md); scalar-based string [`member_array`](efun/member_array.md) and [`map`](efun/map.md). | [Function values and partial application][cd-functions]. | [Unicode strings, bytes, structs, lightweight objects and coroutines][ld-types]; [closures][ld-closures], [reference arguments][ld-refs] and [trimming][ld-trim]. | [Unicode text][fl-strings], mutable [buffers][fl-buffer], [function values and closures][fl-functions], [reference parameters][fl-refs], [class declarations][fl-class-grammar], [named variadic parameters][fl-variadic-declarations], [array literal spreading][fl-parameter-grammar], [promises][fl-async], [`copy` with `PACKAGE_CONTRIB`][fl-copy] and [trimming with `PACKAGE_TRIM`][fl-trim]. | [DGD LPC types and modifiers][dgd-lpc]; function pointers and bound arguments with the [CLOSURES build define][dgd-build]. |
| Text file writes | [`write_file`](efun/write_file.md) stages append (0/default) or whole-file replacement (1), with pending reads and commit-time delivery. | **Not checked**. | **Not checked**. | Core `write_file` uses bit 1 for overwrite and bit 2 for compression. [Implementation][fluffos-file-write-flags]. | **Not checked**. |
| File metadata | [`file_size`](efun/file_size.md), [`file_time`](efun/file_time.md) and [`get_dir`](efun/get_dir.md); [`rm`](efun/rm.md) checks its task's pending entry changes without reading contents. See [Lima adaptations](#lima-file-metadata). | **Not checked**. | **Not checked**. | Core [`stat`][fl-stat-source] combines metadata with loaded-object information or directory results; [`file_length`][fl-file-length] counts LF bytes with `PACKAGE_CONTRIB`. | **Not checked**. |
| Updating live objects | [Queued in-place recompilation](efun/request_object_recompile.md) upgrades a prototype and its current clones atomically, preserving globals by declaring source, name and type; system prototypes can also be upgraded. | In-place program migration: **not checked**. | [`replace_program`][ld-replace] selects an already inherited program and drops extra code/data; it does not recompile source. | [`recompile_object`][fl-reload] updates a prototype and its clones in place, carrying globals by name; inheritors need separate updates. | [`compile_object`][dgd-compile] upgrades an existing object and its clones when the object has no inheritors; matching variable names and types retain values. |
| Commands and grammars | [`command`](efun/command.md#callback-context), [`add_action`](efun/add_action.md), [`parse_command`](efun/parse_command.md), [`parse_sentence`](efun/parse_sentence.md) with [separate actor and command giver](efun/parse_sentence.md#callback-context), caller-owned [`parse_my_rules`](efun/parse_my_rules.md) with [retained LPC callers](efun/parse_sentence.md#callback-context), [multiword verbs](efun/parse_add_rule.md#the-verb), [multiple text captures](efun/parse_add_rule.md#text-captures-and-split-selection), [visible-only slots](efun/parse_add_rule.md), [structured scopes](efun/parse_sentence.md#scope), and [four handler tiers](apply/object/parser_handlers.md), and [`parse_string`](efun/parse_string.md), plus [native command patterns](command-rules.md) and [structured parser registration inspection](efun/query_parser_registry.md). | [`add_action` commands][cd-actions] and object-aware [`parse_command` patterns][cd-parser]. | [`add_action` commands][ld-actions] and optional [`parse_command` patterns][ld-parser]. | [`add_action` commands][fl-actions], [`parse_command` patterns][fl-patterns] and [`parse_sentence` rules and handlers][fl-parser]; build options below. | [`parse_string`][dgd-parse] accepts grammar text and produces parse results. |
| Driver–mudlib interface | [Master, object, living and connection applies](../ulib/doc/applies.md), including [constructor arguments](efun/clone_object.md), explicit [load](apply/master/valid_load.md), [inherit](apply/master/valid_inherit.md) and [destruction](apply/master/valid_destruct.md) permissions; configurable simul-efuns and automatic inheritance in [default.env](../default.env), with optional [`valid_override`](apply/master/valid_override.md) control of explicit efun calls. | [Master callbacks][cd-master] define policy and connections; [simulated efuns][cd-functions] expose mudlib functions. | [Driver hooks][ld-hooks] configure movement, identity and creation; [simul-efuns][ld-simul] expose mudlib functions and structs. | [Master and object applies][fl-driver] for connections and lifecycle; [simul-efuns][fl-simul] extend or wrap efuns. [`valid_override`][fl-valid-override] controls explicit efun bypasses. | A [driver object and automatically inherited auto object][dgd-intro]; the auto object can wrap kfuns. |
| Call origin | [`origin`](efun/origin.md) identifies eight dispatch mechanisms independently of caller identity; named simul-efun calls report `"simul_efun"`, scheduled callbacks `"call_out"`, and explicit named pointers `"function_pointer"`. | **Not checked**. | **Not checked**. | Eight [origin categories][fl-origin]; named local pointers report `"local"` in the reviewed [implementation][fl-pointer-dispatch]. | **Not checked**. |
| Timer scheduling | [`call_out`](efun/call_out.md) takes a function, delay and optional repeat interval; bind callback data into the function. Mudlibs can build a [combat pulse](#lima-combat-pulse) with an owned repeating timer or [NPC action queues](#lima-action-queues) with one-shot timers. Lima [spell casting](#lima-spell-casting), [companion following](#lima-dancing-lights) and [fluid evaporation](#lima-conjure-ale-and-fluids) adapters also use native function callbacks. | **Not checked**. | **Not checked**. | [`call_out`][fl-callout-reference] accepts a string or function callback followed by delay and callback arguments. | **Not checked**. |
| Timer player context | [`call_out`](efun/call_out.md) clears `this_player()` by default; `CALL_OUT_PRESERVE_PLAYER=true` captures it at scheduling and restores it for each firing. `this_interactive()` stays nil. | **Not checked**. | Preserves the scheduling user. [Reference][ld-callout-context]. | Configurable; enabled by default, with a listener-owner fallback when the captured player is unavailable. [Implementation][fl-callouts], [default][fl-runtime-settings]. | `this_user()` is nil for delayed tasks. [Reference][dgd-callout-user]. |
| Timer inspection | [`query_call_outs()`](efun/query_call_outs.md) lists the current owner's timers; an object selects that owner and `-1` selects all owners. Rows include the function pointer, milliseconds remaining, repeat interval and ID. | **Not checked**. | **Not checked**. | [`call_out_info()`][fl-callouts] lists all live owners' timers with callback labels and seconds remaining. | **Not checked**. |
| Object enumeration | [`objects()`](efun/objects.md) lists all loaded objects from the current transaction, including its own placements, in unspecified order. [`object_clones`](efun/object_clones.md) lists one compiled program's clones. [`all_inventory`](efun/all_inventory.md) and [`deep_inventory`](efun/deep_inventory.md) exclude objects destroyed in that transaction. | **Not checked**. | **Not checked**. | [`objects`][fl-objects] lists loaded objects with optional filtering, visibility rules and an array-size cap; [`children`][fl-children] groups by base filename. | **Not checked**. |
| Named global access | [`variable_info`](efun/variable_info.md) reads all declarations; [`store_variable`](efun/store_variable.md) replaces one by name and optional declaring source, with retained-type checks and separate master authorization for foreign writes. | **Not checked**. | **Not checked**. | [`fetch_variable`][fl-fetch-variable] / [`store_variable`][fl-store-variable] with `PACKAGE_CONTRIB`; [lookup and access restrictions][fl-named-globals]. | **Not checked**. |
| Connection observation | Authorized one-to-one [`snoop`](efun/snoop.md), asynchronous typed callbacks, bounded delivery. | Not reviewed. | Not reviewed. | One-to-one snooping; optional callback delivery. [Source][fl-snoop]. | Not reviewed. |
| Connection model | [Raw TCP sockets](tcp-sockets.md) and [Telnet sessions](efun/query_connection.md) with [line](efun/input_to.md) and [character](efun/get_char.md) callbacks and optional [TLS listeners](secure-connections.md). Connection metadata supplies idle time for [Lima player naming](#lima-player-naming). | Driver-managed [Telnet negotiation][cd-telnet]. | Telnet with [mudlib negotiation hooks][ld-telnet] and optional [TLS][ld-tls]. | Configurable [Telnet, binary, WebSocket and TLS listeners][fl-config]. | Configured [Telnet and binary ports][dgd-config]; additional facilities can come from [extension modules][dgd-extensions]. |
| Build and configuration | [Cargo build and installation](../README.md#build-and-install), [environment settings and explicit dotenv files](../default.env). | [Make-based build][cd-readme], with settings in [config.h][cd-config]. | [Configure/Make build][ld-install], with [command-line options and argument files][ld-invocation]. | [CMake build][fl-readme] and a [runtime configuration file][fl-config]. | [Make-based build][dgd-build] and a separate [driver configuration file][dgd-config]. |

Async I/O and resumable LPC functions do not by themselves establish parallel
execution of game-state mutations. The lpc-rs runtime's transaction boundary is
the owning driver task, including calls it makes into other objects; its
[transaction diagnostics](transaction-diagnostics.md) describe retries and
execution allowances. Committed external I/O can still fail after state commits.

LDMud's `parse_command` requires the `use-parse-command` build option, enabled in
the reviewed [default settings][ld-settings]. FluffOS's `add_action` requires
`NO_ADD_ACTION` to be unset, as in the reviewed [local options][fl-options];
`parse_command` belongs to the always-enabled `PACKAGE_OPS`, while
`parse_sentence` requires `PACKAGE_PARSER`, enabled by default in the reviewed
[CMake settings][fl-packages].

## Client protocols and transports

**Built in** means the driver handles the protocol's negotiation or framing;
the mudlib still supplies game-specific messages or data. **Mudlib hook** means
the driver exposes a documented interface for implementing that protocol in LPC;
it does not establish that every mudlib implements it. The labels describe where
the protocol is implemented. Client support and the stated server configuration
are required in either case.

**Not in Telnet handler** describes the reviewed driver's built-in handler only.
**Not checked** leaves support unresolved, including mudlibs, extensions and
proxies that have not been reviewed here.

| Protocol or transport | lpc-rs | CD | LDMud | FluffOS | DGD |
|---|---|---|---|---|---|
| Telnet | [Built in](efun/query_connection.md). | [Built in][cd-telnet]. | [Built in][ld-telnet]. | [Built in][fl-config]. | [Built in][dgd-config]. |
| MXP | [Built in](efun/send_mxp.md). | Not in [Telnet handler][cd-telnet]. | [Mudlib hook][ld-telnet]. | Built in; [`enable mxp`][fl-config] (default `0`). | Not checked; see binary-port note below. |
| GMCP | [Built in](efun/send_gmcp.md). | [Built in][cd-network]. | [Mudlib hook][ld-telnet]. | Built in; [`enable gmcp`][fl-config] (default `0`). | Not checked; see binary-port note below. |
| MSSP | [Built in](apply/master/get_mud_stats.md). | [Built in][cd-network]. | [Mudlib hook][ld-telnet]. | Built in; [`enable mssp`][fl-config] (default `1`). | Not checked; see binary-port note below. |
| MCCP compression | [Not implemented](../lpc-rs-telnet/src/session.rs); MCCP2 is refused. | Not in [Telnet handler][cd-telnet]. | [Built in when compiled with MCCP][ld-mccp]; off in the reviewed [default settings][ld-settings]. | [Built-in MCCP2][fl-telnet] on Telnet; WebSocket connections use transport compression instead. | Not checked. |
| MSP sound protocol | No negotiation in the [Telnet session](../lpc-rs-telnet/src/session.rs); mudlib use of sound tags not checked. | Not in [Telnet handler][cd-telnet]; mudlib use of sound tags not checked. | [Mudlib hook][ld-telnet]. | Built in; [`enable msp`][fl-config] (default `1`). | Not checked. |
| TLS | [Built in](secure-connections.md); configure a separate TLS port and certificates. | Not checked. | [Built in when compiled with TLS][ld-tls]; the mudlib calls `tls_init_connection()`. | [Built in][fl-config]; configure `external_port_N_tls` with a certificate and key. | Not checked. |
| WebSocket | [Not implemented by the listener](../src/telnet/listener.rs). | Not checked. | Not checked. | [Built in][fl-config]; configure a `websocket` port and `websocket http dir`. | Not checked. |

LDMud explicitly names MXP, GMCP, MSSP and MSP in its
[`H_TELNET_NEG` contract][ld-telnet]. The hook receives negotiations and
subnegotiations, and [`binary_message`][ld-binary] sends protocol bytes.
This is a supported mudlib integration path; no particular mudlib's protocol
implementation was tested for this comparison.

DGD's [binary ports][dgd-config] deliver data to LPC without its built-in Telnet
processing, allowing a mudlib to handle the wire protocol itself
([connection implementation][dgd-comm], [`send_message`][dgd-send]). Specific
MXP, GMCP or MSSP implementations on that path have not been reviewed here;
the table makes no absence claim about them.

## Differences when porting

These are selected differences, grouped by source driver. They identify places
where a mudlib's expectations need review; they are not a claim that all other
APIs match.

Each driver's section collects its language, efun, apply, and data-migration
notes. Shared topics below avoid repeating the same examples across drivers.
When checking a port, use [apply tracing](apply/master/error_handler.md) to
inspect missing callbacks, argument and result types, and security decisions.

### `explode`

Leading and trailing delimiters, empty input and an empty delimiter can change
both array lengths and field positions when porting. These results follow from
the reviewed sources, with the settings described below; they are not
cross-driver execution-test results.

| Driver / mode | `explode(",a,,b,", ",")` | `explode(",,", ",")` | `explode("", ",")` | `explode("ab", "")` | `explode("", "")` |
|---|---|---|---|---|---|
| [lpc-rs](efun/explode.md) | `({ "", "a", "", "b", "" })` | `({ "", "", "" })` | `({ "" })` | `({ "a", "b" })` | `({})` |
| [CD, reviewed defaults][cd-explode] | `({ "", "a", "", "b" })` | `({ "", "" })` | `({})` | `({ "a", "b" })` | `({})` |
| [LDMud][ld-explode] | `({ "", "a", "", "b", "" })` | `({ "", "", "" })` | `({ "" })` | `({ "a", "b" })` | `({ "" })` |
| [FluffOS, reviewed defaults][fl-explode] | `({ "a", "", "b" })` | `({})` | `({})` | `({ "a", "b" })` | `({})` |
| [FluffOS, reversible mode][fl-explode] | `({ "", "a", "", "b", "" })` | `({ "", "", "" })` | `({})` | `({ "a", "b" })` | `({})` |
| [DGD][dgd-explode] | `({ "a", "", "b" })` | `({ "" })` | `({})` | `({ "a", "b" })` | `({})` |

CD's reviewed [configuration][cd-config] leaves both `OLD_EXPLODE` and
`KINGDOMS_EXPLODE` undefined: leading and interior empty fields remain, but the
final empty field is dropped. `KINGDOMS_EXPLODE` changes trailing-field handling;
check the source build's defines before assuming these defaults.

FluffOS defaults to [`sane explode string = 1` and
`reversible explode string = 0`][fl-config]. For input containing non-delimiter
text, that removes one leading and one trailing empty field while keeping
interior empties. With `sane explode string = 0`, all leading empty fields are
removed. `reversible explode string = 1` overrides that trimming; the separate
[`explode_reversible` efun][fl-explode-entry] selects the same reversible path.
Even in reversible mode, empty input returns an empty array and an empty
delimiter adds no boundary fields.

With an empty delimiter, lpc-rs also omits boundary fields and returns an empty
array for empty input. With a nonempty delimiter, lpc-rs retains all empty fields,
including the single empty field for empty input, as LDMud does.

An empty delimiter also exposes different character units: CD and DGD split
into bytes, [LDMud][ld-explode-source] and lpc-rs split text into Unicode scalar
values, and [FluffOS][fl-explode] splits into extended grapheme clusters.
LDMud additionally accepts `bytes` arguments; lpc-rs's `explode` accepts strings
only. A combining mark can therefore be a separate field in lpc-rs while staying
with its base character in FluffOS.

Overlapping multi-character delimiters need review too. The
[FluffOS implementation][fl-explode] processes trailing matches before splitting
the remaining text: `explode("ab---", "--")` returns `({ "ab-" })`, and
`explode_reversible("ab---", "--")` returns `({ "ab-", "" })`. lpc-rs consumes
matches from left to right and returns `({ "ab", "-" })`. Reversible splitting
does not by itself establish identical field boundaries.

Review code that indexes the first or last field, treats an empty array as
"no input", counts characters, or relies on split/join round trips. Preserve
the source driver's intended handling explicitly; filtering every empty field
would also remove meaningful interior fields.

### Stored passwords

For an imported password file using a format supported by [`crypt`](efun/crypt.md),
the check `crypt(password, stored) == stored` can retain its existing meaning.
Classic DES hashes are supported. A mudlib can replace a verified DES hash on
login using the native default SHA-512 format:

```c
if (crypt(p, password) != password)
    return 0;
if (sizeof(password) == 13) {
    password = crypt(p, 0);
    save_me();
}
```

Apply this example only to records identified as classic DES. FluffOS's
[`oldcrypt`][fl-oldcrypt] is a different MudOS algorithm, unsupported by native
`crypt`; its records need a separate migration path. A function rename does not
convert a hash. See the [FluffOS notes](#fluffos).

### CD

| Topic | CD behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Array predicate | Calls it `pointerp`. [Reference][cd-pointerp]. | Rename calls to [`arrayp`](efun/arrayp.md), or provide `int pointerp(mixed value) { return arrayp(value); }` as a simul-efun. |
| `reduce` argument order | The callback is first: `reduce(function, mixed, ...)`. [API declarations][cd-api]. | The array is first: `reduce(items, callback, ...)`. Calls and bound arguments in function pointers need the corresponding order. [Reference](efun/reduce.md). |
| Repeating timers | `set_alarm(delay, repeat, callback)`; timer IDs start at `1`. [Reference][cd-alarm], [ID allocation][cd-callouts]. | `call_out(callback, delay, repeat)` registers work at transaction commit; IDs also start at `1`, leaving `0` for an unset handle. The efun name and argument order differ. [Reference](efun/call_out.md). |
| `parse_command` matching | `%s` tries following pattern elements at successive words; destinations are assigned during matching. [Implementation][cd-parse-source]. | `%s` is greedy; destinations change only on a successful match. See the [parser details below](#cd-command-parsing). |
| Regular expressions | A traditional Henry Spencer-derived engine. [Implementation][cd-regexp]. | Rust `regex` syntax, including Unicode classes but no look-around or back-references. Patterns need review even where the efun name matches. [Reference](efun/regexp.md). |
| `save_object` result | Declared `void save_object(string)`. [API declarations][cd-api]. | Returns the save path; the write is deferred to commit. Shared format ancestry does not establish complete interchangeability, particularly for the additional `bytes` encoding. [Efun](efun/save_object.md), [format](save-format.md). |

#### CD command parsing

Review patterns, destination variables, and vocabulary applies together. The
following details come from the pinned [CD parser][cd-parse-source]. The native
[`parse_command`](efun/parse_command.md) reference defines the complete contract;
[`add_rule`](efun/add_rule.md) uses the same native pattern and noun rules.

| Topic | CD behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| `%s` and `%d` | `%s` searches forward for the next match. `%d` accepts a nonnegative numeric prefix, so `5x` can become `5`. | `%s` is greedy. `%d` requires a digit run that fits an integer; signed or spelled-out numbers do not match that capture. |
| Destination writes | Assigns as matching proceeds; extra captures without destinations are discarded. | Writes destinations only after the entire pattern succeeds; too few destinations raise an error before parsing. |
| Invalid patterns | Warns and skips invalid pattern elements. | Rejects malformed patterns with runtime errors. Review patterns that formerly appeared to work despite warnings. |
| Living lookup | `%l` can fall back to `find_living`. | Searches the supplied scope for objects that called `enable_commands`; there is no global-name fallback. |
| Language defaults | Includes English numeral and plural handling when `PARSE_FOREIGN` is absent. | Numeral words, the all word, and pluralization come from master applies. Supply the mudlib's vocabulary explicitly. |
| Noun vocabulary | Uses object and shared master ID/adjective lists, with a default pluralizer. | Every adjective must be listed and the ID must end the captured phrase. Without an object's `parse_command_id_list()`, the driver asks `id(phrase)`; shared master IDs do not apply to that fallback. |
| Prepositions | Takes the first list entry matching at the current word, without reconsidering it after later failure. | Tries splits until the whole pattern and its noun captures resolve; overlapping entries may select a different result. |

Matching in the native parser is case-sensitive and uses whole words. Test
mudlib assumptions about case normalization at its command-entry boundary.

### LDMud

| Topic | LDMud behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Call-out player context | Preserves the scheduling user. [Reference][ld-callout-context]. | Enable `LPC_CALL_OUT_PRESERVE_PLAYER=true`; see the native [`call_out`](efun/call_out.md) contract. |
| String trimming | `trim(text, where, chars)` accepts 1 for left, 2 for right and 0 or 3 for both; the header defines `TRIM_BOTH` as 3. The documented default set is space and tab. [Reference][ld-trim], [header][ld-trim-header]. | Native [`trim`](efun/trim.md) uses 0 for both (the default), 1 for left and 2 for right; 3 is an error. Update header constants and literal modes. The default set also includes LF, CR, vertical tab and form feed; supply `" \t"` explicitly to retain the documented upstream set. Other upstream edge cases were not checked. |
| Array predicate | Calls it `pointerp`. [Reference][ld-pointerp]. | Rename calls to [`arrayp`](efun/arrayp.md), or provide `int pointerp(mixed value) { return arrayp(value); }` as a simul-efun. |
| Reference arguments | `&x` passes a reference without a special callee declaration; indexed lvalues and ranges can be referenced. [Reference][ld-refs]. | Both parameter and argument use `ref`. Only variables can be explicit reference arguments; cross-object and function-pointer calls cannot carry them. This can require interface changes. [Reference](lpc/references.md). |
| Byte values | `bytes` supports byte-string literals such as `b"abc"`. [Types][ld-types]. | There is no LPC bytes literal; values come from file reads, conversions or existing bytes. Bytes are immutable. [Types](lpc/types.md), [`to_bytes`](efun/to_bytes.md). |
| Regular expressions | Traditional and optional PCRE engines are selectable, with engine-specific option flags. [Reference][ld-regex]. | Patterns use Rust `regex`, without look-around or back-references. Numeric flags select filtering/index output, not an engine. Patterns and flags need separate review. [Reference](efun/regexp.md). |
| `terminal_colour(text, 0)` | Leaves colour markup uninterpreted while optionally wrapping the text. [Reference][ld-colour]. | Strips colour tokens. Code using `0` solely to wrap text has different output; use [`break_string`](efun/break_string.md) for ordinary text wrapping. [Reference](efun/terminal_colour.md). |
| `save_object` result and forms | File form returns `0` on success; a no-file form returns serialised data, with optional format selection. [Reference][ld-save]. | Requires a path and returns that path. No format argument or no-file form; callers testing the result as an error flag need changes. [Reference](efun/save_object.md). |

### FluffOS

Native classes support shared header identities, inherited definitions, named
construction, and member reads and writes. Every field replacement checks its
declared type; scalar classes retain nominal identity at runtime. Mixed member
access requires the actual class definition to be visible to the executing code.
Field views reuse storage by name and declared type across compiled definitions.
[`copy_class`](efun/copy_class.md) preserves all retained views in a shallow copy;
[`classp`](efun/classp.md) tests instance kind without requiring a visible
definition. [Native persistence](save-format.md#class-records) preserves nominal
identity and all materialized field views without loading class source files.
Repeated occurrences restore independently. General reflection and foreign class
save import still require further driver support. See [class support](lpc/classes.md).

| Topic | FluffOS behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Random selection helpers | `random(n)` selects an index below a positive bound and returns zero for a zero bound. Empty array indexing errors; string indexing at the final boundary returns integer zero. String positions are graphemes: the reviewed index helper can return only the first codepoint of a short multi-scalar cluster, while longer clusters can error. [Random][fl-random], [length][fl-grapheme-length], [indexing][fl-string-read], [UTF-8 helper][fl-string-read-scalar]. | Lima's `choice` can remain an LPC helper using predicates, [`random`](efun/random.md), [`keys`](efun/keys.md) and [indexing](lpc/indexing.md). Return array elements or mapping values with their existing identities; select mappings by entry, including repeated equal values. String input returns an integer Unicode scalar, with combining marks and joiners occupying separate positions. Preserve zero for an empty string and errors for empty arrays/maps. Guard legitimately empty lists before selecting: testing the result afterward cannot handle an indexing error. Recheck lists after actions that may change them, such as sweep's first hit. |
| Array shuffling | `shuffle` reorders its argument in place, returns that same array, and leaves arrays shorter than two elements unchanged. Aliases observe the new order; element references are preserved. Requires `PACKAGE_CONTRIB`. [Reference][fl-shuffle], [implementation][fl-shuffle-source]. | There is no native `shuffle`; an LPC Fisher–Yates helper can use [`random`](efun/random.md) and array element assignments while preserving identity and aliases. A weighted selector such as Lima's `element_of_weighted` can omit its shuffle: for nonnegative integer weights with a positive, nonoverflowing total, each key still receives probability `weight / total`. A two-pass helper can total the weights and select with one draw, preserving the distribution while changing random-number consumption. Validate weights and overflow explicitly; choose the configured null result for empty/all-zero input. Native `random` rejects negative bounds, whereas the reviewed FluffOS entry point returns zero. [Source][fl-random]. |
| Regex association | `reg_assoc` returns parallel arrays of alternating gaps/matches and their labels. Earliest matches win, with pattern order breaking ties. Each search uses the remaining suffix; the default gap label is integer zero. `pcre_assoc` uses PCRE. [Reference][fl-reg-assoc], [core source][fl-reg-assoc-source], [PCRE source][fl-pcre-assoc]. | A mudlib simul can build the association arrays with [`regmatch`](efun/regmatch.md) and `RE_MATCH_SUBS`; there is no native association efun. Preserve empty gaps and advance empty matches by a Unicode scalar without dropping intervening text. Use an explicit `0` default label when callers require numeric zero. Searching a suffix retains the legacy anchor context; an offset into the original text preserves its anchors instead. Review patterns for the [native regex dialect](efun/regexp.md#dialect), scalar indexing and embedded NUL support. This does not supply full PCRE compatibility. The existing regex efuns share a bounded compiled-pattern cache per VM; frequent calls reuse resident patterns with the same compilation options. |
| Array and mapping predicates | `pointerp` tests arrays; `arrayp` is an alias. `mapp` tests mappings. Each returns integer `1` for its type and `0` otherwise. [Declarations][fl-core-spec], [implementations][fl-type-predicates]. | Use [`arrayp`](efun/arrayp.md) and [`mappingp`](efun/mappingp.md). Rename `pointerp` and `mapp` calls or provide simul wrappers. These predicates test container type, independently of whether the container is empty. |
| Type inspection | `typeof` returns a string such as `"int"` or `"float"`; its `T_*` constants name those strings. [Reference][fl-typeof], [implementation][fl-typeof-source]. | [`typeof`](efun/typeof.md) returns an integer type tag. Replace simple checks with predicates such as [`intp`](efun/intp.md) and [`floatp`](efun/floatp.md), or define mudlib constants for the native tags when retaining a switch. String-valued foreign constants will not match. For a numeric variadic helper such as Lima's `sum`, validate each argument before adding it; numeric strings and nested arrays remain invalid. Call `sum(values...)` to spread an array, or use Lima's `array_sum(values)` wrapper. Preserve integer accumulation and the switch to floating arithmetic when a float is encountered; nil acts as zero only in zero mode. |
| Character input | `get_char` delivers one UTF-8 character or control byte, then returns to line mode unless rearmed. Its reference specifies first-registration-wins and extra arguments after the input. [Reference][fl-get-char]. | [`get_char`](efun/get_char.md) shares transactional registration with `input_to`; the last registration wins across both APIs. Its second argument is boolean no-echo; use ordinary pointer binding for extra arguments, which precede the character. Classic SGA negotiation is supported; RFC LINEMODE is refused. Unread input is preserved across mode changes. Audit repeated registrations, callback argument order, and flag use. |
| Stack inspection | `call_stack(0/1/2)` returns defining programs, executing objects, or function names, starting with the current frame. `all_previous_objects()` aliases `previous_object(-1)`. [Reference][fl-call-stack], [declaration][fl-core-spec]. | [`calling_program(-1)`](efun/calling_program.md), [`calling_object(-1)`](efun/calling_object.md), and [`calling_function(-1)`](efun/calling_function.md) provide aligned caller arrays, excluding the inspecting function. A local helper can include the requesting function as row zero. Use direct no-argument queries for immediate-caller checks. Replace `all_previous_objects()` with [`previous_object(-1)`](efun/previous_object.md) at the call site; a simul wrapper adds its own caller entry. Frame counts and synthetic labels need not match FluffOS. |
| Privileged callback owner chains | Function-pointer dispatch installs a synthetic owner frame before invoking the target, including efun and simul-efun pointers. [Implementation][fl-pointer-dispatch]. | Dynamic pointers, bound `call_other`, and simul-efun pointers can reach the target without an executing owner frame. For Lima-style `unguarded` checks, an inherited helper can wrap those same-owner pointers in `(: $(code)() :)`, using [`function_info`](efun/function_info.md) to select them. Leave foreign-owned callbacks on their existing path. Named callbacks, closures and ordinary efun pointers already provide the needed context; wrapping them all can introduce extra owner entries and reject valid calls. Preserve source, handle, privilege and protection checks, and evaluate bound arguments before entering the privileged callback. |
| Snooping | `snoop` replaces occupied relationships. With `receive snoop` enabled, it calls `receive_snoop(text)` synchronously; otherwise it forwards text prefixed with `%`. The reviewed native setter does not call a master authorization hook. [Source][fl-snoop]. | [`snoop`](efun/snoop.md) errors on occupied endpoints; stop explicitly before changing partners. Start, stop and live queries require [`valid_snoop`](apply/master/valid_snoop.md). Adapt relays to [`receive_snoop(text, target, direction)`](apply/object/receive_snoop.md), delivered in a fresh transaction without player context. Noninteractive relays work without a runtime switch. Hidden input is excluded; errors and overflow retire the registration. Connection transfers terminate observation. Mudlib relays must handle asynchronous delivery and their own subscriber policy. |
| Explicit efun bypass | `valid_override(file, name, main_file)` controls compilation of `efun::` calls; the source file can differ from the main file through inclusion. [Reference][fl-valid-override], [implementation][fl-valid-override-source]. | [`valid_override`](apply/master/valid_override.md) controls explicit calls in LPC-triggered compilation, including closures, headers and macros. Native source paths retain extensions; macro replacement text names its invocation site. Adapt source comparisons explicitly. The hook is optional and omission allows calls; boot master/simul compilation is exempt. Recompilation uses the old master policy. Checks are transactional and repeat on retry; existing compiled code is not revoked. Runtime and inheritance permissions remain separate. |
| Null and undefined | `nullp` aliases `undefinedp`, testing an undefined marker on a numeric value. [Declaration][fl-undefined-alias], [predicate][fl-undefined]. | The default zero mode preserves zero-as-null. Optional [distinct mode](lpc/null.md) separates nil from numeric zero, without reproducing FluffOS undefined-number propagation. Audit each predicate: use [`has_key`](efun/has_key.md) for missing mapping entries, [`nilp`](efun/nilp.md) for the configured null value, and defaults or argument counts for omitted arguments. `nilp(0)` is true in zero mode, so it does not reproduce the undefined marker. Stored nil does not imply a missing mapping key. Use `nil` for absent object returns in distinct mode. |
| Reference arguments | Uses `ref` in parameter declarations and `ref` or `&` at call sites. [Reference][fl-refs]. | Uses `ref` in both positions, with variables as explicit reference arguments. Function-pointer and cross-object calls cannot carry references. FluffOS reference forwarding through function pointers is **not checked** against this revision. [Native reference](lpc/references.md). |
| Value-or-callback evaluation | `evaluate(value, ...)` invokes functions with the remaining arguments and returns non-functions unchanged. The function case returns undefined if the current calling object is destructed. [Reference][fl-evaluate], [implementation][fl-evaluate-source]. | [`evaluate`](efun/evaluate.md) supports the same value-or-callback dispatch using native function-pointer rules, including argument checks, captures, liveness, caller context, and transaction behavior. There is no separate destructed-caller shortcut. Named and bound pointer spellings still need the adaptations described below. |
| Call origin | `origin()` exposes eight categories, including `"simul"` for named simul-efun calls and `"internal"` for `call_out` callbacks. The pointer machinery sets `"local"` for named local-function pointers and `"functional"` for anonymous closures; the result table spells the pointer category `"function pointer"` with a space. [Reference][fl-origin], [dispatch][fl-pointer-dispatch], [strings][fl-origin-source]. | [`origin`](efun/origin.md) returns `"simul_efun"` for named simul-efun calls, matching the [`request_object_recompile`](efun/request_object_recompile.md) selector, and `"function_pointer"` for explicit named/bound/dynamic/efun pointer calls. Update origin constants to the native spellings. Explicit closures report `"closure"`; efun callbacks report `"efun"`, scheduled callbacks `"call_out"`, and driver callbacks `"driver"`, regardless of pointer kind. `evaluate` preserves ordinary pointer classification. Audit origin comparisons and callback paths when porting, especially security checks treating `"local"` as trusted: a named pointer accepted by FluffOS's check will fail that check here. Caller-object and caller-program checks remain separate policy decisions. |
| `sscanf` | Supports `%(regexp)`; checks argument types and raises a missing-destination error when scanning reaches a conversion needing one. [Reference][fl-sscanf], [implementation][fl-sscanf-source]. | Has no regex conversion. Non-string input/format returns `0`; the total destination count is checked before scanning, even if the input would fail earlier. Review error handling as well as format strings. [Reference](efun/sscanf.md). |
| Colour expansion | `terminal_colour` in `PACKAGE_CONTRIB` looks up split text segments, including ordinary text, and can insert a default `RESET` into the supplied mapping. [Implementation][fl-colour-source]. | Looks up only explicit colour tokens and leaves input mappings unchanged. Callers relying on implicit text replacement or the added mapping entry need changes. [Reference](efun/terminal_colour.md). |
| Deep copying | `copy` in `PACKAGE_CONTRIB` copies arrays, mapping values, classes, and buffers; mapping keys remain shared. Repeated container references are copied independently; cycles encountered during value traversal hit the nesting limit. Objects and functions retain identity. [Implementation][fl-copy], [declaration][fl-contrib-spec]. | [`deep_copy`](efun/deep_copy.md) copies arrays, mapping keys and values, and all retained class fields. It preserves cycles and shared references within the copy; objects and functions retain identity, and immutable bytes can remain shared. Rename `copy` calls or supply a wrapper after checking key identity and sharing assumptions. There is no native `copy` alias. Cycle-preserving copying does not make a mudlib traversal cycle-safe: recursive flatteners still need acyclic array nesting or their own cycle detection. |
| `foreach` separator | Uses `in` between the loop variables and the collection. [Reference][fl-foreach], [grammar][fl-parameter-grammar]. | Accepts `in` and `:` with identical iteration behavior. `in` is contextual and remains available as an identifier elsewhere. The collection value is retained at loop entry even if its source variable is reassigned. Native loop scope and type checks remain. [Reference](lpc/foreach.md). |
| Catch blocks | Supports `catch(expr)` and `catch { statements }`; rejects `break` and `continue` that leave the catch. Returns zero on success, a string for runtime errors, or the value passed to `throw`. [Reference][fl-catch], [jumps][fl-catch-jumps]. | Supports both forms with the existing zero-or-error-message result. `throw` stringifies its argument. `return`, `break`, and `continue` may leave catch blocks and remove the exited handlers; inner loops and switches retain ordinary jump behavior. Prior writes remain in the current transaction. [Reference](efun/catch.md). |
| Unnamed prototype parameters | Accepts typed parameters without names, including arrays, in prototypes such as `void start_shell(int);`. [Grammar][fl-parameter-grammar], [declarations][fl-variadic-declarations]. | Accepts named, unnamed, and mixed parameter lists in prototypes. Prototypes remain ignored with a warning; calls are checked against definitions, including eligible sibling or child definitions for otherwise unresolved local names. An object being compiled still requires implementations for every call. Definitions and declared closure parameters require names. [Reference](lpc/anatomy_of_a_program.md#function-prototypes). |
| Trailing call commas | Allows one trailing comma after a nonempty argument list. [Grammar][fl-call-arguments]. | Supports `f(a,)`, including method, inherited, efun and computed function calls, spreads and reference arguments. The comma adds no argument; declaration parameter lists and partial-application slots retain their own syntax. [Reference](lpc/anatomy_of_a_program.md#function-calls). |
| Array literal spreading | Accepts `({ first, xs..., last })`; expression-list generation evaluates the expressions before expanding their arrays. [Grammar][fl-parameter-grammar], [code generation][fl-array-spread-codegen]. | Supports the same syntax and evaluation timing, with array or `mixed` operands and native element-type inference. Expansion makes a fresh shallow array. [Reference](lpc/array_spreading.md). |
| Explicit closure captures | `$(expression)` evaluates at closure creation and retains the result for later calls. Named and bound pointer literals use `(: name :)` and `(: name, arg :)`. [Function reference][fl-functions]. | Supports `$(expression)` alongside existing shared variable captures. Captured references remain shared; the captured value itself cannot be assigned. Named/bound FluffOS literals still need adaptation to `name` and `&name(arg)`. A native `(: name, arg :)` is a comma expression that returns `arg` without calling `name`; use an explicit call inside the closure when a local security wrapper requires a closure callback. [Reference](lpc/function_type.md#explicit-value-captures), [Lima persistence callbacks](#lima-user-incarnation-and-body-switching). |
| Class copying and conversion | `assemble_class` creates positional class storage from an array, without a named definition; `disassemble_class` returns a shallow field array. Their round trip copies the outer record while sharing nested references. Requires `PACKAGE_CONTRIB`. [Implementation][fl-class-assembly]. | Replace copy-only round trips with [`copy_class`](efun/copy_class.md), which preserves nominal identity and retained field views. Converting between different class types requires explicit named construction, even when their layouts match. For example, build `new(class limb, health: data.health, max_health: data.max_health, parent: data.parent, flags: data.flags)` and let omitted integer fields default to zero. Replace layout-count repair checks with a guarded nominal cast and presence check; compatible [field views](lpc/classes.md) do not require resetting a record merely because fields changed. Generic assembly/disassembly and foreign class-save import remain unsupported. |
| Class visibility | Accepts visibility modifiers on class definitions but discards them; inheritance copies classes regardless of the modifier. [Grammar][fl-class-grammar], [declarations][fl-class-declarations], [inheritance][fl-class-inheritance]. | Enforces `private class`: descendants cannot name the class or access its members, including through `mixed`. Public and protected classes remain inheritable, subject to inherit visibility. Defining code retains access. If descendants rely on an upstream private declaration being exposed, make it public/protected or include its definition explicitly. [Reference](lpc/classes.md#declaration-visibility). |
| Inheritance visibility | Supports access modifiers on inherits; private functions and globals become hidden when inherited again. The reviewed build enables `SENSIBLE_MODIFIERS`, so explicit public declarations do not defeat a private inherit. [Grammar rules][fl-inherit-grammar], [compiler][fl-pragma-compiler], [defaults][fl-options]. | Supports public, protected and private inheritance, including local access and restricted simul-efun exports. Other inherit modifiers remain unsupported. [Reference](lpc/inheritance.md). |
| Inherited function qualifiers | Named qualifiers match direct-parent path suffixes with the source extension removed. Both named qualifiers and unqualified `::foo()` search direct parents in declaration order for the first defining the function. Qualified calls through a private direct inherit are rejected. Basic-type keywords can qualify calls, including `object::`. [Compiler, `arrange_call_inherited`][fl-pragma-compiler], [keyword grammar][fl-type-qualifiers]. | Source basenames and unqualified `::foo()` search direct parents in reverse declaration order, giving later inherits precedence. Explicit aliases select one parent and take precedence over automatic names; use an alias for a type-keyword basename such as `object`. Path-suffix qualifiers are unsupported. Private imports remain callable within the importing source. [Reference](lpc/inheritance.md#calls-and-aliases). |
| Predefined source macros | `__FILE__` names the current in-game source; `__DIR__` includes a trailing `/`. Both expand at use time, including in macro bodies. `__VERSION__` is the build's version string. These names cannot be redefined or undefined. [Expansion and protection][fl-predefined], [version registration][fl-pragma-lexer]. `__PORT__` expands to the first configured listener's numeric port. [Registration][fl-configured-port]. | Provides `__FILE__`, `__DIR__`, `__VERSION__`, and numeric `__PORT__`; the version identifies `lpc-rs` and its package version. `__PORT__` uses the compiler configuration's primary plaintext port, even when plaintext Telnet is disabled; it does not report a secondary TLS listener or an actual bound socket. Other driver-specific configuration checks still need adaptation. [Reference](lpc/macros.md#predefined-macros). |
| Warning and type-retention pragmas | `warnings` / `no_warnings` switch warning reporting as compilation proceeds; includes share the setting. `save_types` / `no_save_types` control retention of argument types. Both positive flags are enabled by the reviewed defaults. Unknown pragmas warn. [Pragmas][fl-pragmas], [lexer][fl-pragma-lexer], [compiler][fl-pragma-compiler], [defaults][fl-options]. | `warnings` / `no_warnings` follow each warning's primary source location, including repeated includes and macro invocations. This can differ from FluffOS's reporting-time setting for delayed warnings. `save_types` is accepted silently because function types are always retained. `no_save_types` and other unknown pragmas remain errors even with warnings disabled. [Reference](lpc/pragmas.md). |
| Constructor arguments | `clone_object(path, ...)` forwards extra arguments to the new object's `create`. [Reference][fl-clone]. | `clone_object` forwards extra arguments after global initialisation. Constructors also run without arguments when loading prototypes; use defaults or handle `0`. Supplied declared arguments are type-checked. [Reference](efun/clone_object.md). |
| Call-out player context | With `this_player in call_out` enabled (the default), captures the scheduling command giver; if it is unavailable, uses an `O_LISTENER` callback owner, following shadowing when supported. `this_interactive` is cleared. [Implementation][fl-callouts], [default][fl-runtime-settings]. | Enable `LPC_CALL_OUT_PRESERVE_PLAYER=true`. A missing or destructed saved player remains nil; there is no owner fallback. Repeats and transaction retries restore the originally captured player, and `this_interactive()` remains nil. The setting applies to call-outs; other deferred APIs retain their own contracts. [Native reference](efun/call_out.md). |
| Timer handles | Reserves `0` for invalid handles; handles combine a counter with the current game tick. [Implementation][fl-callouts]. | Timer IDs start at `1` per VM. Store the returned handle; its numeric value need not match FluffOS or be consecutive. [Reference](efun/call_out.md). |
| Timer inspection | `call_out_info()` returns rows of owner, string callback label, and seconds remaining in the reviewed implementation; function pointers are rendered as labels. [Implementation][fl-callouts]. | Use [`query_call_outs(-1)`](efun/query_call_outs.md) for all owners, or pass an object for an owner-specific query. Native rows contain scheduling owner, function pointer, milliseconds remaining, milliseconds between repeats, and ID. The query includes the current transaction's schedules and excludes its cancellations. An LPC helper can project the first three fields, converting callbacks with [`function_name`](efun/function_name.md) and durations with `remaining_ms < 0 ? 0 : remaining_ms / 1000`; the fourth native field is a repeat interval, not callback data. Bare-name grouping combines differently bound callbacks with the same name. A `my_call_outs`-style simul can query `previous_object()` directly, but must return an empty array for a missing caller: a nil selector queries the simul itself. Preserve `-1` for a missing timer in name-lookup helpers. If scheduling wrappers hide callbacks inside closures, retain the original name separately or query a reliably maintained timer ID. |
| Whole-object enumeration | `objects()` accepts optional function or local-method-name filtering; the result is capped by `__MAX_ARRAY_SIZE__`. With `F_SET_HIDE`, visibility depends on caller status or `valid_hide`. Callback filtering can reorder entries. [Reference][fl-objects], [implementation][fl-objects-implementation], [visibility][fl-object-enumeration]. | [`objects()`](efun/objects.md) takes no arguments and returns all transaction-visible loaded objects without truncation or an ordering guarantee. Use `filter(objects(), predicate)` for selection, with native truthiness and callback behavior; use `foreach` when only side effects are wanted. Initialize missing counters before incrementing them in distinct-null mode. For clone counts, exclude prototypes explicitly: subtracting one undercounts when the prototype is absent. There is no corresponding hidden-object category. An LPC method-name wrapper must bind the original caller explicitly. |
| Clone predicate | `clonep()` defaults to the current object; an explicit argument succeeds only for an object carrying the driver's clone flag. [Reference][fl-clonep]. | An LPC helper can inspect the terminal `#<digits>` suffix from [`file_name`](efun/file_name.md) when the mudlib reserves that naming convention for clones. That convention is a mudlib constraint, not a native restriction on ordinary source filenames. For exact classification without it, test membership in [`object_clones(ob)`](efun/object_clones.md), which allocates the clone group's array. A simul helper should default an omitted argument to `previous_object()` while returning `0` for explicit nil, destructed objects and other nonobjects. |
| Clone enumeration | `children(path)` includes the loaded prototype and surviving clones sharing its base filename, in insertion order, even across prototype destruction and replacement. [Reference][fl-children], [object table][fl-children-table]. | [`object_clones(object)`](efun/object_clones.md) excludes the prototype, groups clones by compiled program version, and returns newest first. A mudlib helper can use `find_object(path)`, add the loaded prototype, and reverse the clone order. That helper cannot discover old groups through a missing or replacement prototype. Filtering [`objects()`](efun/objects.md) by base filename can find those groups, but does not reproduce insertion ordering. Audit reload, cleanup, uniqueness and registry-recovery code; in-place recompilation preserves the current group but does not merge earlier groups. |
| Master lookup and reload | `master()` returns the current master object, or zero if absent; the getter does not load it. Destructing the master attempts to load and install a replacement before removing the old object. [Getter][fl-master-getter], [destruction][fl-master-destruct]. | A mudlib `master()` helper can return [`find_object(MASTER_OB)`](efun/find_object.md), provided the rooted mudlib constant matches the driver's configured `MASTER_OBJECT`. A mismatched path can return an unrelated object, so this agreement matters for identity-based security checks. Simul-efun initialization precedes the master: do not cache the initial missing result. Native [`destruct`](efun/destruct.md) does not recreate the master, and a subsequent lookup cannot reload it. Adapt reload commands to queued [`request_object_recompile("master")`](efun/request_object_recompile.md), including authorization and later status checks with [`query_object_recompile`](efun/query_object_recompile.md); it preserves state and identity and does not rerun `create()`. |
| Live recompilation | `recompile_object` updates the prototype and its clones; inheritors need separate updates. [Reference][fl-reload]. | Use `request_object_recompile` and inspect its queued result with `query_object_recompile`; other tools need `valid_recompile` permission to read the status. Migration matches declaring source, name and exact type; compatible named callbacks rebind and closures retain their original code and captures. System selectors support state-preserving master/simul-efun upgrades. Active shadow chains and changes to cleanup eligibility are refused. [Reference](efun/request_object_recompile.md). |
| Parser results and caching | `parse_sentence` documents integer results `1`, `0`, `-1`, `-2`, or a message; `parse_refresh` clears cached object information. [Results][fl-parser], [refresh][fl-refresh]. | An unresolved object can also produce `-3`; `parse_refresh` is a no-op. See the [parser package details below](#fluffos-parser-package). |
| Non-persistent globals | With `SENSIBLE_MODIFIERS`, `nosave` excludes globals from save/restore independently of visibility. The legacy `static` keyword, available without that option, combines exclusion with protected visibility. `nosave` on functions warns and is discarded. [Keywords][fl-nosave-keywords], [storage][fl-nosave-storage], [functions][fl-inherit-grammar]. | `nosave` and `static` are aliases on globals and do not change visibility. Both exclude inherited globals too. `variable_info` reports either spelling as `static`. `nosave` on functions or inherit statements is rejected. [Reference](lpc/nosave.md). |
| Named variadic parameters | A final parameter followed by `...` collects extra arguments. It also enables `varargs`, allowing a missing fixed prefix to be padded with undefined values. `ref` collectors are rejected; a non-array type other than `mixed` produces a warning. [Declarations][fl-variadic-declarations], [flags][fl-inherit-grammar], [packing][fl-variadic-runtime]. | Supports `mixed args...`, `mixed *args...`, and typed array collectors, including in closure parameter lists. A `mixed` collector retains its ordinary local type and initially receives an array. Collectors cannot have a default or `ref`; other scalar types are rejected; direct calls still require the fixed prefix unless defaulted or declared `varargs`. Bare `...` continues to collect into `argv`. [Reference](lpc/variadic_parameters.md). |
| Reverse indexing | Supports `<n` element indices and slice bounds. With `old range behavior: 0`, ordinary negative string/buffer bounds are not end-relative. Reverse array reads outside the array raise errors; string reverse reads special-case `<0` as zero and otherwise check bounds. Strings use grapheme positions. [Ranges][fl-ranges], [indices][fl-indexing]. | Supports `<n` for arrays, strings and bytes while retaining native negative indices and bounds. Reverse slice reads clip to bounds; invalid array reads raise errors, while string/bytes reads return zero. Strings use Unicode scalar positions. Writes support array elements, plain string element assignment, and plain array/string slice assignment; bytes writes are unsupported. [Reference](lpc/indexing.md). |
| Array slice assignment | Equal-length replacements mutate the shared array; length-changing replacements allocate a new array and rebind the owner. [Assignment paths][fl-slice-assignment]. | Plain array slice `=` always constructs a new shallow array and rebinds the target. Review code that relies on aliases observing equal-length edits. Write bounds are strict, with native negative/reverse indexing and adjacent bounds for insertion; upstream edge-case equivalence is unchecked. Bytes and compound slice assignments need adaptation. [Reference](lpc/indexing.md#array-slice-assignment). |
| String slice assignment | Detaches shared strings before range writes and resolves grapheme positions to byte offsets. Equal-byte-length edits can overwrite detached storage; resizing constructs a replacement. [Detachment][fl-string-unlink], [range lvalues][fl-string-write-ranges], [replacement][fl-string-write-replacement]. | Plain string slice `=` rebinds its owner while preserving other references to the old string. Positions count Unicode scalars; bounds follow native negative/reverse indexing and strict write validation. Results preserve NUL and are limited to 8,192 UTF-8 bytes. Review grapheme/byte assumptions and size limits; upstream empty/insertion edge equivalence is unchecked. Compound string writes remain unsupported. [Reference](lpc/indexing.md#string-slice-assignment). |
| String element assignment | Addresses graphemes and rejects a selected grapheme containing multiple code points. Ordinary negative indices are rejected; explicit reverse indices are supported. Replacement rejects NUL and invalid UTF-8 code points. [Lvalues][fl-string-index-lvalue], [replacement][fl-string-index-replacement]. | Plain `text[i] = scalar` replaces one Unicode scalar and rebinds the captured owner, preserving old string references. Accepts ordinary negative/reverse indices and integer scalars including NUL, with strict bounds and an 8,192-byte result limit. Review grapheme assumptions, NUL handling and size limits. Compound writes, stepping, element references and output arguments need adaptation. [Reference](lpc/indexing.md#string-element-assignment). |
| Literal string search | `strsrch(text, needle, flag)` finds the first match with flag `0`, or the last with any nonzero flag. Positions use grapheme clusters; some fast paths differ in boundary checks. Integer zero can match the C-string terminator. [Implementation][fl-strsrch-source], [Unicode helper][fl-string-search], [tests][fl-strsrch-tests]. | The native name is `strindex`; rename calls or provide a mudlib wrapper. Positions and integer needles use Unicode scalar values. Combining marks can match separately; integer zero matches only an actual NUL. Empty string needles match the first or last boundary. [Reference](efun/strindex.md). |
| String trimming | `trim`, `ltrim` and `rtrim` take an optional string set as argument two in `PACKAGE_TRIM`. Omitted or empty sets select space, tab, LF, CR, vertical tab and form feed. Custom sets match Unicode scalars; the wrapper uses NUL-terminated input. [Wrapper][fl-trim], [helper][fl-trim-helper]. | Use [`trim(text, where, chars)`](efun/trim.md): 0/default for both, 1 for left and 2 for right. Move custom sets to argument three. An explicit empty set removes nothing, so omit it to retain upstream defaults. Native strings preserve NUL, and an integer character is also accepted. Use `map(lines, trim, 2)` for right-trim array callbacks. Preserve any mudlib-local helpers with narrower whitespace rules. |
| Literal replacement | `replace_string` supports all matches, a maximum count, or an inclusive occurrence range. Expansion past its configured limit returns an undefined value; some paths treat NUL as the end of the string. [Implementation][fl-replace-string-source]. | Supports the same three call forms with non-overlapping literal matches. Changed results above 8,192 UTF-8 bytes raise errors; embedded NULs are ordinary data. Review overflow handling and output sizes. [Reference](efun/replace_string.md). |
| String joining | The string form of `implode` joins only string entries, omitting nonstrings and their delimiters. [Implementation][fl-implode-strings]. | Native [`implode`](efun/implode.md) stringifies every element, including nil. Filter unwanted values explicitly, or initialize unused display cells with `""`, as in the [Lima colour-table adaptation](#lima-message-files-and-colours). |
| String repetition | `repeat_string(text, count)` in `PACKAGE_CONTRIB` repeats the whole string, reducing a positive count to fit the configured byte limit. Nonpositive counts return an empty string. [Implementation][fl-repeat-string], [reference][fl-repeat-reference]. | Replace calls with `text * count`; parenthesize compound operands. Nonpositive counts also produce an empty string. Native results above 8,192 UTF-8 bytes raise an error instead of returning fewer repetitions. Oversized constant expressions can fail compilation; dynamic overflow is a catchable runtime error. Review callers relying on capped output. [Strings](lpc/types.md). |
| Mixed numeric comparisons | Ordering and equality compare int and float operands using C++ numeric promotion, which can round large integers. [Implementation][fl-numeric-comparisons]. | [All six comparisons](lpc/comparisons.md) accept ints and floats without warnings and compare their exact numeric values. For example, `9007199254740993 > 9007199254740992.0` is true. Natural sorting, min/max and array membership use the same numeric rule. Mapping keys, array set operations and grouping remain type-sensitive. |
| Rounding | `round(float)` in `PACKAGE_MATH` returns a float using the C math rounding operation, with halfway values rounded away from zero. [Declaration][fl-math-spec], [implementation][fl-round]. | [`round(int \| float)`](efun/round.md) also returns a float and rounds halfway values away from zero, alongside [`floor`](efun/floor.md) and [`ceil`](efun/ceil.md). Integer arguments are promoted to float and can lose precision at large magnitudes. Use `to_int(round(value))` where a declared integer result is required; native typing does not implicitly convert the result to int. |
| String membership search | `member_array(integer, text, start)` searches bytes from a byte offset. A negative start or one beyond the byte length raises an error. Search stops at NUL, and integer zero can match the terminator. [Implementation][fl-member-string]. | [`member_array`](efun/member_array.md) searches integer Unicode scalars using scalar offsets. A negative start raises an error; a start at or past the end returns `-1`. Zero matches only actual NUL data, and search continues past embedded NULs. Review byte offsets, especially around file I/O. The optional fourth flags argument is unsupported. |
| Array mapping and filtering | `map_array` and `filter_array` alias `map` and `filter` with array declarations. Callbacks can be function values or method names, with an optional receiver for named methods; extra arguments follow the element. [Declarations][fl-core-spec], [dispatch][fl-array-callbacks]. | Use [`map`](efun/map.md) and [`filter`](efun/filter.md) with function values. Translate `(: name :)` to `name`, bound literals to `&name(args)`, and named-method calls to function values for the intended receiver. Native `filter` accepts arrays and mappings; string filtering needs an explicit loop or character-array conversion. No native aliases are provided. |
| Mapping deletion | `map_delete(mapping, key)` removes an entry in place and returns void. [Declaration][fl-core-spec], [implementation][fl-map-delete]. | Use [`m_delete`](efun/m_delete.md) when the result is discarded. It removes the entry in place but returns the same mapping, so a wrapper that promises void should discard its result. Missing keys are harmless. |
| Mapping callbacks | `map_mapping` aliases `map`; map/filter callbacks receive the key, value, then extra arguments. Entries are traversed through the hash table. [Declaration][fl-core-spec], [map implementation][fl-mapping-transform], [filter implementation][fl-guild-filter-mapping]. | Use [`map`](efun/map.md) to produce new values under the same keys, [`filter`](efun/filter.md) to select entries, or [`foreach`](lpc/foreach.md) for side effects. Native callbacks use function values and traversal follows insertion order. Review callbacks whose outcome depends on order, especially inversion of mappings with repeated values. |
| Array callback edge cases | Empty arrays skip callback setup. `filter` accepts non-integer results, including `0.0`. Both loops read live array slots; `filter` copies an accepted slot after its callback returns. [Map][fl-map-array], [filter][fl-filter-array], [zero test][fl-filter-zero]. | Native callbacks must be function values even for empty inputs. `filter` uses native truthiness, rejecting `0.0`, nil and dead objects. Both efuns snapshot the input slots before callbacks; nested mutable values remain shared. Audit callbacks that edit the traversed array. Array `map` retains every callback result, including configured nil from void functions; use `foreach` when only side effects are wanted. |
| String mapping | Calls the callback with each byte up to the first NUL. A nonzero integer result replaces that byte through a `char` conversion; zero or a non-integer result preserves it. Supports function pointers and named callbacks. [Implementation][fl-map-string], [reference][fl-map-reference]. | [`map`](efun/map.md) passes integer Unicode scalars and requires valid integer scalar results. Zero inserts NUL; invalid results raise errors before the next callback. Use explicit identity returns for missing table entries and `foreach` for void callbacks used for side effects. Update byte-indexed translation tables and named callback syntax; native callbacks use function values. |
| Text length | String length and ordinary indexing use extended grapheme clusters. [Strings][fl-strings]. The string overloads of `member_array` and `map` operate on bytes; see above. | String lengths and positions use Unicode scalar values. A combining sequence or multi-code-point emoji can therefore occupy several positions. [Types](lpc/types.md), [length implementation](../lpc-rs-utils/src/lpc_string.rs). |
| Binary values | Mutable `buffer` values, including indexed/range assignment. [Buffers][fl-buffer]. | Immutable `bytes`, with explicit text/byte conversions. Buffer mutation and implicit conversions need adaptation. [Types](lpc/types.md). |
| Text encoding | `string_encode(text, encoding)` returns a buffer; `string_decode(buffer, encoding)` returns a string. ICU supplies the converters; the reviewed decoder trims trailing zero bytes while more than one byte remains before converting. [Implementation][fl-text-encoding]. | Use [`to_bytes`](efun/to_bytes.md) and [`to_text`](efun/to_text.md) with an explicit encoding. Native support is UTF-8 and Latin-1, rejects invalid input, and preserves NULs. Byte offsets and Unicode scalar offsets differ; decode complete sequences. Audit other encodings and error handling separately. |
| Byte-file reads used as text | `read_bytes` returns zero on read failure or EOF; successful reads return a string after replacing invalid UTF-8. The underlying read seeks by byte offset and enforces the configured transfer limit. [Wrapper][fl-byte-read], [sanitizer][fl-utf8-sanitize], [range read][fl-byte-read-range]. | [`read_bytes`](efun/read_bytes.md) returns immutable bytes or configured nil at EOF; missing files and permission failures raise errors. Use explicit [`to_text`](efun/to_text.md) decoding. For a UTF-8 tail helper, find LF boundaries in bytes and decode the selected suffix once: decoding arbitrary chunks can split characters. Count unterminated final lines, preserve blank lines and delimiters, and handle empty files explicitly. Piped command input is already text and must not be passed as a path. Native range reads currently materialize the whole file before slicing, so a chunk loop does not provide bounded-memory tailing. |
| Regular expressions | Traditional `regexp` and separate `pcre_*` efuns. [Implementation][fl-regexp], [PCRE reference][fl-pcre]. | Rust `regex` syntax; no `pcre_*` API, look-around or back-references. [Reference](efun/regexp.md). |
| Text file writes | `write_file(path, text, flags)` appends by default; bit 1 overwrites, bit 2 compresses, and permission refusal returns `0`. Writes happen during the call, with NUL-terminated contents. [Implementation][fluffos-file-write-flags], [declaration][fl-core-spec]. | [`write_file(path, text, mode)`](efun/write_file.md) accepts only 0/default (append) or 1 (overwrite); compression flags need adaptation. Writes are staged for commit, and pending reads see them. Permission refusal raises a catchable error; delivery failures are logged after commit. Native strings preserve NUL. Replace removal-plus-write save sequences with a single overwrite call so a caught write refusal cannot commit the earlier deletion. |
| `save_object` options | File form returns `1` for success, `0` for failure; flags control saving zero values and compression; a no-file form returns data. [Reference][fl-save]. | Requires a path, returns the path and saves zero-valued globals. No flag or no-file form. Permission/serialisation errors raise errors; delivery failures after commit are logged. [Reference](efun/save_object.md). |
| `restore_object` options | The optional flag `1` preserves globals omitted from the file; otherwise non-static globals are cleared before restoration. [Reference][fl-restore-object]. | Takes only the path and always preserves omitted globals. Remove flag `1`; code relying on the upstream clearing default needs explicit initialization. Permission refusal raises an error; a missing or unreadable file returns `0`. [Reference](efun/restore_object.md). |
| Saved-file migration | The reader skips `#` lines, decodes unescaped carriage returns inside strings as newlines, and reads positional `(/.../)` class records. [Implementation][fl-object-persistence]. | Headers need removal, string values need decoding and re-encoding, and classes need nominal identities and typed named fields. Header removal alone does not establish equivalence. See the [native format](save-format.md) and [Lima persistence notes](#lima-daemon-persistence). |
| Value serialization | `save_variable(mixed)` returns a string and `restore_variable(string)` reads one, using the driver's object-save format. [Save][fl-save-variable], [restore][fl-restore-variable]. | Use [`save_value`](efun/save_value.md) and [`restore_value`](efun/restore_value.md). They use the native [save format](save-format.md), including class and bytes encodings and configured nil handling. Renaming calls does not establish compatibility with existing saved strings; audit and migrate persisted data separately. Cross-driver data interchange is **not checked**. |
| Named global reads and writes | `fetch_variable(name, object?)` and `store_variable(name, value, object?)` default to the current object. Lookup takes the first match, searching inherits before the object's own globals. The implementation rejects a private match on a foreign target; it does not enforce the reference's public-only description. Self-access includes private globals, and `nosave` globals remain accessible. The setter copies the value without a declared-type check. [References][fl-fetch-variable], [setter][fl-store-variable], [implementation][fl-named-globals], [lookup][fl-global-lookup], [assignment][fl-global-assignment]. | Read through [`variable_info`](efun/variable_info.md); no native `fetch_variable` is needed for a name lookup. The native setter takes the target **first**, requires a live object, and accepts an optional exact declaring-source path. Duplicate names require that selector; it does not choose the first match. Writes check retained types, configured nil semantics and scalar class identity. Foreign writes require [`valid_store_variable`](apply/master/valid_store_variable.md), including public fields, separately from inspection permission. Update argument order, duplicate-name handling, type assumptions, and master policy together. |
| Rebinding named-global helpers | `bind` can change an eligible function pointer's owner after `valid_bind` permission. A rebound efun pointer therefore performs named-variable operations as its new owner. [Implementation][fl-bind-source], [pointer context][fl-bound-context]. | Pass the target explicitly to `store_variable` or `variable_info`. Translate the original privilege checks into the native master policies; removing rebinding must not remove its authorization. An inherited helper runs as its containing object, but a simul-efun wrapper runs as the simul-efun object and needs permission to access its caller. General pointer rebinding remains unsupported. |
| Legacy password hashes | `oldcrypt` supports the older MudOS algorithm. [Reference][fl-oldcrypt]. | `crypt` does not recognise that format. Existing password data needs a migration plan; renaming the efun is insufficient. See [stored passwords](#stored-passwords) and the native [`crypt` reference](efun/crypt.md). |

#### Lima port configuration

The Lima port targets **`LPC_NULL_MODE=distinct`** with
**`LPC_CALL_OUT_PRESERVE_PLAYER=true`**. Its adaptations and active
validation use that configuration only. Earlier local runs also exercised
zero mode; those results are historical evidence, not a Lima compatibility
requirement. The driver supports both settings for different mudlibs, with
separate native tests for each. Distinct nil does not emulate FluffOS
`undefinedp` metadata; mapping membership and omitted arguments still need
the adaptations described below.

#### Lima source coverage

A fresh compile inventory on **6 October 2026** checks every **1,180 `.c` file**
in the local overlay after FTP adoption: **534 compile and 646 fail**, with no
compiler crashes or timeouts. These are standalone-source counts, including
partial modules, optional and contributed code. A failed partial module can
compile inside its containing object; a first failing inherit can also hide
later errors. The counts do not measure port completion or establish upstream
bugs. Target objects are not initialized during these checks.

The master, simuls, user, body, both shells, IMUD, IMAIL, HTTP, FTP and RST
compile, as do all **90 verb files**. At that inventory baseline,
**21 of 67 player commands** and **14 of 81 wizard commands** compile. Forty-five
player commands stop first at the missing protected dispatch adapter described
[below](#lima-command-dispatch-and-stdio); `colours` stops at legacy inline-function
syntax. The batches below port thirteen of these commands; other entries still need
dispatch, callback and type adaptations while retaining private handlers.

Other sources expose nil returns, integer literals supplied to float setters,
class/callback syntax, containing-object hooks and override signatures. The
lexical scan's 16 explicit native references all name existing registry entries;
that checks spelling rather than API or runtime compatibility. Successful fresh
startup and the HTTP/FTP tests establish the paths described below, while broad
command, world and optional-source compatibility remains unfinished.

#### Lima basic player commands and help

The subsequent source batch, reviewed on **6 October 2026**, adds the protected
[dispatch adapters](#lima-command-dispatch-and-stdio) to `help`, `brief`, `verbose`
and `save`, preserving their private handlers. Their four sources now compile.
[Help command][lima-player-help], [brief][lima-player-brief],
[verbose][lima-player-verbose], [save][lima-player-save].

In the [help viewer][lima-help-viewer], use native callbacks and nil topic arrays,
search strings for directive colons and split formatter strings into line arrays
before paging. The [RST formatter][lima-rst-viewer] also needs nil for absent
header strings and released code arrays. In [HELP_D][lima-help-timers], remove
empty pathname segments before selecting directory names and use explicit
integer zero for absent restriction rows, retaining the existing comparison
expression and configured levels.

The seven-file adaptation passes nine compile checks and six ordinary-input
functional tests with real Lima master/simuls, user, human body and player shell.
Tests cover shared brief/verbose flags, saved flag restoration in new bodies,
plain/RST/Markdown help, multiple topic selection, missing topics and returning
to the shell. Workspace Clippy and formatting pass. Fixture access methods,
account setup, help index and topic files are excluded from adoption. Native code,
master policy and optional service settings remain unchanged. Full authentication,
help-index rebuilding, broader help applications and driver startup are not
repeated; the source inventory above remains its earlier snapshot.

#### Lima player status commands

The [HP command][lima-player-hp] and [score command][lima-player-score], reviewed
on **6 October 2026**, also need protected dispatch adapters while retaining
their private handlers. Score accepts parsed arrays or adapts the player shell's
raw text with Lima's existing option parser, including `score -t theme`. Use nil
for omitted argument slots and explicit numeric conversions for capacity
thresholds and the integer karma marker.

The [carrying-weight slider][lima-widgets] needs float values and float mapping
keys to match score's fractional capacity data. Two missing-name string returns
in [PARTY_D][lima-party-daemon], loaded by score, need nil. These four source
adaptations pass six compile checks and six ordinary-input functional tests with
the real Lima master/simuls, user, human body and player shell. Tests cover limb
ordering and changed health/intoxication, default score, experience, money,
karma, guild buffs, real party membership, report themes and fractional carrying
weight with widgets enabled and disabled. Workspace Clippy and formatting pass.
Fixture helpers, account setup and weight objects are excluded from adoption.
Wizard target selection, wizard-shell delivery, alternate combat configurations,
broader party workflows and full driver startup remain unchecked by this batch.
Native code and master policy remain unchanged.

#### Lima equipment and skills commands

The [equipment command][lima-player-equip] and [skills command][lima-player-skills],
reviewed on **6 October 2026**, need protected dispatch adapters that retain their
private handlers. Equip reads `wear_info` fields with native dot syntax and passes
nil explicitly for `format_list`'s optional separator. Skills clears its optional
string with nil and converts training points to text within their conditional
branch before joining strings.

These two source adaptations pass nine compile checks and eight ordinary-input
tests with real Lima master/simuls, user, human body, player shell, weapon and
armour implementations. Tests cover empty equipment, weapon class/durability/
damage/properties, primary and secondary armour, removal, empty skills, class
values, missing parent entries, category filters, narrow/simplified displays and
bonuses without changing stored skill points. The equipment fixture follows the
master's existing recipe-before-daemon preload order within the unchanged task
budget. Workspace Clippy and formatting pass. Fixture helpers, items and account
setup are excluded from adoption. Wizard target selection, ranged weapons,
multi-limb coverage, alternate configurations, crafting operations and full
driver startup remain unchecked by this batch; native code and master stay
unchanged.

#### Lima materials and money commands

The [materials command][lima-player-materials] and [money command][lima-player-money],
reviewed on **6 October 2026**, need protected dispatch adapters that retain
their private handlers. Materials forwards its raw argument; money calls its
zero-argument handler. Existing material quantities, currency denominations and
formatting are preserved, including empty `<>` markers in the no-colour theme.

These two adaptations pass eight compile checks and seven ordinary-input tests
with real Lima master/simuls, user, human body, player shell and material/currency
daemons. Tests cover empty and populated pouches, owned-category counts,
`materials all`, material removal, empty money, fractional denomination reports,
multiple-currency labels and spending without report-side balance changes.
Material setup follows the master's existing recipe preload order within the
unchanged task budget. Workspace Clippy and formatting pass. Fixture helpers and
account setup are excluded from adoption. Alternate configurations, narrow/wrapping
display modes, crafting operations and full startup remain unchecked by this
batch; native code and master stay unchanged.

#### Lima rows and display preferences

The [rows command][lima-player-rows], reviewed on **6 October 2026**, needs the
protected dispatch adapter while retaining its private handler. Bare queries
keep MORE unset when it is nil; numeric arguments update the actual player-shell
preference. The already adapted [frames command][lima-player-frames] remains
unchanged and is checked alongside rows on the current library.

Eight compile checks and seven ordinary-input tests pass with real Lima
master/simuls, user, human body, player shell, frame modules and pager. Tests cover
row queries, updates and help; two-line paging of the buffered frame settings
report, advancement and return to the shell; frame style/theme selection; complete
theme listing; and simplification without overwriting stored frame preferences.
The style catalogue uses direct printf output and retains that behavior.
Workspace Clippy and formatting pass. Only rows.c changes; fixture helpers and
account setup are excluded from adoption. Full startup, login persistence,
terminal negotiation and every style/theme/viewport combination remain unchecked
by this batch; native code and master stay unchanged.

#### Lima version and timezone commands

The [version command][lima-player-version] and [timezone command][lima-player-timezone],
reviewed on **6 October 2026**, need protected dispatch adapters retaining
private handlers. Version uses existing native `__VERSION__` and configured
`__PORT__` values; Lima defines `__HOST__` itself in its configuration header.
Timezone must handle a bare query before numeric conversion and use zero as the
display fallback for an unset body timezone. The underlying value stays nil;
explicit zero and fractional offsets remain floats. Native conversion rules and
the body time module stay unchanged. Lima's existing `||` range condition does
not enforce its advertised bounds and is preserved by this port.

Eight compile checks and seven functional tests pass: six ordinary-input cases
cover version metadata, unset timezone queries, whole/fractional offsets, zero
and documented endpoints; an original-handler control reproduces the nil
conversion failure. Version's fixture uses a configured compiler port without
opening a listener. Workspace Clippy and formatting pass. Only these two command
sources change; fixture helpers, control command and account setup are excluded
from adoption. Full startup, actual listener metadata, login persistence, named
zones and invalid inputs remain unchecked by this batch; native code and master
stay unchanged.

#### Lima player time report

The [time command][lima-player-time], reviewed on **6 October 2026**, needs a
protected dispatch adapter retaining its private handler, an explicit long-format
argument to the mudlib time_to_string helper, and to_float before multiplying
integer body timezone hours by a float. Lima's [localtime header][lima-localtime-header]
expects a GMT offset at index 8; native [localtime](efun/localtime.md) returns a
nine-integer array with the daylight-saving flag there. The report parses the
existing [strftime](efun/strftime.md) `%z` result into signed hour/minute seconds
instead, without changing the shared header or native API.

The real [event daemon][lima-event-daemon] and its daemon-data base need no source
changes for the game-time report with the existing configuration and no saved
event registrations. Ten compile checks and five ordinary-input tests pass through
real Lima user/body/shell/frame objects: unset timezone, positive and negative
fractional hours, explicit zero and an integer body offset. A fixed nonzero test
server zone establishes the local/GMT difference against current wall time;
the tests also check game-time/weekday output, reboot/uptime text, unchanged body
values and return to the shell. Workspace Clippy and formatting pass. Only time.c
changes; the wider test viewport, fixture helpers and test server zone are excluded
from adoption.

Lima's existing player-time formula adds the saved offset to the server local
clock, although its prose describes GMT. Its fixed frame labels also remain when
an unset timezone omits the player date row. These source behaviors are preserved.
Full startup, scheduled event execution, daylight-saving transitions, alternate
game-time settings, display combinations and login persistence remain unchecked
by this batch; native code, master and the game-time configuration stay unchanged.

#### Lima title and description commands

The [title command][lima-player-title] and [description command][lima-player-describe],
reviewed on **6 October 2026**, need protected dispatch adapters retaining their
private handlers. Describe uses a bare callback to its private receive_line
handler with no bound arguments. Native APIs, the body title/naming modules and
the modal input module need no changes. Lima retains title name expansion,
automatic name insertion and its 65-visible-character check before name expansion;
stored colour markup remains intact.

Nine focused compile checks and eight ordinary-input tests pass through real
Lima user/body/shell objects. Tests cover default and custom title queries, explicit
and automatic name expansion, visible-length acceptance/rejection, repeated inline
description updates, multiline entry including blank lines, dot completion,
empty completion, abort and automatic completion after six lines. Accepted
descriptions are verified on real human bodies restoring the saved file; editor
completion and abort return to ordinary shell input. The standalone title module
requires query_name from its containing body; the complete human body compiles.
Workspace Clippy with all targets/features and formatting pass.

Only title.c and describe.c change; fixture accessors and account setup are excluded
from adoption. Full startup, login persistence, introductions-enabled settings,
interactive colour negotiation and display combinations remain unchecked by this
batch; native code and master policy stay unchanged.

#### Lima nickname and plan commands

The [nickname command][lima-player-nickname] and [plan command][lima-player-plan],
reviewed on **6 October 2026**, need protected dispatch adapters retaining their
private handlers. Nickname forwards its argument; plan calls its zero-argument
handler. Plan passes nil for absent initial text and a bare private continuation
to the existing text editor. The current configuration enables EVERYONE_HAS_A_PLAN;
body naming/plan methods and the already-ported editor/input modules need no
further changes.

Nine focused compile checks and eight ordinary-input tests pass through real Lima
user/body/shell/editor objects. Tests cover unset and lowercase nickname queries,
ID and room-lookup updates on replacement, multiline plan saves including blank
lines, both completion terminators, repeated edits, empty completion, abort and
editor help/preview. Accepted plans restore on real human bodies; drafts stay
uncommitted until completion, abort retains prior text, and editing flags/modal
handlers clear before ordinary shell input resumes. Workspace Clippy with all
targets/features and formatting pass.

Only nickname.c and plan.c change; fixture accessors and account setup are excluded
from adoption. Full startup, login/nickname persistence, full parser verb
resolution, file/line editor paths and disabled-plan configurations remain
unchecked by this batch; native code and master policy stay unchanged.

#### Lima cold startup and login

The adapted library was exercised through the actual driver and Telnet listener
on **5 October 2026**, against the Lima revision listed above. New-account
registration, human character creation/selection, entry into the configured
Grand Hall, `look`, `inventory`, logout, and saved-account login after a driver
restart work. The stock configuration grants the first account wizard/admin
status, so this exercises the wizard shell. These are lpc-rs execution results;
upstream execution and complete gameplay compatibility remain unchecked.

Keep cold compilation out of a single login or daemon-constructor task. Expand
the master's preload list into separate entries for races, verbs and recipes,
and preload the starting room's object and room dependencies in order. Each
[`preload`](apply/master/preload.md) receives its own unchanged 4,000 ms budget.
The verb daemon must retain already preloaded verbs instead of destroying and
recompiling them. Preload the concrete player/wizard shells, rather than shared
modules whose hooks are supplied by their final compositions.
[Master][lima-master], [preload list][lima-preload],
[verb daemon][lima-startup-verbs].

The starting room also needs ordinary typing/dispatch adaptations: match the
portal's inherited callback arity, separate void calls from `return`, return
nil for an absent readable string, and accept simple-item constructor data
through a variadic collector compatible with its inherited constructor.
Resolve the names module's optional `invis_name` hook through its final object.
Add the protected `dispatch_main` adapters required by the adapted CMD base to
`inventory` and `quit`; their `main` functions remain private.
[Inheritance](lpc/inheritance.md), [CMD base][lima-command-base].

Logout must record the same character key that entry records. The original
body uses the account ID, which differs when an account owns named characters;
the missing last-login record then sends nil to a string formatter on lpc-rs.
Use `lower_case(query_name())` for the logout record.
[Body entry][lima-body-switching], [body cleanup][lima-body],
[last-login records][lima-startup-last-login].

The subsequent IMUD adoption passes a fresh full boot with **151 preloads and
zero failures**, plus real account/character creation, gameplay and restored
login. Restart reproduces the existing RST cleanup permission failure, so a
clean full restart remains unestablished. See [IMUD adoption](#lima-tcp-wrapper-and-i3-adaptation).
The source inventory above records remaining compatibility work beyond startup.

#### Lima verb preloads and parser feedback

The thirteen failing verb files were additionally reviewed on **5 October 2026**
at the listed Lima revision. All compile and preload after adapting nullable
object/string values to nil, separating void calls from returns, and matching
`write`'s inherited `can_verb_rule(string verb, string rule)` signature.
Numeric predicates and counts remain integers. The `drop` emptiness test can
use `sizeof(all_inventory(this_body()))`; it does not need a native
`first_inventory` helper. [Drop][lima-drop-verb], [write][lima-write-verb].

Translate named callbacks to actual function values: use `get_one` in
`handle_obs`, and `all_inventory` in the readable-item traversal. Native
`(: get_one :)` returns a function value without performing the action.
When forwarding a relation to `get_one(object, object, string)`, insert nil
for the absent tool before the relation. The plural `put` callback needs the
same function-value adaptation. [Get][lima-get-verb],
[function values](lpc/function_type.md).

Board scans should test `ob->is_bulletin_board()` directly: an absent method
returns nil in this configuration, and casting that result to int raises an
error. In the master's [`parser_error_message`](apply/master/parser_error_message.md),
the object supplying a refusal can be a plain verb handler with no `short()`.
Add the object prefix only when that call returns a string, preserving the
reason itself. [Master][lima-master],
[parser callbacks](apply/object/parser_handlers.md).

Real Telnet validation exercises vendor/board absence feedback and taking,
reading and dropping the stock Grand Hall map, checking inventory and room
contents as well as messages. The preceding overlay reproduces all thirteen
compile failures. Successful trade, board composition, inscription and every
registered rule remain unchecked by this smoke; no upstream execution is claimed.

#### Lima troll race

The [troll race][lima-troll-race] was additionally reviewed on **5 October 2026**
at the listed Lima revision. Replace its `@LONG` description with adjacent
quoted strings containing explicit newlines, and use `evt.data` for the two
class-field writes in `health_modify_event`. Keep object method calls as arrows;
[class members](lpc/classes.md#construction-and-members) use dots.
The description, racial statistics and damage adjustment remain unchanged.

Real Telnet validation checks the offered race list and complete troll help,
creates and selects a troll character, exercises room/item commands and logout,
then selects the saved troll again after starting a fresh driver process.
The saved account retains the troll body path and race. Both boots preload the
troll successfully. This smoke does not exercise combat damage or establish
ordinary-player-only paths; no upstream execution is claimed.

#### Lima IMUD authentication

The [authentication module][lima-imud-auth] was additionally reviewed on
**5 October 2026** at the listed Lima revision. Translate the expiry filter to
`(: [string mudname, class auth_data auth] auth.timestamp + AUTH_EXPIRE_TIME > time() :)`.
Schedule its private cleanup through the bare `auth_cleanup` function value,
removing the string-name macro and unused callback global. Keep its existing
single self-rescheduling timer, fifteen-minute cleanup interval, ten-minute
expiry comparison and one-use key removal policy.

A focused inherited host supplies delivery, canonical-name and OOB-reply hooks.
Native tests check requests/replies, key consumption and replacement, actual
cleanup callback dispatch, expiry at the boundary, rescheduling, stopping,
restarting and transaction rollback. These checks establish the adapted module;
the full IMUD daemon still fails in `who.c` on `string title = 0`.
[Who handler][lima-imud-who].

The [socket object][lima-imud-socket] uses FluffOS socket creation, connection,
listening, transfer and write signatures. The native [TCP interface](tcp-sockets.md)
now supplies those transport capabilities with different signatures and events;
the wrapper still needs adaptation. Its `external_start` branch is optional
subprocess support, not an IMUD transport operation. A working intermud service
also needs the codec and daemon adaptations below. Router connectivity and
upstream execution remain unchecked.

#### Lima IMUD networking audit

The [IMUD daemon][lima-imud-daemon], [OOB module][lima-imud-oob] and
[socket wrapper][lima-imud-socket] were reviewed on **5 October 2026** at the
listed Lima revision. IMUD uses outgoing TCP to its router, an OOB listener on
`__PORT__ + 1`, and outgoing OOB connections for mail/file transfers. These paths
all use MUD mode; an outgoing-only connector is insufficient. The wrapper's UDP
and subprocess branches are not used by these paths. Excluding optional
`external_start` support from a port adapter requires no subprocess efun.

FluffOS's socket implementation and value serializer were source-reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, with `PACKAGE_SOCKETS` relevant to
sockets and `PACKAGE_EXTERNAL` to subprocesses; IPv6 paths depend on `IPV6`.
No upstream build or execution was performed. MUD mode frames serialized values
with a four-byte network-order length, including the payload's terminal NUL.
Its write results distinguish rejected data from accepted, buffered data, and
release/acquire transfers ownership through a synchronous callback.
[Socket implementation][fl-network-sockets], [value serializer][fl-network-values],
[package signatures][fl-network-signatures], [subprocess implementation][fl-network-external].

The audit recommends a reusable native TCP service carrying
[`bytes`](lpc/types.md), with function callbacks, transactional ownership
and bounded output, and an explicit I3 codec in the mudlib. Native
[`save_value`](efun/save_value.md) has different nil, floating and string forms;
its output cannot be sent directly as an I3 packet. Preserve wire numeric zero
and normalize only nullable protocol fields before typed handler calls.

Socket operations must fit the driver's transaction model: physical IO follows
commit, so admitted bind/connect requests need asynchronous success/failure
events. Preserve per-socket commit order and stable input events across retries.
The logical ownership-transfer callback can run within the current transaction.
Callback context, draining, owner destruction, GC roots and master authorization
need explicit contracts before implementation; the existing interactive
[`write_socket`](efun/write_socket.md) does not provide general TCP sockets.

Compile-only controls also expose ordinary port changes in channel/user-cache
handlers, OOB timers and reply markers, and file metadata/callback reduction.
Later errors remain masked by the file-handler frontier. That audit adopted no
production patch and established no clean complete-daemon compile or transport
execution; the native API was implemented subsequently.

The subsequent TCP interface combines connect/listen setup and uses
one event callback for completion, accepted streams, raw reads, EOF and closure.
Writes admit a whole bytes buffer or refuse it without taking a prefix; the
driver handles partial OS writes. Accepted-stream reads pause during their
listener callback so ownership can be assigned synchronously. Socket commands
need ordered publication through the committer before independent task effect
delivery; physical IO remains outside the committer. The implemented contract is
documented in [TCP sockets](tcp-sockets.md).

The native bookkeeping foundation now provides transactional socket records,
owner inventories, count/output limits, whole-buffer write admission and
commit-ordered command batches. Destruction and failed initialization retire
owned records transactionally, and GC traces registered callback captures.
Retired handles disappear from inspection immediately while retaining their count
charge until queued commands, workers and pending descriptor handoffs are disposed;
retirement cannot bypass the bounds on buffered output by reusing quota early.
Native TCP workers resolve/connect or bind, finish partial
writes in order, drain graceful closes with a timeout, and interrupt retirement.
Readiness, raw input, EOF, accepted children, full-buffer completion and terminal
state publish through the native transaction runner. Transactional FIFO inboxes
retain immutable event payloads until acknowledgement commits; native leases reread
registration on each attempt. Input bytes and pending accepts have separate positive
limits. Pending accepts also reserve VM quota before the physical accept, including
one unpublished slot while a listener waits for a connection. Accepted-child reads
and queued writes stay paused until its listener event succeeds; failure or listener
close discards undelivered children. EOF preserves the write side, and repeated
output-empty transitions share one pending `drained` wakeup. Close and terminal
failure remove queued drained wakeups. Accepted descriptor handoff survives listener
close after successful notification, and activation checks both the scheduling owner
and the callback's owner/receiver. Deterministic native
and local loopback tests cover this layer. The native runtime
dispatches three-argument LPC callbacks serially per handle while allowing
other sockets to progress independently. Every attempt leases the same event with
the current owner and pointer; callback writes and acknowledgement commit together.
Uncaught errors abort callback writes, validate the failed attempt's reads, report
through the existing runtime-error path and discard only that registration's event.
An ownership change invalidates a stale error as well as a successful old callback.
Failed accepts retire undelivered children, and terminal errors still retire metadata;
an unresolvable pointer retires the socket. Entries use driver origin, no player or
command context, and the scheduling owner as caller even for another receiver.
Native pointer type checks, named-function rebinding and retained closure code apply.
The [eight TCP efuns](tcp-sockets.md) expose creation, whole-buffer writes, close,
inspection and synchronous ownership offers, gated by [valid_socket](apply/master/valid_socket.md).
Acquisition requires the named recipient's active offer call. Offers use efun origin
and native catch semantics: a caught error retains prior acquisition, while an
uncaught transaction abort discards it. The driver starts TCP before preloads and
stops it before closing the committer. Lima's bounded I3 codec, MUD-mode TCP
wrapper and tested IMUD router/OOB exchanges are adopted in the local Lima port.
Fresh full-overlay startup and saved-state IMUD/login checks pass; a separate RST
cleanup failure remains on restart.

#### Lima TCP wrapper and I3 adaptation

A further source review on **6 October 2026**, against the same pinned Lima and
FluffOS revisions, finds no additional transport efun required for the router/OOB
paths. Adapt MUD-mode connections/listeners to the native event callback, acquire
accepted children during the listener callback, and deliver `(wrapper, nil)` once
before the child's packets. Keep numeric wire zero unchanged; normalize only
nullable fields before calling typed protocol handlers.

Use bounded framing and an explicit value codec in the mudlib. Native
[`restore_value`](efun/restore_value.md) also has different string escape/newline
semantics and accepts native value forms that do not belong in I3. String and bytes
concatenation have an 8 KiB result limit; conversions can create larger values.
Use bounded fragments or preallocated byte arrays, and send larger frames through
admitted chunks. The native input/output queue budget is not an I3 frame limit.
The diagnostic codec supports a 256 KiB payload, including its terminating NUL,
64 container levels and 32,768 values. It preserves integer wire zero and encodes
native nil as zero. It rejects unsupported values, malformed or incomplete
payloads, duplicate wire keys and nonfinite floats. Floats use the reviewed
writer's six-decimal precision; distinct float keys that alias after formatting
are rejected. Framing extracts one packet at a time, retains unread input and
requires reset after a caught framing or decoding error.

Lima's wrapper closes and immediately destroys its owner. Native destruction
retires queued output, so keep a deliberately closing wrapper alive until its own
queue and native close finish. Actual destruction and driver shutdown still cancel
resources. Clear the daemon's logical reference when intentionally replacing a
router, and guard later callbacks by connection identity. Peer EOF needs explicit
translation to the I3 close path because the native write side stays open.
Catch malformed framing inside the event and commit cleanup: an uncaught callback
error rolls back the cursor while discarding that raw-input event.

The existing master signature compiles, but its wrapper grant must account for
new inspection operations and ownership recipients. Preserve the wrapper's outgoing
privilege check. The absent `send_telnet_nop` keepalive and swallowed outgoing-mail
send failures are existing Lima concerns for integration, not reasons to add native
Telnet framing or weaken queue bounds. Fresh distinct-nil probes and compile controls
support these adaptations.

The diagnostic codec passes compilation and 52 LPC case groups, including
source-derived golden frames, arbitrary fragmentation, coalescing, malformed
input, exact limits and large mudlist/file/mail-shaped packets. Four runtime
tests cover transaction rollback, committed failure/reset, concurrent cursors
and a maximum-size frame received across 33 committed input calls. All checks
use the unchanged four-second task budget; the runtime tests bootstrap the actual
Lima master/simuls. FluffOS is not executed.

The diagnostic MUD-mode wrapper and narrow master grant now pass 16 integration
tests with actual Lima master/simuls/security, loopback peers and deterministic
pre-readiness controls. Output retains at most 512 KiB including frame headers
and 64 frames, retries rejected native writes in order and uses 8 KiB chunks;
the configured native output allowance must admit an individual chunk. Input
dispatches eight packets per batch through transactional continuations. Tests
cover maximum-size frames, accepted-peer nil ordering and independence, EOF
response drain, malformed/truncated input, consumer catches, intentional close,
aborted sends, asynchronous bind failure and foreign-caller restrictions.

The creator controls wrapper mutation; codec methods are protected, native event
pointers private, and endpoint accessors return copies. The original outgoing
privilege check remains; listening retains its existing creation permission.
The initial adapter implements MUD-mode TCP styles, with write callbacks that
notify drained output. A ten-second wrapper deadline bounds closing owners; its
cleanup path is tested by direct driver entry, without timing a real stalled expiry.
The native implementation is unchanged by this mudlib adaptation.

The complete proposed IMUD daemon now compiles after ordinary typing, callback,
class-access, timer and file-metadata adaptations. Twelve integration tests run
its real router/OOB handlers and IMAIL service against local mock peers. They
cover startup, nullable user fields while numeric zero remains an integer, mud
deletion, channel timers, router replacement/shutdown drain, authenticated file
listing and replies, mail admission/acknowledgements, unauthenticated OOB refusal
and asynchronous connection failure. A quota-exhaustion test checks listener
construction cleanup. Failed send admission retains unsent mail.
Router headers and control bodies are checked before their state updates; this
is not a complete I3 service security audit. The absent Telnet keepalive call is
removed from the I3 stream.

The coordinated thirteen-file patch is now adopted in the local Lima overlay.
A fresh full driver boot attempts 151 preloads with zero failures and completes
real Telnet account/character creation, gameplay commands and logout. A second
boot restores the account, character and router password; both boots pass local
mock-router and advertised-port OOB authentication/file-listing checks and stop
with exit zero. The restart also reproduces Lima's previously documented RST
stale-output cleanup permission failure, so a clean full restart is not established.
No RST repair or native change accompanies this adoption. Temporary loopback
configuration and runtime files are not adopted. Actual local-player mail delivery,
file put/get, other socket consumers and actual-router execution remain unestablished.

#### Lima text and binary TCP styles

The [socket wrapper][lima-imud-socket], [HTTP daemon][lima-http] and
[FTP daemon][lima-ftp] were reviewed on **6 October 2026** at the pinned Lima
revision. Extend the local wrapper with text and binary connect/listen styles
using the existing native TCP efuns. Accepted children inherit the listener's
format and deliver `(wrapper, nil)` before data. Text streams encode and decode
UTF-8, retaining partial characters across reads; binary streams deliver native
`bytes` and accept bytes or UTF-8 strings for output. Both use raw stream data
without I3 framing. Empty stream writes do nothing.

Retain the wrapper's output queue and close drain. Write callbacks may send the
next chunk themselves or return it for the wrapper to enqueue, as Lima's FTP file
producer does. Port binary consumers from `buffer` and `read_buffer` to
[`bytes`](lpc/types.md), [`read_bytes`](efun/read_bytes.md) and explicit
[`to_text`](efun/to_text.md) conversions. Wrapper support alone does not make the
HTTP or FTP daemon compatible. Their source adaptations are described below,
and their optional preload entries remain commented. Lima
supplies the older [RCP daemon][lima-rcp] as a disabled `.dontuse` file.

The one-file wrapper extension is adopted in the local Lima port. Six functional
tests cover text/binary connect and accept, split UTF-8 input, raw binary data,
queued writes, callback-produced file chunks and retained MUD framing. Wrapper
and full IMUD compilation, workspace Clippy and formatting pass. That wrapper
change leaves native TCP code and the optional service implementations unchanged.

#### Lima HTTP daemon

The [HTTP daemon][lima-http], [POST example][lima-http-form] and
[web editor CGI entry][lima-http-webed] were reviewed on **6 October 2026** at the
pinned Lima revision. Port the binary read callback to native `bytes` and replace
file `read_buffer` calls with [`read_bytes`](efun/read_bytes.md), handling its
integer-zero EOF result before sending another chunk. Use native class field
access, bare callbacks and typed nil returns.

Decode incoming bytes with [`to_text`](efun/to_text.md) using ISO-8859-1 so the
existing string parser counts one character per wire byte. Form percent decoding
retains its existing behavior. For CGI responses, apply the existing LF-to-CRLF
conversion, encode with [`to_bytes`](efun/to_bytes.md) using UTF-8, and calculate
`Content-Length` from the bytes actually sent. Static files retain their exact
bytes and file-size content length.

The daemon's POST dispatch calls `main(form, headers)`. Add an optional second
mapping parameter to the two shipped entry points while retaining their direct
one-argument use. The web editor entry continues forwarding only the form to its
application. Its application has not been validated by this source port.

The source adaptation uses the existing native TCP APIs and binary wrapper.
HTTP remains optional, with its original preload entry commented and its port
at the configured base plus three. The daemon's existing request parser and CGI
response conventions are retained, including its body-only POST response.

The three-file HTTP adaptation is adopted in the local port. Seven functional
tests pass for root/explicit HTML, exact binary image delivery, an exact file
block, zero/one-argument CGI, UTF-8 CGI content length and two-argument POST form
dispatch. Five CLI compile checks, workspace Clippy and formatting pass. Runtime
checks use lpc-rs with distinct nil, preserved call-out players and the unchanged
four-second task budget. The fixture binds loopback on an ephemeral port and
sends complete request headers; fragmented headers, the web editor application
and broader HTTP conformance remain unchecked. Native TCP, the shared wrapper,
master policy and the frozen predecessor are unchanged.

#### Lima FTP daemon

The [FTP daemon][lima-ftp] and [class/prototype header][lima-ftp-header] were
reviewed on **6 October 2026** at the pinned Lima revision. Place the header's
class declarations after the inherit. Use native class fields, typed nil returns,
bare callbacks and bound timer arguments; logging needs mixed variadic arguments.
Retain the prototypes and give the file producer a bytes result.

Keep the control stream as text and use binary styles for all data connections.
Replace buffer IO with [`read_bytes`](efun/read_bytes.md) and
[`write_bytes`](efun/write_bytes.md) under the existing session privilege. Native
byte writes require an existing file, so recreate the empty upload file after
removing an old one. TYPE A uploads retain CR removal through a reversible
ISO-8859-1 conversion; TYPE I preserves bytes. RETR retains the source's raw
file bytes and existing lack of TYPE A newline conversion. Build directory rows
with [`get_dir`](efun/get_dir.md), [`file_size`](efun/file_size.md) and
[`file_time`](efun/file_time.md), retaining the original listing formats.

Advertise the control connection's numeric local IPv4 address and wait for the
passive listener's actual bound port. The native setup is asynchronous; retain
the 40000–40100 range and ten attempts, with up to 1,000 readiness checks per
attempt at 10 ms intervals. Bind
the session into the common byte callback. Retire the one-use listener through
a subsequent call out so its accept event commits before closing. Native listener
closure discards children whose accept event has not committed, including a
child acquired inside that callback. See [TCP events](tcp-sockets.md).

The common byte callback replaces the source's separate binary passive lookup,
which uses a port key although the passive map stores control-socket keys.
That discrepancy is source-reviewed; FluffOS execution is unchecked. Close
wrappers through `remove` to drain queued replies and file data. Existing
password hashing, wizard/admin selection, anonymous settings and the optional
preload remain unchanged.

The two-file adaptation is adopted in the local port. Six functional tests pass
for anonymous control commands, passive LIST/NLST, active/passive 32 KiB RETR and
actual wizard password login with ASCII/binary STOR. The full daemon/header
compile, workspace Clippy and formatting pass. Tests use lpc-rs, distinct nil,
preserved call-out players and the four-second task budget; temporary accounts,
files and loopback/ephemeral listener settings are excluded from adoption. Native
TCP, the shared wrapper, master policy and frozen predecessor remain unchanged.
External/NAT deployments, binding failure/retry behavior, idle expiry, command
pipelining and broader FTP conformance remain unchecked.

#### Lima RST generation

The [RST daemon][lima-rst-generation] was additionally reviewed on
**5 October 2026** at the listed Lima revision. Replace its `private:` section
with individual private declarations. Directory scans use the names returned
by [`get_dir`](efun/get_dir.md), with [`file_size`](efun/file_size.md) for
directory detection and [`file_time`](efun/file_time.md) for incremental scans;
there is no metadata flag or tuple form of `get_dir`.

Use scalar [`regmatch`](efun/regmatch.md) for warning prefixes and function
prototypes. Native [`sscanf`](efun/sscanf.md) has no regex conversion, and
[`regexp`](efun/regexp.md) accepts arrays. Its indexed result puts the one-based
index before the matched string; adapt the prototype window accordingly.
Translate Spencer word boundaries to Rust regex `\b`. Copy the UTF-8 usage
text with `read_file` and overwriting `write_file`, replacing the absent `cp`.

Pass actual function values to `unguarded` and `call_out`, binding paths where
needed and using bare names otherwise. A closure such as `(: write_indices :)`
returns the function value without invoking it. Keep the existing ten-item
scan batches, filtered directories, saved modification watermark and daily
self-rescheduling sweep. [Function values](lpc/function_type.md),
[timers](efun/call_out.md).

Native validation checks annotated Unicode output, category indices, incremental
callback batches and restoring the saved watermark. A real cold boot preloads
RST and generates 468 pages containing 1,075 function entries under the unchanged
four-second task budget; HELP_D consumes the pages and saves its index.
Complete RST formatting compatibility and upstream execution remain unchecked;
this adaptation adds no native API.

#### Lima intermud mail

The [intermud mail daemon][lima-imud-mail] was additionally reviewed on
**5 October 2026** at the listed Lima revision. Its port needs the bare
`check_queue` function in both timer calls, dotted `.sender` reads from cast
`mail_msg` instances, and nil markers plus `({nil})` removal for invalid string
recipients. Keep integer IDs/timestamps and the existing hourly
self-rescheduling one-shot. [Function values](lpc/function_type.md),
[class fields](lpc/classes.md#construction-and-members),
[null values](lpc/null.md).

A real native cold boot of the subsequently adopted source preloads the daemon
with empty queues, leaving only IMUD's existing who-handler failure. Focused
tests dispatch its hourly callback twice and confirm one successor timer,
unchanged queues and no transport loading. Incoming-recipient checks preserve
the caller restriction, remove unknown To/CC/BCC names through nil markers,
exclude foreign-only recipients and leave empty lists without delivery.
Those checks use a test-only IMUD entry object and the real user registry;
restored nonempty queues, successful delivery, acknowledgements and upstream
execution remain unchecked.
The [IMUD mail module][lima-imud-mail-protocol] uses the transport/OOB machinery
whose unsupported socket dependencies are described above; empty startup does
not establish a working intermud service. This port needs no new native API.

#### Lima event and help timers

Arguments after FluffOS's `call_out` delay are callback data. Native
[`call_out`](efun/call_out.md) instead takes a repeat interval as argument three:
a positive number silently enables repetition, while a string raises an error.
Bind data explicitly, as in `call_out(&process_dir(pathname), delay)` for Lima's
help traversal. Known callbacks without bound arguments use bare names, such
as `call_out(run_scheduler, delay)`. [Upstream contract][fl-callout-reference].

For a single self-rescheduling job such as Lima's event scheduler, replace
`find_call_out("run_scheduler") == -1` with
`!query_call_out(scheduler_id)` and store the ID returned by scheduling. Declare
the handle `nosave`: it belongs to the current VM. Native one-shots are removed
before the callback executes, so the firing callback can schedule its successor.
The query also sees schedules and cancellations made earlier in the same
transaction. Keep the check after the scheduler's work so a direct invocation
still refreshes events while retaining the existing pending timer. This assumes
that all scheduling of this job updates that handle; it is not a general
replacement for name-based lookup. [Native query](efun/query_call_out.md),
[upstream lookup][fl-find-callout].

Retain self-rescheduling one-shots when that is the source's lifetime policy.
A native repeating timer survives errors thrown by a callback that started;
an uncaught error in a one-shot removes that firing and rolls back an attempted
reschedule. Repeating timers therefore change failure behavior as well as how
manual invocations interact with the schedule. [Native contract](efun/call_out.md).

Enable `LPC_CALL_OUT_PRESERVE_PLAYER=true` to retain the scheduling player,
matching Lima's enabled upstream setting. Help also retains its initiator and
can notify that object directly. Use `nil` when clearing the object or returning
an absent topic array, and `!topics` to detect an uninitialized index. The native
setting preserves timer ownership; an ordinary simul wrapper changes it.
A timer with no saved player has no owner fallback. Combat heartbeat adapters
must still establish the combat actor explicitly: the scheduling player can be
a different object. [Callback context](efun/call_out.md),
[Lima configuration][lima-timer-config].

Review completion bookkeeping when moving incremental scans to native
transactions. Help's completion used to run only from a directory callback,
leaving a flat help tree unsaved; an exception before its counter decrement
could also strand the busy state. Use shared completion for the root scan and
directory callbacks, record catchable scan failures, and let queued work drain
before releasing the busy state. Save only a successful index; a failed scan
should retain the previous save and report failure. Also catch completion
notification errors so they cannot undo the final decrement. The reviewed
source is [Lima's help daemon][lima-help-timers].

[`catch`](efun/catch.md) preserves writes and effects made before a caught error,
so directory callbacks already scheduled during a failed scan still need their
counter entries. Partial in-memory results remain visible until another rebuild;
retaining the previous saved file does not restore the previous live index.
[`save_object`](efun/save_object.md) permission and serialization errors can be
caught, but file-delivery failures after commit are logged by the driver.
This bookkeeping does not provide recovery for externally cancelled timers or
an aborted transaction that never reaches this completion routine.

#### Lima object enumeration and state scheduling

Lima's full-list `objects()` calls can use the native efun directly. Convert
predicate calls to `filter(objects(), predicate)`. `objcount` only uses its
callback to increment counters, so use a `foreach` with explicit integer
initialization instead. Domain spawn limits should count clones directly,
including surviving clones whose prototype was removed or replaced.
[Object callers][lima-state-objects], [domain counts][lima-domain-objects].

State queue entries are pairs of a target and extra data; their containing
arrays need `mixed *` declarations. Resolve path jobs when due: an empty match
set removes the job, while multiple matches must remain queued as concrete
targets until processed. Process timestamps oldest first, preserve future jobs
and unfinished work at the processing limit, and remove each current job before
calling it so callback additions and cancellations remain effective. Duplicate
checks must compare actual targets rather than collapsing different clones with
the same base name. Queue membership and ownership queries should inspect the
target slot, excluding objects present only in callback data.
[Reviewed state daemon][lima-state-objects].

Replace this daemon's heartbeat with a named repeating
[`call_out`](efun/call_out.md). A two-second initial delay and repeat interval
match [Lima's reviewed heartbeat configuration][lima-heartbeat-config]. Keep
the timer ID in a `nosave` integer and prevent recursive ticks from bypassing
the processing limit. The stateful module's deferred registration also needs
`call_out(internal_add_to_queue, 0)` in place of its string callback.
[Stateful module][lima-stateful-module].

The adaptation catches and reports each failed update, removes that job and
continues with the remaining queue. Native [`catch`](efun/catch.md) retains
writes made before the error; it does not roll back just the failed object's
changes. A whole-transaction abort still rolls back the tick, and the native
repeating timer remains scheduled. This is an explicit queue failure policy,
not automatic recovery for externally cancelled timers.

#### Lima command dispatch and stdio

Lima's [command base][lima-command-base] calls `main` with four arguments, or
with two during resend, while many concrete commands declare a private handler
with fewer parameters. Native [receiving-object resolution](lpc/inheritance.md#functions-supplied-by-the-receiving-object)
requires a visible implementation and checks calls against its definition;
ignored prototypes cannot supply a private child handler. Making `main`
protected and padding its arguments still leaves array handlers incompatible
with the base's raw-string dispatch branch.

Use a protected four-slot adapter in each command and route the base's calls
through it. The adapter calls the existing private handler with its declared
arguments; for a one-argument handler:

```c
protected varargs void dispatch_main(mixed arg, mixed flags,
                                    mixed input, mixed implode_info) {
    main(arg);
}
```

`varargs` permits the omitted slots during resend. Pass `nil` for missing
stdin. The adapter preserves `origin() == "local"`, the object caller and the
current player, but adds a local frame: caller-function/program inspection
inside `main` identifies the adapter and its source. An `evaluate(main, ...)`
callback instead reports `"function_pointer"` in the handler. Keep private
handlers and protected adapters hidden from external calls, and retain the
shell and resend guards. See [origin](efun/origin.md),
[caller inspection](efun/calling_program.md) and
[variadic parameters](lpc/variadic_parameters.md).

In [stdio][lima-command-stdio], use `nil` for nullable strings, pending command
metadata and the optional `more` continuation. Share the normal/raw initialization
path so each invocation clears its buffer, redirect target, flags and pending
metadata. Native [`explode`](efun/explode.md) retains a trailing empty element;
before passing split output to Lima's line-oriented `more`, remove exactly that
last element when the buffer ends in a newline. Preserve other empty lines,
file output and pipeline payloads.

Adapt [wish's pipeline parsing][lima-wish] together with the
[tokenizer][lima-shell-getopt] and stdio. The original stdio scan compares the
whole argument array with `"|"`, allowing later tokens into filenames; changing
only that comparison loses subsequent pipeline stages. The shell also searches
for input redirection across stage boundaries and removes tokens without their
spacing. Its intended empty prefixes need explicit empty strings/arrays because
native [negative slice endpoints](lpc/indexing.md) count from the end.

The exercised LPC adaptation prepares all stages before running handlers or
performing redirect I/O. It retains complete pipe tails and pairs argument
spacing with each stage. It allows one input file on the first stage and one
overwrite/append target on the last. Empty stages/paths, duplicate redirects,
file output plus a pipe, and pipe input plus an input file are rejected. Quoted
and escaped filenames are decoded; unquoted filename words still join with
single spaces. Ordinary argument spelling and the literal-space delimiter
policy are retained. This is the adaptation's grammar, not a FluffOS guarantee.

A public read-only `query_no_redirection()` on CMD lets preflight recognize raw
commands: they consume the remaining text literally, including operators and
unfinished quotes. Normal stages reject unfinished quotes/escapes. Lookup can
load commands during preflight; syntax validation is not a guarantee of no
lookup effects. Runtime I/O exceptions remain uncaught by the pipeline so an
abort can discard scheduled effects. Native [`catch`](efun/catch.md) retains
earlier writes if its caller catches the error and commits normally.

Native [`rm`](efun/rm.md) sees files created earlier in the same task, allowing
stdio and prepared pipelines to create, append and overwrite one target before
commit. Both null modes and committed invocations are exercised. Pending-entry
checks use exact paths; ancestor-directory moves and aliases remain outside
that view. The retained `cat` file-argument loop still needs an EOF adaptation:
[`read_file`](efun/read_file.md) can return `nil` in distinct mode, which its
`!= 0` check does not reject.

Pipeline execution is exercised with both fixture lookup and the adapted
CMD_D, fixture shell/user objects, real master/simul-efuns and protected command
adapters. The [wizard-shell adaptation](#lima-wizard-shell-startup-and-colours)
also exercises command dispatch through full wish; complete expansion and Lima
boot remain separate work.

#### Lima command discovery and argument rules

Reviewed 27 September 2026 against the pinned [CMD_D][lima-command-daemon] and
[getopt][lima-shell-getopt] sources. Execution checks use the LPC adaptation
with `LPC_NULL_MODE=distinct`, `__SENSIBLE_MODIFIERS__` defined,
and the adapted master/simul-efuns. These are not cross-driver execution tests.

Parenthesize assignments inside logical conditions, use native class member
dots and nullable `nil` values, and declare `query_cmds` as `string *`. Replace
`(: cache_dir :)`, `(: is_file :)` and `(: is_directory :)` with bare function
names: native closures containing a name return its function value instead of
calling it. See [function values](lpc/function_type.md).

The adaptation normalizes directory keys, refreshes command names and rules
together, and removes stale rules/names after their files disappear. Absolute
and wildcard command lookups load the directory's rules before decoding
arguments, including on first use. Otherwise the original behavior depends on
whether a search-path lookup has already populated that cache. Cache refresh
treats missing or metadata-hidden directories as unavailable: [`file_size`](efun/file_size.md)
returns the same sentinel for absence and refusal. Directory listing and rule
read errors propagate. Detect `Cmd_rules` through the authorized directory
listing and read it with ordinary authorization; a separate metadata refusal
must not disable its rules. See [`get_dir`](efun/get_dir.md) and
[`read_file`](efun/read_file.md).

Build each rule completely before installing it. A no-argument rule has an
empty prototype, and extra arguments produce a usage error. Invalid descriptors
remain explicit errors for the affected command; they must not fall back to
raw dispatch. The adaptation reports the rule path and line at lookup without
loading the news daemon. Duplicate definitions still use the last entry, with
a warning at refresh. Usage expansion applies to descriptors, not command
names. Unknown options and missing values stop dispatch; a lone `-` is an
ordinary argument and `--` ends option parsing. Quoted empty values are retained;
`:` cannot be supplied as an option. Absent raw arguments use native nil, while omitted typed slots follow the configured null mode.

Retain the existing numeric-prefix parsing, greedy plural handling and
optional-tail convention for this migration. All slots from the first optional
one onward can be omitted, even if later descriptors lack brackets. Existing
non-commands in an earlier search directory still shadow later commands.
Command caches refresh explicitly; filesystem changes are not watched.
Prepared pipelines with real CMD_D lookup reject invalid rules/arguments/options
before any handler or redirection I/O. Lookup may still load command objects.
The [shell persistence adaptation](#lima-shell-persistence) handles registered
state and save files; full wish input and additional command handlers remain
separate work.

#### Lima shell persistence

Reviewed 27 September 2026 against the pinned [M_SAVE][lima-save-module],
[shell][lima-shell] and [wish][lima-wish] sources. Execution checks use the LPC
adaptation with distinct nil, `__SENSIBLE_MODIFIERS__` and
`EVERYTHING_SAVES` defined and the adapted master/simul-efuns. These are native
execution tests with receiving-object fixtures, not cross-driver or full-login
tests.

Use inherited self-inspection through [`variable_info`](efun/variable_info.md),
explicit-self [`store_variable`](efun/store_variable.md), and
[`save_value`](efun/save_value.md)/[`restore_value`](efun/restore_value.md).
The adaptation resolves registered names uniquely, including private and
`nosave` declarations, and preserves declared types and nominal class identity.
Missing or ambiguous names fail. Simul-efun wrappers would be foreign callers
and require master authorization; inheritance preserves self access.

Call protected `add_save` locally: `this_object()->add_save(...)` cannot invoke
it. Register bare callbacks, validate their returned names, discard zero/nil
omissions, and deduplicate registrations. Keep the registry protected and return
a detached array. Restore only registered fields, rejecting unknown fields and
metadata. The shell uses protected M_SAVE inheritance to hide its raw save/load
entry points; the general module's public restore interface still requires its
receiving object to choose an appropriate caller policy.

Initialize shell registrations before restore and avoid repeating setup at
every save. Invoke file operations inside the `unguarded` thunks, clear
`save_info` with nil, and route edits to the real `save_me` method. Restore alias
temporary data before rebuilding aliases from the daemon defaults. Reconstruct
named bindings and module dispatch from saved names; native
[`functions`](efun/functions.md) already returns public names for another object.
Use bound function pointers for dispatch rather than attempting to serialize
closures. The alias daemon also needs native class-member dots.

Decode string inputs before dispatching mapping or positional data. Validate
inventory structure before applying fields or cloning children, including a
nesting bound. The adaptation accepts decoded child payloads, repairing the
legacy loader's string/array mismatch. It retains sticky recursive saving and
the existing restore-depth convention: mapping roots use level 1 to attach
children without moving the root; positional restores move the root for any
nonzero level. Ordinary recursive saving includes living children, while
`save_things_to_string` excludes living direct children. Native clone/load
permissions still apply. Caught type errors can retain earlier writes; an
uncaught error aborts the task. Structural checks do not create a savepoint.

Execution checks cover actual file writes and a VM restart with aliases, shell
variables and named bindings, along with rejected external persistence calls.
Protected shell-save paths round-trip while direct user file reads/writes are
refused.
The [destruction adaptation](#lima-destruction-and-shell-shutdown) also exercises
owner-driven shell removal and restoration after restarting the VM. The
[wizard-shell adaptation](#lima-wizard-shell-startup-and-colours) extends this to
full wish. The [modal-input adaptation](#lima-modal-input-stack) covers its
registered input callbacks; full user login/logout, including body and mailbox
cleanup, remains separate work.

Existing `save_info` and `saved_items` contain serialized values inside saved
strings. Converting an outer `.o` file does not convert those nested payloads.
The [bundled save migration](#lima-daemon-persistence) does not cover player
shell/body saves, and this snapshot provides no authentic examples. Convert
such data explicitly with known class schemas. Native-encoded positional tests
do not establish foreign save-string compatibility; numeric zero is not
implicitly converted to nil for distinct-mode reference fields.

#### Lima destruction and shell shutdown

Reviewed on **27 September 2026** at the pinned Lima revision: the
[destruction wrapper][lima-overrides], [master][lima-master],
[shell][lima-shell] and [user shell lifecycle][lima-user-shell]. The wrapper and
master routines are unconditional in that snapshot. Native execution checks
use distinct nil, the adapted master/simul/security daemon, and actual shell
inheritance with `__SENSIBLE_MODIFIERS__` and `EVERYTHING_SAVES` defined. No
FluffOS execution or complete Lima login/logout is claimed.

Native [`destruct`](efun/destruct.md) requires
[`valid_destruct`](apply/master/valid_destruct.md), including for self-removal.
Through a simul wrapper the native caller is the simul object, and the defining
program is the wrapper source. An immediate-caller-equals-target policy therefore
rejects the wrapper's zero-argument self-removal. Recover the original requester
from the trusted bridge's caller chain; do not grant the simul unrestricted
foreign-removal permission.

Lima's original wrapper catches `remove()` errors and proceeds to native
destruction. Its shell rejects cleanup from callers other than its owner or
local code. Simply authorizing the bridge can therefore discard the shell after
a cleanup refusal, without its final save. [Wrapper][lima-overrides],
[shell guard][lima-shell].

The adaptation checks permission before cleanup and again at native removal.
Its master helper `valid_destruct_request(target)` accepts only the exact live
simul caller and derives the requester; this helper is mudlib code, not a new
driver apply. The native hook checks actual caller identity and the bridge's
exact defining source. Ordinary self-removal is permitted, while foreign
removal uses the target's write protection and Lima's full security call chain.
The master, simul and security daemon still require privilege 1 for self-removal:
the master's public `remove()` must not turn a foreign request into an unchecked
self request. [Master][lima-master], [security policy][lima-security].

Propagate cleanup errors instead of forcing removal afterward. Missing cleanup
methods and cleanup return values retain their existing behavior. Preserve
argument omission: call `ob->remove()` when no second wrapper argument was
supplied, and forward an explicit argument unchanged. This preserves native
integer or declared parameter defaults; explicit nil does not request a default.
The update convention `destruct(ob, 1)` still forwards integer 1.

Keep the existing owner-driven `shell_ob->remove()` path. An ordinary owner
calling `destruct(shell)` is still a foreign request, and the wrapper does not
impersonate the owner during cleanup. The migrated path saves protected shell
state, removes the shell and restores aliases, variables and bindings after a
VM restart. Tests also cover inherited calls, function pointers, self-removal
from timers, privilege refusals and permission changes during cleanup.

The master preload hook must also allow a constructor to finish by destroying
its object. Use a path call with an empty method name to load without requiring
a surviving object; compile and initializer errors still propagate. Lima's
[memory-check daemon][lima-memcheck] intentionally retires during construction
when `__CHECK_MEMORY__` is undefined. A nil `load_object` result alone cannot
distinguish successful retirement from failed initialization.

A caught error retains earlier transaction effects; an uncaught error rolls
them back. If cleanup destroys itself before throwing, an outer catch can
retain that destruction. The wrapper adds no savepoint. Other public removal
methods still need their own caller policy, and controller/child relationships
such as user/body, mailbox and socket ownership need separate review. Do not
infer those permissions from being a daemon or living under `/secure`.

#### Lima wizard shell startup and colours

Reviewed **27 September 2026** against the pinned [wish][lima-wish],
[base shell][lima-shell], [prompt][lima-shell-prompt],
[history][lima-shell-history], [input forwarding][lima-input-forwarding],
[XTERM256_D][lima-xterm] and [ANSI_D][lima-ansi] sources. Native execution checks
use distinct nil, `__SENSIBLE_MODIFIERS__` and `EVERYTHING_SAVES`, the adapted
master/simul/security policy, and full wish. The user fixture captures modal
callbacks; these checks do not establish real modal input, login or FluffOS
execution.

Qualify `shell::create()` and `shell::setup_for_save()` explicitly. Native
[`::name()` resolution](lpc/inheritance.md#calls-and-aliases) selects
the last parent defining the name. In wish, the unqualified constructor selects
the prompt module, leaving the shell owner unset; unqualified save setup selects
shellvars, omitting alias and binding registrations. Keep the explicit prompt
constructor and shellvars registration as well. Restart checks distinguish
successful variable saves from preservation of the full shell state.

Replace unbound `(: name :)` callbacks with bare function names. Under native
[function syntax](lpc/function_type.md), the former returns a function value
from an expression closure. In this shell it leaves input commands unexecuted,
returns a function instead of a prompt, and builds a path of function values.
Apply the correction to the shell bindings, tokenizer, path and prompt hooks,
variable expansion and completion. Expression closures containing actual calls
retain their purpose.

Declare the cached command path and its query result as arrays, use nil for
failed argument expansion, and declare M_INPUT's no-echo argument as an integer.
The history command must accept the argument array supplied by shell dispatch.
Install path hooks and wizard/admin defaults before parent preparation, and
rebuild the cached path from restored values. Preserve custom path order;
admin extension should not add duplicates on repeated preparation. Empty or nil
paths clear the cache, non-string array entries are filtered, and other saved
path values fail explicitly. The adaptation also accepts empty prompts.

Default shell variables load both colour daemons. For their two fixed patterns,
use the existing `reg_assoc` simul and native [`regexp`](efun/regexp.md) over a
one-element array for match predicates. Replace ANSI_D's constant anonymous
callbacks with native expression closures. This adapts these patterns rather
than supplying a general PCRE API. Checks cover plain, ANSI, xterm and vt100
output, custom translations and unknown colour names; recognized but unknown
Pinkfish names are stripped instead of inserting nil into the output parts.

Trim colour-data lines before parsing: the shipped client-mode file uses CRLF,
which otherwise leaves modes such as `"xterm\r"`. Require a supported default
mode and complete, valid numeric tables. Missing files retain native read
errors; malformed rows fail daemon initialization. Table validation does not
create new driver file or regex capabilities.

The shell still needs broader expansion, saved-value and public-mutator policy
review. In particular, variable updates retain their existing save-before-hook
ordering: catching a hook failure can leave the changed value and save intact.
The [modal stack adaptation](#lima-modal-input-stack) covers input dispatch.
[Login admission](#lima-login-admission) covers full user compilation;
body/logout integration remains separate work.

#### Lima modal input stack

Reviewed **27 September 2026** against the pinned [input stack][lima-inputsys]
and [input forwarding module][lima-input-forwarding]. Native execution checks
use distinct nil, `__SENSIBLE_MODIFIERS__`, `EVERYTHING_SAVES` and the adapted
master/simul/security policy. A fixture user inherits the real input stack;
checks execute the callbacks registered with its native connection and include
full wish dispatch. This exercises mudlib dispatch and committed registrations,
not a network login or FluffOS execution.

Pass `secure` directly to [`input_to`](efun/input_to.md) and
[`get_char`](efun/get_char.md), and register the bare `dispatch_modal_input`
function. The legacy `secure | 2` hides ordinary input because any nonzero native
argument means no-echo. The legacy `(: dispatch_modal_input :)` expression
returns a function instead of processing input. Use nil for absent typed class
and callback values. For dead-handler pruning, inspect
[`function_info(callback)["owner"]`](efun/function_info.md); native `functionp`
is a predicate, without the `FP_OWNER_DESTED` bit.

Return integer `1` from [`process_input`](apply/living/process_input.md) after
modal dispatch. An omitted return lets the same line reach native command
actions afterward. Validate or rebuild an empty stack before examining its lock
or routing `!` escapes. The adaptation limits escapes to unlocked line handlers;
character input, including `!`, remains data.

Keep a one-shot handler's dispatch position before popping it so `modal_pass`
reaches the immediately underlying handler. Stop at the bottom, skip dead owners
while bubbling, and clear the dispatch position afterward. Popping past dead
entries should notify the resumed handler once. During `clear_input_stack`,
discard dead entries and send live handlers their existing `-1` cancellation
without resuming handlers or creating a replacement shell. Ordinary and escaped
callback errors both recapture input; caught errors retain their earlier writes.

These changes retain the existing public stack-mutator authority. `modal_pop`
and `modal_func` still rely on the normal post-dispatch recapture, or an explicit
`modal_recapture` when used outside dispatch, to update the pending input mode
and echo setting. Clearing the mudlib stack is not native input cancellation.
[Login admission](#lima-login-admission) and
[terminal metadata](#lima-terminal-metadata) are covered separately.
Body/logout ownership and character-shell key bindings remain separate work.

#### Lima login admission

Reviewed 27 September 2026 against the pinned [Lima login][lima-login],
[master][lima-master] and [user daemon][lima-user-daemon], with `USE_USER_MENU`
enabled. Native checks use distinct nil, the actual master/simul/security
objects, full primary and backup users, and committed input callbacks. They
cover admission and password handling; no complete game login or FluffOS
execution is established.

Match the native applies: `connect(string remote_ip, int remote_port)` returns
the user object, and `logon(string remote_ip, int remote_port)` returns integer
`1` to accept the connection. A void logon can print its prompt and then be
rejected. Lima's old unused `connect(int port)` parameter does not itself block
this version's dynamic apply, but its annotation misrepresents the supplied
arguments. See [connect](apply/master/connect.md) and
[logon](apply/special/logon.md).

Convert bound login, userinfo, save/restore and switching callbacks to
`&handler(arguments)`; use bare names for unbound callbacks. Native
`(: handler, arguments :)` is a comma-expression closure, which can leave input
and timeout handlers inert. Enumerate current-object
[`query_call_outs`](efun/query_call_outs.md) and cancel each row's ID at index
four where Lima requests no-argument cancellation. The password prompt must
replace the name timer; rejection and timeout must remove the user's timers.

Lima enables `LPC_CALL_OUT_PRESERVE_PLAYER=true`, allowing its `write` wrapper
to reach the captured user from a timeout. The port retains
[`write_socket`](efun/write_socket.md) on the user object for that notice and
its former `receive` output calls, addressing the owned connection directly.
Apply the same account/callback adaptations to `user_bak`, so the master's
fallback actually works when the primary user fails compilation.

Pass a one-element array to native [`regexp`](efun/regexp.md) and test the
result's size for scalar name checks. The Banish daemon also needs native
`.item` class-field access. Tests cover invalid names, name/site ban entries,
registered-user site exceptions, new password confirmation, saved-account
restoration and rejection after three incorrect passwords.

The initial port accepts fresh passwords and stored SHA-512 `$6$` records.
Unsupported or structurally malformed records receive a password-reset notice
and the connection closes without rewriting the account. Hashing errors reject
the password. This is the port's account policy; native
[`crypt`](efun/crypt.md) supports additional Unix formats. Legacy MudOS hashes
require a separate reset or verification/upgrade path; renaming `oldcrypt` is
insufficient. See [stored passwords](#stored-passwords).

Compile and allocate the menu before removing the login handler. Catch startup
errors, dispose of the newly allocated menu through the existing guarded
privilege helper, stop the shell, cancel timers and close with a generic notice.
Do not let modal recovery turn an unsuccessful handoff into a shell prompt.
Checks retain the previous menu compile failure as a negative control and use
a fixture that fails after pushing input. Successful authentication now cancels
the user's login timers before starting the menu; a password timeout must not
close an authenticated user browsing character choices. The
[player menu adaptation](#lima-player-menu-and-frames) covers real menu startup
and input, including clients without terminal reports using the
[terminal adapter](#lima-terminal-metadata). Body entry remains unfinished.

The [GMCP adapter][lima-user-gmcp] can read
[`query_connection(this_object())["gmcp"]`](efun/query_connection.md) and split
the old message at its first space for native
[`send_gmcp`](efun/send_gmcp.md). Empty messages are ignored and package-only
messages use an empty payload. Native disabled-state and negotiated socket
checks validate framing; see [terminal metadata](#lima-terminal-metadata) for
the incoming hook contract and its fixture coverage.

The port removes the user daemon's periodic Telnet NOP scheduling and its user
method. There is no native mudlib API for sending that command, and writing text
does not substitute for a Telnet command. This disables Lima's application
keepalive; restoring it requires a separate transport feature.
Further user-daemon and body dependencies still prevent full startup.

#### Lima terminal metadata

Reviewed 27 September 2026 against the pinned [input system][lima-inputsys],
[GMCP module][lima-user-gmcp] and [FluffOS transport][fl-telnet]. Lima has
`USE_USER_MENU` enabled; the reviewed FluffOS configuration defaults
`enable gmcp` to `0`. Upstream behavior was reviewed in source. Native tests use
real loopback Telnet connections with distinct nil and Lima's full
master/simul/security and user objects. Terminal-negotiation handoff tests use
a menu fixture; a separate [player menu test](#lima-player-menu-and-frames)
exercises the real menu over Telnet. Complete gameplay login and upstream
execution remain untested.

Read terminal metadata from
[`query_connection(this_object())`](efun/query_connection.md). Lima's
`terminal_type(string)` and raw `telnet_suboption(string)` callbacks are not
native applies, so their cached values otherwise remain unset. Its colour table
uses client names: prefer `"client_name"`, fall back to `"terminal_type"`,
lowercase the name and strip a space-separated version suffix. Return
`"unknown"` when neither is available. Keep the second TTYPE emulation name and
MTTS capabilities separate from the client name; an MTTS reply must not become
the name used for colour defaults.

The native [`window_size`](apply/special/window_size.md) apply already delivers
NAWS dimensions correctly. Reading `"cols"` and `"rows"` directly also works
and avoids maintaining a second cache. Preserve Lima's configured defaults
(79 columns and 40 rows here) for each zero dimension and for an unconnected
object. Its original `width || DEFAULT_SCREEN_WIDTH` expression already
preserves the selected value; no logical-operator workaround is needed.

Derive Lima's `utf8()` from a case-insensitive `UTF-8` or `UTF8` charset
agreement. This port does not infer it from MTTS or assume that UTF-8 guarantees
emoji rendering. Unknown charset keeps conservative emoji defaults. The
driver always sends UTF-8 text, and its charset metadata retains the last
agreement after CHARSET negotiation is disabled. TTYPE withdrawal clears its
names and capability fields. Getters follow live metadata, while late reports
leave initialized shell preferences unchanged. Tests cover both negotiation
before authentication and delayed reports after shell initialization.

For GMCP, query the negotiated flag instead of depending on FluffOS's
`gmcp_enable()` callback. Native incoming
[`gmcp(string package, string payload)`](apply/special/gmcp.md) receives two
arguments; the reviewed FluffOS hook receives one combined string. A fixture
checks delivery with modal name input pending, the user as current player and
no previous object. Lima's current module has no inbound handler. A future
handler must account for messages arriving before authentication.

NEW-ENVIRON is refused, leaving Lima's unused environment cache empty. The
application Telnet NOP scheduler remains disabled. Text containing U+00FF and
U+00F1 encodes as UTF-8 text and cannot substitute for the `ff f1` control
command. Keeping idle connections alive and disconnecting idle players are
separate policies; this migration adds neither a keepalive API nor a periodic
sender.

#### Lima player menu and frames

Reviewed **27 September 2026** against the pinned [frame module][lima-frame],
[menu library][lima-menu], [player menu][lima-player-menu],
[player shell][lima-pshell] and [user messages][lima-user-messages]. The reviewed
[configuration][lima-menu-config] enables `USE_USER_MENU`, with auto-login after
10 seconds and a 300-second menu idle timeout. Native checks use distinct nil,
`__SENSIBLE_MODIFIERS__`, `EVERYTHING_SAVES` and the adapted
master/simul/security objects. They include real authenticated menu input and
quit over loopback Telnet; upstream execution remains untested.

Use native `.field` class access, bare callbacks and `&handler(arguments)` for
bound arguments. Parenthesize the class cast before accessing its fields and
assign before negating a menu choice. Pass just the comparator to
[`sort_array`](efun/sort_array.md). Frame section data contains nested arrays;
declare it accordingly instead of as `string *`. Convert `floor` results to
integers explicitly, keep frame output accumulators as strings, and use nil
for typed absence. Test the `simplify` preference by truthiness: comparing an
unset preference with integer zero incorrectly enables it in distinct mode.

For frame colouring, use the existing `reg_assoc` simul with native
[`regexp`](efun/regexp.md). Escape literal hyphens when building the character
set from frame glyphs: native `--` means set subtraction and the ASCII frame
contains repeated hyphens. Return the original output when the effective
gradient is empty, avoiding colour-reset tags between ordinary characters.
Checks cover all five shipped frame styles, selected widths and colours. This
adapts the frame patterns without adding a PCRE API.

Bind each menu timer to its user explicitly. The callback verifies that the
user still exists and is interactive, sets that user as current player for
the operation, and restores the previous context on both success and error.
A vanished owner retires the menu. Schedule the idle timer after menu startup;
schedule auto-login only for a selected character. Activity cancels auto-login
and replaces the idle timer, using IDs from
[`query_call_outs`](efun/query_call_outs.md). This keeps menu policy in the
mudlib. The wrapper selects and validates the menu owner independently of the
player captured by the call-out preservation setting.

Reject character entry with no selection before accessing the registry. Use
the user's actual [`query_fname` accessor][lima-user-bodies] for the selected
body file. Selection and removal share normalized name lookup and bounded
numeric indices; malformed and overflowing choices leave the menu usable.
Bind the who action's command key explicitly. Both manual and automatic entry
use the same handoff path, with registry access before removing the menu input
handler.

Idle timeout, pre-body quit, player-context restoration and retained input
after a registry failure are exercised with the real user and menu. Successful
manual/automatic entry uses explicit registry and user-entry fixtures to check
the selected name, filename and subsequent modal input; it does not load a
body. The who dispatch check also uses a command fixture. The previous USER_D
compile failure remains a negative control; the
[user-data adaptation](#lima-user-data-and-character-ownership) advances real
entry to the body's compilation blockers. Character creation, complete body
entry, bodyful logout and actual player-command integration remain separate work.

#### Lima user data and character ownership

Reviewed **27 September 2026** against the pinned [USER_D][lima-user-daemon],
[user body records][lima-user-bodies], [login][lima-login],
[body switching][lima-body-switching] and [body][lima-body] sources. Native
checks use distinct nil, `USE_USER_MENU`, `__SENSIBLE_MODIFIERS__` and
`EVERYTHING_SAVES`, with the adapted master/simul/security policy. The reviewed
configuration also enables `AUTO_WIZ`. Upstream behavior is source-reviewed;
these are native execution checks, without upstream driver execution.

Replace USER_D's bound variable reflection with guarded accessors on the user
and body objects. Those accessors inspect their own globals through
[`variable_info`](efun/variable_info.md), reject missing or ambiguous declarations,
and use [`store_variable`](efun/store_variable.md) with the declaring program.
Accept calls only from the actual USER_D object and copy mutable values at the
boundary. USER_D retains its caller privilege checks and field allowlists.
This requires no blanket master permission to inspect or modify other objects.
Primary and backup users use the same accessors; live body access is tested with
an explicit fixture because the real body still does not compile.

Read offline records with [`restore_map`](efun/restore_map.md) and replace them
with [`save_map`](efun/save_map.md), using paths without the save extension.
Preserve unrelated entries and numeric zero, and let malformed records fail
before replacement. Validate assignments equally online and offline: password,
plan and wizard-position values are strings or configured nil; failures are nil
or arrays of two-element integer/string records. `save_map` has no object
declaration against which to check a field. Online assignments retain the former
setter's in-memory behavior and do not implicitly call `save_me`. These checks
use native save files, without importing foreign save formats.

Keep account and character identities separate. Account fields accept either
an account name or its registered character; body fields require the character
name. A body query must select that character's save path or a live body whose
name and link match, rather than whichever body its account currently uses.
Do not silently pick among multiple live account objects or a saved name that
identifies both an account and another account's character. This adaptation
raises explicit ambiguity errors; full simultaneous-login/reconnect policy
remains separate work. Names at these APIs must be canonical lowercase letters
and digits, beginning with a letter.

Treat the registry as a character ownership index. Validate its saved structure
and reject duplicate ownership when loading, preserving the file for repair.
Lookup uses the registered character name, without the old visible-body/nickname
fallback. Registration requires an owning interactive user object or privileged
caller, and a matching body record in the account. It rejects another owner's
character and names colliding with another account. Existing corrupt or ambiguous
records require repair rather than an automatic choice of owner. Removal checks
authority and ownership too; a privileged save helper alone is insufficient.

Route menu mutations through the user. The user stages a new body record,
registers it, restores its previous mapping if registration fails, and saves
after success. Return a deep copy from `query_bodies`; restrict menu mutations to
the owning user's trusted menu or its own code, and accept metadata refreshes
only from the attached body. Removing a character clears its selected entry
and saves the updated account, while refusing to remove the
currently attached character. Use bound native callbacks for file and last-login
cleanup. Login registers `query_userid()` as the owner instead of registering the
character as a second account; repeated registration of the same owner is a
no-op. Use `objectp` filtering for body enumeration so distinct nil does not
survive subtraction of integer zero.

This migration does not resolve full body entry or cleanup. The object foundation
adaptation below resolves the human race's `stat_roll_mods` visibility error.
The container and message adaptations advance compilation to the remaining
[body dependencies](#lima-message-composition-and-delivery). Retained checks
reproduce failed entry recovery in the auto-wizard/first-admin route. With a
body fixture, ordinary user cleanup and
link-dead expiry can fail destruction authorization; scoped authority for the
owned body succeeds, but a throwing cleanup still leaves the user attached.
Those remain explicit integration tasks, including the policy for partial
effects when an error is caught. The fixtures do not establish body persistence,
reconnect or a complete playable login.

#### Lima object foundation

Reviewed **27 September 2026** against the pinned [standard object][lima-object],
[description][lima-description], [names][lima-names], [flags][lima-flags],
[hooks][lima-hooks], [message recipient][lima-recipient] and
[Lima stats module][lima-stats] sources. Native checks use distinct nil,
`__SENSIBLE_MODIFIERS__`, `EVERYTHING_SAVES`, `BODY_STATS_LIMA`, `USE_MASS`,
`METRIC` and `USE_SKILLS`, with `USE_SIZE` disabled and the adapted
master/simul/security policy. These are source review and native execution
checks, without upstream driver execution.

Use protected inheritance for `CLASS_STATMODS` in the configured stats module
when descendants name its `stat_roll_mods` class. A private inherit hides that
class from descendants under native [visibility rules](lpc/inheritance.md).
The alternate DND and Rifts stats configurations need separate validation.
Keep description backing fields private: a descendant's bare `long()` resolves
the method without exposing the same-named field. No receiver workaround or
saved-field rename is needed. See [callable name resolution](lpc/function_type.md).

Use configured `nil` for absent strings and mappings, and floating-point literals
for float defaults. Keep absent flag-map entries explicitly numeric zero before
bitwise operations, including nonpersistent flag sets. Format fractional mass
and value explicitly with [`sprintf`](efun/sprintf.md). Pass `this_object()` to
[`file_name`](efun/file_name.md) and retain extensionless path comparisons.

Pass `pluralize` and `resync_visibility` as bare callbacks. A native closure whose
body is just a function name returns that function value; it does not invoke it.
Pluralization stays in Lima's language module. Replace hook folding through
`implode` with [`reduce`](efun/reduce.md) and initialize sum accumulators to zero.
Use [`function_info`](efun/function_info.md)'s owner field with
[`objectp`](efun/objectp.md) to discard callbacks whose owners were destroyed;
native [`functionp`](efun/functionp.md) is a predicate, not an owner-status
bitmask. Retain callback registration order, argument forwarding and each hook
mode's empty default.

The [verb-support module][lima-object-verbs] must compile in ordinary objects
without container methods. After checking `is_container()`, call the public
`is_relation_alias` and `get_relations` methods through the receiver. Do not
depend on the container's private `valid_relation` helper. Capability fixtures
exercise these calls; they do not establish real container behavior.

For the reviewed string-only recipient calls, guard
[`write_socket(msg)`](efun/write_socket.md) with `interactive(this_object())`.
This writes directly to the current object's connection, bypasses `catch_tell`,
and keeps noninteractive delivery silent instead of producing debug output.
It also works under Lima's existing master compilation policy: explicit
`efun::tell_object` calls from this module are denied, and the simul-efun
`tell_object` wrapper rejects calls. Bare `tell` would re-enter the recipient.
[Master policy][lima-master], [simul-efun overrides][lima-overrides].
Keep that policy intact; standalone host compilation does not exercise
[`valid_override`](apply/master/valid_override.md), so test an LPC-triggered
load as well. The reviewed FluffOS `receive` implementation also writes directly
to the current interactive connection. Its buffer input is outside this
string-only adaptation. FluffOS's core
`F_RECEIVE` path was reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, separately from the overview revision.
[Implementation][fl-object-receive], [declaration][fl-object-receive-spec].

Execution checks cover actual object initialization and cloning, independent
clone state, names, flags, descriptions, hook folding and destroyed owners,
fractional mass/value, attached-connection delivery and simple self-removal.
Removal has no environment or inventory, and message checks use native
connection channels. Container behavior is covered separately below. These
foundation checks do not establish body entry, full object/body persistence,
movement, cleanup or reconnect.

For inventory emptiness checks, `sizeof(all_inventory(container))` counts live
members, including immediately after a destruction in the same transaction.
[`deep_inventory`](efun/deep_inventory.md) also excludes destroyed members and
does not traverse destroyed containers. Neither query revives a destroyed object
when a new object takes its path. These native guarantees are covered separately
from the object-foundation migration; they do not adapt Lima's relation or
capacity bookkeeping.

Native [`find_object`](efun/find_object.md) also restores the same object after
an uncaught destruction error rolls back, including bootstrap objects. Catching
the error keeps the destruction; a committed removal stays absent from lookup.

#### Lima containers

Reviewed **27 September 2026** against the pinned [container][lima-container],
[movement][lima-container-move] and [user object][lima-container-user] sources.
Native checks use distinct nil, `__SENSIBLE_MODIFIERS__`, `EVERYTHING_SAVES`,
`BODY_STATS_LIMA`, `USE_MASS`, `METRIC` and `USE_SKILLS`, with `USE_SIZE`
disabled. The size-based configuration needs separate validation. These are
source review and native execution checks, without upstream driver execution.

In `std/container.c`, return `nil` for an absent relation alias and `0.0` for
an invalid relation's floating capacity. Declare `query_mass()` and the
`receive_object()` mass/capacity temporaries as `float`; maximum capacities
remain integers. Replace the `first_inventory` emptiness check with
`sizeof(all_inventory(this_object()))`. This constructs an array and visits
the inventory, so its work grows with the number of members.

For the reviewed string messages, implement `do_receive()` with
`if (interactive(this_object())) write_socket(msg);`. This preserves silent
noninteractive delivery and bypasses `catch_tell` under the existing master
policy. Native checks cover all four message entry points, nested propagation,
exclusion arrays, sound gates and forwarded message types/payloads. Attached
connection channels exercise delivery; a real body's `do_receive` override
and network lifecycle need separate checks.

In `destruct_if_useless()`, use `link && link->is_user()` to protect containers
holding bodies linked to Lima account objects, including disconnected accounts.
This deliberately uses the [mudlib role][lima-container-user]. FluffOS's
`F_USERP` reads `O_ONCE_INTERACTIVE`; `F_EXEC` transfers that flag to the new connection owner
and clears it on the old one. Those core paths were reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, separately from the overview revision.
[Predicate][fl-container-userp], [connection transfer][fl-container-exec].
The role check also protects a never-connected Lima user and does not protect
an arbitrary interactive object without `is_user()`. Native checks cover both
differences and the real user object before attachment and after detachment,
using a minimal body fixture. Full `net_dead`, cleanup scheduling and reconnect
remain unvalidated.

The reviewed container still has an empty integer `query_total_capacity()`
method, so contained weight contributes zero to `query_mass()`. Set an explicit
canonical default relation: `set_relations()` does not set the first relation
as default, and `set_default_relation(alias)` retains the alias string. Forced
`release_object(target, 1)` also returns before deleting the relation entry;
capacity queries prune that entry after destruction. These existing mudlib
behaviors are preserved by the adaptation. [Source][lima-container].

Ordinary moves, fractional capacity limits, refusals, aliases and relation
changes are covered. A caught error after an extension partially updates
relation bookkeeping preserves those writes and can leave them inconsistent
with physical position. An uncaught error rolls back the move. Extensions that
catch such failures need their own recovery; the adaptation adds no savepoint.
The message-module adaptation below advances body compilation further.
Reset/spawn, full descriptions and light propagation, body entry and persistence
remain open.

#### Lima message composition and delivery

Reviewed **27 September 2026** against the pinned [message module][lima-messages],
[living message wrapper][lima-living-messages], [grammar][lima-message-grammar]
and [string helpers][lima-message-strings]. Native checks use distinct nil
and the container configuration above, with the adapted master/simul layer.
These are source review and native execution checks, without upstream driver
execution.

Pass `nil` explicitly as the second argument to the module's two `format_list`
calls; the existing simul uses its default `and` separator. In observer calls
to `compose_message`, use `nil` for the absent viewer. Give the `has` and `done`
mapping counters a numeric default before incrementing them:
`has[key] = (has[key] || 0) + 1;`. Missing mapping entries are nil in distinct
mode, where arithmetic on nil raises an error. Preserve counts rather than
replacing them with boolean flags: the public object-rendering helpers accept
an existing tracking mapping. [Native null rules](lpc/null.md).

Retain the existing mudlib grammar and pluralizer. Native checks cover actor,
target and observer perspectives, pronouns, articles, repeated objects, lists,
alternatives, punctuation and newlines. Delivery checks combine the actual
message, container and living grammar modules with fixture names and receivers.
They exercise nested inventories, participant exclusion, callback arguments and
message flags; an attached native channel receives one rendered action without
invoking `catch_tell`. These fixtures do not establish real-body or client
formatting behavior.

The compatibility changes preserve several existing message-module limitations.
`filtered_inform` does not apply its filter to array observers; its room form
filters the room before propagation, rather than each descendant. `inform`
deduplicates participants but not array observers, and a self-targeted
`targetted_action` can repeat delivery to the actor and its inventory. Repeated
object lists retain their names because their tracking key changes during
rendering. Review these behaviors separately when using those forms.
[Source][lima-messages].

Guard the environment lookup when using `filtered_simple_action` outside the
world. Passing a missing environment to native `all_inventory` selects the
current object, so an orphan's contents can receive the action twice. Also
review `query_msg`'s declared `string` result alongside its callers and
prototypes: the registry can hold arrays of alternatives.

The [effects queue adaptation](#lima-effects-queues) lets the actual living
compile; the [event-info adaptation](#lima-combat-event-records) advances body
compilation into the combat code. The [message/colour adaptation](#lima-message-files-and-colours)
enables real default-message lookup and message-file loading. The isolated
rendering tests retain an explicit daemon fixture; separate checks exercise
the real registry with living and damage-source consumers. Full body entry
and combat remain unvalidated.

#### Lima effects queues

Reviewed **27 September 2026** against the pinned [effects module][lima-effects],
[effect class][lima-effect-class], [disease implementation][lima-effect-disease]
and [combat consumer][lima-effect-combat]. Native execution uses distinct nil
and the same master/simul layer and container configuration as above; upstream
driver behavior was not executed.

Use `.name` for the class field in `find_effect_indexes_matching`. A compiling
module also needs queue repairs: the source has no tail append, adjusts the
wrong relative delays, tests a void removal result, and constructs head prefixes
with `[0..-1]`. That prefix is the whole array under native negative-bound
slicing. Explicitly handle empty prefixes, preserve surviving deadlines, keep
arrival order at equal deadlines and return a meaningful removal result.
`find_effect` returns a class or integer zero and should not declare an integer
result. [Classes](lpc/classes.md), [indexing](lpc/indexing.md).

Keep one owned timer ID and cancel it when replacing the head or clearing the
queue. Query that ID with [`query_call_out`](efun/query_call_out.md), synchronize
the remaining head delay before editing the queue, and retain the existing timer
when its head is unchanged. The adopted adapter keeps integer-second class
fields, rounding the remaining milliseconds upward; changed deadlines retain
whole-second granularity. Other timers on the same object are left alone.
A direct `next_effect` call consumes the pending timer too.

Retain the counter convention: positive N runs N ticks followed by an end hook
on the next interval, zero ends at the first firing, and negative counts repeat.
Omitted repeat/interval arguments select the effect's defaults; explicit zeros
do not. Function intervals must return integers. Negative delays are queued as
zero. The stored class layout and [`deep_copy`](efun/deep_copy.md) isolation are
preserved. Native save/restore preserves the queue, while timer ownership and
callback guards are nosave state. Reinstatement immediately processes the first
entry of a restored queue; calling it again on an active queue does nothing.
There is no offline elapsed-time calculation.

Retire a missing effect or a failing tick/end/reinstate hook and continue the
remaining queue. The adapter reports caught errors through `dump`; native
[`catch`](efun/catch.md) retains callback writes preceding the error. This is
not a nested rollback. A failing start hook propagates without inserting the
new effect. Removing or clearing an effect during its callback prevents its
reinsertion, and reentrant dispatch/reinstatement does nothing. Clear detaches
its entries before end hooks; effects those hooks explicitly add survive.
Ordinary removal continues to omit end hooks. Driver-level task aborts still
follow the native transaction rules.

Native checks exercise mixed queues, cancellation, callback failures and
mutation, elapsed head time, concurrent additions, persistence, an isolated cold
cycle and actual living initialization. The broader retained migration suite
also runs against the adapter. These checks do not establish full body login,
combat, offline timing or effects retained across live recompilation.

Two independent consumer issues remain. `base_disease` wraps sliced arguments
in another array during infection, changing the percentage payload into a nested
array. Combat expects a mapping of effect objects, whereas `query_effects`
returns an array of classes. Resolve that contract in the mudlib. Body/adversary
compilation advances further with the event-info adaptation below.

#### Lima combat event records

Reviewed **27 September 2026** against the pinned [typed event
class][lima-event-info-types], [queue producer][lima-event-producer],
[combat caller][lima-effect-combat] and [combat configuration][lima-combat-config].
Native execution uses distinct nil, `BLOW_TYPES`, `HEALTH_LIMBS` and the same
adapted master/simul layer as above. These checks combine source review with
native execution; no upstream driver execution is claimed.

In `event_damage`, replace `evt->data` with `evt.data`. In `event_to_str`, use
[`sprintf`](efun/sprintf.md) to render object fields and mixed payloads instead
of concatenating them directly with strings. Formatting only the header leaves
the original loop failing on mapping payload elements. The adaptation uses
`%O` for values and `%d` for element indexes. Nested collections use native
debug rendering, and scalar payloads get an explicit data line. This output is
diagnostic text: `%O` preserves literal string contents and remains subject to
the native nesting and output limits.

Preserve the class layout and producer's array/string distinction. Numeric
damage is wrapped with its damage types; arrays and action strings pass through
unchanged. Supplied arrays remain shared with the queued event. The helper
reads the last array element without clamping or validating its type. Its
declared integer result does not validate a value obtained from `mixed`:
passing an action string or malformed payload is outside the numeric-damage
contract. Native checks cover real producers, compound field updates,
cross-object class identity, save/restore and formatting without payload
mutation. [Class semantics](lpc/classes.md).

The [combat-main adaptation](#lima-combat-main) addresses the next caller
failures: integer damage halving, an omitted list separator and literal zero
for absent limb names. The event helper alone leaves these callers unchanged.

The original producer's weaponless fallback damage type is a string, while
ordinary weapons supply an array of names. The [blows adaptation](#lima-combat-blows)
normalizes that fallback to `({"bludgeon"})`, so resistance consumers receive the
same type-array shape for weaponless events. Other consumers, including natural
armour and the
[damage sink][lima-combat-damage-sink], use index 1 for damage. That agrees with
the helper's last-element convention for ordinary two-element payloads;
arbitrary longer arrays do not have an established shared contract.

A separate `BLOW_SIMPLE`/`HEALTH_LIMBS` producer check finds that its
[class][lima-event-info-simple] declares `target_extra` as an object; passing a
limb-name string fails the declared type check. That configuration needs its
own field adaptation. These checks do not establish full combat, body entry, or support
for all combat configurations.

#### Lima combat main

Reviewed **27 September 2026** against the pinned [main module][lima-effect-combat].
Native checks use distinct nil with `BLOW_TYPES`, `HEALTH_LIMBS`, `WIELD_LIMBS`,
`FORMULA_SKILLS` and `PULSE_HEART_BEAT`; no upstream execution is claimed.

Use `nil` for the absent limb name in dispatch, miss and disarm events. Literal
zero fails the event class's string-field check. Replace `damage * 0.5` with
integer division by two, which truncates toward zero without losing precision
through a float conversion. Supply `"and"` to `format_list` and an explicit zero
initial value to the `prevent_combat` hook reduction; an omitted seed becomes
nil and fails its string concatenation.

The silent-message removal loop can retain its original source. Native
[`foreach`](lpc/foreach.md) retains the collection value at entry, so subtracting
messages from the source variable does not change the collection being traversed.
It shares existing array and mapping elements rather than copying them.

Checks cover action events reaching the real queue, integer damage boundaries,
visible and silent hooks, attack exit paths and direct inheritance. Existing
policies remain: a hook returning zero alone silently prevents combat; the
mapping-shaped effect consumer ignores the actual class-array queue; and an
empty ammunition inventory falls through to ordinary damage. Review these
mudlib behaviors separately. Ordinary unarmed combat supplies the attacker as
its weapon; an explicitly nil weapon still fails the direct method call.

The [behaviors adaptation](#lima-combat-behaviors) handles the next module's
object reset, timer calls and callbacks. Full combat and body entry remain
unvalidated.

#### Lima combat behaviors

Reviewed **27 September 2026** against the pinned [behaviors module][lima-combat-behaviors].
Native checks use distinct nil with `BLOW_TYPES`, `HEALTH_LIMBS`, `WIELD_LIMBS`,
`FORMULA_SKILLS` and `PULSE_HEART_BEAT`; no upstream execution is claimed.

Reset the absent `busy_with` object to `nil`. Retain private, nonsaved integer
IDs for busy and hunting timers, and use [`query_call_out`](efun/query_call_out.md)
and [`remove_call_out`](efun/remove_call_out.md) with those IDs. Schedule the
callbacks with `&end_busy(args)` and `&is_hunted_call(who)`; the busy payload is
one argument, including when it is an array. Cancel the old busy timer before
accepting a replacement, so repeated requests do not leave an earlier timer
untracked. On completion, inspect the current timer ID: an action may already
have scheduled its successor. A pending hunt coalesces arrivals until it fires.

Use bare named functions for arrival, movement and interruption. Under native
closure semantics, the original `(: name :)` expressions return a function
value without invoking it. Store the interrupt callback once and reuse that
value when registering it with the [hook module][lima-hooks]; evaluating the
name again creates a different function identity and duplicates the hook.
Retain the stored arrival callback when moving its hook between rooms, and
guard hook setup when the adversary has no environment.

Replace `functionp("do_game_command")`, which tests a string's type, with
[`function_exists`](efun/function_exists.md) and a dynamic call through
`this_object()`. This permits receiving adversaries without that optional method.

Existing policies remain separate work: interruption invokes the action with
nil while leaving its timer pending; an action error consumes the one-shot timer
but leaves busy state set; and a failed healing search permanently sets
`no_heals`. Failure text is emitted only for an action result of `-1`, despite
the source comment describing zero. With Lima's
`LPC_CALL_OUT_PRESERVE_PLAYER=true` setting, timer callbacks preserve the
scheduling player; no saved player still means nil. Full spell and material
actions using Lima's `this_body()` remain unvalidated. The checks exercise the
real module, hooks and native timers
with controlled receiving objects, not full spell/material execution or combat.

The [condition adaptation](#lima-combat-condition) handles the next module's
numeric contract, callbacks, condition display and ammunition cleanup.

#### Lima combat condition

Reviewed **27 September 2026** against the pinned [condition module][lima-combat-condition]
and [drunkenness constants][lima-drunk]. Native checks use distinct nil with
`BLOW_TYPES`, `HEALTH_LIMBS`, `WIELD_LIMBS`, `FORMULA_SKILLS` and
`PULSE_HEART_BEAT`; no upstream execution is claimed.

Declare `stance_impact` as a float initialized to `1.0`, and use
[`to_float`](efun/to_float.md) for the header's mixed integer/float constants.
Convert the final difficulty with `to_int(period * 10 * stance_impact)` when
passing it to the integer `test_skill` argument. Mixed arithmetic already
promotes the integer operand; assignments and function arguments retain their
type rules. Converting the factor to integer first would discard its fractional
effect. Duration is clamped to 5–30 before this calculation.

Supply `"and"` to `format_list` in the condition display, and pass bare
`message_filter` callbacks to the four filtered action calls. The original
`(: message_filter :)` returns a function value under native semantics, so its
truthy result ignores the recipient's filtering decision.

Use `readied = filter(readied, objectp)` for ammunition cleanup. Subtracting
`({nil})` removes literal nil entries but leaves destroyed object references:
array set operations use [collection matching](lpc/comparisons.md#collection-matching),
while scalar equality treats a destroyed object as nil. The live-object filter
also sees destruction in the current transaction. Preserve cleanup on addition,
deduplication and ordering; `query_readied` returns an independent array whose
objects remain shared.

Several existing behaviors remain separate mudlib work. The 50–79 drunkenness
band reuses the 1.2 factor despite the header defining a separate factor of 2.
`wake_up()` targets a literal `"vital"` limb, absent from the reviewed
[saved body definitions][lima-body-definitions], so it clears sleep without
applying a new stun. Urgent prone/stunned checks precede wake attempts; a later
short stun can shorten an existing deadline; and disarm uses the stance skill's
learning cap. The adaptation preserves these policies.

Checks exercise the real condition module and combat message-filter function
with controlled receiving objects, plus direct inheritance. Full combat and
body entry remain unvalidated. The [target adaptation](#lima-combat-targets)
handles the next module's object resets and final-opponent cleanup.

#### Lima combat targets

Reviewed **27 September 2026** against the pinned [target module][lima-combat-targets];
migration checks ran **28 September 2026**. Native checks use distinct nil with
`BLOW_TYPES`, `HEALTH_LIMBS`, `WIELD_LIMBS`, `FORMULA_SKILLS` and
`PULSE_HEART_BEAT`. `TARGETTING_IS_RANDOM` stays disabled in the active port;
three additional target-selection tests enable it in temporary fixtures.
No upstream execution is claimed.

Replace the four object `target` resets with `nil`. After stopping a specific
fight, use `!target && !sizeof(other_targets)` to decide whether to call
`stop_attacking`. Comparing `query_targets()` with a fresh `({0})` or `({nil})`
array never detects this state under native [array identity](lpc/comparisons.md).
Preserve `query_targets()`'s nil primary slot: an idle object returns `({nil})`.
It returns an independent array, retaining opponent order and shared object
references. Target selection prunes destroyed, ghost and unreachable opponents
through the real inventory helpers.

Both `stop_fight()` and `stop_fight(nil)` notify opponents and clear all targets.
Adapt the corpse caller as described under [corpse handling](#lima-corpse-handling).
Reciprocal `stop_hitting_me` uses
[`previous_object`](efun/previous_object.md) to remove the caller's combat link;
it leaves remote pulse cleanup to that object's combat loop. A stale secondary
entry also keeps the initiating pulse active after a specific-target stop until
selection and caller cleanup run. `get_target` itself does not stop a pulse.

Preserve the existing selection preconditions and callback ordering. A live
candidate requires the actor to have an environment; an idle orphan returns
nil. The main combat caller checks the actor's own ghost/condition state.
A caught peer callback error retains earlier mutations, while an uncaught error
rolls back the native attempt.

Checks cover the included and inherited target module, reciprocal combat links
and real inventory traversal. The [heartbeat adaptation](#lima-combat-pulse)
now uses native repeating timers. Target cleanup checks inspect native timer
state, and pulse checks dispatch the actual callback through the runtime.
Full combat and body entry remain unvalidated. The
[adversary message adaptation](#lima-combat-messages) handles diagnostic
rendering and combat preference callbacks.

#### Lima combat messages

Reviewed and exercised **28 September 2026** against the pinned
[adversary message module][lima-combat-messages]. Native checks use distinct nil
with `BLOW_TYPES`, `HEALTH_LIMBS`, `WIELD_LIMBS`, `FORMULA_SKILLS` and
`PULSE_HEART_BEAT`; random targeting stays disabled. No upstream execution is
claimed.

Use [`sprintf`](efun/sprintf.md) in the missing-target diagnostic: `%O` renders
the actor, weapon and possibly nil limb; `%d` renders the integer percentage.
Object values cannot be concatenated directly with strings. Pass bare
`message_filter` callbacks in `dodge_message` and `handle_message`; the original
`(: message_filter :)` returns a truthy function value and ignores preferences.

Capture `omess` after selecting an array alternative and before resolving its
leading `!` message key. Otherwise, `handle_message(({"!miss"}), ...)` retains
the array as filter metadata and bypasses the recipient's hide-misses setting.
Keep the selected key for filtering even when the damage-source registry returns
an array of rendering templates; the message composer handles those alternatives.

Preserve the existing delivery policies. Normal combat filters the room before
propagating to observers; it does not check each observer's preferences.
Dodging filters the actor, its inventory and other direct room occupants
individually. Brief mode replaces only the requested participant's text and
retains its existing format: no trailing newline and at most three characters
of percentage text. Literal message strings still lose their first character
when deriving filter metadata.

Configuration is cached from the current player's shell until explicit refresh.
Missing weapons suppress delivery after message lookup. A caught receiver error
retains earlier deliveries and the throwing receiver's mutation; an uncaught
error rolls back the native attempt. The adaptation preserves these behaviors.

Checks exercise real composition, delivery and the damage-source registry with
controlled receiving actors, including direct inheritance. Full body entry and
combat remain unvalidated. The [armour setup adaptation](#lima-armour-setup)
handles the next helper's absent path prefix.

#### Lima armour setup

Reviewed and exercised **28 September 2026** against the pinned
[armour helper][lima-combat-armour]. Checks use distinct nil with `BLOW_TYPES`,
`HEALTH_LIMBS`, `WIELD_LIMBS`, `FORMULA_SKILLS` and `PULSE_HEART_BEAT`; random
targeting stays disabled. No upstream execution is claimed.

Change `evaluate_path(s, 0, 1)` to `evaluate_path(s, nil, 1)`: the second argument
is a string prefix, so nil expresses its absence. Keep the final `1` to ignore
the current player's working directory. Relative paths resolve from the
receiving wearer's directory, including when the helper runs during its setup.
Each invocation creates a separate clone, initializes it, moves it to the wearer
and then calls `do_wear`. The prototype is not equipped.

Preserve the existing failure policy when making this type adaptation. A refused
move still leads to `do_wear` with a nil environment; the helper ignores the move
result. A caught movement or wear error retains the created clone and earlier
mutations, while an uncaught error rolls back the native attempt. The existing
object guard skips clones destroyed during creation.

Checks exercise real path resolution, cloning and container movement with a
recording `do_wear` callback. The helper now compiles independently; complete
armour-wrapper checks are described [below](#lima-armour-and-weapons), and real
body entry remains unvalidated. The
[wield setup adaptation](#lima-wield-setup) handles the corresponding absent
path prefix in the next helper.

#### Lima wield setup

Reviewed and exercised **28 September 2026** against the pinned
[wield helper][lima-combat-wield], using distinct nil and the combat options
listed above. No upstream execution is claimed.

Change `evaluate_path(wep, 0, 1)` to `evaluate_path(wep, nil, 1)`. As with armour
setup, nil supplies the absent string prefix and the final `1` ignores the
current player's working directory. Relative paths resolve from the receiving
wielder's directory, including during setup. `set_wielding` initializes a clone,
moves it to the wielder and returns `do_wield`'s integer result. A rejection
result leaves the item in inventory.

`add_equipment` only clones and moves items; it never invokes wielding. Omitted
or zero counts create one item, negative counts create none, and positive counts
create that many clones. It passes paths directly to
[`clone_object`](efun/clone_object.md); tested relative paths use the receiving
object's directory without reading the current player's working directory.

Both helpers ignore refused movement: `set_wielding` still calls `do_wield`,
and `add_equipment` continues creating unplaced clones. Caught errors retain
earlier changes, including completed batch iterations; uncaught errors roll
back the native attempt. Preserve this policy when adapting the prefix type.

Checks use real path resolution, cloning and container movement with a recording
`do_wield` callback. Standalone compilation still needs the receiver's callback;
actual weapon state, body entry and combat remain unvalidated. Body/adversary/
human compilation next needs the [stats base adaptation](#lima-stats-base).

#### Lima stats base

Reviewed and exercised **28 September 2026** against the pinned
[stats base][lima-stats-base], using distinct nil, `BODY_STATS_LIMA`, skills
enabled and introductions disabled, with the combat options listed above.
No upstream execution is claimed.

Replace `filter_mapping` with [`filter`](efun/filter.md), and reset the cached
description with nil in both `clear_description` and `init_stats`. FluffOS's
core alias passes the mapping key and value to the predicate, matching this
caller's use of `$1` as a key. These source findings were reviewed at revision
`44532e02` with `F_FILTER` enabled; no optional package is required.
[Declaration][fl-stats-filter-spec], [implementation][fl-stats-filter-mapping].

Those compile fixes are insufficient for initialization. Default the base
module's absent numeric reads to zero, and populate zero-valued modifiers for
known stat names before reset refreshes the scores. Lima's initializer writes
base scores into an empty mapping; clearing only existing modifiers leaves nil
operands in its arithmetic. For increment, initialize an absent entry to zero
and retain `++`, preserving rejection of string and mapping values. The numeric
reader defaults nil only; it does not validate or convert other stored values.

Preserve broad key-prefix matching, including `"stat"` matching `"static"`.
Filtered mappings share nested values, while `query_stats` deep-copies them.
Hook sums retain their existing callback behavior: caught errors preserve
earlier mutations, and uncaught errors roll back the native attempt.

Checks exercise the base through inheritance and inclusion with real hooks and
the colour daemon. The default Lima module also needs the
[initialization and reset changes below](#lima-default-stats).

#### Lima default stats

Reviewed and exercised **28 September 2026** against the pinned
[default stats module][lima-stats], using distinct nil, `BODY_STATS_LIMA`,
skills enabled and introductions disabled. No upstream execution is claimed.

Reuse the adapted base's numeric readers in the child's getters, calculations
and modifier updates. The child's direct mapping reads otherwise bypass the
base's nil defaults. Default transformation matrices and constant vectors also
need explicit numeric zeros: [`allocate`](efun/allocate.md) fills arrays with
nil in this configuration. Receivers must still call `internal_setup` before
calculating derived scores.

Override `reset_stat_increases` to clear and seed modifiers, then call
`recompute_derived`, which refreshes bonuses after computing pure derived
scores. Calling the inherited reset first refreshes too early. Without this
change, a bonus active during initialization reads an absent derived score;
resetting modifiers later leaves derived scores stale. Initialize through the
new reset once and remove the redundant recomputation and refresh calls.
This reduces initialization from three passes over the four base-stat bonus
tags to one, with derived scores available to the callbacks.

Preserve the existing coefficients, skill contributions and integer rounding.
The `+5000` before division by `10000` rounds negative adjustments asymmetrically;
separately rounded `stat_component` results can also differ from the combined
score. Each component still queries all four bonus tags. Caught errors retain
preceding mutations, while uncaught errors roll back the native attempt.
Invalid roll-adjustment totals still raise after clearing stats and the cache.

Checks cover inherited/included composition, real hooks and colour formatting,
controlled skill callbacks, and the adversary's matrix/vector provider functions
with controlled racial callbacks. Full body entry, combat, saved-stat migration,
complete skill integration, introductions and alternate stat systems remain
unvalidated. Monster setup also needs the
[explicit skill-point conversion below](#lima-monster-setup).

#### Lima monster setup

Reviewed and exercised **28 September 2026** against the pinned
[monster setup module][lima-monster-setup], using distinct nil, `BODY_STATS_LIMA`,
skills enabled and `MAX_SKILL_VALUE=10000`. No upstream execution is claimed.

Wrap the complete interpolated skill-point expression in
[`to_int`](efun/to_int.md) before assigning it to `max_skill`. The expression
produces a float, while the cache and monster `set_skill` parameter are integers.
This makes truncation toward zero explicit; an LPC type-assertion cast does not
convert the value. Keep the floating calculation and rank boundaries unless a
balance change is intended. With the reviewed table, levels 1 through 5 use the
`level * 10` fallback, and level 6 yields 19 after truncation. These are native
port observations, not a claim about upstream implicit assignment conversion.

The existing nonzero cache persists across level changes; zero calculations
leave the fallback uncached. Native `||` already preserves the selected point
value. Setup retains its order: query skills and ranks, check for a player body,
initialize stats, set armour/damage, assign skills, update maximum health and
heal. The outer `setup_monster` wrapper checks for a body before querying skills.
Caught callback errors preserve earlier writes and cache state; uncaught errors
roll back the native attempt.

Checks cover levels 1 through 100, inherited/included composition, and real
adversary skills with the adapted stats and hooks. Rank-provider functions are
extracted from the skill daemon, with controlled skill names and receiving
callbacks. Full skill-daemon behavior, body entry, actual advancement/health,
combat and scheduled setup remain unvalidated.

Compilation then exposes the [corpse callers below](#lima-corpse-handling) and a
separate body signature mismatch. The [body setter][lima-body-skills] overrides
the two-argument monster `set_skill` with a three-argument setter whose final
argument is training points. The runtime body guard does not remove that static
argument-count check. The [body skill adaptation below](#lima-body-and-adversary-skills)
gives the final argument an explicit zero default.

#### Lima corpse handling

Reviewed and exercised **28 September 2026** against the pinned
[corpse death module][lima-combat-corpses], using distinct nil, `DEATH_CORPSES`,
`HEALTH_LIMBS`, default Lima stats and skills enabled. Player equipment dropping
is disabled in the reviewed configuration. No upstream execution is claimed.

Change `stop_fight(0)` to `stop_fight(nil)` and `call_out("remove", 1)` to
[`call_out(remove, 1)`](efun/call_out.md). The first supplies the object-typed
stop-all argument; the second schedules the receiving object's named removal
function after one second. Existing native APIs suffice.

Preserve the order: stop fights, stop attacking, emit the death message, clear
effects, delegate corpse production, then schedule removal. The targeting
module already stops attacking during stop-all, so the explicit call repeats
that action. Direct `drop_corpse` only clears effects and delegates to the loot
daemon; it does not stop combat or schedule removal. The default death message
is installed by [adversary setup][lima-adversary], rather than the
[message storage module][lima-death-messages].

Caught callback failures retain earlier writes and skip later work. Uncaught
errors restore the native attempt, including opponents, effects and their
previous timer. Effect end-hook errors caught by `clear_effects` still allow
loot and removal. A later removal failure rolls back that callback alone and
retires its one-shot timer. Repeated death calls remain unguarded and can
duplicate loot calls and timers; these caller adaptations add no recovery or
duplicate-death policy.

Checks exercise the adopted targeting and effects modules, controlled loot and
receiving callbacks, explicit native timer dispatch, and destruction through
the adopted wrapper. Full corpse production, inventory transfer, decay, actual
message delivery, body/health death entry and full combat remain unvalidated.
The [loot daemon][lima-loot-daemon] still fails on assignment syntax in its
meat/pelt conditional. The [corpse object][lima-corpse-object]'s three-argument
`mudlib_setup` also conflicts with [decay][lima-decay]'s zero-argument definition.
Adversary compilation next needs the [limb health adaptations below](#lima-limb-health).

#### Lima limb health

Reviewed and exercised **28 September 2026** against the pinned
[limb health module][lima-health-limbs], using distinct nil, `HEALTH_LIMBS`,
`DEATH_CORPSES`, `ADVANCEMENT_SIMPLE`, default Lima stats, skills and karma
enabled, and limb shields disabled. No upstream execution is claimed.

Replace class-field arrows with dot access, retaining object method calls and
visible class assertions. Use native [`filter`](efun/filter.md) for the
weighted-limb mapping; its callback receives the key and value, and the existing
predicate uses the key. Return nil when no damageable limb can be selected.
In `hurt_us`, return integer zero damage for a non-damageable limb rather than
returning without a value. Existing native APIs suffice.

Three source-logic repairs accompany these adaptations:

- In `update_body_style`, test collection sizes: empty mappings and arrays are
  truthy. Reject an absent style or one without vital/system limbs before
  replacing the remembered style, weights or health mapping.
- Initialize current health from the already adjusted maximum. Calling
  `hp_adjustment` again applies the scaling twice. With the exact
  [simple advancement formula][lima-advancement-simple], a twenty-HP limb at
  level ten and health stat twenty starts at **180/60** before this repair and
  **60/60** afterward. The formula and maximum-health scaling are unchanged.
- In `remove_abuse`, subtract the successful skill-check bonus instead of
  adding it. Removing three from nine abuse leaves three, instead of fifteen;
  larger reductions still clamp at zero.

Checks use the real [body daemon][lima-body-daemon] and adopted
[saved body definitions][lima-body-definitions], including the fourteen-part
humanoid. They cover selectors, movement, damage, disabling limbs, elapsed
healing, reflex, diagnosis and death. Disabling a vital limb or the last live
system limb kills. Ghost state is set before `die`, followed by party lookup
and XP/karma awards. A caught death error retains the dead state without
awards; an uncaught error restores the attempt. Integration with the adopted
targeting, effects and corpse modules ends queued effects before loot,
cancels their timer and schedules removal.

The HP formula is extracted; full advancement, actual corpse creation/movement,
the real party daemon, player-body death, full combat, pulse scheduling and
alternate health/shield configurations remain unvalidated. Most receiving
callbacks are controlled, and timers are explicitly dispatched through the
native callback path. Existing over-cap drinking, live health mappings and
missing-limb error ordering remain unchanged.

Adversary compilation next needs the [limb wielding adaptations below](#lima-limb-wielding).
Body compilation retains the separate [skill-setter signature mismatch](#lima-monster-setup).

#### Lima limb wielding

Reviewed and exercised **28 September 2026** against pinned
[limb wielding][lima-wield-limbs] and [weapon state][lima-wieldable], using distinct
nil, `WIELD_LIMBS`, `HEALTH_LIMBS`, default Lima stats and skills enabled, with
limb shields disabled. No upstream execution is claimed.

Pass nil as the absent object to `can_wield`, and return nil for absent limb
names. Clear weapon fields with `mark_wielded_by(nil, nil)`. Native dynamic calls
accept the old `mark_wielded_by(0)` and supply nil for its omitted array argument,
but that call stores integer zero as owner. Explicit nil arguments preserve the
intended absence of an owner and limb array. Existing native APIs suffice.

Repair weapon bookkeeping alongside these adaptations. When replacing a weapon,
clear its weapon state, then remove every actor slot referencing that displaced
weapon before installing the new one. Clearing only the requested slot leaves
the other half of a two-handed weapon selectable even though it reports no
owner or wielding limbs. Preserve slots belonging to unrelated weapons.

In `unwield_me`, let the wielder's successful release clear ownership through
`mark_wielded_by`; remove the unconditional owner assignment and flag clearing.
After requesting release for a hand flip or dual-to-single conversion, stop if
the weapon still has an owner. A refused hand flip then preserves its original
slot and metadata, and a refused dual-to-single conversion returns without
recursing. Successful release still clears owner, limbs, flags and the move hook.

Checks cover both handed replacements, normal and refused transitions, real
hook dispatch, skill restrictions and the complete adapted wieldable module.
Real limb health and body definitions also verify that disabling an arm clears
the replacement weapon. Caught callback errors retain earlier writes and stop
the transition; uncaught errors restore actor and weapon state. No callback
recovery policy is added, so a caught weapon-clear error can still leave partial
state before slot cleanup.

Keep wider eligibility rules and return conventions separate. Explicit limb
calls still bypass health/membership checks, and several refused `wield` paths
still yield one from `do_wield`. Duplicate dual-unwield callbacks and empty-slot
notifications are unchanged. The [wield][lima-wield-command] and
[dualwield][lima-dualwield-command] commands still need nil string arguments in
their `do_wield(ob, 0, 1)` calls. Inventory movement, parser dispatch, live body
entry, full combat and alternate wield modes remain unvalidated; flags and
several receiving callbacks are controlled in the tests. Complete weapon
wrapper checks and their dependency limits are described
[below](#lima-armour-and-weapons).

Adversary compilation next needs the [limb armour adaptations below](#lima-limb-armour).
Body compilation retains the separate skill-setter mismatch. The
[weapon adaptation](#lima-armour-and-weapons) replaces `object::ob_state()`
with an explicit inherit alias.

#### Lima limb armour

Reviewed and exercised **28 September 2026** against pinned
[limb armour][lima-armour-limbs] and its [wear records][lima-wear-info], using
distinct nil, `ARMOUR_LIMBS`, `HEALTH_LIMBS`, `BLOW_TYPES` and `PULSE_HEART_BEAT`,
with skills enabled and limb shields disabled. No upstream execution is claimed.

Use nil for absent records, objects and arrays, including array sentinels used
to filter absent armour. Remove the legacy `sizeof(wi) == 2` upgrade: native
[`sizeof`](efun/sizeof.md) does not measure class layouts, and compatible
[class field views](lpc/classes.md) already provide an added `secondary` field.
Checks save a native two-field record, restore it in a fresh VM with the current
definition, and verify that old fields survive while `secondary` initially reads
nil. This does not import foreign-driver class saves or reinterpret integer zero.

Repair layer and coverage bookkeeping alongside the nil changes. `remove_item`
must clear the field matching the item, preserving the other layer and unrelated
covering items. Delete a limb record only when both layers and its additional
coverage are empty. Clear the item's recorded coverage across the mapping;
consulting its current `also_covers()` list can leave obsolete coverage behind.
Removal then needs no coverage-provider callback.

Make `update_cover` preserve the order of items that still cover a limb, remove
obsolete coverage, and add each newly covered limb once. Ignore unworn items
and skip coverage entries where the item already occupies a primary or secondary
slot, avoiding duplicate damage-sink calls. Primary wear needs only one call to
`also_covers()`; its original extra call discards the result. Traverse
`keys(armours)` when pruning records, since [mapping iteration](lpc/foreach.md)
retains the live collection and its initial length.

Checks cover layer preservation, shared and changing coverage, duplicate/self
coverage, callback counts and errors, and real limb-health/body definitions.
The event-consumer method is extracted unchanged from
[the blows module][lima-combat-blows]; item callbacks and several receiving
methods are controlled. Caught wear errors can retain an assigned slot; a failed
coverage refresh preserves the existing coverage because the provider runs
before reconciliation. Uncaught errors roll back writes, including removal
followed by a later error. No general callback recovery policy is added.

Generic underlayers still try only the right limb, and low-level wear eligibility
is unchanged. The [armour/weapon adaptation](#lima-armour-and-weapons) fixes the
wearable module's integer returns in void methods and the armour wrapper's
`object::` spelling. Its isolated checks exercise worn flags and removal through
the complete wrapper. Real stat hooks, inventory movement, parser dispatch,
live body entry, scheduled combat and alternate armour modes remain unvalidated.
The pulse and blows adaptations below address the next selected combat modules.

#### Lima combat pulse

Reviewed **28 September 2026** against the pinned [heartbeat pulse module][lima-combat-pulse]
and [distribution heartbeat interval][lima-heartbeat-config]. Native checks use
`LPC_NULL_MODE=distinct`, `LPC_CALL_OUT_PRESERVE_PLAYER=true`, `PULSE_HEART_BEAT`,
`BLOW_TYPES`, `HEALTH_LIMBS`, `ARMOUR_LIMBS`, `WIELD_LIMBS` and `FORMULA_SKILLS`,
with skills enabled and limb shields disabled. No upstream execution is claimed.

Replace the selected module's `set_heart_beat` calls with one private `nosave`
integer timer ID. Schedule `call_out(heart_beat, 2, 2)`, matching the nominal
two-second interval in the reviewed configuration. Use
[`query_call_out`](efun/query_call_out.md) to avoid duplicate timers and
[`remove_call_out`](efun/remove_call_out.md) to cancel the owned timer immediately
when combat stops, then clear the handle. This is a module-local adapter;
it does not add a general `set_heart_beat` API.

In the callback, use [`set_this_player`](efun/set_this_player.md) to establish the
combat actor, restore the previous player on success and error, and rethrow any
caught error. Use ordinary calls: [Lima's master][lima-master] rejects explicit
`efun::` bypasses
from this module, and no simul-efun shadows `set_this_player`. Player
preservation alone is insufficient here: the scheduling player may be an opponent. The wrapper makes Lima's `this_user()`, `this_body()`
and message routing resolve the actor during a heartbeat. Immediate swings
retain the original incoming context. `this_interactive()` remains nil for the
scheduled callback.

Retain the module's existing target selection, immediate-swing rules,
`continue_fight` response, zero penalty and empty attack-speed setter. Native
repeating callbacks that begin execution remain scheduled after a callback
error. An external cancellation still needs stop/start to rearm while the
existing attacking flag is set. The first callback runs two seconds after
start commits; per-object timers do not reproduce a shared heartbeat phase or
actor ordering.

Checks load the pulse through the real master compilation gate, dispatch actual
native callbacks and cover repeat retention, start/stop, transaction rollback,
error reporting, destruction, actor context and real Lima message routing to a
controlled actor. They also compose the real target module and an extracted
attack method. Wall-clock scheduling, full body/NPC
entry and complete combat remain unvalidated.

#### Lima combat blows

Reviewed **28 September 2026** against the pinned [blows pipeline][lima-combat-blows],
[typed producer][lima-event-producer] and [damage sink][lima-combat-damage-sink],
using the configuration above. Native execution exercises the adapted module
with controlled receiving methods; no upstream execution is claimed.

Use dot access for class fields, retaining arrows for calls through object
fields: `evt.target->query_ghost()`. Pass an explicit final percent `0` to
`handle_message` for special string results, and match its declaration to the
five-argument implementation with a mixed first argument. Normalize the
weaponless producer's type field to `({"bludgeon"})`, matching weapon sources.
The original scalar string fails a resistance search with a string needle.

Treat nil from a resistance, effect, health or armour modifier as discarding
that event. Stop its remaining modifiers and result delivery, then continue
later queue entries. The original outer queue already recognizes nil, but its
inner modifier chains pass discarded events to later callbacks or dereference
them. No general provider-error recovery policy is introduced.

Preserve resistance, vulnerability, source, effect, natural-armour and worn-armour
order. Resistance halves once and vulnerability doubles once with integer
rounding; duplicate matches do not stack. Special string results skip effects
and natural armour, still visit the source and worn armour, and retain miss,
fatal and disarm handling. Successful drains clear the queue, including events
appended during delivery. Uncaught failures roll back the transaction; caught
failures retain partial effects and the queue under native catch semantics.

Checks include actual damage-source and damage-sink modules, with the sink
adaptation below covering armed damage and durability forwarding. Adversary
compilation now reaches `std/adversary/formula/skills.c`, whose object/string
concatenation and float-to-integer assignment need review. Body/human retain
the skill-setter argument mismatch. Full armed armour integration, real combat message delivery,
full body entry and alternate combat configurations remain unvalidated.

#### Lima damage sink

Reviewed and exercised **28 September 2026** against the pinned
[damage sink][lima-combat-damage-sink], [damage daemon][lima-damage-daemon] and
[durability module][lima-durability], using the combat configuration above,
distinct nil and preserved call-out player context. No upstream execution is
claimed.

Change `evt->weapon->decrease_durability(reduced)` to
`evt.weapon->decrease_durability(reduced)`. The original class-field arrow fails
after the armour's durability hook, before the weapon's hook. Dot access forwards
the same reduction to the weapon and preserves event identity. The armed path
through the real producer, source, sink and result handler then delivers damage
and clears the queue.

Preserve the sink's existing policies. With an array of damage types, armour
class contributes only for matching resistances or weaknesses; armour class
alone provides no reduction. Duplicate types stack, and a type can have both a
resistance and a weakness. Clamp damage after all types, then calculate wear
from the net reduction. A weakness can make that reduction negative, but
`decrease_durability` takes its absolute value, so both items still lose
durability. Weaponless events call neither durability hook. Action strings,
including empty and one-character strings, already return unchanged.

The [damage-daemon adaptation](#lima-damage-daemon) below initializes the type
registry before resistance/weakness setters use it. Sink checks use recording
hooks and five unchanged durability methods extracted from that module; the
[durability adaptation](#lima-durability) also exercises the complete module.
These checks cover wear ordering, absolute loss, clamping and rollback. The
[armour/weapon adaptation](#lima-armour-and-weapons) exercises the complete item
wrappers with isolated dependencies; real crafting and message loading remain
separate work.

#### Lima damage daemon

Reviewed and exercised **28 September 2026** against the pinned
[damage daemon][lima-damage-daemon] and [daemon-data module][lima-daemon-data],
with distinct nil and preserved call-out player context. Checks use the real
master, persistence module, stock configuration and native-format save files;
no upstream execution is claimed.

In `create()`, replace `if (damage == ({}))` with `if (!sizeof(damage))`, keeping
it after `::create()` so restored state is available first. FluffOS shares one
empty-array instance and compares array pointers, so Lima's original check
works there. Separately created arrays have distinct identities on lpc-rs,
including empty arrays; use a size check to test emptiness.
FluffOS's core allocator and equality operator were source-reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, without optional-package requirements
or FluffOS execution. [Allocation][fl-empty-array], [equality][fl-array-equality],
[native comparisons](lpc/comparisons.md).

Fresh startup then loads and saves all 13 configured types, their abbreviations
and ten magical attack tables. Real weapon damage-type and armour
resistance/weakness setters accept configured types without an explicit load.
A populated saved list remains authoritative, even if configuration changes or
disappears. Empty saved lists reload configuration at the next startup,
including after `clear_damage_types()`. A partial save without the list also
uses configuration. `nosave` attack weights retain their declared values.

Preserve the loader's existing replacement policy: loading replaces the type
list but retains abbreviation and special-attack entries omitted by the new
configuration. Missing configuration publishes an empty daemon without a save;
empty configuration saves an empty registry. Malformed saved state aborts
loading without overwriting the file. An uncaught caller error rolls back
initial publication and deferred saves. Restart and rollback checks exercise
these paths; full combat remains unvalidated. Complete item-wrapper checks and
their dependency limits are described [below](#lima-armour-and-weapons).

#### Lima durability

Reviewed and exercised **28 September 2026** against the pinned
[durability module][lima-durability], [damage source][lima-combat-damage-source],
[damage sink][lima-combat-damage-sink] and [save module][lima-save-module].
Checks use distinct nil, preserved call-out player context, the real master
and small receivers inheriting these complete modules. No upstream execution
is claimed.

Use explicit [`to_int`](efun/to_int.md) conversions at the two floating results:

```c
durability = 1 + random(to_int(current_max_dura * 0.1));
current_max_dura = to_int(current_max_dura * (1 - (random(5) / 100.0)));
```

This retains the original arithmetic and truncates toward zero. FluffOS's core
compiler inserts these conversions for typed integer efun arguments and integer
assignments. Its compiler and grammar were source-reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, with typed function bodies and core
`random`/`to_int` enabled; no optional package is required.
[Efun conversion][fl-durability-conversions], [assignment][fl-durability-assignment].

In `internal_setup`, replace `this_object()->add_save(...)` with the local call
`add_save(...)`. Dynamic dispatch skips this protected helper and silently leaves
durability out of the save. The local call registers `durability` and
`current_max_dura`. The receiver must supply a visible `add_save` definition,
normally by inheriting `M_SAVE`; compiling the durability module alone therefore
reports an unresolved helper. [Sibling function resolution](lpc/inheritance.md#functions-supplied-by-the-receiving-object).

Preserve the original saved-field policy: `max_dura` is not registered. A fresh
receiver lazily adopts the restored current maximum as its original maximum;
a receiver configured before restoration retains that configured original.
Checks exercise save round trips, repeated registration and transactional rollback.

Repair uses rolls 0–4, despite the source comment saying 0–5, and refills to the
truncated new maximum. Small maxima can reach zero, after which percentage
queries divide by zero. `set_max_durability(0)` is ignored; changing a nonzero
maximum leaves existing durability untouched. These are separate item policies,
not reasons to relax native types. Real source/sink and salvage checks cover
wear and damaged-state initialization. Complete armour/weapon wrappers need
the [adaptations below](#lima-armour-and-weapons); real dependency loading and
the crafting/payment workflow remain separate porting work.

#### Lima armour and weapons

Reviewed and exercised **28 September 2026** against the pinned
[armour object][lima-armour-object], [weapon object][lima-weapon-object] and
[wearable module][lima-wearable], using distinct nil and preserved call-out
player context. Checks use the complete item wrappers, their actual inherits,
the real master and simul efuns. No upstream execution is claimed.

Give `OBJ` an explicit alias in each item wrapper:

```c
inherit OBJ base_object;
```

Replace the four `object::` calls with `base_object::`, preserving their
selected parent and order. `object` is a native type keyword, so it cannot be
used directly as a call qualifier. FluffOS accepts basic-type keywords in this
position; its [grammar][fl-type-qualifiers] was source-reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, with no optional-package or
build-option requirement. Native [inherit aliases](lpc/inheritance.md#calls-and-aliases)
provide the needed selection. Other Lima objects using `object::` need their
own alias adaptations.

Keep each wrapper's integer price API while explicitly converting the inherited
float value: initialize `value` with `to_int(::query_value())`, then store it
with `set_value(to_float(value))`. This preserves truncation toward zero,
nonzero-price caching and the existing policy of reapplying the value factor
on each query. Pass `1.0` to armour's float `set_mass` parameter. In the wearable
module, use bare `return;` for the early exits in void `remove` and `do_remove`.
[Integer conversion](efun/to_int.md), [float conversion](efun/to_float.md).

Isolated checks cover setup, price calculation, durability saving, worn/wielded
state and calls to both parents during removal. Armour notifies its wearer
before base-object release; weapon retains base-object release before its
unwield callback. The recording wearer does not establish real body cleanup
or inventory accounting. Persistent flags and durability fields are saved;
adjectives and wielding limbs still need separate registration/setup work.

Real armour and weapon cloning now succeeds with the
[crafting registry adaptation](#lima-crafting-registry) and prepared recipes.
The isolated item tests still replace salvage-category and message-set lookups
in disposable copies; separate integration checks use the real daemons. These
checks do not establish full combat or complete item restoration. Sword and fullplate still need float arguments at their own value
and mass setters; throwable and ranged-weapon consumers retain separate errors.

#### Lima message files and colours

Reviewed and exercised **28 September 2026** against the pinned
[colour module][lima-message-colours] and [message daemon][lima-message-daemon],
using distinct nil and preserved call-out player context. Checks use the real
master, simul efuns, colour daemon, daemon persistence and stock message files.
No upstream execution is claimed.

In `use_colour`, explicitly convert the palette index with
`to_int(floor(...))`, retaining floor, clamping and the zero-width fallback.
Initialize each row in `colour_table` with `allocate(num_horiz, "")` so unused
cells contribute no text. Native [`implode`](efun/implode.md) stringifies every
element, including nil; an uninitialized cell otherwise prints `nil` in a
ragged table. FluffOS's string form skips nonstrings and their delimiters.
That core implementation was source-reviewed at
`44532e0228f34be2184c701a361df4079371b8b0` on the date above, with no optional
package requirement. [Implementation][fl-implode-strings].

Pass an explicit `"and"` separator to both `format_list` calls in the message
daemon's missing/unknown combat-key diagnostics. Keep message-file parsing,
punctuation and repeated-key alternatives in LPC. Fresh loading reads all
eleven stock files, and real living and damage-source consumers retrieve their
default messages. Unicode gradients and visible lengths use native scalar
positions; table checks cover ragged rows, automatic columns and colour markers.

Retain the existing persistence policy. A restored mapping, including an empty
one, takes precedence over message files. Batch loads update memory only;
explicit saving or a non-batch load persists the result. Base-message reloads
merge keys, while combat/living sets replace their mappings. A missing source
directory or malformed save fails initialization; an existing empty directory
produces an empty saved registry. See [daemon persistence](#lima-daemon-persistence).

Refresh clears the registry before loading files. If its caller catches a later
error, an empty or partially rebuilt live registry can remain while the old
save stays unchanged. An uncaught error rolls back the transaction. Making
caught refresh failures atomic would change the mudlib's existing policy.

`colour_truncate` also retains its leading-newline edge case: the slice ending
at `idx - 1` becomes `[0..-1]` and returns the entire wrapped string when `idx`
is zero. Interactive colour selection and the troll-based message test helper
remain unvalidated. Widgets and player colour commands have separate compile
errors. Real item initialization is covered with the
[crafting registry adaptation](#lima-crafting-registry).

#### Lima crafting registry

Reviewed and exercised **28 September 2026** against the pinned
[crafting daemon][lima-crafting-registry] and [material configuration][lima-crafting-materials],
using distinct nil and preserved call-out player context. Checks use the real
master, simul efuns and daemon persistence. No upstream execution is claimed.

Use `nil` for absent object, string and array results, `0.0` for float zeros,
and explicit integer zero for rejected upgrade registration. Supply `"and"` to
list-rendering helpers. Test recipe presence with `!!crafting_recipes[name]`:
a missing entry is nil, which differs from integer zero. A failed
[`load_object`](efun/load_object.md) can return nil without throwing; check both
the caught error and the returned object before invoking recipe methods.

Initialize material and quality definitions before loading recipes, then
validate upgrade schemes against the loaded registry. Preserve nonempty saved
materials as authoritative; an empty saved mapping reloads configuration.
Quality definitions and recipe indexes are `nosave` and rebuild on startup.
Strip the `quality-` prefix from configuration keys, publish the parsed quality
mapping, and use integer powers of two for its weights. The per-category
quality query derives cumulative thresholds from those weights. Stock metal
and wood retain their eight-level distributions; shorter quality lists work
in the registry, but special-weapon randomization still assumes eight levels.
The unused aggregate quality query retains its legacy shared threshold array.

Explicit configuration reload replaces categories, qualities and upgrade
schemes. Parse before publishing, so a caught parse error leaves those tables
and their save unchanged. Missing configuration logs an error and retains the
current tables; fresh startup retains the built-in quality defaults. Invalid
upgrade entries are reported and omitted, while valid entries are saved.
Rebuild both material lookup caches and the longest-name value after category
changes. Removing a category also removes its upgrade schemes.

Recipe reload builds replacement recipe, filename and station indexes, skipping
files with missing metadata, failed loads, throwing getters or invalid result
types. Repeated reloads do not duplicate station entries; removed files lose
their recipes and filenames. Prune upgrade schemes that reference unavailable
recipes. Restoring such a recipe requires reloading configuration or explicitly
registering its upgrade scheme again. A missing recipe directory aborts before
replacing the indexes. These guarantees concern registry publication; caught
errors can still preserve side effects from recipe initialization or getters.
See [transactional error handling](efun/catch.md).

With eleven recipe objects preloaded in separate calls, initialization registers
39 materials, thirteen stock recipes and all seven stock upgrade schemes;
subsequent armour and weapon cloning succeeds. Loading remains synchronous,
with the 4,000 ms execution budget unchanged. The audit's cold stock scan
exceeded that budget in a debug build, mostly during compilation; a complete
cold boot remains unvalidated.

Eight stock recipes still have value-argument, throwable or food/drink compile
errors. The [repair/payment adaptation](#lima-repair-and-payment) covers daemon
payments with a concrete pockets host, and the
[consumer adaptation](#lima-crafting-and-repair-consumers) covers crafting and
repair handlers. The [furniture/exit adaptation](#lima-furniture-and-complex-exits)
loads the four stock stations; the full body retains compile blockers. Registry
support does not establish a complete crafting workflow.

#### Lima repair and payment

Reviewed and exercised **28 September 2026** against the pinned
[crafting daemon][lima-crafting-registry], [money daemon][lima-money-daemon]
and [body pockets][lima-body-pockets], using distinct nil and preserved call-out
player context. Tests use the real master, simul efuns, saved currency registry
and adapted money/pockets implementations. Upstream drivers were not executed.

Align payment with pockets' existing storage: one floating-point balance per
base currency. Convert denomination deposits and withdrawals exactly once;
missing balances mean `0.0`. Keep `query_amt_money`'s rounded integer result,
with an explicit conversion after `round`. Let `subtract_money` accept fractional
amounts, and reject negative additions, negative withdrawals and overdrafts
before mutation. Currency and denomination membership tests must use truthiness:
missing mapping entries are nil, so `!= 0` incorrectly accepts unknown names.

Replace the money daemon's coin-count/change algorithm with a single debit in
base-currency units. Preserve the two-mapping receipt shape: denominations
representing the payment, then an empty change mapping. Insufficient funds
raise an error; repair preflight returns failure without invoking the debit.
Use the same affordability comparison for quoting and withdrawal, allowing
binary rounding differences within `1e-15 * (abs(balance) + abs(amount))`.
Clear such residuals on final withdrawal. This is floating-point accounting;
the existing denomination formatter still limits displayed precision, including
its small rounding allowance. Saved denomination-count wallets need explicit
conversion before use; this adaptation does not infer or migrate them.

Build repair estimates, displays and payments from the same resource plan.
Deduplicate objects, skip undamaged items, treat absent salvage information as
cash-only, and reject invalid salvage categories or percentages before mutation.
A sparse pouch means zero available pieces. Bound tier lookup by both the
category length and the existing five repair values; names beyond those five
have no repair value. Include exact-value materials, reserve every selected
piece across the whole batch, and debit the extra lowest-tier piece used to
cover a remainder. Percentage-rounding remainders remain payable in cash.
Sum remaining durability units before the single cash conversion.

Check the full cash amount and every positive material count before consuming
resources. Exact funds suffice; shortages leave the items, money and materials
unchanged. Pouch removal takes precedence over matching inventory objects,
consistent with the quote. Zero removal is a no-op, and negative counts fail.
Payment never requests zero-count removals. Render costs and savings with
`currency_to_string`; remove the unused `buy_durability` helper and its calls
to the absent `money_to_array` method.

Resource preflight does not make arbitrary callbacks atomic. An unexpected
removal refusal, debit error or repair-hook error stops execution; a caller
that catches it retains earlier effects. Uncaught failure rolls back the whole
attempt. Lima's modal dispatcher catches handler errors, so it does not provide
that rollback guarantee. See [transactional error handling](efun/catch.md).
The [consumer adaptation](#lima-crafting-and-repair-consumers) covers interactive
crafting and repair handlers. Vendor and accountant modules still need their
own migrations; these checks do not establish an end-to-end shop workflow.

#### Lima crafting and repair consumers

Reviewed and exercised **28 September 2026** against the pinned
[crafting module][lima-crafting-module], [repair verb][lima-repair-verb],
[craft verb][lima-craft-verb] and [salvage verb][lima-salvage-verb]. Native tests
use distinct nil, preserved call-out player context, the real master, simul
efuns, modal input dispatcher and payment implementations, with concrete actor,
station and item fixtures. Upstream drivers were not executed. The shared
[verb base][lima-verb-base] needs `file_name(this_object())`; void craft/salvage
handlers must call their delegates and then return without a value.

Bind confirmation arguments explicitly: `&craft_item_confirmed(body, what)` and
`&repair_with_what(body, items)` accept the reply supplied by the modal dispatcher.
The original `(: handler, arg, arg :)` expressions evaluate their final operand
under native closure semantics; they do not invoke the handler. Capture one body
for quoting and payment, and reject confirmation after the active body changes.
Crafting also requires the body to remain beside the station and the recipe to
remain available there. Repair rechecks item access: single targets may be in
the room or the body's inventory, including nested inventory; batch targets
must remain directly carried. The daemon still recalculates resources at payment.

Check the current recipe, every positive material count and the full fee before
creating an item. Use the shared money affordability check for both recipe and
component fees. Prepare the item and validate its XP before charging, require
`MOVE_OK` and actual placement in the intended body, then recheck resources
after delivery hooks. An ordinary delivery refusal consumes no fee or materials
and grants no XP. Dispose of rejected items through their normal `remove()`
method; direct station-requested destruction can fail Lima's security policy.
Grant XP only after delivery and payment. This does not make arbitrary hooks
atomic: a caught debit failure can retain earlier material removals, and a
throwing cleanup hook can leave the prepared object alive. The existing modal
catch policy remains unchanged.

Parse the complete component request against registry names, with an optional
positive decimal count or `a`, `an`, or `one`. Accept mudlib-generated plurals,
including multiword names such as `fiber optics bundles`; reject trailing words,
zero, negative, fractional, malformed and overflowing counts. Pluralization stays
in `M_GRAMMAR`. Preserve ten-to-one upgrades, one-to-ten salvage, flat fees of
one and five base-currency units respectively, and the existing skill difficulty
and random lower-tier bonus. Retire the unused `simple_salvage2` duplicate,
whose category parser and smith path do not match the live callers.

Let `repair_cost_string` accept an item array as well as a single item, using
the daemon's shared money-only plan and currency formatter. Deduplicate batch
targets and display the same fractional cost that payment uses: a 0.1-gold
repair is quoted as `1 silver`, rather than integer zero. Remove the absent
`repair_obj` call and unused bench lookup. The
[furniture/exit adaptation](#lima-furniture-and-complex-exits) covers stock
stations and movement. Full parser/body integration, vendor/accountant flows
and a complete cold boot remain separate work.

#### Lima furniture and complex exits

Reviewed and exercised **29 September 2026** against the pinned
[complex-exit module][lima-complex-exit], [simple-exit module][lima-simple-exit],
[furniture][lima-furniture], [crafting station][lima-crafting-station] and
[method registry][lima-method]. Native tests use distinct nil and preserved
call-out player context, with `USE_COMPLEX_EXITS` and `USE_INTRODUCTIONS`
undefined and `WIZARDS_SEE_HIDDEN_EXITS` enabled. Tests use concrete actors with
real object/container and smart movement, plus the real master, simul efuns and
registry implementations. All four stock station types instantiate; a weapon
station also completes a fixture recipe through the migrated modal consumer.
Upstream drivers were not executed.

Use nil for absent strings/arrays and an empty array when clearing hidden
directions. Declare filtered exit collections as object arrays, access movement
record fields with dots, and give the object inherit an explicit non-keyword
alias in [the complex-exit object][lima-complex-exit-object]. Register unbound
callbacks as `enter_check` and `the_short`; expressions such as
`(: enter_check :)` return a function value under native closure semantics.
Evaluate a callable obvious description when producing a direction string.

Keep a movement method's destination relation separately from its canonical
registry name. `sit on` is registered as `mount`, but must retain `on` in the
movement record. Give furniture an explicit `on` default; crafting stations
retain their `under` default. Use the full matched key for checks, so
`crawl under` is not rechecked as nonexistent `crawl`. Exact canonical names
take precedence over aliases; reject ambiguous execution and normalize aliases
for removal. Preserve the legacy convention that zero passed to `add_method`
selects its default allow check, while `set_method_checks(..., 0)` denies.

In [the object verb wrapper][lima-object-verbs], apply both container relation
and complex-exit checks when an object implements both. Return the delegated
movement result. Execution must independently reject unknown or ambiguous
methods, check access before evaluating the destination, and evaluate that
destination once. Convert a returned object to its path using that captured
result. The previous code evaluated object destinations twice, even before
reporting a denied check.

Route object exit helpers to the existing singular enter/exit message queries,
plural `set_method_checks`, and object description API. Delegate message and
description updates before requiring a simple-exit mapping entry. Keep enter
and exit defaults separate. Read a simple exit's destination field before
evaluation; `(class move_data)exits[direction].destination` casts the destination
itself and fails on a normal string. Preserve the existing caught-error limits:
an exception after a container releases its actor can retain that release;
these adaptations do not add generic movement rollback.

Initialize a missing method registry with a private configuration builder.
Calling its user-guarded public mutation API from startup fails without a user;
the shipped saved table masks this path. Keep guards on public edits and reloads,
trim configuration fields, skip blank/comment lines, normalize prepositions and
deduplicate aliases explicitly. Build a replacement mapping before publication
and save after successful construction. Valid saved data takes precedence over
configuration. See the [shared helper adaptation below](#lima-array-deduplication)
for other `clean_array` callers.

Door and lockable migration is covered [below](#lima-doors-and-locks). Full
parser-dispatched gameplay, the stock body, portal/climbing/vehicle behavior
and a complete cold boot remain unvalidated.

#### Lima array deduplication

Reviewed and exercised **29 September 2026** against the pinned
[`clean_array` helper][lima-clean-array] and its callers, using distinct nil
and preserved call-out player context. These are Lima source findings and
lpc-rs execution results; no upstream driver execution is claimed.

The helper assumes `r & r` sorts its input, then removes adjacent duplicates.
Native intersection preserves input order, so separated duplicates survive.
This can repeat statistics bonus hooks, callback evaluations, message types
and recipients. Replace that assumption with explicit first-occurrence
deduplication using [`member_array`](efun/member_array.md):

```c
mixed *clean_array(mixed *values)
{
    mixed *result = ({});
    if (!arrayp(values))
        error("Bad argument 1 to clean_array");
    foreach (mixed value in values)
        if (member_array(value, result) == -1)
            result += ({value});
    return result;
}
```

This returns a fresh shallow array, leaving the input and its aliases unchanged
while retaining element identities. Equality follows LPC `==`: equal integers
and floats collapse, nil stays distinct from zero, and destructed objects match
nil. Separate empty arrays remain distinct. The first numeric type wins; the
original adjacent-run deletion retained the last equal value. Reject non-array
inputs explicitly: `foreach` alone would accept nil as empty, while the old
helper accidentally returns a dynamically supplied integer zero unchanged.
Request sorting separately when presentation requires it.
[Native comparisons](lpc/comparisons.md).

Keep the returned array in the [FTP flag parser][lima-ftp]:
`flags = clean_array(flags);`. Its original discarded call cannot mutate the
input. In [`process_mail_list`][lima-mail-daemon], expand groups, lowercase the
names, deduplicate, then filter valid users. Lowercasing after deduplication
can reintroduce duplicates, including mixed-case members introduced by
[group expansion][lima-group-daemon].

Checks exercise the real preposition/message daemons and statistics hooks,
plus extracted follow/wander methods. Caller tests execute the edited FTP and
mail function bodies; mail uses the real group/user daemons with fixture group
and account files. The FTP fixture widens its return type to permit its existing
zero return. Complete FTP and mail daemons still have unrelated compilation
blockers, so these checks do not establish working network services or mail
delivery. The membership loop remains quadratic in the worst case; bounded
correctness tests do not establish large-array performance.

#### Lima doors and locks

Reviewed and exercised **29 September 2026** against the pinned
[door][lima-door], [sibling module][lima-sibling],
[lockable module][lima-lockable], [openable module][lima-openable] and
[hidden][lima-hidden-door]/[secret][lima-secret-door] derivatives. Tests use
lpc-rs with distinct nil, preserved call-out player context, `USE_SKILLS`
enabled and `CLOSED_DOORS_ARE_OBVIOUS_EXITS` undefined. Upstream drivers
were not executed; these adaptations use existing native APIs.

Use nil for absent sibling objects and `!!locked` for the integer hook-state
argument. Match the inherited two-argument `door_check` signature in secret
doors. Register `door_check` and `extra_long_stuff` as bare function callbacks;
`(: name :)` returns the function rather than invoking it.

Preserve `set_locked`'s `key_type = locked` default in the door wrapper.
Forwarding an omitted key type as explicit nil loses it, including after a
successful unlock/relock cycle. Magic unlocking and picking should clear the
locked state while retaining the original key type. Stop picking an already
unlocked object before hooks or skill checks. Guard execution as well as parser
checks: refuse locking an open or already locked object, unlocking an unlocked
one, absent key objects and absent key types. Missing keys should produce a
normal refusal instead of a call on nil.

Resolve movement destinations only after permission succeeds. Pass the captured
room to sibling lookup and movement; a permission check must not invoke the
destination callback again. Door execution accepts a live object or loads a
string path, refusing unavailable rooms before movement. Sibling lookup likewise
resolves once per lookup and accepts object results directly. Lima's pathname
canonicalization strips clone suffixes, so use an object-valued destination to
retain a room clone's identity.

Recheck a cached sibling's room and identifier, skip self and absent identifiers,
and reevaluate callable destinations before reusing the cache. Register the
move hook that copies an existing sibling's state. Keep the container's explicit
`on_clone` synchronization safe after that hook has already run. Refresh hidden
exit metadata on initialization, direct state changes and state copies. Notify
the sibling only when its open/closed state changes; preserve its callback's
view of the old state before copying, and the actor-side callback's view after
mutation. Pure lock changes should not emit open/close callbacks.

Change the [open verb][lima-open-verb]'s absent object arguments to nil.
For [tool-based picking][lima-pick-verb], invoke the pick action on lockable
targets while retaining generic tool-opening behavior for other targets.
The migrated pick action accepts the tool and retains the existing skill policy.

Checks use real doors, keys, chest, container creation and verb methods with
fixture actors that record requested movement. They also exercise the
[stock oak door][lima-oak-door]'s callbacks against fixture room-state storage.
Elevator door timing and stock gate wiring are covered below. Full body/parser
integration and a complete cold boot remain unvalidated. Arbitrary callbacks
retain their existing partial-effect limits.

#### Lima elevator doors and gates

Reviewed and exercised **29 September 2026** against the pinned
[elevator door][lima-elevator-door] and [stock gate][lima-gate], with the same
configuration as the door migration above. Upstream drivers were not executed;
these repairs use existing function callbacks, timer handles and cancellation.

Register the elevator's arrival/departure hooks as bare functions, retaining
their pointer identities so repeated `on_clone` calls replace the same hooks.
Replace the string call-outs with `call_out(call_out_close_door, 15)`, correcting
the departure callback's misspelled name. Keep one owned timer: arrival replaces
the previous timer, departure reuses a pending close, and early closure cancels
it. Clear the handle before notifying the controller. A repeated departure
request must wait while the door is open; a closed door can reply immediately.
Keep both peer states and room flags consistent before a closed notification.
Preserve the automatic transition messages, whose base callbacks inspect the
old state; redundant closes should not emit another message or notification.

The stock gate must use its supplied direction instead of the literal `"dir"`.
Delegate its base transition callbacks and maintain `gate_open` in each gate's
own environment. The original player-room/hard-coded North Tunnel updates
leave North Cave's description stale when operated from the tunnel. Refresh
room flags on initialization and copied state as well as ordinary transitions.

Checks use actual door/gate code with fixture actors and room-state storage,
including both stock cave paths. Timer callbacks are dispatched through the
native queue; these checks do not measure wall-clock accuracy or cancellation
of an already-running callback. Moving-room/controller checks are described
below. The unused legacy internal-door class retains its old room/verb interface.

#### Lima moving rooms and elevators

Reviewed and exercised **29 September 2026** against the pinned
[moving room][lima-moving-room], [elevator controller][lima-elevator],
[panel button][lima-elevator-button], [landing button][lima-elevator-call-button]
and [stock setup][lima-stock-elevator]. These use distinct nil and preserved
call-out players, as above. Upstream drivers were not executed.

Schedule arrival with the bare `arrive` function and register the controller's
closed-door hook as a bare function, retaining its pointer identity across
repeated setup. The original closures return function pointers without calling
them. Bind the progress announcement with `&tell_from_inside(this_object(),
"The elevator continues ...\n")`; its original comma-expression closure only
returns the string. Keep separate arrival and progress timer IDs, cancel and
clear both before notifying a landing, and resolve arrival from `travelling_to`;
`where` is nil while moving. A progress callback may finish the trip or start another one:
rearm only while the same trip remains active, using its arrival ID to distinguish
it from a new trip. Repeated arrival and current-stop requests are no-ops.

Validate destinations and load their rooms before changing trip or queue state.
This prevents a caught bad queue insertion from leaving an invalid head, and
prevents an unavailable landing from silently completing a trip. Revalidate the
pending destination at arrival. Arbitrary callback errors retain the existing
partial-effect limits; this adds no recovery policy for routes changed during
travel. Preserve FIFO queue order, call return codes and the landing-before-door
arrival sequence. Closed notifications received during motion must not start
another trip.

Use nil for absent string arguments and `direct_press_obj(object ob)` for both
button permission methods. Check an unkeyed button's key length when rendering
its label. In stock setup, set the door direction to east and derive its initial
destination from `query_location()`, matching the initial Church stop rather than
the original invalid Attic path.

Checks exercise the real moving-room, controller, buttons, press action and door
callbacks, including a two-stop queue and landing lamp updates. They replace
`INDOOR_ROOM` with fixture room-state and placement storage over the real
container; stock object placements are recorded, not spawned as a world.
The separate room-foundation checks below use the actual room hierarchy.
Full stock-world initialization, body/parser entry and cold boot remain
unvalidated. Timers use the native queue with expedited dispatch, without
claiming wall-clock accuracy or cancellation of an already-running callback.

#### Lima room foundation

Reviewed and exercised **29 September 2026** against the pinned
[base room][lima-base-room], [room descriptions][lima-roomdesc] and
[room state][lima-room-state], with the default positive light level and
`OBVIOUS_EXITS_BOTTOM` enabled. These use distinct nil and preserved call-out
players. Upstream drivers were not executed.

Use the mudlib link's `is_user()` marker when separating player descriptions
from other inventory, and pass nil as the `format_list` separator to retain
its default `and`. Return nil for an unknown state description. Expand only
the matched `$state` segments from Lima's `reg_assoc` simul; iterating
every segment can replace ordinary text between markers with another state's
description. Preserve unknown markers and the existing marker expression.

Save the previous forced-look flag, catch the description and recipient call,
restore the flag, then rethrow an error. Resetting it to zero only after a
successful `tell(who, long())` leaks forced mode when the caller catches an
error, and loses the outer mode during a nested look. Use the same positive-light
threshold for change notices as for looking: zero and negative light are dark.
Give `do_look_at_str` its string argument and return a handled result from both
it and `do_smell`; otherwise their verbs append a second fallback message.

Register arrival and departure as invoking function pointers and keep their
identity across repeated chat setup. Own separate IDs for the chat tick and
deferred listener check. Coalesce checks, cancel both old handles on setup,
and clear a tick's handle before invoking its message callback. Start chat
when a listener arrives, keep one tick while listeners remain, and cancel it
after the last departure. A callback that reconfigures chat or moves a listener
leaves its deferred check in charge of rearming. Callback errors retain the
existing error policy; there is no automatic retry.

Use bare function names with [`call_out`](efun/call_out.md) and inspect the
owned ID with [`query_call_out`](efun/query_call_out.md). Remove the original
unused `chat_msg` argument: the native third argument specifies repetition,
rather than a callback argument. Default an omitted interval to 15 seconds,
and reject nonpositive intervals or messages other than a string, nonempty
string array or function before changing configuration or timers. Preserve
the original chance policy: initially 20, updated only by a positive supplied
chance, and tested with `random(100) <= chance`.

Checks exercise the actual base/indoor room, state, descriptions, movement,
hooks, chat timers and look/listen/smell verbs with fixture actors and bounded
real object spawning. Wizard location formatting goes through the real security
daemon. The base/indoor/outdoor and moving-room/elevator hierarchies compile;
this does not establish outdoor weather, full stock-world initialization,
body/parser entry or cold boot. Shared inventory grouping is covered below;
the [school chat examples][lima-school-room-chat] still need their trailing
`#include` semicolons removed. Timer checks dispatch through the native queue by
ID and do not measure wall-clock accuracy or cancel an already-running callback.

#### Lima object grouping

Reviewed **29 September 2026** against the pinned
[grouping helpers][lima-object-grouping],
[object descriptions][lima-description] and
[room descriptions][lima-roomdesc], using distinct nil and preserved call-out
players. Upstream drivers were not executed.

Keep `ob_state()` as a mixed value: ordinary objects return description strings,
and containers use integer `-1` to mark unique instances. Cache each state once
and test both for the sentinel with `intp(state) && state == -1`; the original
int cast throws for strings on lpc-rs. Retain ordinary equality for states and
attributes, and keep unique objects separate even when compared with themselves.
Placed unique objects keep a `count` result of zero; the renderers treat zero or
one as singular.
Checking both sides also keeps integer `-1` unique against float `-1.0`, despite
ordinary numeric equality; two float states of `-1.0` remain ordinary peers.

Compare `setup_args()` with Lima's existing [recursive `cmp`][lima-cmp], rather
than native array identity. A fresh array with the same arguments should group
with its peers. This explicit mudlib comparison preserves native array and
mapping identity, including empty arrays. `cmp` compares acyclic nested arrays
and mapping values recursively, checks mapping key presence, and retains native
equality for other values. Nil and integer zero remain distinct. Cyclic setup
values are outside this helper's contract.

Scope display grouping to the supplied eligible objects. Optional candidate
arrays on `count` and `duplicatep` preserve ordinary caller/environment defaults;
an explicit empty array must stay empty. Select the first representative in
candidate order, ignoring nil and integer-zero entries without modifying the
caller's array. For `inv_list`, apply visibility, short-description, attachment
and untouched filtering before selecting groups. Attached contents still recurse
at the existing indentation depth. The room's nonliving display groups exclude
the viewer, hidden objects, the `except` object and attached objects, and stay
separate from its living descriptions. Pass those candidates through
`show_in_room` so its plural count matches duplicate selection.

Whole-environment grouping can count excluded objects or let an omitted first
representative suppress an included later one. Separate container relations
already supply separate arrays to `inv_list`; candidate-based grouping keeps
one relation from reporting the other's contents. Remove nil entries as well
as integer zero before calling inventory methods in distinct-nil mode.

These are mudlib changes in the grouping helpers, object descriptions and room
descriptions. Existing native APIs suffice. Runtime checks exercise the actual
room, object, container, movement, master and full simul composition with fixture
items and actors. They cover filtered representatives, relation boundaries,
default and explicit candidates, unique containers, nested setup values and
recursive contents. They do not establish complete vendor trade, player-command
dispatch, full body integration or cold boot.

#### Lima thrown objects

Reviewed **29 September 2026** against the pinned
[throwable module][lima-throwable] and [stock stick][lima-stick], using distinct
nil and preserved call-out players. Upstream drivers were not executed.

Replace the module's string-plus-arguments timer calls with partial applications:
`call_out(&land_throwable(target, 1), random(2) + 1)` for hits and
`call_out(&land_throwable(target, 0), random(3) + 1)` for misses. Preserve the
existing delay ranges and one-shot scheduling. With
`LPC_CALL_OUT_PRESERVE_PLAYER=true`, Lima's `this_body()` resolves the scheduling
user's body when the native callback fires. Moving the thrower to another room
does not replace that context; a missing body remains a callback failure.

Use the bare `prevent_throw` function pointer for both prevention-hook
registrations. In lpc-rs, `(: prevent_throw :)` returns a function value instead
of calling it, so Lima's yes/no/error reducer otherwise allows the hook even
when the response is zero or a refusal string. The direct `do_throw` check
already rejects those responses. Preserve the stored response and reuse the
existing hook when setters change it; no native closure change is needed.

Pass `nil` for the unspecified limb field in `do_thrown_damage`: integer zero
cannot populate the selected event class's nullable string `target_extra`.
The vital-damage helper already passes a selected string limb. Render the debug
target explicitly with `sprintf("Target is: %O", target)`. Pass `this_object()`
after the target in the miss message, whose `$o` otherwise becomes Lima's
generic `"something"` fallback.

The stock stick needs float literals for weight and value (`1.0` and `2.0`).
Its fresh `({colour})` setup arrays then work with the grouping helper's value
comparison: equal-colour sticks group and different colours remain separate.
Native array identity is unchanged. These are two mudlib source changes using
existing APIs.

Runtime checks exercise actual object/container movement, room and message
helpers, typed event construction and native timer dispatch with fixture users,
throwers and targets. They include the stock stick with its inherited setup,
grouping and delayed damage producer. Stock recipe dependencies and the crafting
registry are loaded in separate bounded transactions before those stick checks;
a cold clone exceeded the existing crafting budget. This does not establish
cold startup, full player-body combat, damage delivery or parser command entry.
Existing policy also remains: disappearing targets use the no-detonation branch,
ordinary targetless throws schedule no landing hook, and nonweapons bounce
without damage.

#### Lima throw verb and stock consumers

Reviewed **29 September 2026** against the pinned [throw verb][lima-throw-verb],
[club][lima-club], [rock][lima-rock] and [baseball grenade][lima-grenade], with
distinct nil, preserved call-out players and Lima's `USE_MASS` configuration.
Upstream drivers were not executed.

Keep the private `throw_one` helper void and use `return;` after printing its
refusal text. Both callers discard its result. Add `do_throw_obs(mixed *info)`
with `handle_obs(info, do_throw_obj)`, using the bare callback pointer. This
keeps the advertised plural rule on the same acquisition/throw path as the
single-object rule and prints parser error strings. The inherited generic
handler ignores an item's refusal result and adds its own success message,
which otherwise duplicates ordinary throw output or announces a disabled item
as thrown. Preserve the three registered rules and existing fallback strings.

The club needs `set_weight(1.0)`. The rock needs
`set_mass(to_float(SMALL))`, keeping its integer size constant and existing
configuration guards. Preserve their inherited setup, weapon classes, damage
types and skills. The club's existing weapon price getter truncates its `0.1`
base value to zero and chooses a level-derived price; changing its weight does
not alter that pricing policy.

The grenade needs float weight/value literals (`1.0`, `40.0`) and
`object *targets` for its filter result. Replace its damage callbacks with
`&calc_damage(1)` and `&calc_damage(sizeof(targets))`. Here
`(: calc_damage, 1 :)` returns one, and the analogous room literal returns the
target count, rather than calling the damage method. Explicit partial
applications preserve `10 + random(10) / targets` and its separate evaluation
per recipient. An empty list never evaluates the callback. Hits retain the
impact event followed by force damage to a vital limb; misses select attackable
non-body objects and use nil for the unspecified limb. Both explosion branches
remove the grenade. Queued weapon references become nil after its removal;
complete damage delivery remains unchecked.

Checks exercise real verb registration and handlers, stock items, movement,
messages, typed event production and native timer dispatch with fixture users
and bodies. Real crafting, message and damage dependencies are preloaded in
separate bounded transactions. Cold startup and full player/parser/combat
integration remain unchecked. Ordinary targetless grenade throws still do not
detonate, and room explosions still announce once per selected target.
The food and twinkie adapters are described below.

#### Lima food and decay

Reviewed **29 September 2026** against pinned [food][lima-food],
[edible][lima-edible], [healing][lima-healing], [decay][lima-decay],
[twinkie][lima-twinkie], [mongoose cobbler][lima-cobbler] and
[hamburger][lima-hamburger], with distinct nil, preserved call-out players,
`USE_MASS` and `EVERYTHING_SAVES`. Upstream drivers were not executed.

Give food's object inherit an explicit name, such as `inherit OBJ base_object;`,
and call `base_object::mudlib_setup()`: `object` is a reserved type token.
Use float `1.0` for its healing modifier and
[`to_float(query_num_eats())`](efun/to_float.md) in value arithmetic; a cast
does not perform numeric conversion. Match the inherited zero-argument
`direct_eat_obj()` and omit the unused argument in its inherited call. Preserve
integer pricing, the 35% decay discount and zero value after eating or decay.
The stock mongoose cobbler also needs float weight `1.0`.
The corrected food inherit makes both cobbler and pigeon on a spit compilable;
recheck the crafting registry's cooking recipes when adopting these changes.

Rename edible's action helper, whose original name collides with its global
`eat_action` variable, and give its action parameter type `mixed`. On this
driver the original helper call resolves to that variable: string actions raise
a non-function call error, and function actions run twice without healing.
Choose the regular or last-bite action once, run the renamed helper once, then
decrement the bite count or remove the final piece. The helper delivers the
action and calls healing before removal; the original final-bite branch skips
healing. String actions still use `simple_action`, function actions receive no
arguments, and the last action replaces the regular one only for the last bite.
Prefer bare callback names, for example `set_eat_action(eat_message)`.

Guard healing's absent environment before checking `is_living()`. Its recipient
policy remains the living carrier, or the current body otherwise. Passing an
explicit eater changes the action recipient but does not override that healing
policy. These checks use a fixture health sink; full player-body healing remains
unchecked.

Call decay's inherited `add_save` directly. Calling that protected helper through
`this_object()->` cannot reach it here. Hamburger also needs an explicit
`mudlib_setup()` that invokes both food and decay setup; its inherited decay
setup otherwise skips food's bite-count registration. Together these changes
register `num_eats` and `num_decays`. Existing adjective/original-bite save
omissions and complete item restoration remain separate work.

Rename twinkie's custom `throw` method to `do_throw`, the entry used by the real
verb; `throw` is reserved here. Fall back to inherited `do_throw(target)` for
untargeted throws or a disabled/refused item. Targeted throws retain the creamy
filling action and remove the twinkie. This preserves ordinary untargeted throws
and inherited refusal strings.

Checks exercise real foods, eat/throw handlers, movement, messages, save
registration, the state daemon and native timer dispatch with fixture users and
bodies. Decay keeps its existing stages, minute intervals, return values and
auto-removal policy. Hamburger retains its switch fallthrough and remains after
its final stage because auto-removal is off. Generic food still needs
`set_num_eats` before its extra-long percentage calculation; its default original
bite count is zero. Other food/decay consumers need setup composition review.
Poisonous eating also needs the transient adapter below. Cold startup, full
parser/player entry and complete restoration remain unchecked.

#### Lima poison and transient effects

Reviewed **29 September 2026** against pinned [poison][lima-poison],
[transient][lima-transient], [object creation][lima-object] and
[state scheduling][lima-stateful-module], with distinct nil, preserved call-out players,
`USE_MASS` and `EVERYTHING_SAVES`. Upstream drivers were not executed.

Match transient's `void do_effect(object affected)` signature in poison. Its
original zero-argument integer callback fails the inherited contract. Keep
`mudlib_setup()` zero-argument for inherited state registration and poison's
save fields, and move clone-argument configuration to
`setup(int strength, int duration, string action)`. The object creation path
forwards the clone arguments to `setup`; initialization finishes before the
deferred state-registration callback runs. This retains the existing type/name,
strength, action and integer duration calculation `duration / 2`.

Poisonous bites then create or merge the effect, perform their action and heal
once before decrementing or removing food. Matching effects add strength and
duration, preserve the resident action and remove the incoming clone. The
incoming effect's extendability flag controls merging. Ordinary movement returns
integer success; a merge returns nil. The edible caller's unused chained-move
assignment does not require a repair for these checks.

Keep duration units and expiry order explicit when porting: the state module's
default interval is one minute, despite transient's heartbeat wording. An update
applies the effect before checking expiry and decrementing, so a stored count of
two permits three effects and zero permits one. These existing policies remain.
Poison damage is `1 + random(strength)` to `torso`. Detached/nonliving effects
remove themselves without damage; ghosts receive the action before removal.

Checks cover real food, movement, messages, saves, state scheduling and native
timer dispatch with fixture users, bodies and damage sinks. Full player damage,
effect restoration, spell casting and cold startup remain unchecked.
Speed and spell transients need the separate adapters below.

#### Lima speed and spell transients

Reviewed **29 September 2026** against pinned [speed][lima-transient-speed],
[shield][lima-transient-shield], [spell transient base][lima-transient-spell],
[acid arrow][lima-transient-acid-arrow] and
[true strike][lima-transient-true-strike], with distinct nil, preserved call-out
players, `USE_MASS` and `EVERYTHING_SAVES`. Upstream drivers were not executed.

Match `void do_effect(object affected)` in the concrete effects. Speed and
shield keep inherited registration in zero-argument `mudlib_setup()` and receive
clone configuration in typed `setup` hooks; speed also keeps its action/strength
save registration. The spell transient base uses inherited registration plus a
shared `setup(mixed *args...)` initializer for strength, action, caster and limb.
Remove its recursive call to `this_object()->setup()`.

True strike's caller supplies one duration argument. Its typed setup explicitly
initializes the spell base with zero strength and nil action/limb before setting
that duration. Acid arrow accepts its four clone arguments and initializes the
same base, retaining its own fixed duration of two. Its damage callback should
call the stock spell's three-argument `ongoing_damage(caster, target, limb)`;
that function already forwards the no-dodge flag. These retain the cached casting
body and the existing rule that extension sets the resident caster to the current
body.

Retire detached/nonliving speed and acid effects before dispatching their
setter/damage calls. Shield renewal should message only when an incoming effect
and living environment exist. True strike must require a live caster before
comparing it with the current body: two nil values otherwise qualify and attempt
to message a nil body. Bind its cleanup as
`call_out(&do_effect(environment()), 1)` to match the effect hook and retain the
one-second delay. It doubles one positive damage event or sets nonpositive damage
to one, then marks itself spent.

Retain the existing mudlib policies explicitly. Speed merging adds duration and
keeps resident strength/action; removal resets speed to zero. The selected
[heartbeat combat module][lima-combat-pulse] has an empty `set_attack_speed`, so
adapting the transient does not itself change attack frequency. Shield subtracts
a fixed five from bludgeon damage, clamps at zero and ignores configured strength.
Acid arrows remain independent with `cannot_extend()`. Minute intervals and
apply-before-expiry ordering still give the fixed count of two three updates.

Checks exercise native event classes, direct effect hooks, movement, messages,
state scheduling, timer dispatch, cancellation and rollback with fixture users,
bodies and speed/damage sinks. The original acid checks use an explicit fixture
spell registry. The [casting adapters](#lima-spell-casting) below additionally
exercise stock registry, verb and timer paths; the full combat damage pipeline
remains unchecked. Generic identity/extendability, shield fields and spell
caster/limb fields still need save/restore review. Cold startup and complete
restoration remain unchecked.

#### Lima spell casting

Source-reviewed **29 September 2026** at the pinned Lima revision; the adopted
paths were exercised on lpc-rs on **30 September 2026** with distinct nil,
preserved call-out players, `USE_MASS` and `EVERYTHING_SAVES`. No upstream driver
was executed. Sources: [spell base][lima-spell-base],
[registry][lima-spell-daemon], [skill daemon][lima-skill-daemon],
[cast verb][lima-cast-verb] and [combat spell][lima-combat-spell].

The spell base needs integer failure returns and the separator argument to
`format_list`. Align its component methods with `valid_reagent(object)` and
`check_valid_reagent(object, mixed)`, as used by the registry and stock overrides.
Preserve string refusals from circumstance checks and run those checks for room
spells too. Guard nil before living/item tests. Parenthesize delayed target flags,
reject missing or departed targets, and retain room/flexible casts and explicitly
permitted self casts.

Use native [`regexp`](efun/regexp.md) with an array of names and test the result's
size. The registry's void casting wrapper calls its target without returning a
value, clears nil directory entries and registers through `query_name()`. The
cast verb should pass presence flags consistently, pass nil for absent targets,
and supply the missing `STR at OBJ with OBJ` action handler.

The skill daemon needs nil results in two array-returning methods, a `mixed`
local for its existing class-or-integer skill query and class-member syntax
`skill.skill_points` in its percentage calculation. This preserves the query
shapes; it does not establish every skill metadata or administration method.

Put combat difficulty inside `test_skill`, respect `no_dodge` and retain the
supplied limb, using a random limb only when none is supplied. Convert stock
callbacks to native function values: [acid arrow][lima-acid-spell] uses
`&calculate_damage(success, ob)`, [fire beam][lima-fire-beam] uses
`&calculate_damage(success)` and [fireball][lima-fireball] uses bare
`calculate_damage`. Native anonymous expressions `(: function :)` return a
function value, and `(: function, arguments :)` return the last expression;
these spell sites need a callback that produces numeric damage.

Acid's initial damage retains its original target-based skill lookup. Ongoing
damage should guard missing attacker/target and use the supplied cached caster's
rank, allowing it to work without current player context. Keep its fixed duration,
three updates, independent effects and damage formula. Native `||` already
returns the selected operand, so the existing skill, description and busy-delay
fallbacks need no adaptation. Existing interruption policy also remains:
`stop_busy()` cancels the timer; the interrupt hook leaves it pending.

Checks exercise the real registry, skill daemon, verb handlers, busy helper,
movement, messages, effect constructors, state daemon and native timer dispatch
with fixture users/bodies and combat event queues. They cover shield/unlock
immediate casts, true strike's five-second callback, fire beam's repeated
channeling and acid's ongoing damage. The registry is not replaced by a fixture
daemon; one probe catalog entry checks verb argument contracts through its public
mapping inspector. Complete parser/body entry, player health delivery and cold
startup remain unchecked. The [dancing lights adapters](#lima-dancing-lights)
below additionally repair its spell and item lifecycle. Conjure ale's fluid
lifecycle, registry administration and shared channel ownership need separate
review.

#### Lima dancing lights

Source-reviewed **30 September 2026** at the pinned Lima revision; the adopted
paths were exercised on lpc-rs on **30 September 2026** with distinct nil,
preserved call-out players, `USE_MASS` and `EVERYTHING_SAVES`. No upstream
driver was executed. Sources: [spell][lima-dancing-lights],
[item][lima-dancing-lights-item], [companion module][lima-companion],
[stateful setup][lima-stateful], [state daemon][lima-state-daemon] and
[object hooks][lima-object-hooks]. Existing native APIs suffice.

Call the spell-rank helper with only the casting body. Check live clones for
that owner before creating another swarm: state-daemon ownership only sees
registered objects, leaving a gap before the zero-delay registration callback.
If initial movement is refused, remove the new clone before installing following
hooks or announcing a successful summon.

The companion module needs an owned timer ID, [`query_call_out`](efun/query_call_out.md)
and bound callbacks for following and snacks. Dispatch its host combat calls
through `this_object()`. Install one stored move callback, replace it on repeated
setup and remove it and any pending follow timer through the removal hook.
Ignore missing or replaced masters when the timer fires; only announce arrival
after successful movement. Use bare `something_appeared` for the arrival hook.

Name the item's object inherit so its setup can explicitly compose object,
companion and stateful initialization, and supply its cantrip level of zero for
the follow delay. Nonpositive duration removes the item on its first update
instead of leaving an unscheduled light. Positive duration retains the original
`(rank + 2) * (1 + success)` formula and exact update count. State intervals
remain one minute, with the daemon's existing initial spread when first loaded.
Each swarm contributes five units of light.

Checks exercise the real registry, cast verb, busy callback, movement, hooks,
messages, state daemon and native timers with fixture users/bodies. They cover
duplicate prevention before registration, independent owners, following and
room light transfer, rejected moves, vanished casters, expiry, recasting,
hook/timer cleanup, snacks and transaction rollback. Complete parser/body entry,
save/restore, general pet combat/chipping and cold startup remain unchecked.

#### Lima conjure ale and fluids

Source-reviewed **30 September 2026** at the pinned Lima revision; the adopted
paths were exercised on lpc-rs on **30 September 2026** with distinct nil,
preserved call-out players, `USE_MASS` and `EVERYTHING_SAVES`. No upstream
driver was executed. Sources: [spell][lima-ale-spell],
[ale item][lima-ale-item], [fluid base][lima-fluid] and
[drinkable module][lima-drinkable]. Existing native APIs suffice.

Use an absolute clone path for the ale: the stock relative path resolves beneath
the spell directory on lpc-rs. Cache the casting body and its environment, check
the movement result, remove a refused clone and announce success after placement.
Explicitly start evaporation after placement; the stock spell only moves the
item. Casting remains immediate, with its existing reflex and skill checks.

Fluid levels and capacity arithmetic need float values under `USE_MASS`, with
explicit integer conversion for quantity. Absent fluid lookups return nil.
Make the drinkable count formula use float operands before converting the result
to int; retain its existing minimum-one-unit calculation and two drinks per unit.
Consumption does not gradually reduce mass in this patch.

Use the bare evaporation callback: `(: evaporate_puddle_func :)` returns a
function value on lpc-rs without invoking it. Store the native timer ID and cancel
it through movement/removal hooks; removing ID zero does not cancel the object's
pending timers. Starting evaporation replaces its prior timer. The first delay
remains six seconds, later delays twenty seconds, and each update removes one
mass unit. Function-valued evaporation actions retain synchronous evaluation.

Exclude the pouring object from resident-fluid lookup and reject self-merges.
Move incoming fluid successfully before merging it into the resident puddle,
retiring the incoming clone and keeping one timer on the resident. Only announce
successful spills, return the actual movement result from `move_fluid`, and treat
get/drop error strings as refusal before changing fluid state.

Checks exercise the real registry and verb handler, movement, messages, hooks
and native timer dispatch with fixture users/bodies and a fixture vessel. They
cover baseline failures, pouring/merging, refused placement and retrieval,
evaporation, drinking, vanished casters, cleanup and transaction rollback.
[Stock bottles, fluid sources and kegs](#lima-bottles-and-fluid-sources) use the
separately audited and adopted mudlib patch below. The
[separate drink base](#lima-embedded-drinks) has its own diagnostic audit. Full
parser/body entry, item restoration and cold startup remain unchecked.

#### Lima bottles and fluid sources

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS` and
`EVERYTHING_SAVES`. No upstream driver was executed. Sources:
[bottle][lima-bottle], [keg][lima-keg],
[fluid container][lima-fluid-container], [fluid source][lima-fluid-source],
[drink source][lima-drink-source], [fill verb][lima-fill-verb] and
[drink verb][lima-drink-verb]. The adopted seven-file mudlib patch uses existing
native APIs.

Select fluid through `all_inventory`, return nil for absent objects and use float
mass arithmetic. Call container/source host methods through `this_object()`.
Use aggregate container capacity, including unrelated contents, rather than the
first item's nonexistent `query_size` under `USE_MASS`. Give the full-message
helper a different name from its `full_action` field and guard zero capacity
when describing fullness.

Finite fills preserve the incoming amount through `part_fill_with`; renewable
source fills supply the remaining capacity. Partial transfers remain
all-or-nothing. Check the fluid's fill veto before merging, require successful
movement before reporting success or consuming the incoming object, and restore
a resized source seed on movement refusal. The stock fill verb's finite path
otherwise expands a one-unit puddle into a five-unit bottle.

Delegate bottle drinking to its contained fluid and remove the bottle's separate
drinkable counter. A full five-unit ale bottle supplies ten drinks at the retained
two-drinks-per-unit rate; exhausting the ale leaves the bottle intact. Keep the
keg's intentional direct-drinking prohibition.

Dispose of source ID-discovery and drinking samples, including veto and caught
callback-error paths. After filling, dispose only of unplaced temporary fluid;
preserve successfully placed contents or leaked puddles. Cache the fluid name
before a merge can retire its clone. Start evaporation for successful leaks,
guard absent players, and preserve placed contents when a caller catches a
later announcement error. Uncaught errors still roll back the transaction.

Test membership with `member_array(...) < 0`: index zero is a match, and minus
one means absent. Pass the selected source object into the `STR from OBJ` drink
handlers rather than searching again for a nearby source. Checks exercise stock
bottles/kegs and real fill/drink/pour handlers with fixture users, bodies and
enabled/refusing sources. Full parser/body entry, item restoration, cold startup,
and partly consumed merging remain separate work. Water and the remaining
vessel setup contracts are [audited below](#lima-water-and-remaining-vessels).

#### Lima water and remaining vessels

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS` and
`EVERYTHING_SAVES`. No upstream driver was executed. Sources:
[standard water][lima-water-base], [domain water][lima-water-item],
[flask][lima-flask], [tankard][lima-tankard] and [river][lima-river]. The
adopted five-file mudlib patch uses existing native APIs.

Initialize both water forms to one mass unit under `USE_MASS`. Their stock setup
leaves mass at zero, despite a quantity of one. Keep the domain water's optional
`USE_SIZE` initialization. Flask and tankard mass arguments need float literals;
convert the river's integer `TOO_LARGE` constant with `to_float`. Capacities stay
at one unit for a flask and eight for a tankard; the river remains attached.

In standard water, assign IDs before setting the proper name `"some water"`.
Lima's [name synchronization][lima-names] only initializes the primary ID while
the proper name is absent. The stock order leaves a nil primary ID, causing
drinking and pouring name construction to fail on lpc-rs. Reordering preserves
the display name and supplies the `water` primary ID without changing the shared
names module.

Return `move(target) == MOVE_OK` from `move_water`, and reject nil/self targets.
The stock function reports success even when movement is refused. Keep its
occupied-water guard: it refuses a second water object even when spare capacity
remains. Container filling retains the separately adopted merge/top-up policy.

Checks exercise real river, flask, tankard and water objects through fill/drink/
pour handlers, with fixture users/bodies and refusal hosts. A full flask supplies
two drinks and a full tankard sixteen; exhaustion removes water and leaves the
vessel intact. River setup and drinking dispose of temporary samples. Standard
water retains its non-evaporating policy, while domain water retains evaporation
through owned native timers. Refused transfers, finite-volume preservation,
merges and rollback are covered. Full parser/body entry, restoration, cold startup
and partly consumed merging remain unchecked by this audit. The
[separate drink base](#lima-embedded-drinks) is audited below.

#### Lima embedded drinks

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS` and
`EVERYTHING_SAVES`. No upstream driver was executed. Sources:
[drink base][lima-drink-base], [older drink-container module][lima-drink-container],
[beer][lima-beer], [chemo satchel][lima-chemo], [dirty water][lima-dirty-water],
[blackberry cider][lima-blackberry-cider] and [boiled water][lima-boiled-water].
The adopted seven-file mudlib patch uses existing native APIs.

Alias the object inherit for its setup call, remove the redundant drink validator
whose argument list differs from its inherited definition, and use float mass/
price arguments and explicit numeric conversions. The half-price modifier needs
a float declaration. Chemo's drink override must retain the parent's optional
drinkee argument. The older container module needs an empty-safe inventory helper
and host calls through `this_object()` instead of unresolved prototypes.

Bind the alcohol callback as bare `alchohol_drink_action`. An expression closure
containing only that name returns a function value on lpc-rs without invoking it.
Refresh the remaining-drink count before choosing the ordinary or last-drink
action; otherwise a freshly configured single drink uses the stale initial
counter. Compute fullness as remaining drinks divided by the reconstructed total,
with a defined zero-total result. The stock formula describes a partly consumed
bottle incorrectly and can return nil with a drink remaining or divide by zero
when its configured count is zero.

At drink-container execution, refuse closed containers, guard missing contents
and honor an explicitly selected drink only when it belongs to that container.
Embedded drinks select themselves. The stock handler ignores the selected
object and can consume contents despite its closed-container validator.

Retain the existing healing/alcohol coupling, alcohol clamp, minimum-one-unit
drink count, unchanged mass between sips and removal of the whole embedded item
on exhaustion. The last sip skips taste effects but still heals. Computed
remaining-drink value takes precedence over stored price; alcoholic drinks retain
the half-price modifier. These policies differ from the separate fluid/vessel
lifecycle and need a separate decision before changing them.

Checks cover the base and all seven stock callers, real drink handlers, selection/
refusal, exhaustion, bounded alcohol/abuse calls, poison creation and rollback.
User/body fixtures record effects; complete player health delivery is unchecked.
The real crafting daemon registers both drink recipes and computes their XP after
successful recipes are preloaded in separate four-second transactions, with the
stock inventory intact. Both recipe markers already exist. An earlier draft cold
registry scan exceeded that unchanged limit during compilation; full cold startup remains
unestablished. Full parser/body entry, restoration and custom drink reconfiguration
also remain separate work.

#### Lima lock-pick recipes

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES` and `USE_SKILLS`. Fourteen fresh public source comparisons
match original Lima; no upstream driver was executed. Sources include the
[simple][lima-lockpick-simple], [mediocre][lima-lockpick-mediocre] and
[advanced][lima-lockpick-advanced] recipes, [tool module][lima-lockpick-module],
[lockable module][lima-lockable], [pick verb][lima-pick-verb],
[crafting module][lima-crafting-module] and [registry][lima-crafting-registry].
The adopted four-file mudlib patch uses existing native APIs.

Use float price literals in the three recipes: `50.0`, `200.0` and `500.0`.
Their integer arguments do not satisfy the material value setter's float type.
After compilation is repaired, the adopted crafting station's strict XP guard
still rejects these tools because they have no `crafting_xp` method. Treat that
public hook as optional: start XP at zero and invoke it only when
[`function_exists`](efun/function_exists.md) finds it. Continue rejecting nil,
float, negative, excessive and throwing results from defined hooks, with prepared
item cleanup before spending resources. Existing nonzero XP hooks retain their
behavior. Under the station's existing `xp * 10` formula, an absent hook also
means a zero-difficulty crafting skill check.

The recipes store strength `50`/`200`/`500` and break chance `30`/`20`/`10`, but
stock lock picking reads neither field. The pick verb forwards the tool to
`m_lockable::do_pick`; that method checks the body's skill against the target's
lock defense and ignores the tool. Failed attempts do not consume or break it,
and picking without a tool uses the same skill check. The module's metadata
setters do not clamp their values. Retain these stock behaviors for this port;
activating tool bonuses or breakage requires a separate mudlib gameplay decision.

Checks exercise all three setups, real chest/pick paths, refusal hooks, unused
instrumented getters, non-lockable target forwarding, real crafting confirmation,
resource spending, optional and invalid XP, cleanup and rollback. The real
registry scans the unchanged 21-file stock recipe inventory after preloads in
separate four-second transactions; electronics and cooking entries are checked.
Body fixtures record skills and actions and accept movement. Full player skill/
capacity behavior, parser/body entry and cold startup remain unchecked.

#### Lima body and adversary skills

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES` and `USE_SKILLS`. Sixteen fresh public source comparisons
match original Lima; no upstream driver was executed. Sources include
[body skills][lima-body-skills], [adversary skills][lima-adversary-skills],
[the skill class][lima-skill-class], [monster setup][lima-monster-setup],
[the trainer][lima-trainer] and [the skill daemon][lima-skill-daemon]. The adopted
two-file mudlib patch uses existing native APIs.

Give the body setter an explicit `int training_points = 0` default. Inherited
monster setup and the trainer's `set_trainer_skill` supply two arguments;
learning supplies all three. Omitting training points resets them to zero,
including when updating an existing record. Return nil for an absent class
record, use dot access for class fields, and replace evaluation's callback-form
`implode` with a `foreach` sum followed by the unchanged integer percentage
formula. Native [`implode`](efun/implode.md) joins values and does not fold them.

Default absent numeric mapping entries to zero at the body bonus/rank reads and
adversary aggregation division. Missing bonuses otherwise cause arithmetic
errors, and integer getters can return nil through their mixed mapping reads.
An absent cached rank also makes `rank > nil` false under native
[comparison rules](lpc/comparisons.md), silently suppressing the first rank-up;
the comparison needs the same zero default.

Retain stock skill policies. Body setters are unclamped; `query_skill` returns
an independent bonus-adjusted copy with points clamped to 0–10,000. Body
aggregation and evaluation use stored points without bonuses. Adversary
aggregation repeatedly weights the same leaf by path depth rather than reading
parent records. `zero_skills` clears only the skill mapping, and `clean_skills`
removes invalid names without clearing bonus/rank caches. Under the existing
win formula, zero opposing skill gives zero learning gain. These adaptations do
not change those gameplay or cache policies.

Checks exercise real skill modules, the restored skill daemon, learning and
training propagation, rank notifications, copy isolation, bonuses and rollback.
Composed hosts supply level/base-test methods and capture private messages;
selected failure-learning checks force the base-test outcome. The unchanged
trainer setter is extracted to exercise its dynamic two-argument call, while
the complete trainer remains blocked through actions/conversation. The
[combat formula adaptation](#lima-skill-combat-formula) below advances full body
compilation further. Complete player combat, parser/body entry,
saved player-skill migration and cold startup remain unchecked.

#### Lima skill combat formula

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES`, `USE_SKILLS`, `FORMULA_SKILLS`, `BODY_STATS_LIMA` and
`WIELD_LIMBS`. Fourteen fresh public source comparisons match original Lima;
no upstream driver was executed. Sources include the
[skill formula][lima-skill-formula], [combat main][lima-effect-combat],
[damage source][lima-combat-damage-source], [limb wielding][lima-wield-limbs]
and [body skills][lima-body-skills]. The adopted one-file mudlib patch uses
existing native APIs.

Format debug objects with [`sprintf`](efun/sprintf.md) and `%O`. Wrap the first
hit and defense result assignments in [`to_int`](efun/to_int.md), retaining
their integer locals and later integer arithmetic. Moving the conversion to
the return changes rounding: attack value 3, defense value 1 and dexterity
−85 give defense −1 with the existing intermediate truncation, rather than 0
if the expression stays floating point until the end.

Preserve the formula's gameplay policies. Hit clamps to 0–100; defense, damage
and disarm remain unclamped. Restrictions replace the hit/defense attack skill,
divide attack by ten and multiply defense by ten, but do not reduce damage
despite the stock comment. Stat selection tests the original attack name for
the substring `combat/guns`; a `combat/ranged/pistol` name uses the physical
stat. The patch does not change that naming convention or combat balance.

Stock wielding supplies the body itself as the unarmed weapon. Do not replace
that object with nil or add nil guards as part of this adaptation. Disarm
prevention counts an unarmed target's body bonus twice, adds half its mental
stat, and calls `query_anti_disarm()` without an argument. The stock damage-source
getter ignores its object parameter; the omitted argument becomes nil under
native dynamic calls. The disarm skill test still runs with its learning cap,
although its result is ignored. Those behaviors remain unchanged.

Checks exercise the real formula, damage-source metadata, body learning/caps
and combat-main hit, miss and disarm event production. Hosts supply stats,
weapon and skill-test callbacks, with scripted random draws and captured debug
text; an inherited host also exercises the real debug macro without a connected
recipient. Full body/human compilation advances to karma typing errors in
[simple advancement][lima-advancement-simple]. Full player combat, parser/body
entry, saved player migration and cold startup remain unchecked.
The [advancement adaptation](#lima-simple-advancement) below advances body
compilation further.

#### Lima simple advancement

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES`, `USE_SKILLS`, `FORMULA_SKILLS`, `BODY_STATS_LIMA`,
`WIELD_LIMBS`, `ADVANCEMENT_SIMPLE`, `AUTO_LEVEL`, `CAN_LOSE_LEVELS`,
`USE_KARMA` and `XP_FACTOR=250`. Sixteen fresh public source comparisons match
original Lima; no upstream driver was executed. Sources include
[simple advancement][lima-advancement-simple], [body quests][lima-body-quests],
[party awards][lima-party-advancement], [body skills][lima-body-skills] and
[limb health][lima-health-limbs]. The adopted one-file mudlib patch uses
existing native APIs.

Initialize `karma_mod` with `0.0` and assign
`karma = to_int(karma + karma_mod)`. Convert the complete sum, preserving the
integer destination. With karma 100 and modifier −1 the result is 96;
converting only the increment would produce 97. The setter remains unclamped,
the getter clamps to −1000–1000, and modification clamps its argument to −5–5
before calculating the increment and clamping the result.

Keep stock level, XP and callback policies. Explicit zero XP uses stored XP;
explicit zero level selects the current level minus one. `set_level` refreshes
body style. XP gains/losses issue existing commands on level changes and save
the linked receiver. `set_experience` instead saves the linked ambient body
and requires that context; stock callers were not found. Do not introduce a
nil fallback as part of the karma adaptation.

XP retains integer `XP_FACTOR / 15`, the level-ratio transformation, floating
calculation, floor conversion and minimum of one. A negative monster level
can still produce positive XP: player level 1 and monster level −2 yield 32.
Solo `xp_value` applies the recipient's guild buff; party awards call
`calc_xp_for` directly and apply a level share without that buff. The stored
XP modifier does not participate in this calculation. The unused
`skill_xp_plus` helper accepts NPC integer records but fails with body skill
class records; this audit leaves it unchanged.

Checks exercise included and inherited advancement, real body skills, real
limb health and the real quest module. A fixture quest daemon supplies the
authorized caller. Unchanged party XP/karma methods are extracted into a host
with fixture roster, debug and save methods; the complete party daemon still
has string-return-zero errors. Commands, messages, saves and selected stats
are supplied or captured by hosts. These checks do not establish full player
combat, party persistence or quest-daemon integration.

Keep static host requirements visible. Player bodies supply `save_me` and
`do_game_command`; standalone adversaries and selected monsters still fail
without them. With the karma patch, full body/human compilation
advances to the string-form `setup_monster` [`call_out`](efun/call_out.md) in
[the adversary][lima-adversary]. The
[deferred-setup audit](#lima-deferred-adversary-setup) below examines that timer.
Full parser/body entry, player-save migration and cold startup remain unchecked.

#### Lima deferred adversary setup

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES`, `USE_SKILLS`, `FORMULA_SKILLS`, `BODY_STATS_LIMA`,
`WIELD_LIMBS`, `ADVANCEMENT_SIMPLE`, `AUTO_LEVEL`, `CAN_LOSE_LEVELS`,
`USE_KARMA` and `XP_FACTOR=250`. Twelve fresh public source comparisons match
original Lima; no upstream driver was executed. Sources include
[object creation][lima-object], [adversary setup][lima-adversary],
[monster defaults][lima-monster-setup] and [the simul overrides][lima-setup-overrides].
The local port adopts the one-line mudlib patch using existing native APIs.

The patch replaces `call_out("setup_monster", 0)` with `call_out(setup_monster, 0)`.
Keep the delay and the separate transaction. Clone creation calls
`mudlib_setup`, `internal_setup` and the area's `setup` before the callback
runs. Calling the method directly runs defaults before the area sets its level:
the checked level-15 NPC receives level-1 skill points of 10 instead of 250.

The bare callback reaches a derived override when scheduled by the inherited
method. At firing, the stock method tests `is_body` before default work, and
the monster module reads the current level. Player bodies still skip defaults.
Blueprint initialization does not enter the clone setup path, and each
`mudlib_setup` invocation still schedules one callback; the patch adds no
deduplication policy. Named callbacks follow native
[`call_out`](efun/call_out.md) binding and compatible recompilation rules.

The callback preserves the player captured before area setup under Lima's
configured mode; changing that player later in creation does not replace it.
An absent player stays nil, `this_interactive` stays nil, `previous_object`
is the callback owner and `origin` is `"call_out"`. Context fixtures use Lima's
existing `this_user` wrapper. Its obsolete `this_player` stub and compiler
restrictions on explicit efun bypass remain unchanged.

An uncaught creation failure schedules nothing. A callback failure rolls back
its default changes after creation has committed. Destruction before firing
prevents the work, and the one-shot callback is consumed even when it throws.

Checks extract the real object creation and adversary scheduling/guard methods,
inherit the real monster module and use a fixture skill registry with stock
rank thresholds. Living, configuration, area, stats, weapon and health methods
are instrumented hosts. The native clone and call-out execution paths run;
the complete adversary/player lifecycle is not established. With the adopted
patch, full body/human compilation advances to object-typed zero in
[the follow module][lima-setup-follow]. Standalone adversaries and selected
monsters retain missing save/command host definitions. Full parser/body entry,
player-save migration and cold startup remain unchecked.

#### Lima following

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players and the combat/advancement
options listed in the [setup audit](#lima-deferred-adversary-setup). Thirteen pinned
public source comparisons from the audit match original Lima; no upstream driver
was executed.
Sources include [the follow module][lima-setup-follow],
[the verb][lima-follow-verb], [the following monster][lima-follow-monster],
[the wolf caller][lima-follow-wolf], [object hooks][lima-hooks],
[object movement][lima-container-move] and [smart movement][lima-follow-smartmove].
The local port adopts the one-file, eight-site mudlib patch using existing native
APIs.

Clear the object target with `nil` and use [`evaluate`](efun/evaluate.md)
instead of Lima's string `eval` helper. Declare `query_follow_search` as
`mixed *`: its existing callers consume string and object selectors. Replace
the four `(: method :)` callback literals with bare method names. Under native
[closure semantics](lpc/function_type.md), those literals return the function
value without calling it; otherwise follow/arrival/move hooks and the match
predicate silently do no work.

Flatten selector callback results before deduplication and discard nil/zero
results before [`present`](efun/present.md). The module documents callbacks
returning arrays, but its original expansion nests them and lookup throws.
An absent callback result now produces no candidate. The [real follow
verb][lima-follow-verb] lists mixed selectors and toggles object selection with
its original messages. Selection retains a present target and otherwise chooses
randomly among candidates present in the follower's room.

Keep the existing gameplay policies visible: clearing the search list does not
clear the target; an empty search during a move leaves old-room hooks registered;
and `is_following` reads a flag that this module never sets. The patch does not
redesign those policies. With a search, movement detaches the previous room's
hooks and attaches the new room's hooks without duplicates. The departure hook
submits `go <direction>` with the follower as the current body/user and restores
the saved user before rethrowing a response error.

Checks inherit the real follow and hook modules and call the real verb and simul
helpers. Room events, response/action methods and the user message receiver are
instrumented; the complete [smart movement][lima-follow-smartmove] and
[action queue][lima-follow-actions] are not exercised. Standalone modules still
need visible `respond`, `call_hooks` and `add_hook` implementations. Full
body/human compilation advances to `find_call_out` and string-form `call_out`
in the action module; following monsters retain save/command host requirements.
Connected-client delivery, full parser/body entry, saves/upgrades and cold
startup remain unchecked.

#### Lima action queues

Source-reviewed and exercised on lpc-rs **30 September 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players and the options listed
in the [setup audit](#lima-deferred-adversary-setup). Fifteen pinned public source
comparisons from the audit match original Lima; no upstream driver was executed.
Sources include
[the action module][lima-follow-actions], [object hooks][lima-hooks],
[following][lima-setup-follow] and [the quiet command][lima-actions-quiet].
The local port adopts the two-file mudlib patch using existing native APIs.

Replace name-based `find_call_out`, `call_out` and cancellation with owned handles
and [`query_call_out`](efun/query_call_out.md), [`call_out`](efun/call_out.md) and
[`remove_call_out`](efun/remove_call_out.md). Idle actions remain chained
one-shot timers. Responses retain one timer per queued string, `random(3)` delays
and FIFO delivery, including when their timers fire out of order. Prune expired
response handles and install a cleanup hook that cancels module-owned work and
clears its response queue while preserving unrelated timers.

Use actual function values for named callbacks. Cache arrival, departure and
movement callbacks: evaluating the bare movement method on each start creates
fresh function values that bypass the hook module's identity-based
deduplication. The stable movement callback prevents duplicate registrations.
The departure/arrival callbacks cancel work when the last listener leaves and
schedule it when the first listener arrives. Combat suppresses the idle command
while periodic checks continue.

Preserve existing gameplay policies: `stop_actions` cancels the current timer
but installed hooks can restart it; empty action arrays keep listener checks;
`set_actions` bypasses the minimum enforced by `set_delay_time`. A response
exception rolls back its queue removal but consumes its one-shot timer, leaving
the failed head for a later response. Moving to nil still throws and rolls back.
Failure recovery and permanent action disabling are separate changes.

Change the quiet command's `set_actions(100, 0)` to `set_actions(100, nil)` for
its declared array contract. The original dynamic call accepts and stores
integer zero; this change does not fix a runtime rejection. The extracted
command body retains player refusal and already-quiet messages.

Checks inherit real action/follow/hook modules, fire native timers and exercise
the real `do_game_command` through instrumented command lookup/handling. The
command temporarily sets the NPC as the current body/user and restores the
saved player before rethrowing lookup or command errors. Quiet checks extract
its main method and instrument formatted output. Full command-daemon behavior,
parser fallback, connected-client delivery, complete smart movement,
saves/upgrades and cold startup remain unchecked. Full body/human compilation
advances to zero returned from `query_userid` and unknown `query_idle` in
[the naming module][lima-actions-naming]. Missing save/command hosts, Harry's
inline function syntax and the quiet command parent's `dispatch_main`
requirement remain unchanged.

#### Lima player naming

Source-reviewed **30 September 2026** and exercised on lpc-rs through
**1 October 2026** at the pinned Lima revision, with distinct nil, preserved
call-out players, `USE_TITLES` enabled and `USE_INTRODUCTIONS` disabled. Ten
pinned public source comparisons from the audit match original Lima; FluffOS's
idle reference and implementation remain pinned audit evidence. No upstream
driver was executed. The local port adopts the two-file mudlib patch using
existing native APIs.

In [the naming module][lima-actions-naming], return nil from `query_userid()`
when there is no user link, preserving its declared string contract. A live
link returns the account ID even when disconnected; the character's display
name is a separate value. Destructed links take the absent-link path.

Provide `query_idle(object ob)` as a simul helper over
[`query_connection(ob)["idle"]`](efun/query_connection.md). Reject nil and
objects without connections rather than reporting them as active players.
The reviewed FluffOS helper also errors for non-interactive objects and reads
seconds since the last input command. [Reference][fl-query-idle],
[implementation][fl-query-idle-source]. Native connection metadata already
provides the clock, so this does not require another driver efun. Check
`link && interactive(link)` before querying it: native
[`interactive`](efun/interactive.md) can default a nil argument to the current
object, including a connected body with no user link.

Keep the naming policies: idle text begins at 60 seconds, uses the largest time
unit and can accompany the inactive marker; disconnected descriptions retain
the lifeless prefix. Default titles are cached, and ghost descriptions use the
underlying character name even after visibility changes. The describe setter
accepts callers by base name and still invokes saving after an unauthorized
call without changing the description. The nickname setter instead checks the
full filename, rejecting cloned callers; replacing a nickname changes its IDs
without saving, and initialization restores the nickname ID. These caller and
gameplay policies are unchanged. [Titles][lima-naming-title],
[describe command][lima-naming-describe], [nickname command][lima-naming-nickname].

Checks inherit the real naming/title modules and use native connection attachment,
detachment, caller inspection, formatting and `parse_refresh`. Deterministic
idle-boundary checks substitute the simul helper's clock result; separate checks
read the real connection clock. Identity, ID mutations and saving are
instrumented host methods, and command callers are proxies at the real paths.
Complete commands, connected-client output, saves/upgrades, parser targeting and
cold startup remain unchecked. The configured-out introductions branches are
not exercised. Full body/human compilation advances to `filter_mapping` in
`std/body/guilds.c`; standalone naming/title modules still need visible host
methods, and complete describe/nickname/people commands still lack the command
parent's `dispatch_main`.

#### Lima player guilds

Source-reviewed and exercised on lpc-rs **1 October 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players and `USE_TITLES` enabled.
Thirteen public source comparisons match original Lima. FluffOS's core mapping
filter is source-reviewed at the pinned revision with `F_FILTER` enabled; no
upstream driver was executed. The local port adopts the one-line mudlib patch
using existing native APIs.

In [the player guild module][lima-player-guilds], replace `filter_mapping` with
[`filter`](efun/filter.md) and use `.member_level` on the class cast expression.
Keep `$2`: mapping callbacks receive the key, value, then extra arguments in
both the reviewed FluffOS implementation and native filter. Renaming alone
compiles but fails at runtime when the arrow attempts an object-method call.
See [class member access](lpc/classes.md#construction-and-members).
[Upstream filter entry point][fl-guild-filter-entry],
[mapping callback implementation][fl-guild-filter-mapping].

Preserve the existing guild policies separately from this syntax adaptation.
Only `member_level` controls membership; suspension and bans do not remove it.
Revoke deletes only unflagged records, re-add retains flags, and unsuspend can
leave an all-zero record. Revoke/clear leave shell command paths. Privileged
foreign-body additions still target the current player body's shell, while
`clear_guilds` remains public without the mutators' security guard.
The [guild master's release method][lima-player-guild-master] revokes before
banning in modes 2/3: if revoke deletes the record, the subsequent ban returns
-1 and is lost. This existing gameplay issue is characterized, not repaired.

Sixty-one native test cases cover 30 new guild cases and 31 retained naming
cases, whose assertions and observations are unchanged. Class state, membership,
real security/unguarded calls, shell/message rollback and a fresh native private
class-mapping save round trip are exercised. Registry checks and connected-body
selection use exact daemon methods with controlled registry state; release uses
the exact method with controlled policy and no-op guild-message hosts. Full
callers and foreign save import are not exercised. Standalone guilds and the
composed host compile; full body/human advance to the float argument for
`set_mass(100)` and missing `restore_variable` / `save_variable` in
`init_cmd_hook()`. Complete guild daemon/caller diagnostics remain separate;
command dispatch and cold startup are not established.

#### Lima player body setup

Source-reviewed and exercised on lpc-rs **1 October 2026** at the pinned Lima
revision, with distinct nil, preserved call-out players, `USE_MASS`,
`EVERYTHING_SAVES` and `WIELD_STYLE=WIELD_LIMBS`; `USE_SIZE` and introductions
are disabled. Thirteen public Lima comparisons match original source. FluffOS's
core `F_SAVE_VARIABLE` / `F_RESTORE_VARIABLE` entry points and declarations are
source-reviewed at the pinned revision; no upstream driver was executed.
The local port adopts the three-line mudlib patch using existing native APIs.

In [`std/body.c`'s `init_cmd_hook()`][lima-body], pass `100.0` to `set_mass`:
[the mass setter][lima-body-mass] takes a float. Replace `restore_variable` and
`save_variable` with existing native [`restore_value`](efun/restore_value.md)
and [`save_value`](efun/save_value.md). FluffOS's originals serialize one value
using its own save format. This adaptation uses the native
[save format](save-format.md); renaming calls does not convert historical
serialized `saved_items` embedded inside an outer save file.
[Upstream declarations][fl-body-save-spec], [entry points][fl-body-save-source].

Keep the existing restore policy: a matching-base failure is rethrown; a
foreign-base mapping invokes the conversion hook, rewrites the base name and
retries. The stock hook is empty. The adopted `M_SAVE` adapter also accepts
positional saves, but this race-change fallback still requires a mapping.
A matching positional root retains its legacy move to the previous object;
a mapping root does not. Successful restoration clears `saved_items` to numeric
zero. Uncaught failures roll back setup and restored inventory transactionally.
See [the M_SAVE adaptation](#lima-shell-persistence).

Equipment restoration has an existing selector issue: `#ifdef WIELD_SINGLE`
selects the single-style branch because [the combat header][lima-body-combat]
defines that selector even when `WIELD_STYLE` selects limbs. The patch preserves
this branch; restoring nonempty stock equipment needs separate review.

The adoption checks 637 native cases: 609 retained cases and all 28 body setup
audit cases, with unchanged assertions. Body setup and guild observations match
the frozen evidence exactly; other retained observations remain within their
existing checked timing, identity and random bounds. Five exact body methods
exercise inventory restoration and entry ordering in controlled hosts; real
`M_SAVE`, mass, cloning, serialization and movement run, with injected later
failures verifying rollback. Complete body/human objects compile. Fresh human
construction, `su_enter_game()` into a real indoor room and `enter_game(0)`
through the stock Void fallback succeed with a proxy user and controlled channel
daemon. [User callers][lima-body-switching], [start selection][lima-body-start].
That adoption did not exercise full incarnation or shell execution; the
[user-entry audit below](#lima-user-incarnation-and-body-switching) covers those
compositions with channel delivery isolated. Registered look targeting, historical
save import and cold startup remain unchecked.

#### Lima user incarnation and body switching

Source-reviewed and exercised **1 October 2026** at the pinned Lima revision,
with distinct nil, preserved call-out players, `USE_MASS`, `EVERYTHING_SAVES`,
`BODY_STATS_LIMA` and limb wielding. Eighteen public Lima byte comparisons and
six pinned FluffOS reads are recorded. No upstream driver was executed.
The local port adopts the following seven-line mudlib patch using existing
native APIs.

The composed [user][lima-entry-user] can save its account and start the real
player shell. Its original [body persistence][lima-body] uses upstream
bound-pointer syntax such as `(: save_object, USER_PATH(bodyid) :)`. Here that
closure returns the path without saving. Likewise, its restore callback returns
the trailing integer without restoring. Use
`(: save_object(USER_PATH(bodyid)) :)` and
`(: restore_object(USER_PATH(userid)) :)` inside `unguarded`. The native restore
call takes one argument and leaves nosave variables unchanged. These closures
perform the calls locally under the existing security wrapper. Upstream treats
the comma form as a bound function pointer. [Function reference][fl-functions],
[named-pointer grammar][fl-entry-function-grammar],
[native function values](lpc/function_type.md).

In the [mailbox][lima-entry-mailbox], make the same explicit-call changes for
restore, save and removal. Return `nil` for a missing `class mail_msg`, and
replace the unread-count `implode(values(mailbox), callback)` with
[`reduce(values(mailbox), callback, 0)`](efun/reduce.md). Upstream's function
form of `implode` is a left fold, including numeric zero for an empty array.
The mailbox stores zero/one flags, so the explicit zero initializer preserves
its sum. [Fold reference][fl-entry-implode], [implementation][fl-entry-fold].

The adoption checks 658 native cases: all 637 previously selected cases and
the audit's 21 user-entry cases, including three controls. Assertions are
unchanged; user-entry, body setup and guild observations match frozen evidence
exactly. Other observations remain within their existing checked timing,
identity and random bounds. The complete user is a real transactionally created
clone with an attached connection. Actual private
incarnation and input methods are reached through appended test wrappers.
The real body, player shell, security master, mailbox daemon and mailbox run.
Account/body saves, stat restoration, nonempty fixture inventory, blank modal
input, shell save/restart, unread selection and mailbox save/restore succeed.
Injected failures after entry movement verify rollback of body/shell state,
inventory, output and checked save files.

Channel delivery is isolated: the full [channel command module][lima-entry-channel]
rejects `adminp(user) || user = ci.moderator` at compilation. A baseline control
retains this failure and checks rollback. The body starts in the real stock
Void room from a freshly generated native save; no cold-start or password/login
menu flow is established. Inventory uses a small object fixture with real
movement and persistence, not stock wielded/worn equipment or foreign saves.

Keep three further issues visible. The [permanent switch][lima-body-switching]
updates only a local filename and saves the unchanged account body record;
`query_fname` and the saved registration still name the human body. It also
creates/restores the new body before cleaning up the old one, so unsaved live
inventory is not transferred. Finally, it catches old-body cleanup errors:
ordinary-user destruction is denied by the adopted master policy, and a
privileged cleanup reaches [SNOOP_D's][lima-entry-snoop] integer-zero return
from a mapping method. Both leave the old body alive in these checks.
The adopted patch preserves these policies and blockers. Complete mailbox
compilation succeeds; the expanded compile matrix retains all preceding successes
and its other diagnostics match the audit. The
[channel migration below](#lima-channel-daemon-composition) adopts the reviewed
daemon repair; repair body-switch persistence/cleanup separately before claiming
complete player login support.

#### Lima channel daemon composition

Source-reviewed and exercised **1 October 2026** at the pinned Lima revision,
with distinct nil, preserved call-out players, `USE_MASS`, `EVERYTHING_SAVES`,
`BODY_STATS_LIMA` and limb wielding. Ten public Lima byte comparisons and one
pinned FluffOS grammar read are recorded; no upstream driver was executed.
The adopted 22-line patch changes the daemon and its command/moderation mixins;
no native API change is required.

In [the channel command][lima-entry-channel], replace the assignment in `/clear`
with `adminp(user) || (ci.moderator && this_body() == ci.moderator)`.
[Moderation][lima-channel-moderation] stores the current **body**, while
`this_user()` is the account object. Equality with the account denies a legitimate
moderator. The non-nil guard also prevents an account without a body from matching
an unmoderated channel. Format the denial with `user->query_userid()` so it does
not dereference a missing body.

Use `nil` when clearing moderator/speaker/requestor fields, clearing saved
listener/hook arrays, passing an absent sender, returning missing listener/name
results and pruning null listeners. Flags and counts remain numeric zero.
Access [class fields](lpc/classes.md) with `.` before broadcasts and object calls:
`ci.listeners->channel_rcv_string(...)`, `ci.moderator->query_name()` and
`((class channel_info)info[name]).name`. Native `receiver->name` without an
argument list creates a [function value](lpc/function_type.md), and these legacy
field forms fail during compilation or receiver resolution. The reviewed
FluffOS grammar has both dot and arrow member productions. [Grammar][fl-channel-members].

The migration passes 681 native cases: 23 new channel cases, including five
controls, and all 658 previously selected cases with unchanged assertions.
The 705 observations match frozen evidence within existing checked bounds;
channel and user-entry observations match exactly.
The complete cloned/attached user enters through the real
[daemon][lima-channel-daemon] and [account channel module][lima-channel-user].
Registration deduplicates, enforces wizard/admin flags and purges empty transient
channels. Actual output includes tells, emotes, notices and fixture soul data;
history retains twenty records. Errors roll back history and outgoing messages.
Native daemon saves restore permanent flags and live blueprint listener records.
Security-role mappings are seeded through test helpers; production permission
queries execute. The retained user-entry cases keep their original channel fixture.

Moderation queues body requestors, calls a speaker and clears typed fields on
signoff. In that channel migration, raise/lower notices still targeted the
body's `channel_rcv_string`, while the receiver is defined on the account object.
The baseline raise check preserves the missing-notice assertion.
The [notification migration below](#lima-channel-moderation-notifications)
adopts the reviewed destination repair.

The compile matrix passes 409/620, preserving all 406 preceding successes and
matching all eighteen audited targets. Complete daemon compilation advances
from failure to success. Standalone mixins still require host methods;
player channel commands retain their command-parent
blocker. IMUD compilation and SNOOP_D cleanup failures remained in that migration.
Real IMUD transport, SOUL_D parsing, pager/full-shell command dispatch,
password/menu/network login, foreign saves, stock equipment and cold startup remain unchecked.
The [body-switch migration below](#lima-permanent-body-filenames-and-cleanup)
adopts the reviewed repair; complete player login remains unchecked.

#### Lima permanent body filenames and cleanup

Source-reviewed and exercised **1 October 2026** at the pinned Lima revision,
with distinct nil and preserved call-out players. The four-file patch is adopted
over the preceding channel overlay in the local Lima migration.
Nine public Lima byte comparisons are recorded; no upstream driver was executed.
It needs no native API change; the adopted library matches the audited copy.

For permanent [switches][lima-body-switching], call the existing protected
`set_body_fname(query_selected_body(), body_fname)` rather than only saving
after changing a local variable. The [setter][lima-user-bodies] updates the
account body record and persists it. Runtime query_fname, the actual account
save and restore retain the changed filename. Temporary and omitted-name
switches preserve registration.

The adapted [master][lima-master] narrowly permits a requester whose base name
is USER_OB to remove a body whose `query_link()` identifies that requester.
Keep critical master/simul/security checks first and preserve existing privilege
checks for other objects. Keep the simul's master check before cleanup and native
[destruction](efun/destruct.md) authorization on its explicit efun call.
An unrelated account and a nonaccount helper using the owner's player context
remain denied in these checks.
This mudlib ownership rule uses the requester's identity.

In [SNOOP_D][lima-entry-snoop], use nil for a denied mapping result and pruning
absent object references, and object * for filter's selected elements. Allow
local unsnoop_all calls as unsnoop already does, retaining external command
checks. In the [dummy][lima-entry-snoop-dummy], guard clone-only create work with
`if (!clonep()) return;`: initializing the blueprint without a name otherwise
fails before the clone can be created.

The migration verifies 699 native cases: eighteen body-switch cases, including five
controls, and all 681 previously selected cases with unchanged assertions.
The 723 observations match frozen evidence within existing checked bounds;
body-switch and retained channel/account observations match exactly.
Historical account-entry failure controls retain the preceding frozen overlay.
Scoped retries resolve compilation-budget failures without changing the
4,000 ms limit.
The complete real account/body/shell switches and removes its old body.
An injected later entry failure rolls back filename registration, body cleanup,
inventory, account/body save files and outgoing messages. The stock cleanup
catch remains: a cleanup error can still allow switching while retaining the
old body.

The new body restores its disk snapshot before old-body cleanup saves live
inventory. Saved items appear in the new body; unsaved items do not transfer,
although cleanup writes them to the body save. Retained old-item references are
live and uncontained in these checks; subsequent autosaves and garbage collection
are unchecked. Keep this switching policy explicit, and review it separately
before introducing live inventory transfer or changing the save/clone order.

Appended helpers seed a snoop relation between actual attached accounts/bodies.
Production dummy construction and local cleanup run, and the native connection
queue receives start/stop controls on the same connection. A dead dummy
mapping value remains after removal. Public snoop command authorization, cycle
handling, subscriber delivery and TCP processing are not established by this
fixture. The compile matrix passes 411/621, preserving all 409 preceding
successes and matching all sixteen audited targets. SNOOP_D advances to success
and the added dummy target compiles; standalone switch/account mixins and
non_object retain host dependencies.
Cold startup, complete login/takeover, stock equipment and foreign saves remain
unchecked. The [notification migration below](#lima-channel-moderation-notifications)
keeps destination repair separate.

#### Lima channel moderation notifications

Source-reviewed and exercised **1 October 2026** at the pinned Lima revision,
with distinct nil and preserved call-out players. The one-file patch is adopted
over the preceding body-switch overlay in the local Lima migration and matches
the audited library byte for byte. Eight public Lima byte comparisons are
recorded; no upstream driver was executed. No native API change is needed.

Keep [moderation][lima-channel-moderation] roles and requestors as bodies. For
raise/lower notices, resolve the moderator body's `query_link()` and invoke the
[account's `channel_rcv_string`][lima-channel-user] when a link exists. The
original body receiver is absent, so the notice is silently lost. A nil receiver
raises an error natively; guard that link so its absence does not undo the queue
change or requester feedback. Preserve real receiver errors rather than catching
them. Use the existing account message pipeline without broadcasting or adding
channel history.

The migration verifies 717 native cases: eighteen notification cases, including
four controls, and all 699 previously selected cases with unchanged assertions.
The 741 observations match frozen evidence within existing checked bounds;
notification and retained channel/account/body-switch observations match exactly.
The historical missing-notice assertion retains the frozen body-switch library
as a deliberate baseline control. Two copied body-switch control fixtures
prepare human/Void blueprints after repeated setup compilation timeouts; their
actions retain the 4,000 ms limit and every assertion remains unchanged.

Actual attached accounts verify private raise/lower delivery, duplicate/no-op
paths, body queue ordering, absent links and rollback of both output destinations.
A third listener remains silent. Tests seed roles and missing links through
helpers and use a throwing actual account receiver; TCP and public role
administration are not established. Human and Void blueprints are warmed in the
new fixture; cold startup and full login remain unchecked.

Compilation remains 411/621, preserving every preceding outcome. All twelve
audited targets match exactly; only three standalone moderation error locations
move by four lines. Full CHANNEL_D and secure/user compile; standalone host
dependencies remain. Network/shell dispatch and moderator role behavior across
body switching remain unchecked. Source review finds account/body identity
comparisons in the [command's speaker gates][lima-entry-channel]; the
[permission audit below](#lima-moderated-speaker-permissions) reviews them separately.

#### Lima moderated speaker permissions

Source-reviewed and exercised **1 October 2026** at the pinned Lima revision,
with distinct nil, preserved call-out players and the notification overlay.
At the audit, the two-line diagnostic patch was not adopted and needed no native
API change; the [migration below](#lima-moderated-speaker-migration) adopts it.
Ten public Lima byte comparisons are recorded: eight frozen comparisons and
two fresh ones. No upstream driver was executed.

The [command's two speech gates][lima-entry-channel] compare the account from
`this_user()` with body-valued moderator/speaker fields. This rejects both
assigned roles once both fields are populated. Compare `this_body()` with
`ci.moderator` and `ci.speaker` instead; the [body helper][lima-speaker-context]
returns the account's `query_body()`. Keep account identity for listener lists,
output and admin checks, and keep body identity for moderation roles/requestors.
The same correction applies to plain speech and semicolon/colon emotes.

Preserve the original condition requiring both moderator and speaker.
Unmoderated or speaker-less channels retain the open floor; admins without a
body role get no speech bypass. Queued bodies must be called before speaking.
Actual `/call` transfers permission, speaker signoff reopens the floor, and the
account must still be listening before either gate.

The audit verifies 85 native cases: 26 new cases, including five controls, and
all 59 retained channel/body-switch/notification cases with unchanged sources,
assertions and exact raw observations. An initial Rust fixture type ambiguity
was corrected before execution. Twelve new history expectations confused raw
color-marked history with rendered account output; the final checks assert both
exact forms separately. All 26 new cases pass focused revalidation; retained
assertions and the 4,000 ms budget remain unchanged. Tests verify recipient
broadcast, role/queue transitions, denial before soul lookup and rollback of
history and all three recipient queues.

Literal emotes use an explicit unknown-emote lookup fixture. A separate
stock-[SOUL_D][lima-speaker-soul] control reaches its existing compile failure
after passing the repaired gate, preserving history/output rollback. This does
not establish recognized soul parsing or full emote dispatch. The stock compile
survey passes 9/14 in both libraries, with identical outcomes, errors and source
locations. Full CHANNEL_D and account composition compile; standalone host
dependencies and SOUL_D blockers remain. Cold startup, public moderator-group
administration, TCP, shell/network dispatch, IMUD and role behavior across body
switching remain unchecked. The [migration below](#lima-moderated-speaker-migration)
adopts the two-line permission repair; SOUL_D remains a separate audit.

#### Lima moderated speaker migration

Adopted **1 October 2026** at the pinned Lima revision, with distinct nil,
preserved call-out players and the exact two-line patch over the notification
overlay. Both speech gates compare bodies with moderator/speaker fields;
listener lists, output and admin checks retain account identity. The audited
open-floor, queued-body, role-transfer, signoff and listener policies remain.
No native API change is needed.

The migration verifies 743 selected cases and 767 observations: all 717 preceding
cases plus 26 permission cases, including five controls, with unchanged assertions
and action budgets. Original permission rejection controls keep the frozen
notification library; stock SOUL_D retains its actual blocker. Permission and
retained account/body/channel/notification observations match exactly; 18 raw
differences elsewhere retain existing checked bounds.
Nine cases initially timed out compiling the full human body at 4,000 ms.
All nine pass an unchanged focused retry; the failed run and retry are archived.
No fixture setup, assertion or action budget changed.

Compilation remains 411/621, with identical outcomes, errors and source locations;
all fourteen audited targets match. Full CHANNEL_D and account composition
compile, while standalone host dependencies and stock SOUL_D blockers remain.
Literal emotes use an explicit unknown-emote lookup fixture; a separate stock-SOUL
control retains its real compile failure and rollback. Recognized souls, IMUD,
TCP, full login/cold startup, public role administration and role behavior across
body switching remain unchecked. Audit stock SOUL_D separately.

#### Lima soul daemon

Source review at Lima `eefaff66adcae9b01e16059574c925b0d63e70a1` on
1 October 2026 identifies two independent adaptations in
[SOUL_D][lima-speaker-soul]. Use typed `nil` for its missing array/string results,
the observer `compose_message` object argument, and the bare array return in
`parse_imud_soul`. Keep integer predicates numeric. This resolves twelve compile
diagnostics without relaxing native types.

Make the restore callback and six save callbacks explicit calls, for example
`unguarded(1, (: save_object(SAVE_FILE) :))`. Their original comma-style bodies
return their last expression (`1` or the save path) without calling the efun.
The current [access wrapper][lima-access] evaluates those closures but performs
no restore or save. The typed-only control compiles but restores no emotes,
adverbs or rules and leaves the save file unchanged on mutation. Use `restore_object(SAVE_FILE)` without the legacy preserve flag;
native restoration already preserves fields absent from the file.
See [restore_object](efun/restore_object.md) and
[save_object](efun/save_object.md). No native API change is needed.

The diagnostic one-file patch restores 2,645 shipped emotes, 397 adverbs and
3,862 registered rules. Actual parser selection and [message composition][lima-messages]
cover empty, living-target, text and adverb rules, aliases and multipart messages;
the shipped `wave` works without fixture seeding. Production local execution
informs attached actor/target bodies. Save/reload, malformed-restore retry,
missing-parent errors and later-error rollback are checked. Commit-time save I/O
failure retains committed memory and logs the failure, as documented by the
native efun; it cannot be reported back through this callback.

Recognized channel emotes expose a separate receiver problem:
[channel_rcv_soul][lima-channel-user] compares account identity with body-valued
soul recipients. Actor and target both receive observer wording. The diagnostic
SOUL patch leaves this as a control; repair the receiver separately. Local
IMUD-format generation with real temporary dummies works, but remote-target
lookup retains the `imud/auth.c:91` function-token compile blocker. Four selected
public emote commands retain their CMD `dispatch_main` host dependency.
Wizard add/remove/move command entry, remote transport, the complete shipped
vocabulary, TCP, full login and cold startup remain unestablished.

The audit verifies 50 cases/observations: 24 SOUL cases, including four controls,
and 26 frozen permission cases with identical assertions and observations.
Selected compilation is 8/14 → 9/14; all other diagnostics match across baseline,
typed-only and diagnostic libraries. Six quality gates, exact patch reproduction
and 152 predecessor sets are verified. Nine pinned public byte comparisons
support source review; no upstream driver was executed. Historical matrices,
native suites and preload surveys were not rerun. At the audit, the active
overlay remained `speaker-permission/adapted-lib` and the patch was unadopted.

The 1 October 2026 migration at `596dca3e` adopts that exact nineteen-line patch
over `speaker-permission/adapted-lib`. The active overlay is now
`soul-daemon/adapted-lib`, matching the audited diagnostic library byte for byte.
Native/original sources, integer predicates, access checks and parser/message
policies remain unchanged.

All 50 audit cases and observations pass unchanged, including the 24 SOUL cases
and 26 permission cases. Original compile/persistence and channel/IMUD controls
retain their frozen sources and expectations. Selected compilation remains 9/14,
with identical outcomes, errors and locations. Six gates, exact patch reproduction,
45 copied records, seven append records and 153 predecessor sets are verified.
Nine public byte comparisons remain frozen audit evidence; no new fetch, upstream
driver execution, native suite, historical matrix or preload survey was needed.
Channel recipient identity, remote IMUD, public wizard command entry, complete
vocabulary, TCP, full login and cold startup remain unestablished. Audit the
channel receiver next with the adopted SOUL daemon.

#### Lima channel soul recipients

Source review at Lima `eefaff66adcae9b01e16059574c925b0d63e70a1` on
1 October 2026 identifies two problems in
[channel_rcv_soul][lima-channel-user]. It compares receiving accounts with the
body-valued participant list produced by [SOUL_D][lima-speaker-soul], and its
fixed branches only handle an actor and one target. A second target and ordinary
observer therefore select the first target's slot even after correcting identity.

Select `member_array(query_body(), data[0])` for the receiving account, use that
message slot when found, and otherwise use `data[1][<1]`, the final observer
message. Keep first-match actor priority when actor and target are the same body.
Use the receiver's own getter: [this_body][lima-speaker-context] follows
`this_user()` and still returns the sender's body during broadcast. An explicit
control shows that using it gives every listener the actor's text. This one-file
Lima adaptation uses existing native operations and requires no driver change.

The diagnostic verifies personalized actor/target/observer output for ordinary,
colon, multipart and two-target souls, raw delivery, reversed sender/target roles,
self-targets and bodyless listeners. Unregistered listeners receive nothing.
Actual moderator and called-speaker commands retain their body gates; a nonrole
body gets only denial feedback. History retains observer text, unknown emotes
retain literal fallback, and later errors roll back all four recipient queues
and history. The tests use the actual adopted SOUL parser and message composer;
audit verbs and templates are seeded through production add-emote calls.

Twenty cases/observations are verified: eighteen new cases, including three
controls, and two frozen channel cases with identical assertions and observations.
One new denial expectation initially omitted the account renderer's second LF;
the corrected exact assertion passes focused revalidation. Selected compilation
is 9/15 in both libraries across thirty checks. Full secure/user composition
compiles; its standalone channel mixin adds `query_body` to existing host
requirements. The other fourteen compile results match exactly. Six gates,
exact patch reproduction and 154 predecessor sets pass. Public source evidence
is reused from the pinned SOUL audit; no new fetch or upstream driver execution
was needed. Historical matrices, native suites and preload surveys were not rerun.
The audit left the patch unadopted on `soul-daemon/adapted-lib` and recommended
receiver migration before auditing CMD `dispatch_main` composition.

#### Lima channel soul recipient migration

The recipient migration adopts the exact audited one-file patch over
`soul-daemon/adapted-lib`; active `channel-recipient/adapted-lib` matches the
frozen diagnostic byte for byte. Only `secure/user/channel.c` changes, removing/
replacing five lines and adding/replacing four. Native driver and original Lima
remain unchanged.

All twenty cases/observations pass in one run with identical assertions, output,
controls and action budgets. Original and deliberately faulty receivers retain
their frozen control libraries. Selected compilation is 9/15, matching every
audited diagnostic outcome, error and location; full secure/user compiles.
Six gates, exact patch reproduction, twenty-five copied source records, eight
appends and 155 predecessor sets pass. Public evidence remains frozen; no new
fetch, native suite, historical matrix or preload survey was needed.

Audit CMD `dispatch_main` host composition next for public emote command entry.
Remote IMUD, complete vocabulary, public wizard input, body switching during
delivery, network, full login and cold startup remain unestablished.

#### Lima emote command dispatch

Source review at Lima `eefaff66adcae9b01e16059574c925b0d63e70a1` on
1 October 2026 confirms that the four carried-forward command blockers need
the existing CMD adapter: [feelings][lima-emote-feelings],
[rmemote][lima-emote-remove], [mvemote][lima-emote-move] and
[addemote][lima-emote-add]. A protected varargs four-slot mixed `dispatch_main`
calls each unchanged private `main(arg)`. Native visibility, composition and
argument checking remain sufficient; no prototype or driver change is needed.
Addemote additionally needs `nil` for its string rule initialization and explicit
calls at three modal callbacks. Typed-only controls compile but never run the
rule/message handlers because the old expressions return partial pointers.

Sixteen new command cases and twenty unchanged recipient cases verify thirty-six
observations. Actual player-shell discovery runs feelings; other commands use an
appended helper on the actual shell and production CMD's caller guard. Visibility,
permission denials, seeded wizard/admin successes, buffered output, prompted and
inline multipart addition, aborts, save/reload and transactional rollback pass.
Wizard role/directory are setup; WISH discovery and TCP input remain untested.
Compilation improves 12/19 to 16/19 over forty checks including two controls;
the four commands compile, and the fifteen other outcomes remain unchanged.

Six gates, exact patch reproduction, twenty-four copied source records, eleven
appends and 156 predecessor sets pass. Six fresh public files match original
Lima bytes at the pinned revision; nine SOUL comparisons remain frozen evidence.
No upstream driver, native suite, historical matrix or preload survey was rerun.
The audit kept its four-file patch diagnostic on `channel-recipient/adapted-lib`
and recommended command migration before auditing [LOG_D][lima-emote-log].
Successful addemote saves SOUL but leaves log bytes unchanged; source review
finds a partial-pointer unguarded write callback. The unchanged logger compiles
in two additional checks.

#### Lima emote command migration

The migration adopts the exact four-file command patch over
`channel-recipient/adapted-lib`; active `emote-command/adapted-lib` matches the
frozen diagnostic byte for byte. Four dispatch adapters, one typed-nil
initialization and three explicit modal callbacks preserve private typed handlers.
Native driver and original Lima remain unchanged.

All thirty-six cases/observations pass in one run with identical assertions,
output, controls and action budgets. Original and partial adaptations retain
their frozen control libraries. Selected compilation is 17/20, matching every
audited diagnostic outcome, error and location across nineteen command-context
targets and LOG_D; the four selected commands compile. Six gates, exact patch
reproduction, twenty-nine copied source records, eleven appends and 157 predecessor
sets pass. Six reviewed public files and nine older SOUL comparisons remain frozen
evidence; no new fetch, native suite, historical matrix or preload survey was needed.

Audit LOG_D addemote logging next through the adopted command. The log remains
unchanged after successful addition. Other SOUL consumers, public wizard discovery,
remote IMUD, complete vocabulary, network, login and cold startup remain unestablished.

#### Lima logger callbacks

Source review at Lima `eefaff66adcae9b01e16059574c925b0d63e70a1` on
2 October 2026 finds that [LOG_D][lima-emote-log] constructs a bound `write_file`
pointer inside `unguarded` without invoking it. Use an explicit closure call:
`unguarded(1, (: write_file(legal_logs[$(which)], $(what)) :));`.
The current access wrapper does not adapt ordinary native efun pointers.
This one-line library change preserves the allowlist, privilege, paths and void
API; no native API or broader security-wrapper change is needed.

Fourteen new logger cases and two unchanged command controls verify sixteen
observations. All 24 legal symbols pass: 21 append exact payload bytes and three
add one timestamp prefix. Login/quit share a file; snoop uses the secure directory.
Prompted and inline multipart addemote save and log once, while aborts do not log.
SOUL save, log append, modal-stack consumption and queued output roll back together
on a later uncaught error. Alias order, same-task reads, account permissions,
daemon reload, rejected names, empty/NUL payloads and write errors pass.
Missing parents and directory targets now expose native write errors that the
old no-op concealed. Retained controls use the unchanged adopted command overlay.

Compilation remains 12/12 in baseline and diagnostic libraries over 24 checks.
Six gates, exact patch reproduction, 24 copied fixture files, two unchanged test
fragments, thirteen appends and 158 predecessor sets pass. Seven reviewed originals
match their pinned Git blobs; six public comparisons and nine older SOUL comparisons
remain frozen evidence. No upstream driver, native suite, historical matrix or
preload survey was rerun. Tests use distinct nil, preserved players and four-second
actions. The audit kept the logger patch diagnostic on `emote-command/adapted-lib`.

The [log header][lima-log-header] declares clan, clan_error, feedback and question,
but LOG_D does not allow them. Stock clan/reporter callers need a separate audit;
this patch keeps their rejection. Adopt the explicit write callback next, then
audit remaining SOUL commands. Full logger-caller integration, public wizard
discovery, remote IMUD, vocabulary, network, login and cold startup remain
unestablished.

#### Lima logger migration

The migration adopts the exact one-line logger callback over
`emote-command/adapted-lib`; active `logger/adapted-lib` matches the frozen
diagnostic byte for byte. Explicit invocation preserves the allowlist, privilege,
paths, aliases, timestamp rules and void API. Native driver and original Lima
remain unchanged.

All sixteen cases/observations pass in one run with identical assertions, output,
controls and four-second budgets. Baseline controls retain their frozen library.
Prompted and inline multipart addemote save and log once; aborts do not log.
SOUL saves, log appends, modal input and output roll back together. All 24 legal
symbols, alias order, same-task reads, account permissions, reload, payload bytes,
rejected symbols and write errors retain their audited behavior.

Compilation is 12/12, matching every audited diagnostic outcome, error and
location. Six gates, exact patch reproduction, twenty-nine copied source records,
thirteen appends and 159 predecessor sets pass. Six public comparisons, nine
older SOUL comparisons and seven pinned Git-blob matches remain frozen evidence.
No new fetch, upstream driver, native suite, historical matrix or preload survey
was needed.

Audit remaining SOUL command consumers next through the adopted shell entry.
Four declared clan/reporter symbols remain absent from LOG_D's allowlist;
their integration is separate. Full caller logging, public wizard discovery,
remote IMUD, vocabulary, network, login and cold startup remain unestablished.

#### Lima SOUL listing commands

Source review at Lima `eefaff66adcae9b01e16059574c925b0d63e70a1` on
2 October 2026 finds ten remaining SOUL command consumers without the adopted
CMD adapter. The [adverbs][lima-listing-adverbs],
[emoteapropos][lima-listing-apropos] and [showemote][lima-listing-show] listing
commands need only the protected varargs four-slot mixed adapter calling each
unchanged private `main(arg)`. Native typing, visibility and dispatch suffice.
Adverbs keeps framed pipe output and a zero-count frame for no matches;
emoteapropos's direct usage write bypasses pipes; showemote's native `%O` output
has unquoted rule strings. Existing menu entry and SOUL query behavior remain.

Eighteen new cases and two unchanged logger controls verify twenty observations.
Actual player-shell discovery runs adverbs/emoteapropos; showemote uses the real
shell's guarded CMD entry. Filtering, empty results, menus, pipelines, template
display, hidden handlers/adapters, rejected direct dispatch, second-account output,
read-only state and output rollback pass. The retained save/log and rollback
controls keep their original bodies, assertions, observations and frozen library.

Compilation improves 4/14 to 7/14 over thirty checks including two controls.
All three selected commands compile; eleven other target diagnostics stay
unchanged. Six gates, exact patch reproduction, twenty-six copied fixture sources,
two retained fragments, fourteen appends and 160 predecessor sets pass. Fourteen
original commands match pinned Git blobs; six public comparisons remain frozen.
No upstream driver, native suite, historical matrix or preload survey was rerun.
Tests use distinct nil, preserved players and four-second actions. The audit kept
the three-file patch diagnostic on the unchanged logger overlay.

[Verbs][lima-listing-verbs] additionally assigns zero to a string. Typed nil makes
its adapter control compile, but it still returns only a newline for existing
rules: its header/indented-rule scanner does not match native
[`parse_dump`](efun/parse_dump.md) rows. The existing
[`query_parser_rules`](efun/query_parser_rules.md) inspector is object-local.
Verbs remains unchanged pending separate global-inspection review. Source-only
follow-ups cover shorthand editor arguments, semote preview, random generators
and tell soul typing/recipient identity; their runtime behavior is unchecked.

The migration below adopts the three listing adapters; parser inspection for
verbs remains the next audit.
Complete SOUL consumers/vocabulary, clan/reporter logging, public wizard discovery,
remote IMUD, network, login and cold startup remain unestablished.

#### Lima SOUL listing migration

The 2 October 2026 migration adopts the exact three listing adapters over the
logger overlay. Adverbs, emoteapropos and showemote retain their private typed
handlers, menus, query behavior and output. Native driver and original Lima
remain unchanged; the adopted library matches the frozen diagnostic byte for byte.

All twenty cases and observations match the final audit: eighteen listing cases
and two logger controls. Only library/fixture directory paths change in the
harness; baseline and isolated verbs controls keep the frozen logger library.
Compilation is 7/14, matching every audited outcome, error and location. The
three selected commands compile, and seven existing failures remain visible.
Six gates, exact patch reproduction, thirty-two copied source records, fourteen
appends and 161 predecessor sets pass. Fourteen pinned Git-blob matches and six
public comparisons remain frozen; no new fetch, upstream execution, native suite,
historical matrix or preload survey was needed.

Audit global parser inspection for verbs next. Its typed adapter control compiles
but its scanner lists nothing from native parse_dump; query_parser_rules remains
object-local. Verbs stays unchanged. Other SOUL consumers, clan/reporter logging,
public wizard discovery, complete vocabulary, remote IMUD, network, login and
cold startup remain unestablished.

#### Lima parser inspection

The 2 October 2026 audit of [verbs][lima-listing-verbs] uses the same pinned Lima
revision and FluffOS `2c27287500daf48a87a1f89d69a97d8b9603028d`, with
`PACKAGE_PARSER`. The reviewed [FluffOS dump][fl-parser-dump] separates verb
headings from owner/rule lines and annotates synonyms. Lima expects that display
format. Native [`parse_dump`](efun/parse_dump.md) is global but flat;
[`query_parser_rules`](efun/query_parser_rules.md) is object-local.

Twelve cases and observations pass, including two unchanged listing controls.
Distinct registrations on the same owner, `go` with `to OBJ` and `go to` with
`OBJ`, produce identical text in both native inspectors. Rule newlines also break
a one-line reader. Tests verify global/local scope, multiword keys, preserved
rule whitespace, duplicate owner/order, expanded aliases, pending changes,
rollback, owner-scoped removal and read-only account context. The typed verbs
control still lists nothing. The audit created no production diagnostic patch
and left the listing overlay, native driver and original Lima unchanged.

Recommend a structured inspector with separate verb, rule, owner and handler-base
fields, optional owner filtering, registration order and fresh result containers.
The audit proposed `query_parser_registry`; the implementation below supplies
that API while retaining both text inspectors. Verbs still needs its structured
record adaptation, exact filter and SOUL-rule exclusion, CMD adapter and nil.

Focused compilation is 5/7: four production and two control diagnostics match the
frozen evidence, and the isolated owner fixture compiles. Six gates and 162
predecessor sets pass. Two pinned public sources were fetched; Lima bytes match
its original and local Git blob. FluffOS evidence is source review, without
upstream execution. Tests use distinct nil, preserved players and four-second
actions. No native suite, historical matrix or preload survey was rerun. Public
wizard discovery and the remaining SOUL consumers, logging, vocabulary, network,
login and cold startup remain unestablished.

#### Lima structured parser inspection

The 2 October 2026 implementation adds
[`query_parser_registry`](efun/query_parser_registry.md), returning fresh mappings
with registered verb, original rule, owner object and handler base verb. Nil or an
omitted owner selects all live parser registrations; a live object filters by
identity, and a destructed selector returns an empty array. The null selector
follows the configured driver mode. Registration order, duplicate entries,
multiword keys, rule whitespace and synonym base identity remain intact.

Thirteen new native tests exercise both driver null modes, direct/pointer calls,
owner/global queries, independent result containers, action-table exclusion,
pending edits, rollback, destruction, type/arity checking and actual commit
conflicts after an intervening registry edit. Complete workspace tests pass.
Twelve frozen Lima inspection cases retain their exact bodies and observations in
distinct mode. Six quality gates and 163 predecessor artifact/report sets pass.
Lima's original and active listing overlay are unchanged; no new public fetch or
upstream execution was needed.

The command adaptation audited below uses these records, preserving the exact
verb filter, SOUL-rule exclusion and output, with the existing CMD adapter and
nil. Complete wizard-shell discovery, other SOUL consumers, logging, vocabulary,
network, login and cold startup remain unestablished.

#### Lima verbs command

The 2 October 2026 audit prepares a one-file adaptation of
[verbs][lima-listing-verbs] over the listing overlay. Its private typed handler
uses [`query_parser_registry`](efun/query_parser_registry.md), grouping rules by
their full registered verb key in first-registration order. Rule order and
duplicates remain intact across owners. The exact, case-sensitive filter retains
multiword keys; nil and an empty string select all. Synonyms appear under their
registered keys. SOUL exclusion still tests the displayed verb and rule, even
when another object registered the same rule. The command retains its leading
newline, comma separators, final newline and buffered CMD output, with the
existing protected adapter and typed nil.

Sixteen new cases and two unchanged controls pass. They cover grouping, bare and
multiword rules, whitespace/newlines, aliases, exact filters, SOUL exclusions,
private/protected visibility, the shell caller guard, terminal/pipeline output,
account context, read-only state, owner lifecycle and registration/output
rollback. The tests enter through the actual shell's guarded CMD route; public
wizard command discovery remains unchecked. Original adapter-only string-zero
and typed-scanner empty-output controls retain their exact assertions and
observations.

Focused compilation is 9/11: all four diagnostic targets compile, while baseline
verbs and its adapter-only control retain their errors and locations. Six gates,
exact patch reproduction and 164 frozen predecessor sets pass. During the audit,
original Lima, the active listing overlay and native sources remained unchanged;
the patch was prepared separately. The original command matches its pinned Git
blob and frozen public source. No new public fetch, upstream execution, native
suite, historical matrix or preload survey was needed. The migration below
adopts this one-file patch; other SOUL consumers, logging, vocabulary, network,
login and cold startup remain unestablished.

#### Lima verbs migration

The 2 October 2026 migration adopts the exact one-file verbs patch over the
listing overlay. The adopted library matches the audited diagnostic byte for
byte, retaining structured rule grouping, exact filters, SOUL exclusion, CMD
output and the adapter/nil changes. Native driver and original Lima remain
unchanged.

All eighteen cases and observations match the final audit, including both frozen
controls. Only the active library directory changes in the harness; controls
retain their original library. Four focused targets compile with unchanged
outcomes and diagnostics. Six gates, exact patch reproduction, three unchanged
fixture sources and 165 frozen predecessor sets pass. Pinned source evidence
remains frozen; no new fetch, upstream execution, native suite, historical
matrix or preload survey was needed.

Audit public wizard command discovery for verbs and showemote next. Their guarded
CMD entries are tested; directory discovery and authorization through ordinary
shell input remain unchecked. Other SOUL consumers, logging, vocabulary, network,
login and cold startup remain unestablished.

#### Lima wizard command discovery

The 2 October 2026 audit uses the same pinned Lima revision and adopted verbs
overlay. The actual [user shell selection][lima-user-shell] starts
[wish][lima-wish]; ordinary modal input discovers verbs and showemote through
CMD_D and the guarded CMD entry. The configured administrator directory is
`/trans/admincmds`. Player input cannot discover either wizard command, including
explicit wizard command paths. Restart after promotion selects wish. Changing
wizard paths excludes a cached command, and resetpath restores discovery.

Fifteen cases and observations pass: thirteen new cases and two unchanged verbs
controls with matching observations and libraries. Tests cover default/exact and
multiword filters, SOUL exclusions, templates/usage/missing names, both-command
redirects and pipelines, append output, owner guard, history/read-only payload
state and outer-error rollback. The [modal dispatcher][lima-inputsys] catches
the foreign-context shell error and recaptures the owner's prompt without
running the command; an isolated shell entry verifies the uncaught guard
separately. Fixture-only methods expose protected history, while normal discovery
uses the existing input dispatcher. Shared support remains unchanged.

Six focused production targets compile. Six gates and 166 frozen predecessor
sets pass. Original Lima, active overlay and native sources remain unchanged;
this audit recommends no production patch. Three pinned public source renderings
were reviewed and local originals match pinned Git blobs. Commands-header fetches
failed, so its configured macro was reviewed locally. This is source review and
local adapted-library execution; no upstream execution, native suite, historical
matrix or preload survey was needed. Tests keep distinct nil, preserved players
and four-second actions.

Audit shorthand emote editors stupidemote and targetemote next through ordinary
wizard input. Role provisioning/revocation, restored saved-path policy, backtick
expansion, other SOUL consumers/vocabulary, logging, remote IMUD, network, login
and cold startup remain unestablished.

#### Lima shorthand emote editors

The 2 October 2026 audit uses the same pinned Lima revision and active verbs
overlay. [Stupidemote][lima-shorthand-stupid] and
[targetemote][lima-shorthand-target] both lack the CMD adapter. Add the existing
protected varargs adapter and pass a one-element message array through the
declared three-argument [SOUL API][lima-speaker-soul]. Keep their private nomask
string handlers. Adapter-only, three-argument scalar and four-argument array
controls already run under current dynamic calls; the API cleanup preserves
their generated templates and registrations.

Twenty cases and observations pass: eighteen new cases and two unchanged verbs
controls with matching bodies, libraries and observations. Actual wish input
discovers both diagnostic editors. Tests cover saved templates/reload,
replacement and retained unrelated rules, duplicate registrations, multiword
keys, usage/output routing, redirects/appends/pipelines, private visibility and
caller guards, account/body/channel/log context and outer-error rollback of
templates, parser rules, saved data, history, shell, files and deferred output.
Stupidemote's direct usage write bypasses redirection; targetemote buffers usage.
Shorthand edits retain their existing omission of LOG_ADDEMOTE logging.

Targetemote registers STR before OBJ/LIV. Under native
[first-success selection](efun/parse_sentence.md#result), an ambiguous living
name therefore matches STR; explicit LIV composition works. SOUL ordering at
registration, restore and replacement needs a separate audit before claiming
ordinary targeted dispatch. Reordering these command calls alone would retain
earlier registrations and saved-rule ordering.

Focused compilation is 16/18: active 4/6, diagnostic 6/6, controls 6/6. The two
active failures retain frozen missing-adapter diagnostics. Six gates, exact
two-file patch reproduction and 167 frozen predecessor sets pass. Shared support,
original Lima, active overlay and native sources remain unchanged; the patch is
diagnostic. The pinned SOUL source rendering was reviewed. Command fetches failed
and matching original/pinned local Git blobs were reviewed. This is source review
and local adapted-library execution, without upstream execution, a native suite,
historical matrix or preload survey. Tests keep distinct nil, preserved players
and four-second actions.

Adopt the two-file patch next, then audit SOUL rule ordering. Other SOUL
consumers/vocabulary, logging, remote IMUD, network, login and cold startup remain
unestablished.

#### Lima shorthand emote migration

The 2 October 2026 migration adopts the exact audited two-file patch over the
verbs overlay. Stupidemote and targetemote now use the guarded CMD adapter and
declared three-argument SOUL API with message arrays. Private nomask handlers,
generated templates and registration order retain their audited behavior. The
active shorthand-emote library matches the frozen diagnostic byte for byte;
original Lima and native sources remain unchanged.

All twenty case bodies, assertions and observations match the audit. Adopted
cases load production command files without diagnostic overwrites; missing-adapter,
argument and verbs controls retain frozen libraries or sources. Focused compilation
is 6/6 with identical audited outcomes and diagnostics. Six gates and 168 frozen
predecessor sets pass. Configuration changes only the library directory. Frozen
source evidence is reused without new public fetches, upstream execution, a native
suite, historical matrix or preload survey. Lima keeps distinct nil, preserved
call-out players and four-second actions.

Audit SOUL ordering at registration, restore and replacement next. STR still
shadows OBJ/LIV for ambiguous living names under native first-success selection;
explicit LIV composition works. Duplicate registrations and the original shorthand
logging omission remain intact. Ordinary targeted dispatch, other SOUL consumers,
vocabulary, logging, remote IMUD, network, login and cold startup remain
unestablished.

#### Lima SOUL rule ordering

The 2 October 2026 audit follows the shorthand migration on the same pinned Lima
revision. Active SOUL registration and restore select STR before a living target;
reordering targetemote alone fixes a fresh name but leaves older verbs affected.
Keep native first-success selection and adapt [SOUL][lima-speaker-soul] to rebuild
its stored rules in priority order at create, add, remove and move.

The one-file diagnostic uses the reviewed [FluffOS token weights][fl-parser-rule-weights]
for SOUL's rule forms: starting at one, add four for LIV, three for OBJ/LVS, two
for OBS and one for text/literal words; object `:v` subtracts one. Stable sorting
keeps saved mapping insertion order for equal priorities. It removes duplicates
after repeated edits, restores surviving registrations after partial removal,
and clears/rebuilds source and destination registrations after a move. Existing
templates and caller guards retain their behavior.

Twenty-three cases and observations pass: twenty-one new cases and two unchanged
verbs controls. Actual targetemote input and actor/target delivery, object/text/
empty fallbacks, replacement, complex/multiword rules, ties, old/legacy/invalid
saves, invalid edits, ownership, removal/move, guards and rollback pass. All
3,862 stock saved rules across 31 shapes register uniquely under the unchanged
four-second action budget. Removal/move daemon tests use fixture accessors on
legitimate command objects; ordinary directory discovery and authorization remain
unchecked. Two invalid-rule probe inputs were corrected from a valid literal
to unsupported OBJ:q without changing the diagnostic or weakening assertions.

Focused compilation is 16/16. Six gates, exact patch reproduction and 169 frozen
predecessor sets pass. Native sources, original Lima and the active shorthand
overlay remain unchanged; the SOUL patch is diagnostic. Frozen pinned source
hashes remain intact and two public FluffOS renderings were reviewed. This is
source review and adapted-library execution without upstream execution, a native
suite, historical matrix or preload survey. Lima retains distinct nil and
preserved call-out players.

This is SOUL-local precedence, not general FluffOS weighted selection. Equal
weights retain native mapping order rather than newest-registration traversal;
rebuilding can move SOUL's entries relative to other owners in the global registry.
Other owners' rules remain intact, and native global selection/callback effects
are unchanged. Adopt the SOUL rebuild next, then audit ordinary rmemote/mvemote
entry. Complete vocabulary delivery, other SOUL consumers, logging, remote IMUD,
network, login and cold startup remain unestablished.

#### Lima SOUL ordering migration

The 2 October 2026 migration adopts the exact audited SOUL patch over the shorthand
overlay. The new active library matches the frozen diagnostic byte for byte;
only `daemons/soul_d.c` changes. Registration rebuilds at create, add, remove and
move retain the audited living/object precedence, stable equal-weight order,
deduplication and removal/move fixes. Templates, command handlers and caller
guards retain their behavior. Original Lima and native sources remain unchanged.

All twenty-three case bodies, assertions and observations match the audit.
Four fixture sources are copied unchanged, and support changes only the adopted
library directory. Baseline and verbs controls retain frozen libraries and sources.
All 3,862 stock rules register uniquely under the existing four-second action
budget. Focused compilation is 8/8 with identical audited outcomes and diagnostics;
six gates and 170 frozen predecessor sets pass. Configuration changes only its
library directory. Frozen source evidence is reused without new public fetches,
upstream execution, a native suite, historical matrix or preload survey. Lima
keeps distinct nil and preserved call-out players.

Audit ordinary rmemote/mvemote entry, directory discovery and authorization next.
Their daemon tests use fixture accessors on genuine command objects. SOUL-local
precedence retains the documented mapping ties and global registration effects;
it does not reproduce general FluffOS weighting or callback effects. Complete
vocabulary delivery, other SOUL consumers, logging, remote IMUD, network, login
and cold startup remain unestablished.

#### Lima emote removal and move command entry

The 2 October 2026 audit exercises ordinary wish input on the adopted SOUL overlay.
Both editors are discoverable to wizards; player paths and foreign shell contexts
cannot reach them. Rmemote requires an existing wizard home directory, including
for admins. Mvemote requires administrator status but no home directory. The
reviewed configuration uses WIZ_DIR=/wiz with EXPANDED_WIZ_DIR undefined.
[Configuration](https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/config.h),
[removal handler][lima-emote-remove], [move handler][lima-emote-move].

Whole and specific-rule removal, rename and destination replacement retain correct
registrations across reload. Redirects, append and pipelines route status output;
outer errors roll back mutations, saves, history and output. Active
`mvemote foo foo` reports success and persists deletion. A one-condition diagnostic
SOUL guard refuses identical names before any change, retaining ordinary lifecycle
and authorization. It is not adopted. Multiword destinations register and parse,
but the editors retain their pinned first-space source boundary; quoting does not
make a multiword source editable. [SOUL implementation][lima-speaker-soul].

Twenty-three cases and observations pass: twenty-one new and two unchanged verbs
controls, with eighteen active cases and three diagnostic cases. A player refusal
wording assertion was corrected for the seven possible random nonsense messages;
source-derived expectations, added discovery checks and normalized observations
preserve the permission and data checks. Focused compilation is 12/12; six gates,
exact patch reproduction and 171 frozen predecessor sets pass. Original Lima,
active library and native sources remain unchanged. Distinct nil, preserved
call-out players and four-second actions remain the target.

Pinned local Git blobs match the reviewed original files. The public SOUL
rendering was reviewed; new command/path fetches failed, and frozen wizard
source evidence was reused. This is adapted-library execution and source review,
without upstream driver execution, a native suite, historical matrix or preload
survey. Adopt the self-move guard next, then audit semote preview. Live account
provisioning/revocation, complete vocabulary delivery, other SOUL consumers,
logging, remote IMUD, network, login and cold startup remain unestablished.

#### Lima emote lifecycle migration

The 2 October 2026 migration adopts the exact self-move guard from the command
audit over the SOUL ordering overlay. Only `daemons/soul_d.c` changes, and the
entire active library matches the frozen diagnostic byte for byte. Identical-name
moves now fail before changing emote data, parser registrations or saves.
Ordinary removal, rename, destination replacement, administrator/caller checks
and rollback retain audited behavior. Command handlers retain their existing
first-space source boundary.

All twenty-three case bodies, assertions and normalized observations match the
audit. Three cases exercise the adopted guard and ordinary lifecycle/admin/rollback
behavior; eighteen previous-library cases retain the frozen SOUL ordering overlay,
including the deletion control. Two historical verbs controls retain their frozen
libraries and sources. Three fixture files are copied unchanged; support changes
only the adopted directory. Focused compilation is 6/6 with matching outcomes
and diagnostics. Six gates and 172 frozen predecessor artifact/report sets pass.
Configuration changes only its library directory. Distinct nil, preserved call-out
players and four-second actions remain the target.

Frozen source evidence is reused without a new public fetch, upstream execution,
native suite, historical matrix or preload survey. Original Lima, native sources
and previous overlays are unchanged. Audit semote preview next. Live account
provisioning/revocation, complete vocabulary delivery, other SOUL consumers,
logging, remote IMUD, network, login and cold startup remain unestablished.

#### Lima semote preview

The 2 October 2026 audit follows the emote lifecycle migration on Lima
`eefaff66adcae9b01e16059574c925b0d63e70a1`. [Semote][lima-semote-preview]
lacks the protected CMD dispatch adapter and cannot load. Adding only that
adapter leaves a second defect: its two-target preview loads `/cmds/player/semote2`,
which is absent from the pinned tree. The failed load returns nil, matching the
nil onlooker perspective. An onlooker consequently sees “Somebody and you”
instead of two hypothetical targets.

The two-file diagnostic adds the existing dispatch adapter and a distinct,
grammar-only `/obj/mudlib/semote_target` object named “Somebody else”. It inherits
Lima's pronoun/reflexive grammar and is not registered as a command. Actor, both
targets and onlooker receive the correct preview text. No native API or SOUL
change is needed; the patch remains diagnostic.

Semote retains its five exact forms, in order: empty, LIV, STR, LIV STR and
LIV LIV. Other forms are skipped, with empty output if none qualifies. Frozen
stock metadata counts 3,762 covered rule entries and 100 others across 31 shapes;
this is not complete vocabulary execution. Text slots keep the underscore
placeholder and existing [punctuation behavior][lima-message-grammar]. Terminal
and menu output use paging; pipeline/file output retains the raw payload.

Twenty-one new cases and two unchanged verbs controls pass. Coverage includes
ordinary player/wizard discovery, aliases, message parts, all four perspectives,
pronouns/possessives/reflexives, usage, redirects/appends, pipelines, handler
visibility, CMD/shell-owner guards, restore and rollback. Previewing changes no
SOUL save, templates or registrations and sends no emote to another user. The
existing public menu entry matches ordinary terminal output without its prompt;
complete [plmenu][lima-semote-menu] interaction is unestablished. Full plmenu
compilation fails at `complete_choice(0, ...)`, whose first parameter is a string,
in both active and diagnostic libraries. That distinct-nil port blocker remains
separate from the preview adaptation.

Focused compilation has ten successes from thirteen probes: original semote's
four adapter errors and two identical plmenu failures remain visible. Diagnostic
semote and its helper pass; shared dependency outcomes are unchanged. Six quality
gates, exact patch reproduction and 173 predecessor artifact/report sets pass.
Unsuccessful fixture runs are preserved; corrected expectations distinguish
paging, placeholder punctuation and the player-only direct-entry accessor.

Pinned local Git blobs match the reviewed originals. The public SOUL rendering
was reviewed; new semote/menu fetches failed. This is source review and adapted-
library execution, without upstream driver execution, a native suite, historical
matrix or preload survey. Distinct nil, preserved call-out players and four-second
actions remain the target. Original Lima, active overlay and native sources are
unchanged. Adopt the semote adapter/helper next, then audit random generators.
Complete menu interaction, other SOUL consumers, tell recipient typing, logging,
remote IMUD, network, login and cold startup remain unestablished.

#### Lima semote migration

The 2 October 2026 migration adopts the exact audited semote adapter/helper over
the emote lifecycle overlay. Only `cmds/player/semote.c` changes and
`obj/mudlib/semote_target.c` is added; the entire adopted library matches the frozen
diagnostic byte for byte. Ordinary command input now discovers and loads semote,
and its distinct second target restores correct two-target/onlooker previews.
The five existing forms, formatting, message parts, read-only behavior, caller
checks, restore and rollback retain audited behavior. SOUL is unchanged.

All twenty-three case bodies, assertions and normalized observations match the
audit. Eighteen cases exercise the adopted library; three retain the frozen
previous library and adapter-only control, including both original defects.
Two historical verbs controls retain their frozen sources/libraries. Four fixture
files are copied unchanged; support changes only the adopted directory. Focused
compilation has six successes from seven probes, matching every audited outcome
and diagnostic. The full-menu `complete_choice(0, ...)` string-argument blocker
remains visible. Six quality gates and 174 predecessor artifact/report sets pass.

Configuration changes only its library directory. Distinct nil, preserved
call-out players and four-second actions remain the target. Frozen source
evidence is reused without new fetches, upstream execution, a native suite,
historical matrix or preload survey. Original Lima, native sources and previous
overlays are unchanged. Audit random generators next. Semote retains its five-form
preview limit. Complete menu interaction, complete vocabulary delivery, other SOUL
consumers, tell recipient typing, logging, remote IMUD, network, login and cold
startup remain unestablished.

#### Lima random emote commands

The 2 October 2026 audit follows the semote migration on Lima
`eefaff66adcae9b01e16059574c925b0d63e70a1`. Both [random][lima-random-emote]
and [random2][lima-random-generator] lack the protected CMD dispatch adapter.
Random selects an existing emote with an exact empty or LIV rule, then executes
it through the body's normal game-command parser. Names are trimmed and
lowercased; an unknown name retains the existing untargeted fallback.

Adding only the adapter lets random work with eligible data, but an empty pool
errors in choice and a nonempty pool with no matching rule exhausts the four-second
evaluation budget. The one-file diagnostic filters eligible names once, refuses
an empty result with `No suitable emotes.`, and makes one choice. This retains
equal weights among eligible names under the existing uniform-choice contract;
that is source reasoning, not a statistical distribution test. The list is scanned
once per invocation; full-stock performance and delivery remain unestablished.

Twenty-one new cases and two unchanged verbs controls pass. Coverage includes
ordinary/targeted/self-target delivery, name normalization and fallback, eligible
membership, a 65-entry sparse pool, empty/nonmatching pools, restore, visibility,
CMD/shell-owner guards and rollback. Real emote delivery goes directly to actor,
target and onlookers; it is not CMD pipeline output. Refusal status follows CMD
redirects/appends and pipelines. Failed outer transactions preserve deliveries,
shell/history, SOUL templates/save/registrations and channel state. Empty string
templates remain truthy and retain their existing blank delivery. Global name
lookup can find a remote body, but the game parser uses local scope; remote-target
execution was not tested.

Random2 is an experimental text generator, not another selector. After adding
only its adapter, compilation reports a mapping declared as a string and three
zero-to-string assignments. A separate type-only control changes the mapping/value
annotations and nullable string initializers, then exposes unknown `get_config`.
These controls are compile probes, not a working port. Training, weighted
generation, readiness, batching, sentinels, termination, target/adverb availability
and mapping-limit policy need a dedicated audit; no native API is added here.

Focused compilation has ten successes from fifteen probes, including staged
controls; all nonzero outcomes remain recorded. Shared dependency outcomes are
unchanged. Six quality gates, exact patch reproduction and 175 predecessor
artifact/report sets pass. Unsuccessful fixture runs are preserved. Shared support
changes only library selectors; temporary SOUL data is seeded through an appended
fixture helper, with no command accessors added.

Pinned local Git blobs match the reviewed originals. New random-command public
fetches failed, and frozen SOUL evidence is reused. This is source review and
adapted-library execution, without upstream execution, a native suite, historical
matrix or preload survey. Distinct nil, preserved call-out players and four-second
actions remain the target. Original Lima, native sources and active overlay are
unchanged; the selector patch remains diagnostic. Adopt it next, then audit
random2's training and mapping-limit policy. Complete menu interaction, complete
vocabulary delivery, other SOUL consumers, tell recipient typing, logging, remote
IMUD, network, login and cold startup remain unestablished.

#### Lima random emote selector migration

The 2 October 2026 migration adopts the exact audited selector patch over the
semote overlay. Only `cmds/player/random.c` changes; the entire adopted library
matches the frozen diagnostic byte for byte. The protected CMD adapter allows
ordinary command discovery to load random. Filtering eligible names once and
refusing an empty result removes the empty-pool error and nonmatching-pool loop.
Exact empty/LIV rules, normalized names, unknown-name fallback and empty-template
truthiness retain their audited behavior. Original Lima and native sources remain
unchanged.

All twenty-three case bodies, assertions and normalized observations match the
audit: seventeen adopted-library cases, four frozen previous-library controls
and two historical verbs controls. Four fixture sources are copied unchanged;
support changes only the adopted library directory. Action delivery, refusal
redirects/appends and pipelines, caller checks, restore and rollback retain the
audited results. Focused compilation has five successes from six probes, with
identical outcomes and diagnostics; random2 retains its missing dispatch adapter.
Six gates, exact adoption and 176 predecessor artifact/report sets pass.

Configuration changes only its library directory. Lima keeps distinct nil,
preserved call-out players and four-second actions. Frozen source evidence is
reused without new public fetches, upstream execution, a native suite, historical
matrix or preload survey; this adaptation adds no native API.

Audit random2 training and mapping-limit policy next. Its staged compile controls
remain separate evidence, not an adopted port. Full-stock selector performance,
statistical distribution, remote-target execution, complete menu interaction,
complete vocabulary delivery, other SOUL consumers, tell recipient typing,
logging, remote IMUD, network, login and cold startup remain unestablished.

#### Lima experimental random text generation

The 2 October 2026 audit follows the selector migration on the same pinned Lima
revision. [Random2][lima-random-generator] learns two-word forward/reverse contexts
from SOUL templates. Its optional argument selects a training word, not the named
recipient suggested by its usage text. Delivery independently chooses an adverb
and a local attached body, including the actor.

Active random2 retains its missing CMD adapter. Adapter-only compilation exposes
four strict type errors; the frozen type-only control then exposes unknown
`get_config`. Replacing that query with an LPC control value permits compilation,
but long templates fail on nil/string context arithmetic. A separate context
conversion control then exposes missing-counter increments. Short strings of
three words or fewer and multipart arrays are skipped; an empty model lacks
readiness and missing-context guards. These are isolated controls, not a port.

The legacy `(: process :)` callback returns a function under native closure
semantics. In a 101-entry short-template pool, the timer fires without consuming
the last entry; an explicit process call consumes it. A separate bare-function
control completes the batch. The reverse-model size guard runs after forward
mutation and after scheduling the next batch. With initialized counters, explicit
context conversion, a bare callback and a zero LPC probe limit, it leaves a
forward-only prefix and still processes a later batch. The zero value is a policy
boundary probe, not an alternate null mode or a native mapping limit.

Manually seeded weighted lookups preserve membership and return nil for numeric/
nil end transitions; this is not a distribution test or complete trained
generation. Empty-string transitions remain truthy. Unknown-word status uses
terminal `write` even in CMD pipeline mode. Direct delivery errors with no
adverbs; self-target selection produces the existing duplicate actor/target
messages, and a second local player receives the onlooker message.

FluffOS's reviewed `get_config` reads indexed driver settings, and its mapping
insertion enforces the configured entry cap. Source review uses
`44532e0228f34be2184c701a361df4079371b8b0`, with `F_GET_CONFIG` in the core package;
no upstream driver was executed. Native lpc-rs has no corresponding configured
mapping-entry cap. Adding a query solely for this command would not repair its
training policy. [Query implementation][fl-random-config-query],
[mapping insertion][fl-random-mapping-limit].

Sixteen new cases and two unchanged verbs controls pass, with eighteen recorded
observations. Focused compilation has eight successes from eleven probes; the
three failures remain explicit and all active diagnostics are unchanged.
Six gates, exact probe reproduction and 177 predecessor artifact/report sets pass.
Initial failed fixtures, callback findings and the subsequent lint correction
are preserved. Pinned local originals match Git blobs; public pinned renderings
could not be fetched, and current official configuration prose was reviewed as
supplementary evidence. Distinct nil, preserved call-out players and four-second
actions remain the target; original Lima, native sources and active overlay stay
unchanged.

Defer this experimental generator and audit tell recipient typing next. A future
port needs explicit sentinels and structured contexts, initialized counters,
template-shape policy, readiness checks, bounded generation, correct batch
callbacks and a mudlib-owned model budget enforced before forward/reverse changes.
End-to-end trained generation, full-stock training, adversarial termination,
remote/offline/NPC targeting, complete menu interaction, other SOUL consumers,
logging, remote IMUD, network, login and cold startup remain unestablished.

#### Lima tell recipients and reply

The 2 October 2026 audit follows the experimental generator audit on Lima
`eefaff66adcae9b01e16059574c925b0d63e70a1`. Both [tell][lima-tell-command] and
[reply][lima-reply-command] lack the protected CMD dispatch adapter. Adding the
adapter alone lets plain tell work; no new native API is needed.

Tell's two SOUL result declarations should be `mixed *`: the result contains a
body-recipient array and a message array. Adapter-only and type-only controls both
compile, so this is a contract correction rather than another observed compiler
blocker. The destination from `find_user` is an account, while SOUL recipients are
bodies. Use `who->query_body()` for recipient membership; otherwise the named
account receives the onlooker message. A two-target template confirms the correct
first/second perspectives while delivery stays private to the named account.

The self-send guard compares an account with the sender's body and therefore
delivers twice, records history twice and replaces reply with the sender's own
userid. Compare `who` with `this_user()` instead. Plain and SOUL self-tells then
record once, preserve reply and deliver only the sender copy.

Reply works through ordinary command input and the existing protected resend
contract. Both commands retain private handlers, protected adapters, caller guards
and tell's reply-only resend permission. Uppercase account names, literal emote
fallback, usage/refusal paths and private delivery are covered. Both commands
retain `no_redirection()`: `>` and `|` remain literal message text, without creating
a file or executing another command. Foreign shell player context cannot forge
private delivery.

[Tell history][lima-tell-history] retains the newest twenty messages per account.
Displaying and clearing history works; clearing preserves the reply target.
History and reply are nosave in the reviewed build. An outer failure after an
actual tell rolls back output, account history/reply, shell/history, channel
state, SOUL save data and parser registrations.

Twenty new cases and two unchanged verbs controls pass, with twenty-two recorded
observations. Four new cases use active/adapter/type controls; sixteen use the
two-file diagnostic. Focused compilation has thirteen successes from fifteen
probes; both original dispatch failures remain explicit, and shared dependencies
are unchanged. Six gates and exact patch reproduction pass. An initial Rust build
failure and its type-annotation correction are preserved; no runtime assertions
changed. Distinct nil, preserved call-out players and four-second actions remain
the target. Original Lima, native sources, active overlay and 178 predecessor
artifact/report sets remain unchanged.

Pinned local originals match Git blobs. New public tell/reply renderings could
not be fetched; frozen SOUL evidence is reused. The active pipeline parser was
reviewed as native overlay code, not pinned upstream source. No upstream execution,
native suite rerun, historical matrix or preload survey was run.

Adopt the exact tell/reply patch next, then audit complete plmenu interaction and
its known distinct-nil string-argument blocker. Remote IMUD runtime,
linkdead/inactive/bodyless recipients, missing sender body, offline/NPC targets,
history persistence, long pager interaction, complete vocabulary delivery, other
SOUL consumers, logging, network, login and cold startup remain unestablished.

#### Lima tell and reply migration

The 2 October 2026 migration adopts the exact audited two-file tell/reply patch
over the random-emote overlay on the same pinned Lima revision. Only
`cmds/player/tell.c` and `cmds/player/reply.c` change; the entire adopted library
matches the frozen diagnostic byte for byte. Protected CMD adapters allow ordinary
discovery to load both commands. SOUL target lookup uses the destination body,
and self-send suppression compares accounts. Targets receive their own perspective;
self-tells deliver and record once while preserving the previous reply target.

All twenty-two case bodies, assertions and normalized observations match the
audit. Sixteen cases use the adopted overlay; four retain frozen previous-library
controls, and two retain historical verbs sources/libraries. Six fixture/control
files are copied unchanged, and support changes only the adopted directory.
Private delivery, emote fallback, reply guards, literal no-redirection suffixes,
bounded history, refusal paths, foreign shell context and rollback retain their
audited behavior.

All six focused compiles pass with identical audited diagnostics: SOUL, wish,
secure/user, CMD_D, tell and reply. Six gates and exact adoption pass. Frozen
evidence is reused without new fetches or upstream execution; no native suite,
historical matrix or preload survey was rerun. Distinct nil, preserved call-out
players and four-second actions remain the target. Original Lima, native sources,
previous overlays and 179 predecessor artifact/report sets remain unchanged.

Audit complete plmenu interaction and its known distinct-nil string-argument
blocker next. Remote IMUD runtime, linkdead/inactive/bodyless recipients, missing
sender body, offline/NPC targets, history persistence, long pager interaction,
complete vocabulary delivery, other SOUL consumers, logging, network, login and
cold startup remain unestablished.

#### Lima public mailer entry and ownership audit

Reviewed **5 October 2026** against the pinned
[User facade][lima-user-mailbase], [base mailer][lima-mailer-base],
[standard][lima-mailer-standard], [Mh][lima-mailer-mh],
[mail command][lima-mail-command], [mailbox][lima-entry-mailbox],
[master][lima-master] and [destruction simul][lima-overrides].
A foreign cold `query_mailer()` caches a handler for the caller's mailbox in
another User; subsequent owner calls reuse that wrong binding. A public
constructor call can also rebind an existing handler. Mailbox content
checks and Mh's selected-owner checks continue to refuse the mismatched context;
both accounts retain their receipts, unread state, cursor and saved bytes.

An idle standard `begin_mail(nil)` can assign foreign modal input before its
header reads are refused, leaving the owner unable to enter. The existing
input-owner check blocks foreign nil entry after assignment, but the recipient
branch can still push a Subject prompt on the caller's User. Mh `begin_mail`
remains informational; composition uses `Mm`. Arbitrary programs in the correct
owner context can currently begin either handler, and warm cached queries work
without a player.

Foreign `unload_mailer()`/`remove()` requests destroy the handler, though explicit
destruction through the simul is refused to the same unprivileged caller.
`remove()` delegates self-destruction, which the stock master intentionally allows.
This does not bypass native authorization. A foreign default-selection request
accepts an unlisted public fixture program; the cached handler remains until
unloading, and the next owned query clones the chosen program. This establishes
unrestricted selection, not a load-permission or privilege bypass.

Owned reads, full standard/Mh composition/delivery, lazy selection and restart
remain usable. News replies reject an unrelated caller. Separate original and
saved receipt bytes remain intact during these context failures. Uncaught outer
errors roll back cached clones, input changes, default changes, rebinding,
destruction and output. The menu's `start_mail` call and legacy body facade are
source-reviewed; complete execution of those paths remains unchecked.

**Recommendation:** guard User lazy creation/configuration/unload and mailer
ownership before effects, restrict default choices to supported handlers, prevent
constructor rebinding and guard removal. Preserve owned commands, menu/news
callers and self/owned or already authorized cleanup. A command-only caller list
would exclude stock entry sites. No native or master-policy change is indicated;
this audit adopts no patch, and the active export-access overlay stays unchanged.

**Sixteen fresh cases pass** using two actual Users, bodies, connections,
separate daemon-sent originals, native saved classes and real standard/Mh input.
Ten fresh CLI checks pass **9/10**, including the stock menu and User facade;
only the standalone shared shell retains its four unresolved concrete hooks,
and both actual shells pass. Four quality gates pass, including warning-free
workspace Clippy across all targets/features. Full menu execution remains
unchecked. Thirty pinned Lima reviews include twenty-five reused and five additional
reviews; seventeen native reviews retain their source hashes. All 262 predecessor
artifact/report sets remain preserved. The initial fixture's explicit native
`destruct` reference was refused at compile time by `valid_override`, even in an
unused branch; corrected probes use the guarded simul, with diagnostic receipts
retained. This was a fixture correction, without production permission changes.

Execution is local lpc-rs with distinct nil, preserved call-out players,
4,000 ms actions, `USE_USER_MENU` and `.o` saves. Native overview cells and original
Lima remain unchanged. Upstream-driver execution and a fresh public-source fetch
remain unchecked. Full menu/login, legacy body mail, disconnect/account cleanup,
concurrent initialization, remote delivery and startup remain separate.

#### Lima mail export authorization repair

The **5 October 2026** local port adds a current `wizardp(this_user())` check to
shared [base mailer][lima-mailer-base] `cmd_read` whenever a destination is
supplied. Nonwizards receive a clear refusal before message selection, building,
path expansion or file authorization. Both [standard][lima-mailer-standard] modal
input and selected [Mh][lima-mailer-mh] shortcuts therefore refuse hidden exports,
including public `/tmp` create/append requests and default message numbers.
Rejected lost-message exports retain the owner's stale receipt and saved bytes.

Ordinary reads pass nil for the destination and retain their behavior. Wizard
exports keep header/body selection, wizard Mail paths and native
[`write_file`](efun/write_file.md) authorization through the
[master][lima-master] and [security daemon][lima-security]. Authorized lost
reads/exports still warn and prune only the owner's stale receipt without
writing a marker. Wizard file refusals retain path/reason feedback and subsequent
owned exports remain usable. Outer aborts discard writes or refusal feedback.
For parsed nonwizard exports, membership refusal precedes message-number bounds;
malformed syntax retains command usage. Wizards retain bounds feedback.

The same bound handler refuses exports after fixture wizard membership removal;
the retained wizard shell does not bypass the check. Complete production demotion
and shell replacement remain unchecked. Help, reads, actual player-shell routing,
protected `cmd_read` and foreign User/Mh caller guards retain their policies.
Only one Lima file changes; native APIs, master policy, original Lima and overview
cells remain unchanged.

**Nineteen fresh cases pass:** seventeen adopted-overlay cases and two
previous-overlay controls reproducing hidden public exports and preauthorization
lost-message pruning. Both real mailers use stock Users, bodies, connections,
daemon sends, native saved classes and owned mailboxes. Sixteen CLI checks retain
**7/8 clean on each overlay**, including both complete shells. Standalone shared
shell checks report the same unresolved `query_save_path` (twice),
`execute_command` and `query_shellname` hooks. Four quality gates pass, including
fresh warning-free workspace Clippy across all targets/features.

Twenty-five pinned Lima source reviews are reused with original/previous/adopted
hashes; eleven native reviews retain their source hashes. All 261 predecessor
artifact/report sets remain preserved. Execution is local lpc-rs with distinct
nil, preserved call-out players, 4,000 ms actions, `USE_USER_MENU` and `.o` saves.
Upstream-driver execution and a fresh public-source fetch remain unchecked.
Other public mailer entry points and owner/player context, starting with
`begin_mail`, are the next boundary; remote routing, long paging/ranges,
concurrency, full account deletion, disconnect recovery and startup remain
separate.

#### Lima mail export authorization and command exposure audit

Reviewed **5 October 2026** against pinned [standard][lima-mailer-standard],
[Mh][lima-mailer-mh], [base mailer][lima-mailer-base],
[User mail bridge][lima-user-mailbase], [shared shell][lima-shell],
[player shell][lima-pshell], [wizard shell][lima-wish] and
[mail command][lima-mail-command]. Both mailers hide file exports from nonwizards
in help, but accept them through normal input. Nonwizards can create and append
header/body exports under the stock public `/tmp` protection, including current
message defaults, without changing unread state or saved receipts.

Native [`write_file`](efun/write_file.md) still consults the
[master][lima-master] and [security daemon][lima-security]. Bare filenames resolve
to the wizard Mail directory and are refused in the ordinary fixture; secure and
foreign wizard destinations retain their bytes and show path/reason feedback.
Missing-message exports nevertheless prune and save the owner's stale receipt
before any write check. Other owners retain theirs. Invalid arguments and outer
rollback keep their existing policies.

The actual player shell reaches standard modal mail through `mail` and lazily
creates selected Mh on `M` shortcuts. Standard selection leaves those shortcuts
to ordinary command lookup. Nonwizard help and reads remain usable. Wizard
controls retain export help and owned saves. Removing fixture wizard membership
while retaining a bound handler hides help immediately but leaves public exports
available; complete privilege revocation and shell replacement remain separate.
Protected `cmd_read`, foreign Mh parsing and foreign User bridge calls retain
their existing caller guards.

The wizard-only help suggests a wizard-only export policy. **Recommendation:**
check current wizard membership inside shared `cmd_read` when a destination is
supplied, before selecting or building the message. Refuse clearly, preventing
both hidden exports and rejected-export pruning, while preserving ordinary
reads, wizard exports and native file authorization. No native/master change is
needed. The active export overlay remains unchanged; this audit adopts no repair.

**Fourteen fresh tests/observations pass** with real stock Users, bodies,
connections, daemon sends, native saved classes, owned mailboxes and standard/Mh
input. Eight CLI checks pass **7/8**: the standalone shared shell reports
unresolved hooks supplied by its concrete programs, and both actual shells pass.
Four quality gates pass, including fresh workspace Clippy with all targets/features
and warnings denied.
Twenty-five pinned Lima reviews include nineteen reused and six additional
reviews; eleven native reviews retain their hashes. All 260 predecessor
artifact/report sets remain intact. Development cursor expectation and
existing-handler preparation corrections retain their receipts.

Execution is local lpc-rs only, with distinct nil, preserved call-out players,
4,000 ms actions, `USE_USER_MENU` and `.o` saves. Upstream-driver execution and a
fresh public-source fetch remain unchecked; original Lima and overview cells
remain unchanged. Other public handler entry points, long paging/ranges, remote
routing, concurrency, account deletion, disconnect recovery and startup remain
separate.

#### Lima lost-message file export repair

The **5 October 2026** local port changes only the shared
[base mailer][lima-mailer-base]. Its message builder displays the existing
lost-message warning and returns nil, preventing header/body exports from
creating files or appending the warning to existing files. The owner's stale
receipt still gets pruned and saved; other owners retain their receipts.
Ordinary lost-message reads keep their prompt and numeric cursor, allowing the
next read without starting a pager for the warning.

Export writes catch native [`write_file`](efun/write_file.md) exceptions and
report the destination and reason. Missing parents, directory destinations and
master permission refusals now produce feedback through normal standard/Mh
input. Healthy exports retain append mode, bytes, selected defaults and unread
state. Malformed-record handling, body salvage and protected entry keep their
existing policies. Uncaught outer errors discard writes, receipt pruning, saves
and success/error feedback. The success message acknowledges staging;
commit-time I/O failures remain separate.

**Twenty-three fresh tests/observations pass**: twenty-one on the adopted overlay
and two controls reproducing both defects on the previous help overlay. Fourteen
core CLI checks pass, previous 7/7 and adopted 7/7. Validation uses real stock
Users, bodies, connections, MAIL_D sends, native saved classes and owned
mailboxes; the historical audit runner remains frozen. Four quality gates pass,
including workspace Clippy with all targets/features and warnings denied.
Nineteen pinned Lima and eleven
native source reviews are reused, preserving all 259 predecessor artifact/report
sets. The initial terminal wrap assertion correction retains its receipts. The
one-file patch is adopted under `local/`; original Lima, native APIs, master policy and
overview cells remain unchanged. Reviewed source claims use the pinned
[standard][lima-mailer-standard], [Mh][lima-mailer-mh],
[mailbox][lima-entry-mailbox], [master][lima-master],
[security][lima-security] and [input forwarding][lima-input-forwarding] contracts.
Execution is local lpc-rs only, with distinct nil, preserved call-out players,
4,000 ms actions, `USE_USER_MENU` and `.o` saves; upstream-driver execution and a
fresh public-source fetch remain unchecked.

Audit nonwizard export authorization and mail command exposure next. Remote
routing, other delivery faults, long paging/ranges, concurrency, disconnect
recovery, account deletion and startup remain separate.

#### Lima lost-message file export audit

Reviewed **5 October 2026** against pinned [base mailer][lima-mailer-base],
[standard][lima-mailer-standard], [Mh][lima-mailer-mh] and
[mailbox][lima-entry-mailbox], alongside the [master][lima-master],
[security][lima-security] and [input forwarding][lima-input-forwarding] contracts.
Both normal mailers, through both header and body export, write the missing-message
display marker to a new or existing destination and claim a successful save.
The owner's stale receipt is pruned; other owners retain theirs. Existing export
bytes receive the marker appended without a trailing newline.

Healthy exports preserve Unicode, header/body selection, unread flags, cursors,
saved receipts and message bytes. Bare filenames use the wizard Mail directory;
explicit paths remain explicit, and omitted numbers use the actual selected
second sorted key. Corrupt saves or invalid bodies retain evidence and block
export. Invalid sender metadata blocks headers while raw body export remains
available; nil recipient lists omit those header lines. Invalid arguments and
protected remote entry write nothing.

Native [`write_file`](efun/write_file.md) raises for missing parent directories
and directory destinations. Normal User input catches those errors and recaptures
the prompt, skipping the mailer's failure feedback. A failed lost export still
commits its earlier receipt pruning. Uncaught outer errors discard file creation
or marker appends, pruning, saves and output; retries remain usable. Commit-time
I/O failures remain separate. **Recommendation:** display the lost warning and
return nil from the shared builder, and catch/report export write refusals in
`cmd_read`. Preserve reads, pruning, append semantics and rollback; no native
change is needed, and no repair is adopted by this audit.

**Seventeen fresh tests/observations pass**, covering real Users, bodies,
connections, daemon sends, native saved classes, owned mailboxes and standard/Mh
input. Seven core CLI checks and four quality gates pass, including warning-free
workspace Clippy. Nineteen pinned Lima and eleven native reviews are recorded;
all 258 predecessor artifact/report sets
remain intact. Development fixture and diagnostic-build corrections retain their
receipts. The active help overlay/configuration, original/native/master inputs
and overview cells remain unchanged. Execution is local lpc-rs only with distinct
nil, preserved call-out players, 4,000 ms actions, `USE_USER_MENU` and `.o` saves;
no upstream execution, fresh public-source fetch or historical harness rerun.
Remote routing, other delivery faults, long paging/ranges, concurrency, disconnect
recovery, account deletion and startup remain separate.

#### Lima mail help correction

The **5 October 2026** local port corrects displayed strings in the
[standard][lima-mailer-standard] and [Mh][lima-mailer-mh] mailers. New-mail and
forwarding help shows comma-separated destinations; forwarding includes its
required recipient list. Forwarding and reply help/usage shows optional message
numbers, with the existing current-selection explanation. Both screens explain
spaces around commas and use comma-based group examples with their own `m`/`Mm`
command. The [base mailer][lima-mailer-base] recipient parser and all executable
code remain unchanged; masking string contents reproduces both previous files
byte for byte. Native APIs, master policy and overview cells remain unchanged.

**Four fresh CLI checks pass**, covering both mailers on the previous and adopted
overlays; workspace formatting and tracked whitespace also pass. The preceding
forward/reply audit's nineteen runtime cases and warning-free workspace Clippy
are retained as earlier evidence, without new runtime tests or a Clippy rerun for
this text-only correction. Three pinned Lima source reviews are reused and all
257 predecessor artifact/report sets remain intact. Execution is local lpc-rs
compilation only with distinct nil, preserved call-out players, 4,000 ms actions,
`USE_USER_MENU` and `.o` saves; no upstream-driver execution or fresh public-source
fetch. Lost-message file export is next.

#### Lima forwarding and reply audit

Reviewed **5 October 2026** against pinned [base mailer][lima-mailer-base],
[standard][lima-mailer-standard], [Mh][lima-mailer-mh],
[text editor][lima-text-editor] and [input forwarding][lima-input-forwarding].
Actual normal input forwards and replies to explicit or current message numbers,
including a selected second sorted key. Quotation preserves Unicode, blank and
last lines. Plain replies target the sender; reply-all unions the sender and
original To/Cc recipients, including self, with one delivery per account.
Original bytes/unread flags and numeric cursors remain unchanged; new own copies
can occupy a different position, and replies/forwards use fresh thread IDs.

Real reply editing/Cc callbacks preserve prior drafts on abort, save accepted
text, remove successful drafts and return to the mailer/shell. Blank Cc stores
one empty string that routing drops, consistent with native
[`explode`](efun/explode.md). Nil original recipient lists remain usable;
malformed lists block reply-all while plain reply/forward retain their routing.
Missing messages prune one owner; corrupt/invalid metadata retains evidence.
Invalid arguments, nonpublic remote entry and uncaught outer forward/reply-send
errors preserve the appropriate state; retries remain usable.

Both mailers' help advertises space-separated recipients, while the base mailer
splits comma-separated strings. A real spaced forward fails with delivery-error
feedback because USER_D rejects the combined canonical name; a comma list works.
Help also obscures optional numbers and omits the forwarding destination in its
main list. **Recommendation:** correct both help/usage texts and their group
examples, preserving the existing parser and mail policies. No native change is
needed; lost-message file export remains the next runtime boundary.

**Nineteen fresh tests/observations pass**, with real Users, bodies, connections,
daemon sends, native saves, owned reads and editor/Cc input. Seven core CLI checks
and four quality gates pass, including warning-free workspace Clippy. Sixteen
pinned Lima and seven native reviews are recorded; all 256 predecessor
artifact/report sets remain intact. The active setter overlay/configuration,
original/native/master inputs and overview cells stay unchanged. Execution is
local lpc-rs only with distinct nil, preserved call-out players, 4,000 ms actions,
`USE_USER_MENU` and `.o` saves. Historical harnesses and clocks are not
rerun/compared. Group expansion is source-reviewed here; remote routing, delivery
faults, long paging, concurrency, disconnect recovery and startup stay separate.

#### Lima mailbox setter ownership repair

The **5 October 2026** local port adds owning User context checks to both public
[mailbox][lima-entry-mailbox] setters. Foreign ordinary/privileged Users and
absent User context are rejected before state changes, including already-read
or absent-key no-ops. Denied calls preserve receipts, cursors, saved bytes and
message records. Owners retain public access through the mailer and User;
MAIL_D delivery/recovery need no exception. No privilege bypass, cursor clamping,
saved-data migration or native/master change is introduced.

The private [User body setup][lima-body-switching] routine is executed on a real
User through an invocation suffix in its module. It keeps the first unread
selection and alert, the last position when all mail is read, and -1 for an
empty mailbox. Flags and saves stay unchanged. This covers the internal routine
under owner context, without establishing complete login or body switching.
Real [standard][lima-mailer-standard]/[Mh][lima-mailer-mh] input retains unread
selection/persistence, navigation/header/export policies, missing-record pruning,
damaged-record retention and uncaught-error rollback.

**Twenty-two fresh tests/observations pass**, including two fresh controls that
reproduce the previous overlay's foreign flag/cursor writes. Fourteen core CLI
checks remain 7/7 → 7/7; four quality gates pass, including warning-free workspace
Clippy. Eleven pinned Lima and five native reviews are reused; original sources,
native/master inputs and all 255 predecessor artifact/report sets are preserved.
The exact patch changes only the mailbox. Execution is local lpc-rs only with
distinct nil, preserved call-out players and 4,000 ms actions; `USE_USER_MENU` is
enabled and saves use `.o`. Overview cells stay unchanged. Historical harnesses
and clocks are not rerun/compared. Audit forward/reply composition next; long
paging/ranges, lost exports, concurrent maintenance, remote queues, full account
deletion and startup remain separate.

#### Lima unread mail audit

Reviewed **5 October 2026** against pinned [mailbox][lima-entry-mailbox],
[base mailer][lima-mailer-base], [standard][lima-mailer-standard],
[Mh][lima-mailer-mh] and [User body setup][lima-body-switching]. Both mailers' `n`
commands choose the first unread sorted key, regardless of the cursor. Healthy
displayed reads select that key, clear only its owner's unread flag and save it
through reload. Headers, navigation and raw owned reads preserve flags; wizard
Mh export also preserves flags/selection. New delivery becomes unread at its
actual sorted position. Standard blank input reads unread mail, then exits when
exhausted; explicit `n` reports exhaustion and keeps the modal mailer open.

Missing unread records prune one owner's receipt, allowing the next command to
read the next survivor. Corrupt records or invalid display metadata retain their
unread flags and bytes, so repeated `n` reports the same first damaged record.
Explicit healthy numeric reads still work. Uncaught outer errors after actual
standard/Mh input roll back flags, cursor, saves and delivered output.

Both public mailbox setters lack the ownership check used by reads/deletion.
Calls under an ordinary foreign User context can clear another account's unread
flag persistently or change its live `nosave` cursor. A privileged foreign User
is denied an owned
read but succeeds through both setters. **Recommendation:** check owning User
context before `set_message_read` and `set_message_index`, while preserving
their owner mailer/User callers and current no-op, unread/error/export policies.
No MAIL_D-only restriction, privilege bypass or native API change is needed.
User body setup also calls the cursor setter; source review shows that its
empty-mailbox position is -1, so cursor clamping would be a separate change.

**Seventeen fresh tests/observations pass**, using real Users/bodies, connections,
sends, native saves, owned mailboxes and both mailers' normal input. Seven core
CLI checks and four quality gates pass, including warning-free workspace Clippy.
Eleven pinned Lima files and five native sources/references are reviewed; all
254 predecessor artifact/report sets are preserved. Execution is local lpc-rs
only, with distinct nil, preserved call-out players and 4,000 ms actions;
`USE_USER_MENU` is enabled and saves use `.o`. The active cursor overlay,
original/native/master inputs and overview cells remain unchanged. Historical
harnesses/clocks are not rerun/compared; no repair is adopted by this audit.
Body setup is source-reviewed rather than executed. Full login/body switching,
long paging, lost-message exports, forward/reply, concurrent maintenance, remote
queues, full account deletion and startup remain separate.

#### Lima mailbox cursor repair

The **5 October 2026** local port changes only [the mailbox][lima-entry-mailbox]
and [base mailer][lima-mailer-base] from the recovery overlay. Owned missing-message
reads share ordinary deletion's receipt removal, cursor adjustment/clamping and
save bookkeeping. Removing an earlier key preserves the selected message;
removing the selected key chooses its successor or the remaining last message.
An empty mailbox keeps cursor zero, and normal reload still resets the `nosave`
cursor to zero. Ordinary daemon deletion must succeed before receipt removal.

Displayed reads locate the requested key after building their output. A lost key
keeps the corrected selection and existing lost-message feedback; healthy reads
select and mark the actual key read. Both [standard][lima-mailer-standard] and
[Mh][lima-mailer-mh] can then read the surviving messages normally. Header ranges
still select against their initial snapshot, while retained rows use positions
adjusted for earlier removals. Lost rows are unnumbered. Multiple missing records
and a later corrupt record retain correct selectable numbers. Foreign/corrupt
reads retain evidence, peer ownership stays isolated, and uncaught failures
discard receipt/cursor/save changes. Recovery insertion policy is unchanged.
The stock standard mailer's empty prompt remains `[1 of 0]`; empty headers
report no mail.

**Twenty-one fresh tests/observations pass:** nineteen adopted cases and two
fresh previous-overlay controls for the old out-of-bounds default read and stale
header numbers. Actual Users/bodies, connections, sends, native saves, owned
mailboxes and real mailer input cover selection, headers/ranges, multiple/empty
pruning, valid reads, ordinary deletion, reload, guards and rollback. Fourteen
core CLI checks remain **7/7 → 7/7**; four quality gates pass, including
warning-free workspace Clippy. Exact two-file/configuration checks preserve
original/native/master inputs and all 253 predecessor artifact/report sets.
Ten pinned Lima reviews and five native reviews are reused from the audit; the
changed files are reviewed on **5 October 2026**. Execution is local lpc-rs only,
with distinct nil, preserved call-out players and 4,000 ms actions;
`USE_USER_MENU` is enabled and saves use `.o`. Historical harnesses/clocks are not
rerun/compared. Native APIs and overview cells are unchanged. The unread audit
above recommends ownership checks on the two setters; long paging,
exports, forward/reply, concurrent maintenance, remote queues, full account
deletion and startup remain separate.

#### Lima mailbox cursor audit

Reviewed **5 October 2026** against pinned [mailbox][lima-entry-mailbox],
[base mailer][lima-mailer-base], [standard mailer][lima-mailer-standard] and
[Mh mailer][lima-mailer-mh]. An owned read prunes/saves a physically missing
message's receipt without adjusting the cursor. Removing an earlier sorted key
changes the selected message; removing the selected last key leaves an index
outside the remaining mailbox. Reload persists receipt removal but resets the
`nosave` cursor to zero. Corrupt records and foreign reads preserve state;
ordinary deletion already adjusts/clamps the cursor correctly.

The base mailer displays a lost-message placeholder and unconditionally assigns
the old requested position afterward, so a mailbox-only correction would be
overwritten. Both mailers can then report out-of-bounds reads; standard mail can
show `[3 of 2] mail:`. Headers retain numbers computed before pruning: losing
the first of three messages leaves healthy rows labelled 2 and 3 in a two-message
mailbox, making the displayed 3 unselectable. Selected ranges have the same
problem; missing records outside a selected range remain untouched.

**Recommendation:** share receipt-only removal, cursor adjustment/clamping and
save bookkeeping between successful ordinary deletion and owned missing reads.
In the base mailer, avoid overwriting selection/read flags for a pruned read key,
number retained headers at their current positions, and leave lost rows
unnumbered. Preserve initial range selection and lost-message feedback. No native
API, global reconciliation or recovery-insertion policy change is needed.

**Fifteen fresh tests/observations pass**, with actual Users/bodies, connections,
sends, native saves, owned mailboxes and both mailers' real input paths. Valid
read recovery, peer isolation, ordinary deletion, reload, corrupt/foreign reads
and uncaught rollback are covered. Seven core CLI checks and four quality gates
pass, including warning-free workspace Clippy. Ten pinned Lima files and five
native sources/references are reviewed; all 252 predecessor artifact/report
sets are preserved. Execution is local lpc-rs only, with distinct nil, preserved
call-out players and 4,000 ms actions. Historical harnesses/clocks are not
rerun/compared. The recovery overlay, original/native/master inputs and overview
cells remain unchanged by this audit; the cursor repair above adopts its
recommendation. Longer pager/range interaction, exports, unread selection,
forward/reply, concurrent
maintenance, remote queues, full account deletion and startup remain separate.

#### Lima mailbox recovery repair

The **5 October 2026** local port changes only [MAIL_D][lima-mail-daemon] and
[the mailbox][lima-entry-mailbox]. Rebuild calls a separate `recover_message`
method after its existing filename and complete pending-owner/account checks.
Recovery requires the actual MAIL_D caller, preserves any known receipt's flags
and the live cursor, and inserts/saves an absent receipt unread. It sends no BIFF
alerts, including for newly reconstructed mailboxes. Repeated/duplicate recovery
leaves known mail alone. Ordinary new-mail delivery keeps its guard, unread save
and notifications; see [BIFF settings][lima-mail-biff].

Missing receipts in an older mailbox backup are recovered without resetting its
known read flags or the peer's state, and persist through reload. The cursor
still starts at zero on normal reload because it is `nosave`. Stale-receipt policy
is unchanged: missing message reads prune only that owner's receipt, while
present corrupt records or ownership disagreements error and retain evidence.
Empty/omitted pending owners and obsolete backups are not globally reconciled.
A caught later-file error can retain earlier new receipts/saves; uncaught errors
discard them, with no recovery alerts or known-flag resets in either case.

**Nineteen fresh tests/observations pass:** seventeen adopted cases and two fresh
previous-overlay controls for read reset and repeated BIFF. Actual Users/bodies,
connections, sends and native saves cover live flags/cursor, repeated/duplicate
recovery, lost/partial boxes, caller checks, pending validation, reload, ordinary
delivery, stale receipts and caught/uncaught boundaries. Fourteen core CLI checks
remain **7/7 → 7/7**; four quality gates pass, including warning-free workspace
Clippy. Exact two-file/configuration checks preserve original/native/master
inputs and 251 predecessor artifact/report sets. Fourteen pinned Lima sources
and five native sources/references are reviewed on **5 October 2026**. Execution
is local lpc-rs only, with distinct nil, preserved call-out players and 4,000 ms
actions; historical harnesses/clocks are not rerun/compared. Native APIs and
overview cells are unchanged. The cursor audit above recommends a coordinated
mailbox/mailer repair; concurrent maintenance, bulk reconciliation, remote queues,
full account deletion, other commit-time I/O failures and startup remain separate.

#### Lima mailbox recovery audit

Reviewed **5 October 2026** against pinned [MAIL_D][lima-mail-daemon],
[mailbox][lima-entry-mailbox], [mailer][lima-mailer-base] and
[BIFF settings][lima-mail-biff]. Rebuild uses the ordinary new-message delivery
method, overwriting every existing receipt with unread and announcing new mail
to online recipients with BIFF enabled. Repeating rebuild repeats the alerts;
duplicate pending owners produce multiple alerts for one receipt. A missing
mailbox is recovered unread, while surviving mailboxes also lose their read
flags. Message saves and bodies stay unchanged.

Rebuild does not remove stale receipts. Empty pending lists, omitted owners and
restored old mailbox backups retain them. A physically missing message is pruned
only when each owner reads it. Present corrupt saves or pending-owner mismatches
instead report an error and retain the receipt and bytes. A caught later-file
failure retains earlier flag resets, saves and alerts; an uncaught failure
discards them. See [catch](efun/catch.md) and [save delivery](efun/save_object.md).

**Recommendation:** add a separate MAIL_D-only mailbox recovery method that
preserves known flags and the live cursor, adds absent receipts unread, and sends
no BIFF alerts. Keep ordinary new-mail delivery and existing stale-receipt
handling. Bulk reconciliation would need a complete trustworthy view of message
ownership and saved/live mailboxes; it is unnecessary for this repair.

**Thirteen fresh tests/observations pass**, using two real Users/bodies,
connections, actual sends, native saves and owned mailboxes. Seven core CLI
checks and four quality gates pass, including warning-free workspace Clippy.
Fourteen pinned Lima sources and five native sources/references are reviewed;
250 predecessor artifact/report sets are preserved without rerunning historical
harnesses or comparing historical clocks/values. Execution is local lpc-rs only,
with distinct nil, preserved call-out players and 4,000 ms actions. The active
maintenance overlay, native APIs, master policy and overview cells are unchanged
by this audit; the recovery port above adopts its recommendation. Full account
deletion, stale cursor adjustment, concurrent maintenance, remote queues, other commit-time I/O failures
and startup remain unestablished.

#### Lima mailbox maintenance guards

The **5 October 2026** local port changes only [MAIL_D][lima-mail-daemon] and
[the mailbox][lima-entry-mailbox]. Rebuild checks previous privilege 1 before
scanning. Each basename must match a nonnegative integer key and the configured
save extension exactly; aliases, signs and overflow are rejected before restore.
The entire pending list must pass shape and local-account/canonical-name checks
before this record delivers any receipt. Missing/invalid records report errors;
files and prior receipts remain available for repair. No owner inference,
normalization or saved-data rewrite is added. Receipt delivery requires the
actual MAIL_D caller before flags, saves or notifications change.

A private helper shares ordinary deletion's ownership/receipt removal, cursor
adjustment and save with nuke. Nuke persists each completed receipt deletion
before the next key, then stops and propagates the first damaged-record error,
keeping remaining mail. A caller catching that error commits consistent completed
shared/last-owner deletions, including after reload. Uncaught errors retain normal
transaction abort semantics. Successful/empty nuke resets the cursor and unlinks
its save. Ordinary owner checks, privileged foreign-owner access and the stock
extra argument keep their behavior. The helper is hidden from external calls.

**Twenty-five new tests/observations pass:** twenty-two adopted cases and three
fresh previous-overlay controls, using two real stock Users/bodies, actual sends,
native class saves and owned mailboxes. Tests cover guard precedence, receipt
injection/reset denial, helper invisibility, healthy/partial/empty nuke, cursor
behavior, shared/final-owner deletion, exact retained damaged bytes, reload,
whole-list validation, bad account/save names, overflow, authorized rebuild,
peer reconstruction and caught/uncaught record boundaries. The controls reproduce
previous unguarded rebuild, foreign receipt insertion and stale nuke receipts.
Actual map/directory order selects failures without historical clock comparisons;
no historical harness or observation values are rerun/compared.

Fourteen CLI checks remain **7/7 → 7/7**; four quality gates pass, including
warning-free workspace Clippy. Exact two-file patch/configuration checks preserve
native/master/original inputs and 249 predecessor artifact/report sets. Fourteen
pinned Lima sources below and seven native sources/references are reviewed on
**5 October 2026**. Execution is local lpc-rs only, using distinct nil, preserved
call-out players and 4,000 ms actions. No native API or overview feature changes.
Rebuild keeps its unread/reset and empty/duplicate/stale-receipt policies; a caught
later record failure can retain earlier valid record updates. The recovery audit
above reviews these policies and recommends a separate recovery method.
Account-deletion caller blockers remain the preceding audit's evidence; full account deletion, other
commit-time I/O failures, concurrent maintenance, remote queues and startup
remain unestablished.

#### Lima mailbox maintenance audit

Reviewed **5 October 2026** against the pinned [MAIL_D][lima-mail-daemon],
[mailbox][lima-entry-mailbox], [privilege wrapper][lima-access] and
[security daemon][lima-security], using the current distinct-nil port.
Nuke denies ordinary owners and foreign callers; privileged deletion preserves
shared peers and removes the saved message at the last owner. The extra argument
in stock nuke callers is accepted by the dynamic call, so it is not a blocker.
Full account deletion remains unestablished.

Rebuild has no privilege gate: an ordinary User can rebuild other users' boxes
and reset read flags. Receipt delivery's caller check is commented out, allowing
foreign insertion of a receipt for a nonexistent message. Rebuild bypasses the
pending-owner validator: numeric owners can fail after earlier recipients have
changed, nonaccount owners acquire saved boxes, and an empty owner returns
success despite a NUL-path save failure logged at delivery. Its unsaved receipt
disappears on reload. Empty pending lists leave stale receipts; duplicates
replace the same flag. Nonnumeric saved basenames are interpreted as key zero
rather than consuming the listed file.

A caught nuke failure can commit earlier pending-owner deletions while retaining
all mailbox receipts, causing later reads of those healthy records to fail their
owner check. Caught rebuild failures can likewise retain earlier file/recipient
updates. Uncaught errors discard those transactional changes; `catch` itself
provides no rollback. See [catch](efun/catch.md) and
[save delivery](efun/save_object.md).

**Recommendation:** guard rebuild with previous privilege 1, require MAIL_D for
receipt delivery, and validate canonical save basenames and the complete pending
list/local account names before per-record delivery. During nuke, remove and
persist each successful receipt deletion before processing the next key; stop
and propagate the first damaged-record error while retaining remaining mail.
Keep privileged foreign-owner deletion, shared ownership and ordinary abort
semantics. No owner inference, save rewriting or native API change is needed.
At audit completion this recommendation was not implemented; the adoption above
now applies it. Rebuild's unread/reset policy and stale-receipt reconciliation
remain separate decisions.

Twenty fresh local cases cover these paths with two stock Users/bodies, daemon
sends, native class saves and real mailboxes. Actual map/directory order selects
partial failures without historical clock assumptions. The seven core CLI mail
targets remain clean; suicide retains four inherited CMD `dispatch_main` errors,
and admtool's user module retains two base/menu type errors. Neither complete
caller workflow was executed. Four quality gates pass, including warning-free
workspace Clippy; integrity checks preserve 248 predecessor artifact/report sets.
Fourteen pinned Lima sources and five native sources/references are reviewed.
Execution is local lpc-rs only, with distinct nil, preserved call-out players and
4,000 ms actions. Other commit-time I/O failures, concurrent maintenance, remote
queues and full startup remain unestablished. No overview feature, native/master
input or active-library change is made.

#### Lima saved-mail retrieval error handling

The 5 October 2026 local port changes [MAIL_D][lima-mail-daemon] and the
[base mailer][lima-mailer-base]. The daemon clears its temporary message before
restore and after every outcome, propagating restore/nominal consumption errors
after cleanup. A successful restore without a message now raises an invalid-save
error, retaining the present file and both receipts for repair. Native
missing/unreadable-file returns and permission errors keep their semantics;
caller and owner checks still precede restore. No owners are inferred, fields
rewritten or mailboxes scanned. The mixed class/API and native restore policy
remain unchanged.

Reads, body-only exports, replies and forwards catch retrieval failures before
changing state. A shared private reporter prints the selected number and reason
once, including default current-message selections. Failed actions leave unread
flags, cursor, tools, composition text and export files intact. Headers catch
each retrieval separately, keep the original number and unread marker in its
error row, then continue healthy selected headers. Filtered ranges retain
original numbers. Existing metadata/body guards, physical missing-file rows,
pending-owner guards and deletion error reporting retain their behavior. Later
filesystem/commit failures do not gain a rollback guarantee.

**Twenty-two newly written tests/observations pass:** twenty adopted cases and
two fresh previous-overlay controls, all in distinct nil mode. Two real stock
users/bodies, daemon sends, native class saves, owned mailboxes and standard/Mh
dispatch cover corrupt syntax/field/global/nominal types, pending shape/member
errors, nil/empty/message-less files, slot cleanup, healthy header continuation,
selected/default numbering, failed actions, repair/read/reload, physical missing
files, deletion, caller precedence and preceding field guards. Exact damaged
save bytes and both receipts survive; repairing the record restores reading
without redelivery, with the peer still unread and its body preserved through
reload. The controls reproduce previous silent errors/invalid slot retention and
present-nil pruning. No prepared provider or daemon failure hook is installed.
Historical harnesses and observation values are not rerun or compared; original
keys/current displayed positions avoid clock ordering assumptions.

Fourteen CLI checks stay **7/7 → 7/7**; four quality gates pass, including
warning-free workspace Clippy. Exact two-file patch/configuration checks preserve
original/native/master inputs and 247 predecessor artifact/report sets. Eight
pinned Lima sources below and three native source/reference reviews are dated
**5 October 2026**. Execution is local lpc-rs only, using distinct nil, preserved
call-out players and 4,000 ms actions. No native API, master policy or overview
feature changes. Next audit mailbox nuke/rebuild authorization and partial
failures with damaged saves. Arbitrary unreadable-file I/O, legacy import, stale
cursors, remote queues and commit-time failures remain separate work.

#### Lima saved-mail retrieval error audit

The 5 October 2026 local audit of [MAIL_D][lima-mail-daemon],
[mailbox][lima-entry-mailbox] and the [base mailer][lima-mailer-base] finds that
message retrieval exceptions still escape body reads, headers, replies and
forwarding. Real standard/Mh dispatch suppresses them, emitting only the prompt;
bound-handler catches expose the failures. Truncated native syntax, invalid
class field values, nonclass/foreign-class globals and pending-list/member
errors preserve exact damaged save bytes, both receipts/unread/cursor state
and composition text, but give no useful read/header feedback. A bad middle
header suppresses all accumulated healthy rows; selecting healthy ranges works
with original numbering and unread markers. Pending failures also block reply,
forwarding and body-only export silently, leaving tools/output files intact.

Present nil-message, empty and message-less saves restore successfully but yield
no message. MAIL_D returns nil, and each requesting mailbox prunes its receipt
as if the file were missing; the exact damaged file remains. These cases start
with a clean temporary daemon slot. Nonclass and foreign-class globals leave
that slot invalid after a nominal consumption error; the daemon observer then
encounters the same error. Native [restore_object](efun/restore_object.md)
intentionally retains absent globals and does not check global assignment types;
serialized class fields validate their declared types, and scalar class globals
enforce nominal identity when consumed. The fix belongs in the daemon's temporary
slot lifecycle and the mailer's error handling.

**Eighteen newly written tests/observations pass** against the unchanged active
overlay. Two real stock users/bodies receive daemon sends into native class saves
and owned mailboxes. Only temporary saved files change; real standard/Mh commands,
bound handlers, header paging, exports, reads, reload and deletion are exercised.
Repairing a native sender field restores normal reading and changes only the
caller's unread marker, with the peer's body preserved through reload. Physical
missing-file header rows still retain healthy rows and prune only the caller's
receipt. Foreign/direct callers are rejected before malformed parsing. The
preceding deletion repair already reports native restore failures once without
removing the save or receipts. No corruption frequency is established.

Recommend clearing the daemon's temporary slot before and after caught restore,
propagating errors and rejecting a successful restore with no message. Preserve
invalid present saves/receipts for repair; retain native missing/unreadable-file
semantics. Catch retrieval errors before actions change state and report the
selected number/reason once. Catch each header retrieval independently, add a
numbered error row with its unread marker, and continue other selected headers.
Infer no owners, rewrite no fields and retain caller checks before restore.
At audit completion this recommendation was not implemented; the adoption above
now applies it without a native API change.

Seven CLI targets and four quality gates pass, including warning-free workspace
Clippy. Integrity checks preserve original/native/master inputs and 246
predecessor artifact/report sets. Eight pinned Lima source reviews below and
three native source/reference reviews are dated **5 October 2026**. Execution is
local lpc-rs only, using distinct nil, preserved call-out players and 4,000 ms
actions. Historical harnesses and observation values are not rerun or compared;
original keys/current displayed positions avoid timestamp assumptions. No native
overview changes. Arbitrary unreadable-file I/O, legacy save import, remote queues,
mailbox nuke/rebuild, stale cursors and commit-time failures remain separate work.

#### Lima pending-owner mail deletion guards

The 5 October 2026 local port changes [MAIL_D][lima-mail-daemon] and the
[base mailer][lima-mailer-base]. Saved local pending owners must be an array
of nonempty strings before owned reads/deletes use the list. Reading retains
its owner-membership rule; deletion now requires membership before subtracting
names, saving or removing the message. Caller checks still precede restore and
metadata inspection. Empty lists cannot remove shared saves, and absent owners
retain stale receipts until repair. Duplicates remain valid and are removed
together. No account-existence check, recipient inference, normalization or
repair machinery is added; the mixed class field and remote numeric form remain.

Each selected mailer deletion catches its own failure, reports the original
selected message number and reason once, then continues other selections.
Successful deletions retain their ordinary confirmation. Invalid pending fields
leave exact save bytes and both owners' receipts/unread/cursor state intact;
the existing descending loop and snapshot of keys still determine selections.
Later filesystem/commit failures do not gain a rollback guarantee.

**Nineteen newly written tests/observations pass:** seventeen adopted cases and
two fresh previous-overlay controls, all in distinct nil mode. Two real stock
users/bodies, daemon sends, native saved classes, owned reads/deletes and
standard/Mh dispatch cover invalid shapes/elements, membership, preservation,
field repair followed by shared deletion, duplicate handling, healthy remaining
owner reload/last deletion, caller precedence, batch continuation and missing
files. Both interfaces report the damaged first selection once and still delete
the healthy selection. Composition text remains intact. Missing-save delete
failure is now visible; ordinary header retrieval still prunes only the caller's
missing receipt. The controls reproduce previous shared-save loss and silent
batch abortion. No prepared provider or daemon failure hook is installed.
Historical harnesses and observation values are not rerun or compared; selection
uses original keys/current displayed positions without timestamp assumptions.

Fourteen CLI checks stay **7/7 → 7/7**; four quality gates pass, including
warning-free workspace Clippy. Exact two-file patch/configuration checks preserve
original/native/master inputs and 245 predecessor artifact/report sets. Pinned
source review below is dated **5 October 2026**; execution is local lpc-rs only,
with distinct nil, preserved call-out players and 4,000 ms actions. No native API,
master policy or overview feature changes. The retrieval audit above establishes
malformed-save and pending-list read/header failures. Mailbox nuke/rebuild,
stale cursors, remote queues and commit-time failures remain separate work.

#### Lima pending-owner mail deletion audit

The 5 October 2026 local audit of [MAIL_D][lima-mail-daemon] and
[mailbox][lima-entry-mailbox] finds no validation of the saved pending-owner list.
The [mail class][lima-mail-message-class] declares this field mixed; its comment
says IMUD mail uses a number while local mail uses an array of names. The audit
covers local native saves and owned receipts, without remote conversion or
legacy-format import. Reading requires owner membership; deletion verifies the
mailbox caller but subtracts the owner without checking membership or list shape.

Nil, zero, integer, string and mapping pending values throw on owned read/delete,
leaving files and both receipts intact. Actual standard/Mh dispatch suppresses
the deletion error without useful feedback. Arrays containing real owners plus
an integer, nil or empty name allow reading and deletion, then retain an orphaned
save after both real owners delete. An empty list lets one recipient delete the
shared save while another still has a receipt; the other receipt is pruned on
its next missing-file read. An absent owner is denied reading but can delete
its stale receipt, leaving a listed peer's save unchanged. Duplicate sole-owner
entries are removed together. No corruption frequency is established.

**Sixteen newly written tests/observations pass** against the unchanged active
library. Two real stock users/bodies and native accounts receive daemon mail;
the temporary fixture changes only saved pending values. Real owned reads/deletes,
standard modal commands, Mh shortcuts and bound-handler observation catches
establish data loss, orphaning and silent errors. Healthy sharing retains the
remaining owner's membership/body through mailbox/daemon reload and removes the
save at the last delete. Foreign-mailbox/direct-daemon deletion remains denied;
a malformed first selected item blocks healthy batch deletion, and missing-save
delete failure retains receipts until ordinary retrieval prunes them. Composition
text stays intact. No prepared provider or failure hook is installed. Record
selection uses original keys/current displayed positions, without clock-order
assumptions; historical harnesses and observation values are not rerun or compared.

Recommend array/nonempty-string checks for local pending owners in MAIL_D and
owner membership before deletion mutates a record. Keep damaged saves/receipts
for repair, including the absent-owner case; retain duplicate handling. Catch
and report each selected deletion failure in the [base mailer][lima-mailer-base],
then continue other requested messages. Infer no owners, rewrite no damaged
fields, impose no account-existence rule and retain the mixed provider field.
At audit completion this recommendation was not implemented; the adoption above
now applies it without a native API change.

Seven CLI targets and four quality gates pass, including warning-free workspace
Clippy. Integrity checks preserve original/native/master inputs and 244
predecessor artifact/report sets. Pinned source review below is dated
**5 October 2026**; local execution uses distinct nil, preserved call-out players
and 4,000 ms actions, without upstream-driver or remote execution. No native
overview feature changes. Read/header error presentation, malformed native-save
feedback, mailbox nuke/rebuild, stale cursors and commit-time disk failures remain
separate work.

#### Lima saved message metadata guards

The 5 October 2026 local port changes only the [base mailer][lima-mailer-base].
Before headered reads, replies or forwards change state, it requires string
sender/subject values and an integer date that [ctime](efun/ctime.md) can represent.
Headered reads and reply-all also require each consumed To/Cc field to be nil or
an array of strings. Invalid metadata is reported before marking read, opening
tools, replacing a draft or sending. Header enumeration displays a numbered
invalid-metadata row with its unread marker and continues to healthy records.
It checks only the fields used in the summary. Basic replies and forwarding
ignore unused recipient fields; body-only export bypasses metadata checks.

Reply-all treats whole nil To/Cc lists as empty only while assembling recipients.
Saved [mail class][lima-mail-message-class] fields stay unchanged. Integer zero
lists and non-string elements are rejected under distinct nil. Empty subjects,
empty arrays and representable zero/negative dates retain their behavior. Existing
body guards remain first. Native malformed-class-value rejection still preserves
files and receipts for repair, and missing files still prune their own receipts.
The new header row handles invalid metadata in records that restore successfully;
feedback for malformed native saves remains a separate boundary.

**Twenty-three new tests/observations pass: twenty-one repaired cases and two
fresh previous-overlay controls.** Real standard modal commands, Mh shell
shortcuts, editor/Cc continuations, [MAIL_D][lima-mail-daemon], native saves and
owned mailbox reads cover reported errors, healthy headers, intact saves/drafts,
normal nil-list reply-all delivery, body-only recovery, empty/date values, class
rejection with repair, body guards and missing-file pruning. The two newly written
controls retain prior header suppression and nil-list reply-all failure. No
prepared message provider or daemon failure hook is installed. Current status
checks account for terminal wrapping; bodies, drafts and saved files remain exact.
Historical harnesses and observation values are not rerun or compared, and receipt
selection assumes no clock ordering.

Fourteen CLI checks remain **7/7 → 7/7**; four quality gates, including warning-free
workspace Clippy, and exact patch/integrity checks preserve 243 predecessor
artifact/report sets. Original Lima, native APIs and master policy remain unchanged.
Source review uses the pinned links below, reviewed **5 October 2026**. Local
execution uses distinct nil, preserved call-out players and 4,000 ms actions,
without upstream-driver or remote execution. No native overview feature changes.

The pending-owner adoption above applies the deletion guards. Native malformed
save feedback, stale cursors, remote queues, persistent news recovery, commit-time
disk errors, disconnects, concurrent composition and full startup remain separate
work. No recipient-routing or account-validation policy is added.

#### Lima saved message metadata UI audit

The 5 October 2026 local audit of the [mail class][lima-mail-message-class] and
[base mailer][lima-mailer-base] finds that the saved-body guard does not validate
header or recipient metadata. Native class restoration rejects integer
sender/subject values and nil dates, preserving the damaged file and mailbox
receipt; repairing those files restores reading. It accepts nil strings,
unrepresentable integer dates and arbitrary mixed To/Cc shapes. Nil sender or
subject and an out-of-range date break headered reads, replies and forwarding.
A bad record also aborts the entire header batch, hiding healthy siblings.
Actual [standard][lima-mailer-standard]/[Mh][lima-mailer-mh] dispatch suppresses
these exceptions without a useful result.

Optional nil To/Cc lists display normally but fail array concatenation in
reply-all. An integer To value is silently omitted from read headers, while a
scalar string or mapping recipient list fails formatting. An array containing
an integer opens the reply-all editor, then [MAIL_D][lima-mail-daemon] silently
filters that element by sizeof and delivers to the valid string recipient.
The successful send removes the accepted draft; the source save stays intact.
Basic replies and forwarding ignore unused recipient fields. Body-only export
recovers exact Unicode and blank-line text despite unusable metadata, preserving
the save, unread state and existing composition. Empty subjects/recipient arrays
and representable zero/negative dates remain usable.

**Sixteen newly written tests/observations pass** against the unchanged active
library. Real daemon sends create native saves and owned receipts; the temporary
fixture changes saved field values after opening the actual UI. Standard modal
commands, Mh shell shortcuts, bound-handler observation catches, actual editing
and owned mailbox reads establish errors, repair, filtered delivery and body-only
recovery. No prepared message provider or daemon failure hook is installed.
Seven CLI targets and four quality gates pass, including warning-free workspace
Clippy. Integrity checks preserve original/native/master inputs and 242
predecessor artifact/report sets. Historical harnesses and observation values
are not rerun or compared, and receipt selection assumes no clock ordering.

Recommend checks where each action consumes metadata: string sender/subject,
dates that [ctime](efun/ctime.md) can represent and optional arrays of string
To/Cc recipients.
Validate reply-all recipients before opening its editor, treating nil lists as
empty only when assembling recipients. Leave stored fields, basic reply/forward
unused fields and body-only export alone. Header enumeration should show an
invalid-metadata row and continue to healthy messages. Preserve existing
malformed-save rejection and missing-file pruning. This recommendation is
**not implemented** at audit completion; the adoption above implements it
without a native API or new recipient-routing policy.

Pinned source review below is dated **5 October 2026**; local execution uses
distinct nil, preserved call-out players and 4,000 ms actions, without upstream
driver or remote execution. No native overview feature changes. Pending-owner
metadata, stale cursors, malformed native-save feedback, remote queues and
persistent news recovery remain separate work.

#### Lima mail delivery error reporting and news reply recovery

The 5 October 2026 local port changes the [base mailer][lima-mailer-base] and
[newsreader][lima-news-reader]. Shared mailer error handling reports catchable
MAIL_D failures and returns failure to composition, forwarding and news replies.
Composition retains its saved draft; forwarding retains its original message.
Caller-checked news sending returns a result, and the private continuation
reopens failed replies in the existing [text editor][lima-text-editor], with
original edited lines and captured subject/recipient. Delivery uses a separately
wrapped copy. The UI says finishing attempts another send, and caught delivery
errors warn that recipients may already have mail. Reopening sends nothing;
abort discards unsaved news text, while empty completed arrays remain valid.
Successful sends return to the reader. A separate composition file stays intact.

**Eighteen new tests/observations pass: sixteen repaired cases and two fresh
previous-overlay controls.** Real standard modal commands, Mh shell shortcuts,
NEWS_D, editor, [MAIL_D][lima-mail-daemon], native class saves and owned mailbox
reads cover early/late errors, single reporting, retained forwarding originals,
exact plain/quoted news buffers, manual retry/abort, invalid destinations,
empty completion, Unicode/blank and long lines, captured metadata after article
removal, composition retention and caller/private-callback security. Thirteen
repaired cases and both controls activate temporary one-shot daemon hooks.
Late errors can leave delivered mail; explicit retries can duplicate it. No
production hook or automatic retry is installed. Current message assertions
account for terminal wrapping; body/buffer assertions remain exact. Historical
harnesses and observation values are not rerun or compared, and receipt selection
does not assume timestamp ordering.

Fourteen compile checks remain **7/7 → 7/7**; four quality gates, exact patch
application and integrity checks preserve 241 predecessor artifact/report sets.
Only the two mudlib files change; original Lima, native APIs and master policy
remain unchanged. Source review is pinned below, reviewed **5 October 2026**.
Local execution uses distinct nil, preserved call-out players and 4,000 ms
actions, without upstream-driver or remote execution. No native overview feature
changes. This adds live news text recovery rather than a persistent news draft
or delivery rollback.

The metadata guard adoption above applies those base-mailer checks.
Persistent news drafts/metadata, remote queues, commit-time disk failure,
disconnect recovery, concurrent composition and full startup remain separate
boundaries.

#### Lima forwarding and news reply delivery-error audit

The 5 October 2026 local audit finds two gaps in the [base mailer][lima-mailer-base]
and [newsreader][lima-news-reader]. Composition catches delivery errors, but
forwarding and news replies bypass that continuation. The [modal dispatcher][lima-inputsys]
catches exceptions without reporting them. Failed forwards therefore return to
their prompts without a delivery result; the original message and composition
draft remain. A failed news reply also loses its edited buffer after the
[text editor][lima-text-editor] cleans up. Invalid destinations are reported,
but undeliverable news reply text is still discarded.

**Fifteen newly written tests/observations pass** against the unchanged active
library. Nine activate temporary one-shot hooks in the real [MAIL_D][lima-mail-daemon]
before local receipt delivery or immediately before return. Actual standard
modal commands, Mh shell shortcuts and plain/quoted news editing demonstrate
silent errors, buffer loss and retained late delivery. Explicit forwarding retry
after a late error creates a duplicate. Six controls cover invalid destinations,
normal sends, abort and caller/private-callback guards. Existing composition
text remains unchanged. Receipt assertions select by subject/original key rather
than timestamp ordering; historical harnesses and observation values are not
rerun or compared.

Seven composed-object CLI targets compile; a standalone input fragment requires
its user host. Four quality gates and integrity checks preserve 240 predecessor
artifact/report sets. Only temporary fixtures contain failure hooks; the active
library/configuration, original Lima, native APIs and master policy remain
unchanged. Source review is pinned below, reviewed **5 October 2026**, with
local execution using distinct nil, preserved call-out players and 4,000 ms
actions. These injections test error handling, without establishing error
frequency or executing remote delivery. No native overview feature changes.

Recommend catching delivery errors in the shared mailer and returning send
status to the newsreader. Reopen failed news replies with their original edited
lines and captured subject/recipient, warning that another completion retries
the send and recipients may already have mail. Resending remains explicit;
forwarding needs no new draft because its original survives. The repair above
implements this two-file mudlib recommendation. Persistent news drafts,
disconnect recovery, delivery atomicity, remote queues, concurrency and full
startup remain separate boundaries.

#### Lima reply draft preservation

The 5 October 2026 local port changes the [base mailer][lima-mailer-base] to use
the existing [text editor][lima-text-editor] for replies. Quoted lines stay in
memory until editing finishes, so abort leaves an existing composition draft
unchanged and creates no persistent quote file when no draft existed. A private
completion saves the accepted reply before the existing Cc/send path. Catchable
save errors reopen the same buffer for retry. Nil means abort; an empty completed
buffer remains valid. Finishing the reply intentionally replaces previous saved
text, and the UI announces that boundary.

**Fifteen new tests/observations pass: thirteen adopted cases and two fresh
previous-overlay controls.** Real standard/Mh commands, modal dispatch, editor,
line editor, owned mailbox reads and native message saves cover aborts,
reply-all, absent/unreadable previous files, Unicode and blank lines, empty source
and edited bodies, save error/retry and abort, input-stack teardown, nested staging
cleanup and accepted-reply retention after failed delivery. Directory targets
trigger native preflight write errors; no send-failure hooks or prepared message
providers are used. The previous controls demonstrate overwrite and quote-file
creation on abort. Receipt checks do not assume timestamp ordering or compare
historical observation values.

Ten compile checks stay **5/5 → 5/5**; four quality gates and exact patch/integrity
checks retain 239 predecessor artifact/report sets. Only the base mailer changes;
original Lima, native APIs and master policy remain unchanged. Source review is
pinned below, reviewed **5 October 2026**. Local execution uses distinct nil,
preserved call-out players and 4,000 ms actions, without upstream execution or
historical harness reruns. This adds no native API or overview feature.

The delivery-error audit above covers forwarding and newsreader mail replies,
which bypass the composition Cc callback. Metadata storage, automatic retry,
remote queuing, commit-time disk failure, disconnect recovery, concurrent
composition and full startup remain separate boundaries.

#### Lima composition draft recovery adoption

The 4 October 2026 local port adopts the exact one-file retention/reporting patch
in the [base mailer][lima-mailer-base]. Unsuccessful composition sends keep their
saved body; successful sends remove it. Missing/unreadable drafts explicitly
report that nothing was sent, and empty readable drafts remain valid. Catchable
send errors report possible prior delivery and keep the draft. Reopening saved
text announces that the user must check the new recipient and subject.
Wizard `dead.letter` appends and explicit abort semantics remain unchanged.

**Fourteen tests and fourteen observations pass** through the adopted files.
Working-case assertions, common Rust helpers and copied LPC helpers match the
audit after removing the overlay selector and diagnostic logging argument.
Real standard/Mh commands, modal auto-pop, editor, native saves and owned mailbox
reads cover failed/successful sends, explicit resend, wizard fallback, mixed
recipients, missing/unreadable/empty files, abort and shared draft recovery.
Two cases retain failure hooks in the actual MAIL_D in the temporary test library:
a late exception can retain local delivery, and a manual retry duplicates it.
The active library has no hooks. The four historical controls stay frozen, and
observation labels remain without clock-dependent value comparison.

Ten compile checks stay **5/5 → 5/5**; four quality gates and exact patch/integrity
checks retain 238 predecessor artifact/report sets. Original Lima, native APIs
and master policy remain unchanged; historical harnesses are not rerun. Source
review remains pinned below, reviewed **4 October 2026**. Execution uses distinct
nil, preserved call-out players and 4,000 ms actions, without upstream execution.

Reply draft preservation above now protects previous saved text until the edit
is accepted. Metadata storage, automatic retries, remote queuing,
forwarding/news-reply errors, deletion failures, concurrent composition and full
startup remain separate boundaries.

#### Lima composition draft recovery audit

Reviewed **4 October 2026** against the pinned [base mailer][lima-mailer-base],
[text editor][lima-text-editor], [input dispatcher][lima-inputsys] and
[mail daemon][lima-mail-daemon]. After an edited message reaches Cc, the preceding
mailer deletes its draft even when delivery reports no valid destination.
Ordinary users lose the body; the wizard-only `dead.letter` fallback retains
body text without subject or recipient metadata. Catchable send exceptions leave
the draft but disappear inside modal dispatch. An exception after local saves
can leave a delivered message, so a manual retry can duplicate it.

The **one-file diagnostic patch, adopted above**, removes a composition draft
only after a successful send. Missing/unreadable drafts report that nothing was
sent; empty readable drafts remain valid. Send exceptions report possible prior
delivery and retain the draft. Reopening saved text announces that the user must
check the new recipient and subject. Wizard dead letters and explicit abort
semantics remain: abort discards new buffer edits and leaves an existing disk
draft unchanged. Reply quoting still replaces that disk draft before editing,
even when the reply is later aborted; that is a separate repair boundary.

**Eighteen tests/observations pass: fourteen diagnostic cases and four active
controls.** Commands use the real standard modal dispatcher or guarded Mh entry,
then real subject/editor/Cc dispatch, account-owned mailbox receipts and native
message saves. Tests cover failed/successful sends, wizard fallback, explicit
resend with new metadata, mixed recipients, missing/unreadable/empty files,
fresh/resumed abort, shared recovery across handlers and reply overwrite. Four
cases inject early/late failures into the actual local mail daemon; they establish
local save retention and duplicate manual retry, without executing remote mail.
No prepared message provider or clock-dependent transcript comparison is used.
Ten compile checks stay **5/5 → 5/5**; four quality gates and integrity checks
retain 237 predecessor artifact/report sets. Active/original/native/master
inputs remain unchanged; historical harnesses are not rerun.

The retention/reporting patch is adopted above. Next protect a retained
composition draft when a reply is aborted. Metadata persistence, automatic
retries, remote queuing, forwarding/news-reply errors, draft deletion failures,
concurrent composition and full startup remain unestablished. Execution uses
distinct nil, preserved call-out players and 4,000 ms actions, without upstream
execution. This library-only audit adds no native API or overview feature.

#### Lima saved message body guard adoption

The 4 October 2026 local port adopts the exact one-file guard in the
[base mailer][lima-mailer-base]. Reading, quoting and forwarding require an
array of string lines; empty arrays remain valid. Invalid bodies report an error
before changing unread flags, draft files, saved copies, editor/pager state or
delivery. Scalar/syntax restore errors remain visible, while missing files keep
the existing lost-message report and persisted stale-receipt removal.

**Fourteen tests and fourteen observations pass** through the adopted files.
Working-case assertions and shared helpers match the audit after removing the
fixture selector and diagnostic logging flag. Real native saves, standard modal
input, guarded Mh parsing and shell shortcut forwarding cover nil/non-string
bodies, parser rejection and file repair/retry, empty arrays, missing files,
draft preservation and Unicode delivery. Observation labels remain without
clock-dependent transcript comparison; the four failure controls stay frozen.
Ten compile checks remain **5/5 → 5/5**; four quality gates and integrity checks
retain 236 predecessor artifact/report sets. Original Lima, native APIs and
master policy remain unchanged; historical harnesses are not rerun.

The composition draft recovery audit above covers cancellation and delivery
reporting. Other malformed header/recipient fields, multi-message cursor handling
after stale-file pruning, remote delivery and full startup remain separate.
Source review remains pinned below, reviewed **4 October 2026**;
execution uses distinct nil, preserved call-out players and 4,000 ms actions.

#### Lima saved message body UI audit

Reviewed **4 October 2026** against the pinned
[base mailer][lima-mailer-base], [mail daemon][lima-mail-daemon] and
[mailbox][lima-entry-mailbox]. A present saved record can have a nil body;
reading then fails at concatenation and replying/forwarding fail in `map`.
Restored array fields check their outer type, so an invalid integer line can
also reach quotation and be forwarded as text.

The **one-file diagnostic guard, adopted above**, requires an array of strings
before reading, quoting or forwarding. Empty arrays are valid. Invalid bodies
produce an explicit error while retaining the unread receipt and any existing
draft; they open no pager/editor, create no saved copy and deliver no mail.
Native restore still rejects scalar bodies and corrupt value syntax. Those
files and receipts remain available for administrator repair; the audit verifies
retry after repairing the save. Missing files retain the existing lost-message
report and persisted stale-receipt removal.

**Eighteen tests/observations pass: fourteen diagnostic cases and four active
controls.** Real saved messages feed standard modal input and the caller-checked
Mh parser. The working cases cover read/reply/forward in both handlers, invalid
lines, parser rejection/retry, empty arrays, missing files and Unicode forwarding
through the actual shell shortcut and recipient-owned mailbox. Controls reproduce
the nil errors and integer-line forwarding. Delivery assertions do not depend on
timestamp ordering. Ten compile checks stay **5/5 → 5/5**; four quality gates and
integrity checks retain 235 predecessor artifact/report sets. Native APIs and
master policy remain unchanged. Execution uses distinct nil, preserved call-out
players and 4,000 ms actions, without upstream execution or historical reruns.

Next audit cancellation draft recovery and delivery failure reporting through
the adopted UI. Other malformed header/recipient fields, multi-message cursor
handling after stale-file pruning, remote delivery and full startup remain separate.

#### Lima body mailbase and contributed board replies

Reviewed **4 October 2026** against the pinned Lima sources below. Stock
[body composition][lima-body-mail-composition] does not inherit the
[body mailbase][lima-body-mailbase]; its declaration in the
[body command module][lima-body-mail-command]
has no caller there. Stock mail entry uses the user-owned handler instead.
Isolated execution of the unused module creates a separate cached handler;
changing its preference does not replace that handler until unload.

The [contributed board][lima-contrib-board] and
[replynote command][lima-contrib-replynote] still call
`this_body()->query_mailer()`. Their installation is incomplete in this checkout:
the [instructions][lima-contrib-board-installation] require an absent
`boarddata.c`, and the [portable-board example][lima-portable-board] references
an undefined `BOARD_OB`.
The contributions also need nil returns, current command dispatch and other
ordinary LPC adaptations before execution. The first reported compiler errors
are an inventory, not a complete migration checklist.

Isolated callback controls using the real editor show that their comma-style
functionals return the final expression without invoking the receiver. Use an
explicit bound pointer such as `&receive_reply(id)` for editor completion.
Changing to the user mailer alone is insufficient: the
[base mailer][lima-mailer-base] authorizes only NEWSREADER for `send_news_reply`.
An optional board port needs a deliberate sender permission policy, recipient
arrays and captured subject/destination data. Source review also finds message
re-fetching at completion and unconditional success output; these paths were not
executed through the unported contributions.

**Seven runtime tests/observations pass; six of twelve compile checks are
clean.** One failure is an abstract body command module whose concrete body
compiles; the other five concern contributions or the portable-board example.
The runtime checks cover stock ownership, the isolated unused module and copied
send/callback expressions, not complete board delivery. Four quality gates and
preservation checks retain 234 predecessor artifact/report sets. Execution uses
distinct nil, preserved call-out players and 4,000 ms actions, without upstream
driver execution. No stock patch or native feature is warranted by this audit.
Keep the legacy boards optional; the saved message body UI audit above addresses
their proposed follow-up.

#### Lima newsreader mail reply adoption

The 4 October 2026 local port adopts the exact one-file repair in the
[newsreader][lima-news-reader]. Plain and quoted replies reject absent/removed
articles before editing. A private continuation captures the starting subject
and destination and supplies the recipient array required by the
[mailer's API][lima-mailer-base]. A started draft can finish after subject edits
or group removal; external calls cannot invoke the completion method. Existing
quotation, wrapping and display-poster addressing remain.

**Twelve tests and twelve observations pass** through the adopted overlay with
unchanged working-case assertions and common helpers after removing the fixture
selector. Actual posts, reader/editor input, standard/Mh handlers and saved local
mail cover plain/quoted replies, Unicode, removed entry, captured context,
private callback access, cancellation/retry, wrapping and invalid destinations.
Observation labels are retained without comparing clock-dependent transcripts;
the five failure controls remain frozen in the audit. Ten focused compile checks
stay **5/5 → 5/5**; four quality gates and integrity checks preserve 233 predecessor
artifact/report sets. Historical harnesses and upstream drivers are not rerun.

The body mailbase and contributed board reply audit above closes that boundary.
Remote delivery, malformed saved bodies in the UI, historical saves and draft
recovery remain separate boundaries. Source review stays pinned to the Lima
revision below, reviewed **4 October 2026**; execution uses distinct nil,
preserved call-out players and 4,000 ms actions.

#### Lima newsreader mail reply audit

The 4 October 2026 local audit of [newsreader replies][lima-news-reader] finds that
removed articles retain their record with a nil body: plain replies still open,
while quoted replies fail when mapping the missing body. The editor completion
callback also fetches the article again, so an author's subject edit changes the
reply subject and removing the group breaks delivery. The public callback can
send mail when invoked externally without an editor.

The **one-file diagnostic repair, adopted above**, rejects absent/removed
articles before editing, binds the starting subject and recipient to a private
continuation, and supplies the recipient array declared by the
[mailer's API][lima-mailer-base]. Ordinary replies already deliver with the old
scalar argument through its normalization helper; no native typing relaxation
is needed. Existing lowercased display-poster addressing, quotation and wrapping
remain. System display labels may have no corresponding mail account; the
existing no-destination report is retained.

**Seventeen tests/observations pass: twelve working cases and five controls.**
Actual news posts, reader input, modal editors, standard/Mh handlers and saved
local mail cover plain/quoted replies, Unicode, removed entry, captured context,
private callback access, cancellation/retry, wrapping and invalid destinations.
Mailbox bodies are read in the recipient's context. Ten focused compile checks
remain **5/5 → 5/5**; four quality gates and integrity checks preserve 232 frozen
predecessor sets. The active Mh overlay, original Lima, native APIs and master
policy remain unchanged. Historical harnesses and upstream drivers are not rerun.

The adopted repair is described above; independent body mailbase and contributed
board reply callers follow. Remote delivery, UI handling of
malformed saved bodies, draft recovery and full startup remain unchecked. Source
review stays pinned to the Lima revision below, reviewed **4 October 2026**;
execution uses distinct nil, preserved call-out players and 4,000 ms actions.

#### Lima Mh shortcut integration adoption

The 4 October 2026 local port adopts the exact audited three-file repair in the
[shared shell][lima-shell], [user mailbase][lima-user-mailbase] and
[Mh handler][lima-mailer-mh]. Uppercase `M` shortcuts now reach the selected stock
Mh after alias expansion through both player and wizard shells. The bridge and
parser validate the user, owned shell, selected handler and mailbox owner.
Unknown/malformed shortcuts fall back to ordinary shell handling; standard mail
retains its modal behavior. Cached selection follows `query_mailer()` until
unloaded. Native APIs and master policy do not change.

**Fifty-five tests and fifty-five observations pass** through the adopted files:
twelve Mh cases, thirteen reader cases, eleven delivery cases, eleven handler
cases and eight mailbox lifetime cases. Actual shells, aliases and fallback,
caller checks, reads/navigation/deletion, composition/editor/Cc, replies, paging,
saved copies and forwarding work alongside retained persistence, delivery,
access, transaction abort and account lifetime behavior. Fifty-four test bodies
and assertions match after removing the Mh fixture selector. One existing test
is renamed and now requires successful shell dispatch instead of its absence.
Observation values are recorded without comparing clock-dependent transcripts;
the four audit failure controls remain frozen. Ten selected compile checks stay
**4/5 → 4/5**: user, both concrete shells and Mh compile, while the abstract shared
shell retains four unresolved hook calls when compiled alone. Four quality gates
and integrity checks preserve 231 predecessor artifact/report sets.

Newsreader mail replies are audited above. Independent body mailbase consumers,
missing/malformed
bodies in the UI, remote/local-at-mud delivery, historical saves, draft recovery,
detached ownership, delayed error-log flush, authentication/sockets, full startup
and concurrency remain unchecked. Source review stays pinned to the Lima revision
below, reviewed **4 October 2026**, with distinct nil, preserved call-out players
and 4,000 ms actions. This is local lpc-rs execution; historical harnesses and
upstream drivers are not rerun.

#### Lima Mh shortcut integration audit

The 4 October 2026 source review finds no caller for
[Mh's `parse_Mh_cmd`][lima-mailer-mh] in either stock shell, despite the handler
advertising `M?`, `M#` and related shortcuts. Its caller check is also unfinished,
and a malformed two-character suffix such as `hX` executes as `h`.

A three-file diagnostic repair routes uppercase `M` shortcuts after alias
expansion in the [shared shell][lima-shell], through the
[user mailbase][lima-user-mailbase] to the selected stock Mh handler. An idle
standard/default handler is not instantiated by this hook. Cached selection
matches ordinary `query_mailer()` behavior until the handler is unloaded. Mh
commands receive their text before wizard argument/pipeline expansion; unknown
or malformed shortcuts fall back to ordinary shell dispatch. The bridge checks
the current user and owned shell; Mh checks the current user, selected handler
and mailbox owner. Empty/malformed suffixes return unhandled. Native APIs and
master policy do not change.

The diagnostic patch was **not adopted during this audit**; its later adoption
is described above. **Sixteen new tests and sixteen
observations pass**: twelve working cases and four controls reproducing the two
unwired shells, unchecked caller and malformed suffix. Actual player/wizard input
supports lazy selection, aliases and alias bypass, standard/cached fallback,
read/header ranges/cursor/next unread/deletion, subject/editor/Cc delivery,
literal editor input, reply abort/retry, help paging, wizard saved copies and
forwarding. Foreign object/user calls and absent player context are rejected
without reading the guarded message. Ten selected compile checks remain
**4/5 → 4/5**: user, both concrete shells and Mh compile; the abstract shared
shell retains its four unresolved hook calls when compiled alone. Four quality
gates and integrity checks preserve 230 predecessor
artifact/report sets.

**Recommendation:** adopt the exact three-file patch, then audit remaining mail
consumers and failure reporting, starting with newsreader mail replies. Body
mailbase consumers, missing/malformed bodies in the UI, remote/local-at-mud
delivery, historical saves, detached ownership, delayed error-log flush,
authentication/sockets, full startup and concurrency remain unchecked. Source
review stays pinned to the Lima revision below, reviewed **4 October 2026**, with
distinct nil, preserved call-out players and 4,000 ms actions. This is local
lpc-rs execution; upstream drivers and historical harnesses are not rerun.

#### Lima mail reader adoption

The 4 October 2026 local port adopts the exact audited three-file repair:
[MAILBOX][lima-entry-mailbox] adjusts/clamps the cursor by the deleted message's
sorted position, [Mh][lima-mailer-mh] supplies zero for bare `#`'s current-message
default, and the [base mailer][lima-mailer-base] replaces a fresh reply's quotation
seed. Aborting then restarting a reply no longer duplicates quoted text.
Cancellation retains the draft; successful delivery removes it. Native APIs and
master policy do not change.

**Forty-three tests and forty-three observations pass** through the adopted files
without diagnostic production replacement: thirteen reader cases, eleven delivery
cases, eleven handler cases and eight mailbox lifetime cases. Actual saved bodies,
standard read/header ranges/next unread, long paging, editor/reply-array/reply-all
and Cc callbacks, forwarding, wizard saved copies and deletion ranges/cursor repair
work alongside retained persistence, local/group/offline delivery, access checks,
transaction abort, public composition and account lifetime behavior. Retained test
bodies and assertions match after fixture/observation wiring and one reader test
rename. Observation values are recorded without comparing clock-dependent
predecessor transcripts. The three original failure controls remain frozen in the
audit. Ten selected compile checks remain **5/5 → 5/5**; four quality gates and
integrity checks preserve 229 predecessor artifact/report sets.

The active overlay now includes all three repairs. Mh shortcut integration is
audited above; remaining mail consumers/failure reporting follow. Independent body
mailbase callers, missing/malformed body handling in the UI, remote/local-at-mud
delivery, historical saves, draft recovery, detached ownership, delayed error-log
flush, authentication/sockets, full startup and concurrency remain unchecked.
Source review stays pinned to the Lima revision below, reviewed **4 October 2026**,
with distinct nil, preserved call-out players, 4,000 ms actions and no
upstream-driver execution. Historical harnesses are not rerun.

#### Lima nonempty mail handler audit

The 4 October 2026 follow-up audit finds three small mudlib repairs. The
[mailbox][lima-entry-mailbox] uses unread count to adjust its cursor after
deletion; deleting the only read message leaves index `-1`, so a default reply
fails even after new mail arrives. Adjust by the deleted message's sorted
position and clamp to the remaining range, using zero for an empty mailbox.
The [Mh handler][lima-mailer-mh] calls `to_int(nil)` for bare `#`; supply integer
zero for its current-message default. The [base mailer][lima-mailer-base] appends
a fresh reply quotation to the retained draft; abort/retry duplicates quoted text.
Replace the quotation seed when starting a new reply. Cancellation still retains
the draft, while successful delivery removes it. Native APIs and master policy
do not change.

The three-file diagnostic patch was **not adopted during this audit**; its later
adoption is described above. **Sixteen new tests and sixteen observations pass**:
thirteen working cases and three controls for those
failures. Actual saved bodies support standard read/header ranges/next unread,
long-message paging, default/numbered/reply-all editor callbacks, real Cc delivery,
forwarding, saved copies and deletion ranges/cursor repair. Saved-copy testing
initializes wizard membership and home-directory owner protection through Lima's
security daemon. Mh public parsing supports the selected read/reply/forward/delete
flows, returning to the shell; stock `M…` shortcuts remain unwired. Ten selected
compile checks remain **5/5 → 5/5**, since these are runtime behavior failures.
Four quality gates and integrity checks preserve 228 predecessor artifact/report
sets. Historical harnesses are not rerun.

**Recommendation:** adopt these three repairs, then audit remaining mail
consumers, Mh integration and failure reporting. Independent body-mailbase
callers, missing/malformed body handling in the UI, remote/local-at-mud delivery,
historical saves, draft recovery, detached handlers, authentication/sockets,
complete startup and concurrency remain unchecked. Source review is pinned to
the Lima revision below, reviewed **4 October 2026**, with distinct nil, preserved
call-out players, 4,000 ms actions and no upstream-driver execution.

#### Lima mail delivery adoption

The 4 October 2026 local port adopts the exact audited one-file
[MAIL_D][lima-mail-daemon] repair: four class/message absence sites use nil,
five privileged callbacks make explicit calls, restoration uses the native
one-argument API, empty recipients are filtered and plain local addresses are
lowercased/deduplicated together. Native APIs and master policy do not change.

**Thirty tests and thirty observations pass** through the adopted library without
diagnostic production replacements: eleven delivery cases, eleven retained mailer
cases and eight retained mailbox lifetime cases. Actual saved class bodies,
reload, shared deletion, offline/group delivery, missing-body cleanup, rebuilding,
access checks, transaction abort and public composition work alongside the
existing modal/editor/pager and account lifetime behavior. All retained assertions
are unchanged. Mailer observations match exactly; mailbox comparison normalizes
three login timestamps. Delivery comparison normalizes generated message keys,
transaction timing, directory order and clock-dependent ordering in the two-body
case. Five historical controls remain frozen in the audit; its failed mailer Cc
compile boundary is superseded by actual public delivery. Ten selected compile
checks improve **4/5 → 5/5**. Four quality gates and integrity checks preserve
227 predecessor artifact/report sets.

The active overlay now includes the MAIL_D repair. The next audit covers nonempty
handler read/reply/forward flows and remaining consumers. I3/remote and local
`user@mud` delivery, Mh shortcuts, body-mailbase callers, historical/malformed save
files, long paging, failure reporting, complete startup and concurrency remain
unchecked. A caught I3 failure can still leave prior local deliveries committed;
the plain-local duplicate repair avoids that unnecessary path. Source review
remains pinned to the Lima revision below, reviewed **4 October 2026**, with
distinct nil, preserved call-out players, 4,000 ms actions and no upstream execution.

#### Lima mail persistence and local delivery audit

The 4 October 2026 audit of [MAIL_D][lima-mail-daemon] finds a one-file porting
patch. Use distinct nil at its four class/message absence sites. Replace five
`(: function, arguments :)` literals with explicit calls inside closures: native
comma expressions do not invoke their first operand. The old message-key loop
returns a filename and reaches the 4,000 ms limit. Drop `restore_object`'s legacy
flag `1`; native restoration already preserves globals omitted from the file.
Filter empty recipient strings before group expansion, and lowercase/deduplicate
plain local addresses together. Blank Cc otherwise reaches USER_D's canonical-name
error; case-variant duplicates inflate the recipient count and spuriously enter
the unported I3 mail daemon. Native APIs and master policy do not change.

The diagnostic patch was **not adopted during this audit**; its later adoption
is described above. **Sixteen tests and sixteen observations pass**: eleven working diagnostic cases and five controls for the original
class-null error, bounded comma-expression loop, restore arity, duplicate local
routing and blank Cc. Actual saved class bodies round-trip through MAIL_D and
MAILBOX, including reload, local/group delivery, offline receipt/cache cleanup,
last-recipient file removal, missing-body nil cleanup, mailbox reconstruction,
sender/owner checks and outer-transaction abort. Public shell/editor/Cc composition
now sends a real local body and removes its draft. The duplicate-routing control
reports an I3 error after its local body/receipts commit; a caught error does not
roll back those writes. No prepared mail-body provider
or replacement mailbox cleanup is injected. Ten selected compile checks improve
**4/5 → 5/5**; four quality gates and integrity checks preserve 226 predecessor
artifact/report sets.

The active overlay remains at the preceding mail-handler adoption. Recommend
adopting this one-file patch, then auditing nonempty handler read/reply/forward
flows and the remaining consumers. I3/remote delivery, local `user@mud` addresses,
Mh shortcut wiring, body-mailbase callers, historical save files, long pager
interaction, failure reporting, full startup and concurrency remain unchecked.
Source review is pinned to the Lima revision below, reviewed **4 October 2026**;
local execution uses distinct nil, preserved call-out players and 4,000 ms actions,
without upstream execution or rerunning historical harnesses.

#### Lima mail handler adoption

The 4 October 2026 local port adopts the exact audited four-file patch to the
[base mailer][lima-mailer-base], [standard handler][lima-mailer-standard],
[Mh handler][lima-mailer-mh] and [public command][lima-mail-command]. Explicit void
returns, distinct nil for absent strings, bare unbound callbacks, applied bound
callbacks and a mixed editor recipient parameter replace their stock assumptions.
Protected CMD dispatch preserves the private main function and inherited caller
check. Native APIs and master policy do not change.

**Twenty tests and twenty observations pass** through the adopted library without
diagnostic production replacements: twelve mailer cases and eight retained mailbox
lifetime cases. Standard modal navigation, help paging, handler recreation,
composition/abort/Cc and command access checks work alongside account cache
retention and last-session release. Mailer observations match the audit exactly;
mailbox observations preserve all assertions and normalize only three login
timestamps. Sixteen compile checks improve **3/8 → 7/8**; the remaining MAIL_D
class-null assignment failure is unchanged. Four quality gates and integrity
checks preserve 225 predecessor artifact/report sets.

[MAIL_D][lima-mail-daemon] still blocks real message bodies and delivery. Stock
modal input catches the Cc failure silently and leaves the staged draft. Mh's
public parser works for selected empty-inbox commands, while its advertised M
shortcuts remain unwired in both stock shells. Actual read/reply/forward/delete,
address/group/I3 paths, body-mailbase consumers, detached handlers, full startup
and concurrency remain unchecked. The next audit covers MAIL_D persistence and
real local delivery. Source review remains pinned to the Lima revision below,
reviewed **4 October 2026**; local execution uses distinct nil, preserved call-out
players and 4,000 ms actions, without upstream execution.

#### Lima mail handlers and public command audit

The reviewed [base mailer][lima-mailer-base], [standard handler][lima-mailer-standard],
[Mh handler][lima-mailer-mh] and [public command][lima-mail-command] need adaptations
in four local files. Use explicit output followed by `return;` for void returns,
`nil` for absent strings/output filenames, bare names for unbound callbacks and
`&name(args)` for bound callbacks. The editor completion recipient parameter needs
`mixed`: new mail binds recipient text, while replies bind a recipient array.
The public command needs a protected CMD dispatch adapter; its private entry point
and inherited [caller check][lima-command-base] remain.

The diagnostic patch was **not adopted during this audit**; its later adoption is
described above. Selected compilation improves **3/8 →
7/8**, preserving lifecycle successes. The remaining [MAIL_D][lima-mail-daemon]
blocker assigns integer zero to a class-valued saved message. **Fifteen new tests
and fifteen observations pass**, covering actual shell command dispatch, standard
modal input, empty-inbox navigation,
help paging, handler destruction/recreation and composition through the real editor
and Cc prompt. The actual bound Cc callback exposes the daemon compile error; stock
[modal input][lima-inputsys] catches it silently, returns to the shell and leaves
the draft file. Successful delivery is not established.

Mh's public parser handles the selected empty-inbox cases directly. Its advertised
`M…` shortcuts have no stock caller in either [player shell][lima-pshell] or
[wizard shell][lima-wish]; selecting Mh and entering `M$` still reaches ordinary
command lookup. Wire that mudlib integration separately. Real message
read/reply/forward/delete, recipient processing, local/I3 delivery, body-mailbase
callers, detached handlers, full startup and concurrency remain unchecked.
Source review is pinned to the Lima revision below, reviewed **4 October 2026**;
this is local execution in distinct nil mode with preserved call-out players and
4,000 ms actions, without upstream execution or native/master changes. The next
recommendation is to adopt the four-file handler patch, then audit MAIL_D.

#### Lima shared mailbox lifetime repair

The 4 October 2026 local port adopts the audited one-file MAILBOX_D guard.
Explicit unload retains the account mailbox while another standard USER_OB clone
owns that account, excluding the calling user that is retiring. Idle close uses
live USER_OB account IDs, including linkdead sessions, instead of interactive
`users()`. Pending account sessions count too. Last-session release still uses
the existing authorized self-removal path. The shared message cursor and its
nosave reset on reload remain. [Mailbox daemon][lima-mailbox-daemon],
[user removal][lima-entry-user], [mailbox][lima-entry-mailbox].

Nineteen tests/nineteen observations run through the adopted overlay: eight
mailbox lifetime cases and eleven reconnect cases in a new harness. Interactive
and linkdead peers, wizard duplicates, regular no, affirmative takeover and
another account's unload request retain live cached references. Explicit cleanup
keeps connected/linkdead caches, closes offline caches and returns ASK_AGAIN.
The last owning session releases the mailbox despite an unrelated observer;
saved synthetic keys/flags reload with saved bytes unchanged. Reconnect checks
retain selected identity, body links, editor/timer cleanup without saving edits,
callback retry/revalidation, deferred didlog paging and ambiguity refusal.

Cache/prompt observers use actual mailboxes and the standard prompt expression;
actual mailer UI and MAIL_D delivery/content/deletion are not exercised. Fourteen
uninstrumented checks keep six lifecycle compositions clean and retain eight
independent mailer/mail-command blockers. MAILER's void-cast parser blocker and
the mail command's missing protected CMD adapter are the next audit boundary;
further errors remain unchecked behind them. [Mailer base][lima-mailer-base],
[standard handler][lima-mailer-standard], [Mh][lima-mailer-mh],
[mail command][lima-mail-command].

Workspace Clippy/format checks and 223 frozen predecessor sets pass. The adopted
patch matches the audit exactly; native/original/master sources remain unchanged.
Distinct nil, preserved players and four-second actions remain. Backup/guest
ownership, detached handlers, natural idle scheduling, authentication, sockets,
full startup and concurrency remain unchecked. Pinned upstream source review is
distinct from local execution.

#### Lima shared mailbox lifetime and idle cleanup audit

The 4 October 2026 audit retains the reconnect/takeover overlay. MAILBOX_D caches
one mailbox per account, but each user removal unconditionally unloads it.
Retiring either of two characters, a wizard duplicate session, or a bodyless
newcomer after regular no invalidates another session's cached mailbox. Yes can
also invalidate a cache the newcomer already acquired. Idle close retains only
interactive `users()`, so a bodyful linkdead user can lose its mailbox before its
removal timer fires. MAIL_D's source also calls close after delivery; actual
delivery is not executed here. [Mailbox daemon][lima-mailbox-daemon],
[user removal][lima-entry-user], [mail daemon][lima-mail-daemon],
[native users](efun/users.md).

The mailer stores a mailbox reference. Calls through a destructed reference
return native nil; the standard prompt then fails at index plus one. The audit
uses a cache/prompt observer with the actual mailbox, rather than the mailer UI.
MAILER currently fails to parse `(void)printf(...)`; standard and Mh inherit that
blocker. The mail command also lacks the protected CMD dispatch adapter. Further
errors remain unchecked beyond these compile blockers.
[Mailer base][lima-mailer-base], [standard handler][lima-mailer-standard],
[Mh handler][lima-mailer-mh], [mail command][lima-mail-command].

A one-file daemon diagnostic retains a mailbox while another standard USER_OB
clone owns the account, excluding the retiring caller on explicit unload. Idle
close uses live USER_OB account IDs, including linkdead sessions. Last-session
release, existing authorized self-removal, account-wide cursor and cursor reset
on reload remain. The active library and master policy are unchanged.

Eight tests/sixteen observations compare known baseline losses with this
diagnostic through real accounts/bodies, public entry, user removal, modal
answers, connection detach/net_dead and mailbox save/restore. Explicit cleanup
keeps connected accounts, closes offline caches and returns ASK_AGAIN; the
diagnostic additionally keeps linkdead caches. Synthetic saved keys/flags reload
after last-session release with saved bytes unchanged. Actual message bodies,
mail UI and delivery are not exercised.

Fourteen uninstrumented checks keep six lifecycle compositions clean and reproduce
eight mailer/mail-command blockers. Workspace Clippy/format checks and 222 frozen
predecessor sets pass; pinned upstream review is distinct from local execution.
Distinct nil, preserved players and four-second actions remain. The diagnostic
was not adopted at this audit; its subsequent adoption is described above.
Backup/guest users, detached handlers, natural idle scheduling,
authentication, sockets, full startup and concurrency remain unchecked.

#### Lima selected-character reconnect and takeover repair

The 4 October 2026 local port adopts the audited three-file repair. Existing-logon
selection matches account ID and the actual body's character name, skips bodyless
sessions and refuses multiple owners of the selected character. Takeover callbacks
retain that name through retry and wizard no; yes revalidates the predecessor's
current account/body before transfer. Different characters on one account enter
separately. Existing wizard no still creates a fresh selected-character body and
keeps the old session; regular no retires only the newcomer.
[Body selection][lima-user-bodies], [existing logon and answers][lima-body-switching].

Login initialization waits for a body, including successful callback paths.
A long didlog opens its pager after the takeover answer and completed transfer.
MAILBOX_D unloads through the mailbox's existing public `remove()` for authorized
self-destruction. The idle close loop uses the same method, but its execution
and shared mailbox lifetime are not established. Master policy remains unchanged.
[Public entry][lima-login], [didlog display][lima-did-user],
[mailbox daemon][lima-mailbox-daemon], [mailbox removal][lima-entry-mailbox].

Twenty tests/56 observations run against the adopted library: eleven reconnect
cases and nine didlog command tests/45 observations. Real saved accounts/bodies,
shell/modal input, loaded mailboxes, editors and pager cover linkdead/yes/no
cleanup, separate characters, invalid answers, predecessor disappearance/body
change, deferred paging and ambiguity refusal. Successful transfers check the
actual body link, old user/editor retirement and timer cancellation; dirty source
stays unsaved. Normal quit before yes needs no prepared mailbox cleanup. Didlog
command output, persistence and existing permission/visibility guards remain.
No diagnostic production sources are substituted by this adoption harness.

Twelve uninstrumented CLI checks keep USER_OB, USER_D, MAILBOX_D, human, I and
didlog clean in preceding/adopted libraries. Workspace Clippy/format checks and
221 frozen predecessor sets pass. The adopted patch matches the audit exactly;
native/original sources remain unchanged. Distinct nil, preserved players and
four-second actions remain. Authentication, sockets, backup/guest users, full
startup, idle/shared mailbox lifetime and concurrency remain unchecked. Pinned
upstream source review is distinct from local execution. The subsequent shared
mailbox lifetime and idle cleanup audit is described above.

#### Lima selected-character reconnect and takeover audit

The 4 October 2026 audit retains the didlog-command overlay. Existing-logon
lookup matches account ID alone: reconnecting another character can take the
wrong linkdead body or ask to throw off a different interactive character.
The takeover callback drops the selected name on wizard no and after the old
user disappears, and yes does not revalidate a changed predecessor body.
The diagnostic matches account and actual body name, retains the name in callbacks
and rejects ambiguous selected-character owners before transfer.
[Body selection][lima-user-bodies], [existing logon and answers][lima-body-switching],
[public entry][lima-login].

Login initialization also runs while confirmation is pending. A long didlog
pushes a pager above the question, so y pages instead of answering. The diagnostic
defers channels/display until a body is accepted, including callback completion.
Existing wizard no policy still creates a fresh selected-character body without
retiring the old session; regular no still dismisses the newcomer.
[Modal dispatch][lima-inputsys], [didlog display][lima-did-user].

A loaded account mailbox exposes a separate local teardown refusal: MAILBOX_D
calls `destruct(mailbox)` through a stack the master does not authorize. Modal
catches conceal the error: a partial transfer can leave the predecessor live
and the physical body link unchanged, or a dismissed newcomer still alive.
The diagnostic calls the mailbox's existing public `remove()` for authorized
self-removal, with master policy unchanged. Earlier reconnect fixtures did not
load that mailbox and did not establish this cleanup path.
[Mailbox daemon][lima-mailbox-daemon], [mailbox removal][lima-entry-mailbox],
[native destruction authorization](apply/master/valid_destruct.md).

Eleven tests/22 observations compare baseline reproductions and a three-file
diagnostic through real saved users/bodies, shells, modal input, editors, pager
and mailbox. Cases cover different characters, loaded-mailbox linkdead/yes/no
teardown, invalid answers, predecessor disappearance/body change, pager ordering
and duplicate selected-character owners. The disappearing-predecessor baseline
first records the stock quit refusal, then explicitly prepares mailbox self-removal
to isolate its wrong-character retry; the diagnostic needs no such preparation.
Dirty editor source remains unsaved, and successful transfer checks the body link
against the actual user object. Passing audit assertions reproduce known baseline
failures as well as the corrected diagnostic behavior.

Eight uninstrumented compile checks keep USER_OB, USER_D, MAILBOX_D and human clean
in both libraries. Workspace Clippy/format checks and 220 frozen predecessor sets
pass. Distinct nil, preserved players and four-second actions remain. The patch
was diagnostic at this audit; its subsequent adoption is described above.
Authentication, socket dispatch, backup/guest users, full startup, idle/shared
mailbox lifetime, other-account takeover attempts and concurrency remain unchecked.
Pinned upstream source review is distinct from local execution.

#### Lima didlog command adapters

The 4 October 2026 local port gives `I` and `didlog` protected `dispatch_main`
adapters that pass raw arguments to their private main functions. Existing CMD
shell/resend guards remain. `didlog` uses nil for absent string values and its
continuation, guards the absent pattern before `strlen`, and retains the default
singular day heading. Its seven parameter-taking aliases refuse missing, empty
or next-option values before mutation. Existing single-token parsing remains.
[Posting command][lima-did-post-command], [reader][lima-did-command],
[CMD][lima-command-base], [command lookup][lima-command-daemon].

The preceding DID_D guard also blocked legitimate admin commands: their maximum
privilege is `Mudlib:wizcmds`, which does not imply `Mudlib:daemons`. The setter
retains its daemon privilege path and additionally accepts the exact loaded
`/cmds/wiz/didlog` command, an admin user and a successful
`check_previous_privilege("Mudlib:wizcmds")`. This checks every caller while
skipping the daemon's own different privilege. Other wizard commands cannot
use the allowance, even in an admin context. Authorized reset still clears the
selected version's rows. The security database and master policy are unchanged.
[Privilege hierarchy and stack checks][lima-security].

Nine focused tests/45 observations cover real shell input, empty commands,
posting/live notices/save/reload, day/all/version/glob reports, option aliases,
fourteen missing-value refusals, off/on display, file redirection, literal posting
suffixes, direct/command permissions and entry/visibility guards. Modal input
catches command errors; denial cases also repeat through the actual shell with
the caller stack retained to capture the refusal. Inaccessible private/protected
methods return native nil without mutation. [Shell][lima-wish],
[native call_other](efun/call_other.md).

Eight dispatch-only compile checks expose three further `didlog` string-sentinel
errors; eight final uninstrumented checks make DID_D, USER_OB and both commands
clean, preserving both baseline successes. Workspace Clippy/formatting checks
pass. Native/original/previous sources remain unchanged, and pinned source review
is distinct from local execution. Lima keeps distinct nil, preserved players and
four-second actions. Selected-character reconnect and takeover responses follow;
HTTP delivery, authentication, special LIMA version handling, old/malformed saves,
full login/startup and concurrency remain unestablished.

#### Lima didlog daemon repairs

The 4 October 2026 local port repairs DID_D and the user's login display. The
daemon needs explicit integer-zero and nil returns, an array regexp predicate
(`sizeof(regexp(({row_text}), pattern))`), `mixed *` for timestamp/string rows,
and export loops over the current version's rows. Native `__PORT__` supplies the
untouched stock ports header; no diagnostic constant is installed.
[Daemon][lima-did-daemon], [user display][lima-did-user],
[native regexp](efun/regexp.md), [native macros](lpc/macros.md#predefined-macros).

Version changes now check the existing `Mudlib:daemons` privilege before clearing
rows or saving. Authorized selection of an existing version retains its explicit
reset semantics. Login's off flag suppresses automatic display without advancing
the last-read timestamp; enabling it restores display, and live notification
muting remains intact. Future cutoffs still select all rows for the command's
`-all` convention. A pattern with no matches retains the header-only result,
while a timestamp-empty query runs its continuation. [Reader][lima-did-command].

Nine focused tests/nineteen observations cover actual public wizard reconnect,
the interactive takeover prompt, off/on display, pager quit, posting, notification
muting, save/reload, pattern/cutoff selection, four plain/dated file exports and
four version refusals with unchanged state/saved bytes plus an authorized reset.
Role fixtures register named wizard privileges and mirror normal user-entry
privilege assignment; they use direct calls through the existing guard.
[Security setup][lima-security], [user entry][lima-body-switching].
Eight uninstrumented compile checks make DID_D and USER_OB clean in the adopted
overlay; `I` and `didlog` retain four CMD `dispatch_main` errors apiece.
Workspace Clippy and formatting checks pass; no native code or master policy
changes. Pinned source review is distinct from local execution. Lima keeps
distinct nil, preserved players and four-second actions. Next port those CMD
callers and their argument handling, then audit selected-character reconnect and
takeover responses. Command/HTTP execution, full login/startup, old saves,
malformed versions/rows, the special LIMA save/version path and concurrency
remain unestablished.

#### Lima native configured port macro

The 4 October 2026 driver provides reserved integer `__PORT__` from the compiler
configuration's primary plaintext port. Default is 24960; `LPC_PORT` overrides its
`PORT` alias. Headers, auto-includes, aliases, argument expansion, stringification
and integer `#if`/`#elif` expressions use that value. Zero is defined but false;
active redefinition/undefinition is rejected. Compiled objects retain the literal
even under a different runtime configuration. The macro remains the configured
plaintext port when plaintext Telnet is disabled, without substituting a TLS
port or discovering an actual socket/ephemeral binding.
[Native macros](lpc/macros.md#predefined-macros).

Eight new tracked regressions and extended predefined checks pass: 304
preprocessor tests, 3 compiler macro tests and 4865 full workspace tests (2 existing
ignored tests), including doctests. Workspace build, all-target/all-feature
warning-free Clippy, warning-free docs, formatting and whitespace checks pass.
Six distinct-nil Lima CLI probes use the rebuilt binary and untouched stock
`ports.h`: default base/HTTP/FTP/RCP are 24960/24963/24964/24965, alias base 33100
and prefixed-over-alias base 44200 produce their offsets, and disabled plaintext
with base zero/TLS 4040 yields 0/3/4/5. These executable header fixtures use one
new source in a disposable library copy; they add no port shim or listener.
The actual active USER_OB compiles; DID_D's four diagnostics reduce to its two
existing return-type errors. [Ports header][lima-did-ports],
[didlog daemon][lima-did-daemon].

Original/active Lima sources and configuration, master policy and all 217 frozen
predecessor artifact/report sets remain unchanged. Source review reuses the
4 October pinned FluffOS registration evidence without upstream execution; its
macro reads the first configured listener, which can differ from our explicit
primary plaintext port. [FluffOS registration][fl-configured-port].
The initial out-of-root CLI fixture refusal and corrected Rust fixture builds
are retained. Lima keeps distinct nil, preserved players and four-second actions;
shared builds use two jobs/serial tests without overlapping CLI work or copying
build artifacts. Actual binds, TLS handshakes, stock DID_D runtime and full startup
are not claimed. Next adopt the daemon's explicit returns and local data/permission
repairs, preserving the reader's future-cutoff `-all` convention, then port its
CMD callers and audit selected-character/takeover behavior. Historical diagnostic
shims must be removed before running their adapted sources on the native macro.

#### Lima didlog daemon and wizard login audit

The 4 October 2026 audit retains the reconnect-account overlay. DID_D has four
baseline diagnostics: undefined `__PORT__` variable/symbol, a bare return from an
int function and a zero return from a string-array function. A temporary two-file
control defines port 24960 and makes those returns `0` and `nil` respectively.
It does not add native port support or change the active library. Eight compile
checks improve clean targets from one to two out of four: USER_OB stays clean,
DID_D becomes clean, and both `I` and `didlog` still stop at four inherited
`dispatch_main` errors. Their option handling remains unchecked; typed-string
zero sentinels and `strlen(pattern)` also need review after command adaptation.
[Daemon][lima-did-daemon], [ports][lima-did-ports],
[posting command][lima-did-post-command], [reader command][lima-did-command].

Nine tests/twelve observations pass as audit assertions on the compile-only
control. Public wizard reconnect now completes with the same body, discarded
unfinished edits, old editor/flag/lock cleanup, timer cancellation and setup help.
Interactive entry keeps its takeover prompt and leaves the newcomer bodyless;
no response is tested. Short login output excludes entries at/before the cutoff,
and long output pages through the stock 40-line limit. Modal `q` destroys the
pager and returns to the shell. Login advances read time before paging completes
and still displays with `didlog_off=1`; that flag does suppress live notices.
Actual posting records timestamp/string rows, excludes the poster, notifies a
wizard peer and preserves data through native save/reload. `dump_entries` is
misdeclared `string *` despite returning nested rows.
[User didlog handling][lima-did-user], [pager][lima-pager],
[more helpers][lima-more-helpers].

Several independent defects remain explicit: both plain-text export modes
index the version mapping with integers and fail on nil before writing; an unprivileged regular player can call `set_version`
and persistently erase the current version's records. Web formatting with an
explicit version works, with diagnostic HTTP links at base+3, but actual CGI/HTTP
entry is unchecked. Pattern queries fail because DID_D passes scalar strings to
native `regexp`; use the existing one-element array/`sizeof` adaptation.
Pattern-selection behavior remains unchecked.
[Native regexp](efun/regexp.md), [daemon][lima-did-daemon].

Implement configured numeric `__PORT__` next, then adopt explicit daemon returns,
array-based pattern checks, current-version export indexing and row typing.
Preserve the future-cutoff all-entry sentinel supplied by `didlog -all`; it is
a caller convention, not a driver timestamp defect. Review login-off behavior
and protect the public version setter using existing daemon/admin privilege
conventions. FluffOS's port macro
uses the first configured listener, without an optional-package guard; its core
scalar regexp form returns a numeric match result (`F_REGEXP` enabled). Both
sources were reviewed at `44532e0228f34be2184c701a361df4079371b8b0`, without
upstream execution. After daemon repairs, port both commands to CMD dispatch and
verify their version/pattern arguments before returning to character matching and
takeover responses. [Port macro][fl-configured-port],
[scalar regexp][fl-scalar-regexp], [declaration][fl-scalar-regexp-spec].

Four quality gates and all 216 predecessor artifact/report sets remain intact.
Initial fixture failures and the real scalar-regexp blocker are retained. Native,
original and active library/configuration sources and master policy are unchanged;
distinct nil, preserved players and four-second actions remain the Lima target.
Synthetic timestamps/version seeds and an admin observer define the fixture
boundary. The special LIMA save/version identity, old save migration, deletion,
unknown/malformed versions and rows, missing/bodyless recipients, paging during
takeover, authentication/socket loop/private input clearing, natural timer expiry,
backup or different-character reconnect, command/HTTP execution, concurrency,
hot reload, commit-time I/O failures, optimized performance, historical/full
native reruns and complete startup remain unestablished.

#### Lima reconnect account selection adopted

The 4 October 2026 local port repairs USER_D registry validation during overlapping
logons. Normal `enter_game()` registers the selected character before body
transfer; global lookup previously rejected the newly restored user and its
link-dead predecessor as duplicate live account objects. Existing owning-user
authorization now returns that exact caller, whose body records register/remove
validate. Privileged other-account operations retain global/offline lookup. The
global duplicate-account guard, query/set routing, authorization predicate,
record schema and character ownership/name conflict checks remain unchanged.
Missing or malformed requestor records refuse without consulting the old account.
[Registry][lima-user-daemon], [login][lima-login],
[body switching][lima-body-switching].

Nine tests/seventeen observations pass directly on the adopted one-file overlay,
without a protected-transfer bypass or control replacement. They cover ordinary
player entry, wizard transfer/interactive prompting, requestor-only records,
missing/malformed records, four permission refusals, three privileged duplicate
lookup refusals, two ownership/name conflicts, and offline privileged registration/
removal. Six focused compiles are clean; one DID_D check retains four known
diagnostics. Four quality gates pass and 215 predecessor artifact/report sets
remain intact. Native/original/previous sources and master policy are unchanged;
configuration changes only the library path. Distinct nil, preserved call-out
players and four-second actions remain the Lima target.

Regular-player entry completes in the fixture. Wizard entry now transfers the
same body, removes the old user/shell/editor, clears its flag/lock and cancels
the removal timer, without saving unfinished edits. Its later login display
fails compiling DID_D: undefined `__PORT__` variable/symbol, a bare int-function
return and an integer-zero string-array return. Interactive wizard entry installs
the existing takeover prompt without transferring the body, then reaches the
same didlog failure. Caught errors retain those earlier effects; this does not
establish complete wizard reconnect. Audit DID_D and the wizard login display next.
[Didlog daemon][lima-did-daemon].

The initial failing runtime is retained; an unprivileged test forwarder was
replaced with direct owner-scoped privileged calls, while the real DID_D error
remains explicit. Native detach/manual net_dead apply, saved-account restore and
fixture admin membership define the tested boundary. Authentication/full login,
socket-loop/private input-slot clearing, natural timer expiry, takeover responses,
different selected characters, backup reconnect, concurrency, hot reload,
commit-time I/O failure, optimized performance, historical/full native reruns and
complete startup remain unestablished; no upstream execution is claimed.

#### Lima wizard-editor user removal cleanup adopted

The 3 October 2026 local port adopts the exact one-line control from the
disconnect/reconnect audit, over the editor pipeline guard overlay. User
`remove()` clears modal handlers while its owner still exists, before
mailbox/body/shell teardown. This cancels the editor and releases its session,
owned editing flag and command lock immediately. The 300-second link-dead grace
period remains intact; unfinished edits are discarded at removal and earlier
explicit saves remain on disk. Protected existing-logon body transfer preserves
the same body and its new user link while releasing the old editor's flag/lock.
[User][lima-entry-user], [inputsys][lima-inputsys],
[body switching][lima-body-switching], [session][lima-ed-session].

Nine tests/ten observations pass directly on the adopted overlay, without
injecting control replacements or manually cleaning abandoned sessions. Four
uninstrumented user/editor/human/wizard-shell compiles and four quality gates
pass, preserving 214 predecessor artifact/report sets. Besides the audited
disconnect/removal/transfer cases, ordinary user removal without an editor
still works, a synthetic throwing child modal handler is logged without stopping
editor/user teardown, and an explicitly saved edit survives owner removal.
Native/original/previous sources and master policy are unchanged; configuration
changes only the library path. Distinct nil, preserved call-out players and
four-second actions remain the Lima target.

Normal `enter_game()` after saved-account restore still refuses the new and old
link-dead user as duplicate live account objects in the port's USER_D registry.
That refusal is recorded before directly testing protected existing-logon logic;
the adoption does not establish a complete successful reconnect. Repair legitimate
account selection next, retaining authorization and duplicate-account guards.
[Login][lima-login], [registry][lima-user-daemon].

The actual scheduled removal callback is prioritized early through native
dispatch; natural timer expiry is not established. Native detach followed by a
manual net_dead apply, owner-side observation wrappers and fixture admin
membership define the tested boundary. Authentication/full login,
socket-loop/private transport input-slot clearing, other modal consumers,
arbitrary reentrant callbacks, periodic world cleanup, concurrency, commit-time
I/O failure, optimized performance, historical/full native reruns and complete
startup remain unestablished; no upstream execution is claimed.

#### Lima wizard-editor disconnect and reconnect audit

Reviewed **3 October 2026** against pinned Lima [user][lima-entry-user],
[inputsys][lima-inputsys], [body switching][lima-body-switching],
[user shell][lima-user-shell], [body][lima-body], [editor session][lima-ed-session]
and [editor command][lima-ed-command] sources. Execution uses the active local
port at native revision `004d1f40`; no upstream driver execution is claimed.

Native detach followed by Lima's real `net_dead()` apply retains the editor,
unsaved buffer, modal stack and lock during its 300-second grace period. Later
user removal tears down the body and shell without clearing modal handlers,
leaving an orphan editor. Calling the protected body-transfer routine directly
removes the old user but also leaves the session's owned `F_IN_EDIT` flag on the
transferred body until session cleanup runs. The dead-owner lock entry is stale
metadata, not a permanently blocking lock: the command checks owner liveness.
Explicitly invoking the real session cleanup releases those remaining resources.

A diagnostic one-line control clears the modal stack before user teardown,
cancelling the editor while its owner is alive. It releases the flag/lock on
scheduled removal, explicit removal and reconnect, without saving unfinished
edits or changing the link-dead grace period. Ordinary editor quit and live-owner
modal cancellation still release their resources. The control is recommended
for local adoption; active sources/configuration and master policy are unchanged.

Six tests/fourteen observations pass in debug; eight focused compiles and four
quality gates pass, preserving 213 predecessor artifact/report sets. Fixtures
use actual saved users/bodies, wizard shells, CMD discovery, master policy and
modal/editor state. The actual recorded removal timer is checked in milliseconds
and prioritized early through native dispatch, so natural 300-second expiry is
not established. Reconnect seeds a fixture admin membership to enter existing-
logon logic; authentication and full login are not exercised. Public native
detach and a manual net_dead apply exercise this boundary; the socket loop and
its private transport input-slot clearing are not executed.

Distinct nil, preserved call-out players and four-second actions remain the Lima
target. No native API change, historical/full native rerun, optimized performance,
periodic world-cleanup sweep, other modal-consumer/callback-failure behavior,
interactive takeover, hot reload, concurrency, commit-time I/O failure or complete
startup claim is made. Adopt modal clearing before user removal next, with normal
removal and protected body-transfer/cancellation validation.

Normal enter_game after saved-account restore has a separate active-port USER_D
blocker: its account lookup rejects the new and old link-dead user as duplicate
live account objects before body transfer. Both variants record that refusal,
then invoke protected existing-logon logic directly to test its cleanup boundary.
This does not establish a successful complete reconnect. The modal-clear control
leaves the registry guard unchanged; normal-entry account selection needs a
subsequent repair. [Login][lima-login], [registry][lima-user-daemon].

#### Lima wizard-editor pipeline guard adopted

The 3 October 2026 local port adopts the exact three-file control from the
preceding incoming-pipeline audit, over the tokenizer cache overlay. CMD gains
protected `no_pipeline()` metadata and a public nomask query; ed opts in, and
pipeline preparation checks membership before stage mains/stdio execute. Incoming
producers therefore do not run before an interactive-editor refusal. Outgoing
editor pipes now get preparation-time pipeline feedback; direct redirection keeps
the editor's suffix guard. Raw `no_redirection()` dispatch remains a separate
contract. No native API or master-policy change; configuration changes only the
local library path.

Eleven tests/26 observations pass in debug, using the adopted overlay directly
without injected control files. Six pipeline tests/twelve observations cover the
reviewed shapes; five copied editor-routing tests/fourteen observations protect
quoted Unicode/operator filenames, retained escape spelling, successful saves,
redirection refusal and downstream pipes. Five incoming shapes preserve actual
command/session state, account bytes and source contents, leave no producer event,
then recover through ordinary editing. With pipes before and after ed, no
downstream tell is delivered. Ordinary pipelines and incoming raw codesay still propagate or
deliver their literal payloads through actual CMD discovery/stdio, users, wizard
shells, master policy and modal/editor state. Producer/sink commands are explicit
synthetic fixtures with native transactional file events.

Seven concrete/module/fixture compiles are clean; standalone CMD retains its four
known missing-`dispatch_main` diagnostics, supplied by concrete commands. Four
quality gates, exact patch adoption and all 212 predecessor artifact/report sets
pass. Original Lima, native sources and previous overlays remain unchanged.
Distinct nil, preserved call-out players and four-second actions remain the target.
Pinned [ed][lima-ed-command], [CMD][lima-command-base], [stdio][lima-command-stdio],
[wish][lima-wish] and [CMD_D][lima-command-daemon] source evidence remains separate
from execution on lpc-rs; the coordinated pipeline module is local port code.
No upstream execution, optimized performance repeat, historical/full native rerun,
hot reload, concurrency, commit-time I/O failure or complete startup is claimed.
Lookup may lazily create commands before refusal, so arbitrary constructor effects
remain outside the guard. Audit editor disconnect/reconnect cleanup next.

#### Lima incoming wizard-editor pipeline audit

Reviewed **3 October 2026** against pinned Lima [ed][lima-ed-command],
[CMD][lima-command-base], [stdio][lima-command-stdio], [wish][lima-wish]
and [command daemon][lima-command-daemon] sources. Tests execute the active local
port at native revision `2b2a1754`; its coordinated pipeline module is local port
code absent from that upstream snapshot. No upstream driver execution is claimed.

`producer | ed filename` runs the producer and opens the editor on the named file,
ignoring piped text. With an additional outgoing pipe, the editor's suffix guard
refuses after the producer runs: no editor or downstream tell starts, but the
producer's committed effects remain. CMD's `no_redirection()` selects raw dispatch
and discards stdin; it does not declare pipeline membership unsupported. The
editor's raw suffix omits earlier pipeline stages, and its normal refusal does
not roll back those stages.

A diagnostic three-file control adds explicit CMD `no_pipeline()` metadata,
enables it in ed and checks membership during preparation before any stage's
main/stdio execution. Ordinary pipelines, direct editing and quoted operator
filenames still work. Raw codesay still allows incoming pipelines and delivers
its own argument literally. Command lookup can lazily create objects before the
check, so refusal before arbitrary constructor effects is not established.
The control is recommended for local adoption; the active overlay is unchanged.

Six tests/24 observations pass in debug. Synthetic feed/pass commands register
through actual CMD discovery and record native transactional file effects;
actual saved users, wizard shells, master policy, modal input and editor state
are exercised. Cases cover adjacent pipes, empty output, two producers, explicit
ed paths, quoted Unicode/operator filenames, producer file input/missing input,
missing editor filenames, outgoing refusal and raw codesay. Eight concrete/module
compiles are clean; two standalone CMD checks retain the same four existing
missing-`dispatch_main` diagnostics supplied by concrete commands. Four quality
gates pass and 211 predecessor artifact/report sets remain intact.

Native/original/active sources, configuration and master policy remain unchanged.
Distinct nil, preserved call-out players and four-second actions remain the Lima
target. No optimized performance repeat, historical/full native rerun, hot-reload,
concurrency, commit-time I/O failure or full login/network/startup claim is made.
Adopt the reviewed control and validate existing editor routing next.

#### Lima wizard shell tokenizer character cache adopted

The 3 October 2026 local port adopts the exact one-file character-cache control
from the preceding cost audit, over the editor account/codesay repair. The
[tokenizer][lima-shell-getopt] caches each character in its quoted and unquoted
inner loops. Escape handling, closing-quote index advancement, delimiters, token
spans and spacing retain their behavior. This reduces redundant native indexing;
the native Unicode scalar contract and remaining per-index copy/walk cost stay
unchanged. No native API or master-policy change; configuration changes only the
local library path.

Twenty focused tests/31 observations pass in each of debug and optimized builds:
13 copied codesay cases, five copied editor-routing cases and two adoption cases.
All 33 reviewed token/spacing expectations match, including operators, escapes,
unfinished input, tabs/newlines and Unicode. Nine near-limit dispatches per build
reach normal size-refusal feedback before broadcasting, then recover with ordinary
self/room delivery. Every action keeps the 4000 ms budget; no expected deadline
is accepted. The slowest measured debug action takes 1.06 seconds; optimized warm
medians are 3.39–6.49 ms. First commands include lazy loading, not complete startup.

Codesay retains literal payloads, independent users/account preferences, another
user's live editor, scratch-file preservation and caller/resend/visibility guards.
Editor quoted Unicode/operator filenames and retained escape spelling still save
correctly. Output/input/append/adjacent/missing-target routes refuse before file
effects, and a downstream tell receives no message. Account/modal state and
scratch-file absence remain checked on large input.

Four focused compiles and four quality gates pass, with exact adoption and all
210 predecessor artifact/report sets intact. Pinned source review remains separate
from native execution; no upstream driver was run. Distinct nil, preserved call-out
players, two Cargo jobs, serial tests, shared nonincremental builds and debug level
1 remain in force; release retains thin LTO. Frozen historical harnesses, the full
native suite, unrelated module/handoff matrices and profiling were not repeated.
The multiline case uses direct modal input, not network multiline framing.
Measured results establish these parsing/refusal shapes, not large-message delivery
or every near-limit input. Incoming editor pipelines, full login/network lifecycle,
concurrency and complete startup remain separate.

#### Lima near-limit wizard shell input cost audit

Reviewed **3 October 2026** against the pinned [tokenizer][lima-shell-getopt],
[wish][lima-wish], [shell][lima-shell] and [command daemon][lima-command-daemon]
sources. Timing checks execute the active local port on native revision
`246456a2`, with distinct nil, preserved call-out players and unchanged 4000 ms
actions. Debug/test and release builds use debug level 1, no incremental
compilation, two Cargo jobs and serial tests; release retains thin LTO.
Upstream source review is separate from execution; no upstream driver was run.

The original local tokenizer completes all measured optimized ordinary-input
cases within budget. The two recorded comparisons put near-limit unquoted
snippets at about 16–26 ms warm, and quoted snippets at 5–7 ms. First-command
measurements include lazy command loading, with the largest at 83 ms; these are
not complete startup measurements.
In debug builds, unquoted near-limit input reaches the deadline, while quoted
input completes. Isolated optimized measurements put tokenization at 16–22 ms for
8183 bytes, versus under 1 ms each for suffix extraction, warm command lookup,
pipeline preparation and indentation. Length scaling and native source review
identify repeated string indexing as the expensive path: each index copies the
string and walks to the requested character.

A marked one-file diagnostic control caches each character in the quoted and
unquoted tokenizer loops. Warm optimized near-limit unquoted dispatch falls to
about 3–6 ms; all nine large control dispatches complete in debug too, under
1.2 seconds. Thirty-three token/spacing cases match the original exactly,
including operators, escapes, unfinished quotes, tabs/newlines and Unicode.
Refusal still precedes broadcast, ordinary source text reaches self/room intact,
and account/modal state and scratch-file absence remain intact. Recommend the
local character cache; it is not yet adopted. No native API or budget change.

Five tests/eight observations pass in each build, alongside four focused
compiles and four quality gates. An optimized tokenizer-only profile excludes
bootstrap; 62 of its 67 samples include the native indexing handler, with 27 in
character traversal. The short capture supports component attribution rather
than a precise system-wide percentage. All 209 predecessor artifact/report sets
and active/native/original sources remain intact. Expected debug deadline
observations remain visible rather than being treated as command success.
Optimized timings establish these input shapes, not every near-limit input.
The multiline size case enters the real modal dispatcher directly and does not
establish network multiline framing. Incoming pipelines, full network/login
behavior, concurrent scheduling and complete startup remain separate. Frozen
historical harnesses and the full native suite were not rerun.

#### Lima editor account and codesay repair

The 3 October 2026 local port changes four mudlib files over the audited editor
command overlay. The [account setter][lima-editor-account] now refuses unsupported
setup codes before changing memory or saving: zero selects defaults; nonzero
codes fit in one byte and have shiftwidth 1–15. An immediate save_me exception
restores the previous value and propagates for session feedback. Ordinary saves,
fresh-user restore, reentry, independent accounts, restrictions and outer rollback
retain their behavior. Its public caller policy and private master hooks stay
unchanged; commit-time I/O recovery remains unchecked.

[Codesay][lima-ed-codesay] adds protected raw CMD dispatch and M_ED inheritance.
The module's protected indent_code helper shares the existing I indenter using
local lines/width, preserving live editor input, marks, search, settings and files.
It retains physical lines/final-newline presence and literal/comment/directive/
text-block payloads while normalizing leading indentation. Inline code remains
inline. Input/output use the module's 8192 UTF-8 byte, 2048-byte line, 1000-line
and 64-level nesting bounds; widths are 1–15 and codesay keeps default four.

Codesay formats in memory, validates delivery size including headers, and gives
refusal feedback before broadcasting. It uses no scratch file, account save or
editor session. The [user receiver][lima-user-messages] implements the existing
TREAT_AS_BLOB flag as literal delivery, bypassing wrapping, colour substitution,
simplify formatting and emoji replacement. Spaces, Unicode and colour-marker
source text survive self/room delivery. Prompt newline and optional debug-prefix
behavior remain, as does ordinary message formatting. Command caller/resend/
visibility checks and raw-operator behavior stay active. No native API changes.

Twenty-eight consumer cases, 101 module cases and 12 handoff regressions pass,
with 141 observations, five focused compiles and four quality gates. All 208
predecessor artifact/report sets and native/original/prior sources remain intact.
The module suite includes 96 copied scenarios and five helper cases; corrupt
legacy account setup is restored from disk for its fallback scenario. Pinned
source review is separate from native execution; no upstream driver was run.

The initial full run hit the unchanged 4000 ms deadline on a near-limit snippet
while finding/loading codesay, before formatting. Its message-size guard passes
through a marked direct command observer with the same budget; ordinary-input
cases separately cover dispatch and delivery. Near-limit shell performance,
guest/full login, reconnect/disconnect teardown, incoming pipelines, command
reload, concurrent scheduling, commit-time I/O failure and complete startup
remain separate. Older frozen harnesses and the full native suite were not rerun.

#### Lima editor accounts and codesay audit

The 3 October 2026 audit exercises real [user editor settings][lima-editor-account]
through the repaired wizard session. Saved packed flags/shiftwidth reach the
account file, restore into a fresh user and return on editor reentry. Unsaved
options and scroll remain session-local; two users save independent accounts,
restricted saving is refused, and uncaught outer errors roll back value and save.
Private master setup hooks are not public setters; M_ED's owner methods suffice.

The existing public setter accepts invalid integers and foreign-user context.
Invalid persisted codes refuse startup without a stale lock and can be reset to
zero. An injected immediate save_me exception is caught by ED after the setter
changes memory, leaving the old disk value. Commit-time I/O failure is unchecked.

Active [codesay][lima-ed-codesay] retains four CMD errors; dispatch-only control
reveals three unresolved editor calls. A diagnostic one-file M_ED binding then
executes the original main through real wizard input. Simple indentation, real
self/room delivery, raw operators, caller/visibility guards and isolation from
another user's wizard editor work. No control is adopted or codesay activated.

Blind punctuation splitting changes string/character literals and line comments,
then broadcasts after ignoring indentation diagnostics. Overlong-line refusal
follows scratch overwrite without useful feedback. Sequential users share the
retained fixed scratch path; concurrent scheduling is unchecked. Ordinary
[user message wrapping][lima-user-messages] also collapses indentation and spaces
inside literals. NO_ANSI plus NO_WRAP preserves spaces but strips literal colour
markers. The declared TREAT_AS_BLOB flag has no active user-receiver behavior.

Twenty-six tests produce 30 passing observations, including marked dispatch,
backend, immediate-save and message-flag controls. Five focused compiles, locked
warning-free workspace Clippy, formatting and preservation pass with 207 prior
evidence sets intact. Native/original/active sources, master policy and configuration
remain unchanged. Lima uses distinct nil, preserved players and 4000 ms actions;
pinned source review and lpc-rs execution are separate from upstream execution.

Next validate account setup values and restore the prior value on immediate save
failure. Bind codesay to bounded in-memory indentation preserving physical lines
and literal/comment payloads, with explicit refusal feedback; inline snippets may
stay inline initially. Honor TREAT_AS_BLOB for literal message delivery. These
mudlib repairs need no native API. Caller-ownership policy, guest/full login,
reconnect, command reload, incoming pipelines and full startup remain separate.

#### Lima wizard editor command repair

The 3 October 2026 local port activates the [wizard `ed` caller][lima-ed-command]
over the repaired session with one command-file change. Protected dispatch uses
CMD's disabled-routing argument convention; main remains private and nomask.
The existing wizard command rules, caller/resend guards, canonical paths and
native file authorization remain active.

The command captures the initiating user and releases only its matching filename
lock. Both session completion and cancellation use that callback. Caught
clone/start failure retires any surviving session, unlocks and reports the error;
current file changes only after successful startup. Failure of that assignment
also retires the already-started editor. EOF/removal and cancellation from another
user's action now unlock correctly while preserving unrelated editing sessions.
Backend limits and permission refusal remain unchanged.

Creation disables [CMD stdio routing][lima-command-stdio]. Reusing the shell's
tokenizer refuses unquoted pipes/input/output/append in the editor suffix before
file mutation or downstream execution. Same-file redirection preserves its
source, and a downstream real tell sends no message. Quoted routing characters
remain usable in filenames; existing escaped arguments retain their backslash.
Incoming pipelines are not established by these checks.

Twenty-four command tests produce 33 passing observations; twelve unchanged
text/file/news handoff regressions retain exactly their preceding observations.
Marked controls force backend load, late session output and current-file setter
failures. The command, wish, session and complete user compile, with the command's
four prior CMD errors resolved. Warning-free locked workspace Clippy, formatting
and preservation pass with all 206 predecessor evidence sets intact. Lima uses
distinct nil, preserved call-out players and 4000 ms actions; no native API or
master-policy change is needed. Pinned upstream source review remains separate
from these lpc-rs executions.

Next audit account editor settings/persistence and codesay consumers. Full network
disconnect behavior, command reload during editing, commit-time I/O failure and
full startup remain unestablished. The audit below describes the preceding
unrepaired overlay and dispatch-only diagnostic control.

#### Lima wizard editor command audit

The 3 October 2026 audit tests the [wizard `ed` caller][lima-ed-command] over the
repaired session. The active command retains four missing `dispatch_main`
errors. A diagnostic control adds only the protected CMD adapter and exercises
ordinary wizard-shell input with real command rules, users, security and M_ED.
It does not activate or repair the command in the active overlay.

With dispatch alone, Unicode/blank-line editing, quoted filenames, new-file
creation, current-file reuse and normal `q`/`Q` lock release work. Two wizards
share canonical-path locks correctly until normal completion. Native permission
refusal preserves the file and active session; caller/resend/handler guards hold.
An uncaught outer action failure rolls back command/session state and output.

Directory, setup, backend-size and clone/load refusals leave a stale filename
lock after the session retires. Startup also changes current file before success,
and modal input discards the error without command feedback. The command's
1,000,000-byte guard does not cover the module's 8192-byte/1000-line caps. EOF and
external removal release session input/flag/clone state but retain the caller's
lock because it does not supply cancellation. A second wizard can remain denied
with no live edit.

[CMD stdio][lima-command-stdio] initializes routing before dispatch. In the
diagnostic control, `ed file > file` removes the source and leaves a stale lock
with no editor running. Native metadata observes pending removal while directory
listing still reads disk; source review explains M_ED's startup refusal through
that visibility difference, separately from the executed file-removal result.
A real downstream `tell` in `ed file | tell ...` runs while editing remains active.
These results require a routing policy before interactive command activation.

Nineteen new command cases, twelve unchanged session/text/news handoff regressions
and an isolated redirect follow-up pass their observed expectations. One command
and one handoff case force backend load refusal using the old session. Clippy,
formatting, four focused compiles and preservation pass, with 205 predecessor
evidence sets intact. The active library/configuration, native sources and
master policy remain unchanged. Distinct nil, preserved players and 4000 ms
remain the target; pinned Lima source review is not upstream execution or complete
wizard-editor compatibility.

Next coordinate protected dispatch, owner-checked lock release, completion/
cancellation and caught clone/start failure with feedback. Set current file after
successful startup and block interactive routing before stdio mutations or
downstream execution. Preserve command rules, quoted paths, backend caps and
native file authorization. Account/codesay activation, network disconnects,
command reload during editing, commit-time I/O failure and full startup remain open.

#### Lima editor session repair

The 3 October 2026 port binds inherited M_ED to
[ED_SESSION][lima-ed-session], corrects the setup getter/setter types and uses
bare input/prompt callbacks. Ordinary calls to unshadowed native functions let
the module load through the unchanged Lima master; no override-policy allowance
is added. Native file authorization and parsed editor restrictions remain active.

Single-entry startup validates the backend before publishing input/flag state.
Idempotent termination releases owned state before callbacks, retires despite
callback errors and preserves an editor started by a parent or completion
callback. A separate optional cancellation callback handles EOF, failed startup,
removal and abandoned-session cleanup without importing an unfinished draft.
Automatic cleanup catches callback errors so teardown can commit; ordinary
uncaught action errors retain native transaction rollback.

[M_INPUT][lima-input-forwarding] clears its stored owner before forwarding
pop/removal. The [user input system][lima-inputsys] checks callback ownership and
removes the exact handler, preserving foreign handlers and notifying a parent
only when removing the top entry. Existing editing flags are borrowed for normal
LIFO nesting; arbitrary out-of-order closure of nested editors sharing that
boolean flag is unestablished.

The [text editor's][lima-text-editor] real `~e` handoff now edits and imports saved
Unicode/blank-line drafts. Cancellation releases its staged file and preserves
the parent's draft. Clone-specific filenames separate nested editors; an
existing draft path is refused without claiming or deleting it. File-mode
completion uses atomic overwrite, retaining the source and editable draft on a
permission refusal. Real news composition edits/posts through the backend and
restores reader/shell handlers. Failed startup retains usable text editing.

Ninety-six module regressions, twenty-four new session cases and twelve new
handoff cases pass. One late-startup failpoint and one forced load-refusal control
are identified separately; no recorder backend or master allowance is used.
Second-user isolation, warning-free workspace Clippy, formatting and preservation
pass, with 204 predecessor evidence sets intact. Five repaired modules/complete
user targets compile; the wizard command retains its exact four CMD errors.
Distinct nil, preserved players and 4000 ms remain unchanged. Pinned Lima source
review and lpc-rs execution do not establish upstream execution or full editor
compatibility.

The [wizard caller][lima-ed-command] still needs command dispatch and lock release
on cancellation/start failure before activation. Account/codesay consumers,
network disconnects, commit-time I/O failure and full startup remain open.
No native editor or modal-ownership API is added.

#### Lima editor session frontend audit

The 3 October 2026 audit binds inherited `M_ED` to a temporary
[ED_SESSION][lima-ed-session] and corrects its setup getter to `int`; a second
control uses bare input/prompt callbacks. The active overlay retains the
fallback and its exact seven session compile errors. No native API or frontend
repair is adopted.

LPC-triggered loading also exposes a module policy blocker: the
[master's][lima-master] `valid_override` rejects its explicit `efun::` calls.
That refusal leaves no session, handler or flag changes. CLI compile-only
success does not test this hook. Ten lifecycle controls narrowly allow those
calls from M_ED in a temporary master while retaining runtime file permissions;
they do not establish production activation or use a recorder backend.

With the real engine bound, legacy callbacks return functions instead of
calling the input/prompt methods. Directory or saved-setup startup refusals
leave a handler, flag and inactive clone; repeated entry installs a duplicate
handler before the backend refuses it. Input-stack teardown destroys the
session but retains its owned editing flag. Completion errors leave a live
inactive clone after popping input and clearing the flag.

The [input system][lima-inputsys] invokes the parent's return callback during
pop, before session flag cleanup; a throwing parent leaves the flag and clone
live. An injected out-of-order close shows that
[M_INPUT][lima-input-forwarding] removes the top handler even when another
object owns it. This is an ownership control, not normal shell delivery to a
covered handler. Clearing the old flag before completion correctly preserves
a new editor started by that callback and should survive the repair.

The first-character warning filter also warns on literal append text and still
forwards current-file writes. Parsed module restrictions refuse an addressed
write to another file; restricted handoff saves to the original staged file
remain intentional. The [wizard command][lima-ed-command] releases filename
locks only through completion. Lock retention after caught startup failure or
EOF is a source inference; actual command execution remains blocked by four
CMD dependency errors.

Eleven new control cases and six unchanged real fallback regressions pass.
Clippy, formatting, exact focused compile diagnostics and preservation pass,
with 203 predecessor evidence sets intact. Distinct nil, preserved players and
4000 ms remain unchanged. Next repair module compile policy, setup signatures,
bare callbacks, single-entry startup and idempotent owned-state termination,
including callback errors and targeted handler removal. Validate wizard-lock
cleanup and real handoff before consumer activation. Source review of the
pinned Lima files is not upstream execution or full editor compatibility.

#### Lima LPC indentation

The 3 October 2026 port implements whole-buffer `I` in inherited `M_ED`;
`set shiftwidth 1..15` now controls real indentation. Blocks and unbraced control
bodies use that many spaces, continuations use half with a minimum of one, and
switch labels align with the switch block. A structural lexer/parser handles
LPC array/mapping/closure delimiters, native closure statements, nested if/else,
loops, classes and catch blocks. It preserves token spelling and physical lines
without compiling or expanding macros. No native editor API is added.

Only leading whitespace changes; ordinary blank lines become empty. Multiline
literal/comment continuation lines, directives and their continuations, and
legacy text-block payload/terminators are retained exactly. The latter is editor
preservation, not added compiler syntax. Malformed or unfinished input, ambiguous
conditional delimiter branches, excessive parser nesting and growth beyond the
existing line/buffer caps are refused before publishing text. Cursor and marks
survive; dirty changes only when indentation changes. Input autoindent remains
unavailable, and no byte-for-byte match to upstream formatting is claimed.

Ninety-six actual-module cases pass, including 30 new cases and 1000-line
formatting under 4000 ms; five formatted native examples compile. Six real Lima
fallback regressions and Clippy/formatting/preservation gates pass, with 202
predecessor evidence sets intact. Full frontend/account/codesay integration is
unestablished. The module compiles while exact session/CMD blockers remain.
Consumers still use the fallback. The next work is frontend callback/type/
restriction/cleanup and wizard-lock repair, followed by consumer activation.

The pinned [editor][fluffos-object-editor] and [codesay][lima-ed-codesay] sources
were reviewed on this date using the previously recorded modern editor build
settings. Upstream traversal mutates lines as it proceeds; the port validates a
replacement buffer before publication. Source review is not upstream execution.
Codesay's existing unqualified splitting of braces/semicolons can alter quoted
contents; the consumer still needs that separate repair.

#### Lima editor options and help

The 3 October 2026 port extends inherited `M_ED` with `set`, output options,
owner setup methods and paged help. `set number/nonumber`, `list/nolist` and
`print/noprint` affect real output; `set scroll 1..100` controls scrolling/help.
`set shiftwidth 1..15` stores the width for later LPC indentation, which remains
unsupported. Unsupported options and malformed values are refused. Global
bodies cannot change settings or enter help; input-mode text remains literal.

Startup optionally calls the owner's public `query_ed_setup()` method;
`set save` calls public `set_ed_setup(code)`, requiring both methods and an
unrestricted session. No methods means defaults and unavailable persistence.
Zero means unset; supported codes use the reviewed legacy number/list/print
bits and low four-bit width. The eightbit bit is accepted and emitted while
native strings always preserve Unicode. Other flags and invalid widths are
refused. Setup callbacks use ordinary owner methods rather than legacy private
master editor applies or a privileged simul-efun delegate. The completion
message acknowledges handing preferences to the owner, not durable delivery.
Hooks retain normal caught-error semantics for their own side effects.

`h` and focused help topics return command text. Longer help enters mode -2;
Return advances and `q` returns to command mode without exiting the editor.
Invalid help input restores the page and buffer. The last page clears help
state. Numbering/listing and automatic edit output retain the existing caps
and rollback behavior.

Sixty-six actual-module cases pass, including 20 new cases; six real Lima
fallback regressions pass. Setup tests use controlled owner callbacks and file
policy, not ED_SESSION/account integration. The module compiles while exact
session/codesay dependency blockers remain. Clippy, formatting and preservation
of 201 predecessor evidence sets pass. Native APIs and existing file policy
are unchanged. The pinned [editor source][fluffos-object-editor] and Lima setup
methods were source-reviewed on this date using the previously listed modern
editor branch settings; no upstream execution is claimed.

The local overlay changes only M_ED. Consumers still use the existing fallback;
LPC indentation and frontend callback/type/cleanup/wizard-lock repairs remain
open. No complete editor compatibility is claimed.

#### Lima editor search and substitution

The 3 October 2026 port extends the inherited `M_ED` module with `g`/`v` line
selection and `s` substitution. Unaddressed selection covers the buffer;
unaddressed substitution uses the current line. Printable punctuation delimiters
other than backslash, escaped delimiters and shared remembered patterns are
supported. Searches use the native [regex dialect and cache](efun/regmatch.md).
Replacement supports full-match `&`, captures `\1` through `\9`, literal
ampersand/backslash/delimiters and `\n`, `\r`, `\t`, `\b`. Missing optional
captures expand to empty text; nonexistent capture references are errors.
`g` replaces every occurrence per line and `p` prints changed lines. Unsupported
or duplicate flags are refused. Unicode offsets, anchors and empty matches are
handled without dropping text or repeating the boundary after a nonempty match.

Global selection permits printing, listing, line numbers, deletion and
substitution. It selects original lines once, tracks address-driven edits and
does not reselect inserted lines. Nested selection, file operations, input-mode
and exit commands are refused when executed in a global body. Whole-buffer
substitution stages a replacement buffer before changing state; simple global
deletion and substitution are batched to avoid repeated full-buffer work.
Errors, including later invalid addresses and output/byte/line limits, restore
the entire command's editor state. Existing save policy and staging semantics
remain unchanged.

Forty-six actual-module cases pass, including 24 new cases and 22 carried
forward; six real Lima fallback regressions also pass. Full 1,000-line print,
substitute, global substitute and delete operations fit the unchanged
four-second action budget in these fixtures. This is module validation, not
activation of find/grep/locate/replace or an ED_SESSION workflow. The module
compiles, while the session's seven errors and codesay's four dependency errors
remain. Clippy/formatting and preservation of 200 predecessor evidence sets
pass. No native API, upstream execution or broader performance claim is added.

The active local overlay changes only `std/modules/m_ed.c` over the core port.
Distinct nil and preserved call-out players remain the target. Setup/options,
paged help, LPC indentation and frontend lifecycle integration are still open;
`set number` in Lima's replace consumer remains unsupported until options land.

#### Lima inherited editor core

The 3 October 2026 port adds `M_ED`, an inherited mudlib module with protected
`ed_start(path = "", restricted = 0, scroll = 20)`, `ed_cmd(command)` and
`query_ed_mode()` methods. Buffers, cursor, marks and options are private,
`nosave` state on each owner. The active local overlay adds this module and its
header macro; Lima's six consumers do not inherit it yet. No native editor efun
is added, and the existing text-editor fallback remains active.

The core supports append/insert/change modes, printing/listing/numbering,
delete/join/move/copy, marks, file load/read/name, overwrite/append, scroll and
save/quit. Addresses include numbers, current/last line, offsets, marks,
comma/semicolon ranges, whole-buffer `%` and wrapped forward/backward regex
searches with remembered patterns. Searches use the native
[regular-expression dialect](efun/regmatch.md). Command mode is 0, inactive is
-1 and input mode reports the next positive line number. Invalid commands
restore editor state; unsupported commands return errors.

Limits are explicit: 8,192 UTF-8 bytes including each line's LF, 1,000 lines,
2,048 bytes per line and 1,024 bytes per command or filename. Over-limit edits
are refused without truncation. Files normalize the final line to LF.
Restrictions are checked after address parsing; input-mode text is literal.
File calls bind native efuns directly, retaining the owning object and defining
module for the normal file-policy hooks. Because
[`file_size`](efun/file_size.md) reports -1 for both denial and absence, accepting
a new file also requires an authorized [`get_dir`](efun/get_dir.md) check of the
literal filename. Denied metadata on an existing file is not accepted as empty.

Saves use [`write_file`](efun/write_file.md) replacement without preliminary
removal and report "Staged" rather than durable delivery. Full overwrite of
the selected file clears the modified flag; partial writes and append preserve
it. Permission refusal preserves the buffer and original file. Uncaught task
errors discard editor mutations and file effects together.

Twenty-two new tests execute the actual module with controlled file policies;
six existing real Lima fallback cases pass against the new overlay. These
validate the core and fallback separately, without an editor frontend workflow.
Clippy and formatting pass. The module compiles; ED_SESSION retains its seven
errors and codesay its four command-dependency errors. Original Lima, previous
overlays, native implementation and 199 predecessor evidence sets are preserved.
Lima keeps distinct nil, preserved call-out players and four-second actions.

Global/inverse selection, substitution, setup/options, paged help and real LPC
indentation remain unfinished; `I` errors rather than acting as a no-op.
Session callbacks/types/cleanup and wizard locking still need integration.
Full editor compatibility and upstream execution are not claimed.

#### Lima editor save foundation

The 3 October 2026 change adds general
[`write_file(path, text, 1)`](efun/write_file.md) whole-file replacement.
Mode 0/default still appends; other modes are rejected. Replacement reuses
committed temporary-file writes and rename, with the owning object and
defining program retained for authorization. A caught permission refusal
records no replacement or preliminary deletion. An uncaught task error discards
the pending write, and same-task reads observe it before commit. Commit-time
I/O failures still go to the debug log; this is not a durable save acknowledgment.

[FluffOS file source][fluffos-file-write-flags] at the listed revision was
reviewed on this date with the core efun enabled; no upstream execution is
claimed. Its bit 1 selects overwrite and bit 2 selects compression. Native
support covers uncompressed mode 0 or 1 only. The existing Lima `codesay`
overwrite call now matches the native signature, but that consumer remains
blocked in its command dependency before editor calls can be validated.

The active Lima overlay and four-second distinct-nil configuration remain
unchanged. This supplies the save primitive for a future inherited editor
module; it does not implement the editor APIs, command grammar or indentation.

#### Lima editor backend scope

The 3 October 2026 audit finds six Lima consumers with 26 calls to `ed_start`,
`ed_cmd` and `query_ed_mode`. They need interactive editing, global searches,
substitutions, file writes, numbering and LPC indentation. This is a full
editor-engine scope; a command recorder cannot provide it.

Pinned [FluffOS editor source][fluffos-object-editor] at
`44532e0228f34be2184c701a361df4079371b8b0` is reviewed with `OLD_ED` disabled,
`RECEIVE_ED` disabled, four-space indentation and `ED_INDENT_CASE` disabled.
The checked-in defaults enable `OLD_ED`; no build or upstream execution is
claimed. Public retrieval failed; reviewed local files match pinned Git blobs.
The [specification][fluffos-object-editor-spec] declares three optional start
arguments; the implementation treats a two-argument second value other than 1
as scroll size. [Modes][fluffos-object-editor-mode] are command input 0,
inactive -1, help paging -2 and a positive next input-line number. Restrictions
are checked by the parsed engine, including addressed commands. Restricted
sessions still permit writing/exiting through their selected file.

Four isolated architecture probes establish that inherited LPC globals can
hold separate buffers, inherited file calls authorize the owning object and
defining module, and uncaught errors roll back buffer and file effects together.
A caught write refusal after `rm()` commits the earlier deletion of the file.
These prove runtime building blocks, not a Lima editor or command grammar.

The audit recommended general overwrite support for
[write_file](efun/write_file.md), reusing the existing committed temporary-file
replacement effect; the [save foundation above](#lima-editor-save-foundation)
now provides it. [FluffOS write flags][fluffos-file-write-flags] include
replace and compression bits; only uncompressed replacement is recommended
here. Then a real inherited mudlib module can provide the editor API, grammar
and state without new native editor efuns. Existing deferred I/O failure
reporting remains; replacement does not imply a durable save acknowledgment.
Session callbacks/types/restrictions/lifecycle and wizard locks need repairs.
Regex dialect, full grammar, indentation, permissions and limits require
implementation tests before editor compatibility can be claimed.

Seven focused compiles preserve the session's seven errors. Six CMD-based
consumers stop at four `dispatch_main` dependency errors each; those probes
cannot establish their later editor diagnostics. Four gates and source/hash
checks preserve native/original/active sources and 197 predecessor sets.
The handoff overlay remains active in distinct nil with preserved call-out
player and four-second actions. No engine, native API or configuration change
is adopted by this audit.

#### Lima line-editor handoff port

The 3 October 2026 port changes only the [text editor][lima-text-editor]
over the pager/editor overlay. `~e` loads the session before staging a draft.
The unsupported session now yields "Line editor unavailable; continue editing
here." while preserving the buffer, text handler and editing flag. Repeated
requests create no temporary file; normal text editing and real news posting
continue. Direct callers receive the same fallback without needing to catch
the load failure. Previously orphaned files remain untouched in this path.

When a session can load, staging removes the old file before writing the draft.
A staging failure gives feedback and removes the unused session using the
editor's existing privilege mechanism. The parent tracks the staged path and
removes it on return, completion, abort or input-stack teardown.

Fourteen cases pass: four unchanged baseline cases, six real adapted-library
cases and four labelled recorder controls. Real cases cover retries, Unicode
and blank lines, text completion/abort/teardown, file save/abort, account and
observer isolation, news posting and preservation of unowned old files.
Controls cover replacement, simulated-result return, missing staging directory
and retry, refusal to replace a directory and nested input-stack cleanup.
The recorder implements no editing and is not adopted.

Nine focused compiles pass seven targets and retain all seven [session][lima-ed-session]
errors in both baseline and adopted libraries. Four gates, exact one-file patch
replay and preservation checks cover original/native/baseline sources and 196
predecessor sets. Distinct nil, preserved call-out player and four-second actions
remain the target; no native API or upstream driver execution changes.

A real backend still needs separate scope for editing commands, buffers, modes,
setup and file authorization. Standalone session callback/type/restriction/lifecycle
repairs, backend-start errors, cleanup I/O failures, network disconnect,
simultaneous editors, wizard commands and direct API consumers remain open.

#### Lima line-editor session and handoff audit

The 3 October 2026 audit reviews [ED_SESSION][lima-ed-session] and the
[text editor's][lima-text-editor] `~e` handoff over the pager/editor port.
The active session fails compilation: `ed_start`, `ed_cmd` and `query_ed_mode`
are absent, and `query_ed_setup()` declares void while returning a value.
The text editor itself still compiles. A real line-editor backend remains
unsupported.

Entering `~e` stages the draft before loading the session. The input dispatcher
catches the load failure, preserving the draft and text editor handler, but
leaves the temporary file. Retrying appends another copy of the draft.
Aborting or completing text entry leaves that temporary file behind. A real
news post can still finish in text mode after the failed handoff. If the error
escapes the task instead of being caught, temporary staging, output and state
roll back; account bytes and a second user's state remain unchanged.

Two separate wrapper controls provide a test-only command recorder and an
integer setup getter. One retains legacy callbacks; the other changes input
and prompt callbacks to bare function values. The recorder supplies selected
mode/exit/error responses; it does not parse editor commands or edit files.
Its results establish wrapper behavior, not line-editor compatibility.

Legacy callbacks do not dispatch quit and return a function as the prompt.
Bare callbacks establish controlled completion, setup forwarding and prompt
selection. The restriction wrapper prints "Operation not allowed" for seven
leading command letters but then forwards the commands to the backend.
Whitespace and address prefixes also bypass that warning. This establishes
forwarding defects; no actual unauthorized file operation is executed.
Standalone input teardown destroys the session while leaving its owned
editing flag set. Startup errors leave its object, handler and flag live;
continuation errors pop input and clear the flag but leave the session alive.

A controlled `~e` return imports an externally simulated file result, removes
the temporary file and restores the parent text editor's handler/flag before
normal completion. This exercises actual handoff callbacks and native files
without claiming that the diagnostic backend performs editing.

Thirteen cases pass: four active-library cases and nine wrapper controls.
Seven focused compiles preserve all seven active session errors and pass the
text editor, both controlled sessions and complete diagnostic user. Preliminary
controls also retain the setup return-type failure before its diagnostic fix.
Four gates and exact diagnostic replay preserve original/native sources,
the active overlay/configuration and 195 predecessor artifact/report sets.
Distinct nil, preserved call-out players and four-second actions remain the
target; no native suite, broad compile survey or upstream driver is run.

The recommended next patch repairs failed-handoff staging, cleanup and fallback
feedback while keeping text entry usable. A real backend and session repairs
need separate scope: bare callbacks, integer setup getter, command restrictions,
owned-flag cleanup and destruction after startup/completion failures. The
[wizard ed command][lima-ed-command] also uses session completion to unlock
files. Pinned source review finds direct editor API consumers in
[codesay][lima-ed-codesay], [find][lima-ed-find], [grep][lima-ed-grep],
[locate][lima-ed-locate] and [replace][lima-ed-replace]; their adaptation and
lock lifecycle remain separate.
Real editing commands, file permissions, save failures and network disconnect
remain unestablished. The test backend is not an adopted compatibility shim.

#### Lima pager and text editor port

The 3 October 2026 port changes only [the pager][lima-pager] and
[text editor][lima-text-editor] over the news reader port. It adopts bare
input/prompt/completion callbacks, explicit nil for typed empty state and
[`regmatch`](efun/regmatch.md) for scalar regex searches. Both tools now run
through Lima's existing modal input system.
[Function values](lpc/function_type.md).

Explicit file-edit abort preserves the original file and passes nil to the
continuation. Successful file saves remove the old file and write the buffer;
empty buffers save empty content rather than a display placeholder. File
loading removes only the terminal delimiter element retained by native
[`explode`](efun/explode.md), preserving real blank lines. LF-terminated files
save unchanged; a final unterminated line retains the editor's existing
normalization to a linefeed. Empty pager files take the empty-content branch
and finish normally.

Completion clears the editor's owned flag and removes the modal handler before
calling the continuation. Both tools catch callback errors, destroy themselves
and propagate the original error, preserving a live tool caller and the player
context during the callback. A continuation can start another editor without
losing its new handler or flag. Initial pager read errors also remove its
handler and destroy the pager before propagating, including when the caller
catches the error. Initial inline completion must not pop the shell a second
time if its continuation fails.

Input-stack teardown has already removed each handler before passing `-1`.
The editor clears only the flag it owns, destroys itself and returns, without
popping another handler or calling its continuation. Pager teardown retains
its silent destruction behavior. This validates actual stack clearing, not
network disconnect.

Twenty-one cases pass: ten unchanged audit observations, five repaired
regression cases and six additional file/lifecycle cases. Real shell/news
posting, long reading, followups and aborts run through the tools. Tests also
check six file-content forms, caught errors, callback context, chained editing,
flag ownership, observer isolation and uncaught-entry rollback. Six focused
compiles pass both adapted tools and the complete user/reader, retaining the
preceding pager compile refusal and editor compile success as controls.
Four gates, exact two-file patch replay and preservation checks pass for
original/native sources, the previous overlay and 194 predecessor artifact/
report sets. The active configuration changes only the library path.

Distinct nil, preserved call-out players and four-second actions remain the
target. Pinned source review is reused without upstream execution or a new
fetch; no native API or broad compile survey is added. Line-editor integration,
wizard file escapes, simultaneous nested editing, large files, mail, other
continuation consumers, save-write failures, actual disconnect and broad
startup remain unestablished.

#### Lima pager and text editor audit

The 3 October 2026 audit reviews pinned [pager][lima-pager],
[editor][lima-text-editor], [output helpers][lima-more-helpers] and
[input stack][lima-inputsys] sources over the news reader port. The active
library remains unchanged. A separate compile diagnostic replaces four typed
zero assignments with nil and adapts scalar search to
[`regmatch`](efun/regmatch.md). A second diagnostic also uses bare input,
prompt and editor-completion function values. No repair is adopted yet.
[Function values](lpc/function_type.md).

The active pager fails compilation. Once it compiles, its legacy input and
prompt literals still return function values rather than invoking callbacks:
quit does not complete and prompt processing errors. The active editor compiles
but ignores text, completion and abort inputs. Bare callbacks establish normal
paging, forward/backward search, file navigation and text editing. Both nil
and integer zero work as initial editor text in the configured distinct-nil
mode; this does not require a second null mode or another adaptation.

The diagnostic tools expose additional mudlib defects. Aborting a file edit
deletes its original file. Saving unchanged LF-terminated text adds a blank
line because native [`explode`](efun/explode.md) retains the terminal empty
element and the editor appends a linefeed. Empty file paging displays a blank
line because empty strings are true. Continuation errors remove modal handlers
but prevent tool destruction. An initial directory-read error is caught by
the pager constructor, which returns with a live, detached pager.

Clearing the actual input stack removes the editor handler before delivering
`-1`. The editor destructs without clearing its editing flag or returning
before indexing that integer. This is input teardown, not an executed network
disconnect. Preserve its silent teardown policy while clearing the flag,
destroying the editor and returning; do not pop an already removed handler.
The [user switch][lima-user-switch] uses this reset before changing bodies.

Fifteen audit cases pass, retaining failures as assertions. Real shell/news
input posts a sixty-line article, pages it, creates a followup, aborts another
post and restores the shell. Uncaught entry errors roll back tool objects,
handlers, flags, output and account bytes, with a second user unchanged.
Eight focused compiles preserve the active pager failure and pass the active
editor, both diagnostic variants and complete diagnostic user/reader.
Four gates and exact diagnostic verification preserve original/native
sources, the active overlay and 193 predecessor artifact/report sets.

The recommended repair stays in the two tool files: adapt callbacks/types and
scalar regex matching, preserve files on abort, handle empty/trailing-delimiter
file content and guarantee error/teardown cleanup. No native API is needed.
Distinct nil, preserved call-out players and four-second actions remain the
target. Pinned local source review is separate from execution; no upstream
driver or new public fetch is claimed. Line-editor integration, wizard file
escapes, nested editing, large files, mail, other continuation consumers,
actual disconnect and broad startup remain unestablished.

#### Lima news command, reader callbacks and read state

The 3 October 2026 reader port changes [news dispatch][lima-news-command],
[the reader][lima-news-reader], [user read state][lima-news-user-state] and
[the numeric-set helper][lima-news-number-sets] over the daemon persistence
port. Protected dispatch reaches the command's private entry. Callback values
use bare names or `&name(arguments)`; message-mode search also binds its mode.
The daemon, production access policy and native driver API are unchanged.
[Function values](lpc/function_type.md).

Build unread IDs from active articles minus saved read IDs, excluding removed
articles and holes in the ID sequence. Queue refresh follows the selected
thread or numeric order, retains queued read history and drops removed entries.
Refreshing active messages also clears the other groups’ stale unread caches.
Threading changes refresh that queue and save the preference. Group listing
must not change the selected group; catch-up must refresh the unread cache.

Missing user state returns empty group arrays and an empty read set. Directly
reading an unregistered group records it as unsubscribed. Legacy highwater
conversion writes back to the mapping: positive integers become read ranges,
zero becomes an empty read set, and obsolete groups are pruned. This is
schema-specific conversion after native restoration; ordinary saved zero stays
zero. [Native save format](save-format.md).

The set helper requires additional repairs: empty prefixes need explicit
handling instead of `[0..-1]`, removal must preserve comma boundaries, adding an inverted interval leaves
the set unchanged, and subtraction must preserve gaps between active ranges.
Its validator fixes an assignment in the empty-set check and checks the parsed
suffix, since native [`sscanf`](efun/sscanf.md) counts a trailing `%s` even when
it captures an empty string. Malformed read sets retain Lima's warning and
catch-up policy. The helpers remain mudlib code.

Push the reader handler once and replace it for later menu transitions.
Subscription `q` cancels the remaining signup prompts; a subsequent reader `q`
returns to the previous handler. Missing-group and initial selection cancellation
also remove the reader's handler. Real player-menu entry returns to the menu
before returning to the shell.

Forty-two cases pass: nineteen reader/set cases, fourteen retained daemon cases
and nine retained who cases. The retained assertions and normalized observations
are unchanged. Tests exercise actual user input, short `more` output, read-state
saves and fresh-VM restoration, callback execution, sparse numeric-set checks,
new arrivals at queue end, observer isolation and uncaught-error rollback.
Four gates and exact four-file patch verification preserve original/native
sources, prior overlays and 192 predecessor artifact/report sets.

Focused compiles pass the command, reader, complete user, set helper, daemon
and who while preserving the preceding command/reader failures. The pager still
fails compilation; the editor compiles, but its interactive workflow remains
separate work. Distinct nil, preserved call-out players and four-second actions
remain the target. Pinned
local source review is reused without upstream execution or a new fetch;
no native suite or broad compile survey is rerun. Long paging, editor/mail
flows, move callbacks, EOF/disconnect cleanup, the reader's unforwarded check
flag, existing-installation conversion and broad startup remain unestablished.

#### Lima news daemon startup and native persistence

The 3 October 2026 daemon port changes only [news_d][lima-news-daemon] over the
who overlay. It adopts the audited native serialization/type/member syntax and
thread comparator, with integer thread-list results. Saves now pass bound efun
pointers through Lima's real access wrapper. Snapshots checkpoint the full
daemon; journal replacement records posts, replies, group changes, removals and
header edits without rewriting the snapshot on each post.

Check journal size before reading: missing and zero-byte files initialize empty
deltas, while nonempty files must restore to a mapping. Replay and tombstone
pruning traverse arrays of keys, so removal cannot shift the remaining mapping
iteration. After replay, initialize empty journals for every surviving group,
rebuild threads and checkpoint. An edited article must be added to its journal
even when it was restored from an earlier snapshot. Native
[`save_value`](efun/save_value.md), [`restore_value`](efun/restore_value.md),
[`save_object`](efun/save_object.md) and [function values](lpc/function_type.md)
provide these contracts without a new driver API.

Fourteen news cases cover real native files, fresh-VM snapshot/journal replay,
continued IDs, followups, nil-body tombstones, group changes, header edits,
empty/sparse journals, malformed-input refusal, direct-write policy and
uncaught-error rollback of live state, files, loading and output. Nine retained
who cases keep their assertions and observations. Privileged group operations
use test-only user callbacks through actual `unguarded`; no production security
policy or service is replaced. No constructor control is needed.

Six focused compiles preserve the preceding daemon and unchanged command/reader
blockers while passing the adapted daemon, complete user and who. Four gates and
exact patch verification preserve native/original sources, previous overlays and
191 predecessor artifact/report sets. Distinct nil, preserved call-out players
and four-second actions remain the target. Frozen pinned source evidence is
reused without new fetches or upstream execution; no native suite or broad
compile survey is rerun. The native references and overview contracts are unchanged.

The files use the [native save format](save-format.md); existing-installation
conversion remains separate work. Snapshots and journals are separate files,
without tested recovery from an OS crash or delivery failure between writes.
The listed malformed inputs do not establish complete schema validation.
Archive/mail forwarding, wider restriction/thread policy and broad startup
remain unchecked. Port news dispatch, reader callbacks, subscriptions, unread
sets and legacy highwater updates next; this daemon port does not establish
a functioning news UI.

#### Lima news reader, daemon and saved data

The 2 October 2026 audit reviews the pinned [reader][lima-news-reader],
[daemon][lima-news-daemon] and [user news state][lima-newsdata] on the unchanged
who overlay. A separate three-file compile diagnostic adds protected news
dispatch, native serialization names, explicit nil, class-member dots and
strict callback/type adaptations. It preserves the reader's unread-removal
logic and the legacy named function literals so their runtime failures remain
visible. The diagnostic is not adopted.

Compilation alone is insufficient. Cold startup raises an error for a missing
`recent` file: native [`read_file`](efun/read_file.md) does not return false for
that case. With a test-only constructor control, real posts exist in memory but
the legacy save callbacks write neither snapshot nor journal. Native
`(: name, argument :)` is a comma-expression closure, not a bound call;
use `&name(argument)` for `unguarded` callbacks and
bare names when no arguments are bound. [Function values](lpc/function_type.md).

The same literal mismatch makes the subscription predicate select unsubscribed
groups and leaves the real reader's Yes/Quit callbacks ineffective. Its unread
cache removes the active unread article, and legacy integer-highwater conversion
reassigns a loop variable without updating the stored mapping. These are
demonstrated lpc-rs source-port failures, not cross-driver execution results.

Saved data needs its own conversion boundary. This Lima tree contains no news
database. Source-reviewed FluffOS saves use a `#/` header, positional class
records and raw carriage returns for saved linefeeds. Reviewed at
`44532e0228f34be2184c701a361df4079371b8b0`, with the core save/restore facilities,
on 2 October 2026; public fetch attempts failed, and pinned local Git blobs were
reviewed without upstream execution. [Save implementation][fl-news-save-source].
Synthetic header/class samples are rejected without rewriting their files.
Native news-class values round-trip with newlines. Explicit nil is a valid body,
while zero remains zero and fails the declared string-field check. Merely renaming
the serializer does not convert those formats. [Native save format](save-format.md).

Nine runtime cases, ten focused compiles, four quality gates and exact diagnostic
reproduction preserve the original, active and native sources and 190 predecessor
artifact/report sets. Five cases use an explicit constructor control to reach
the runtime failures; it is not part of the diagnostic. All actions retain
distinct nil, preserved call-out players and four seconds. No native API or
overview contract changes. Port daemon startup and fresh native persistence
first, then reader callbacks and read-state logic. Existing-installation data
conversion requires a separate schema-aware tool; real database migration,
daemon restart/journal replay and a functioning complete reader remain untested.

#### Lima who command migration

The 2 October 2026 migration adopts the one-file [who][lima-who-command] patch
audited below. Protected dispatch, explicit nil arguments, the duration-helper
selector and actual body filtering now run in the active overlay. Disconnected
bodies remain in the wizard/admin inactive view; prototype and bodyless nil
entries are excluded. News is unchanged and still needs its reader/daemon port.

All nine runtime cases and normalized observations match the audit, with
unchanged assertions. The entire adopted library matches the frozen diagnostic
byte for byte. Three fixture/control files are copied unchanged; shared support
changes only the library selector, and the probes change two captured-output
destinations and their line wrapping. Role/visibility filtering, menu recovery,
frame-width and no-player limitations, the legacy nil failure control and
uncaught-error rollback retain their audited behavior.

Adopted who and complete user compile with the audited diagnostics. Four quality
gates and exact adoption checks pass. Configuration changes only the library
directory: distinct nil, preserved call-out players and four-second actions
remain the target. Native source, original Lima, previous overlays and 189
predecessor artifact/report sets remain unchanged. Frozen public source evidence
is reused without new fetches or upstream execution; no native suite or broad
compile survey is rerun. News reader/daemon and saved-news migration are next.

#### Lima who filtering and inactive users

The 2 October 2026 [who][lima-who-command] audit follows native elapsed uptime
support on the unchanged settings overlay. A separate diagnostic changes only
`cmds/player/who.c`: the previously reviewed command adapter, explicit nil
arguments and duration-helper selector, plus `filter(u, objectp)` before counting
and rendering bodies. The original `u -= ({0})` retains nil returned by bodyless
user objects, including the loaded user prototype. Its wizard-only `-m` view
then fails with a string-plus-nil error. The body filter removes those entries
while retaining disconnected users that still have bodies.

Nine runtime cases cover real shell and menu rendering, player aliases,
wizard/admin roles, visibility, native connection detach followed by Lima's
`net_dead`, bodyless users, empty results, repeated frame initialization,
state/account/output isolation and uncaught-error rollback of command loading
and output. Ordinary users see visible connected bodies; wizards and admins
include invisible ones. Ordinary `-m` falls back to the normal view. The pinned
source's `-i` uses the same wizard filter as `-w`; its implementor label does
not restrict it to admins. Unknown flags render the normal table, with the
computed warning unused.

Two existing mudlib limitations remain explicit. The frame renderer omits
columns that do not fit: a long name can remove the Name column at width eighty,
while width 120 restores it in these fixtures. Calling `get_who_string` without
a player fails in `frame_init_user` despite the later no-user visibility branch.
These are separate from the distinct-nil body filter. Plain mode, ASCII frames,
the default human level and empty wizard positions are the checked settings;
other client styles, level systems, positions and network lifecycle remain
unchecked.

The one-file diagnostic was ready for adoption independently of news; the
migration above adopts it. Three focused compiles retain the active who's
missing-adapter control and pass the diagnostic who and complete active user.
Four quality gates and exact patch
checks pass. Native/original/active sources and 188 predecessor artifact/report
sets remain unchanged. All new Lima actions retain distinct nil, preserved
call-out players and the four-second allowance; no upstream driver was run.

#### Lima elapsed uptime support

The native addition after the 2 October 2026 command audit supplies
[`int uptime()`](efun/uptime.md): whole monotonic seconds since this VM's state
was created. It is shared by objects within the VM, independent of object loading
and wall-clock adjustments, and read anew on each call or retry. MSSP's `UPTIME`
continues to mean the Unix timestamp of startup.

FluffOS's source-reviewed core implementation uses current time minus boot time;
lpc-rs uses its monotonic startup clock. The reviewed revision, core guards and
public sources are recorded in the command audit below. This does not establish
cross-driver execution equivalence under clock adjustments or suspension.

With the native clock, the frozen who diagnostic compiles and renders through
real shell and menu input. Three Lima cases cover the unchanged active command's
missing-adapter control, two-player listings and player-only aliases, repeated
rendering, subhour footer text, account/channel isolation and menu/shell recovery.
At the native-clock baseline the active overlay was unchanged. The audit and
migration above complete the role/visibility/linkdead/bodyless checks and adopt
the who-only patch. News still needs its reader/daemon port.

#### Lima news and who command entries

The 2 October 2026 audit reviews [news][lima-news-command] and
[who][lima-who-command] at the pinned Lima revision, on the unchanged settings
overlay after compiled regex reuse. Both need protected command dispatch adapters.
Who also needs explicit nil string arguments and `time_to_string(..., 0)`;
its helper has a required long/short selector. At that baseline, those edits left
only the missing native `uptime()` calls. No clock stub or command patch was
adopted by the audit.

FluffOS core declares `int uptime()` and subtracts driver boot time from current
time. This was source-reviewed at `44532e02` on 2 October 2026, with `PACKAGE_CORE`
and `F_UPTIME`, without upstream execution. [Declaration][fl-uptime-spec],
[implementation][fl-uptime-source], [reference][fl-uptime-reference]. At that audit
baseline lpc-rs had no LPC uptime API; its MSSP `UPTIME` was already the Unix
timestamp of VM startup. A mudlib's own load timestamp need not match driver
startup. The elapsed uptime support described above removes this native blocker.

The corrected news entry compiles but reaches four real
[reader][lima-news-reader] diagnostics: mutable value-capture syntax, an integer
used as an ID array, typed zero and a void return mismatch. The
[daemon][lima-news-daemon] has ten diagnostics involving legacy serialization
names, the restore preserve flag, typed zero and class arrow access. Use
[`save_value`](efun/save_value.md)/[`restore_value`](efun/restore_value.md), drop
restore flag `1`, use explicit nil and dot member access; persisted data needs a
separate migration audit. The user news-data module's standalone `save_me`
diagnostics are supplied by the complete user composition, which compiles.

Seven runtime cases pass for shell/menu failure handling, prompts, recovery,
caller guard, duration formatting and player/account isolation. Native
`load_object` returns nil on compile failure, leading to shell unknown-command or
channel fallback. Reader failures are caught and discarded by Lima modal input;
the menu stack and prompts survive. Successful command loading survives catch,
so these checks do not establish complete transactional rollback or working
news/who output. Completed who filtering, articles, subscriptions, persisted
news and remaining reporting/remote entries remain separate ports.

#### Lima compiled regex reuse

The 2 October 2026 native change adds runtime compilation reuse to
[`regmatch`](efun/regmatch.md) and [`regexp`](efun/regexp.md). Each VM retains
up to 64 pattern/compilation-option pairs, evicting the least recently used
entry. Capture/anchor flags, start offsets and filtering/index flags keep their
existing meaning; only compilation options distinguish cached entries. Invalid patterns
remain runtime errors. Compilation runs outside the cache lock, and concurrent
misses use the first resident success. The shared command cache helper now lives
under `util`, with command-cache behavior unchanged.

On the unchanged Lima settings overlay, complete `frames themes` actions now
finish in **950–960 ms in debug** and **91–94 ms optimized**, under the same
4000 ms allowance. The optimized audit baseline was 945–1114 ms; debug reached
the limit on all three attempts. All six new outputs match the frozen 1676-byte
table exactly, with eleven themes, unchanged state/account bytes and a usable
shell. This fixture uses plain mode, ASCII frames and width eighty. Separate
ASCII/Unicode probes retain generation samples and conversion/wrapping byte
counts for plain, ANSI and XTerm modes. No Lima source patch or configuration
change is needed.

Ten new native tests cover reuse, all compilation options, eviction, invalid
patterns, concurrent callers, efun flag separation, validation precedence and
configured nil/capture/offset results. The full workspace suite passes 4840 tests
with two ignored. All twenty-six settings cases pass; the active copy replaces
its former theme-timeout expectation with successful rendering. The other
case bodies and frozen harnesses are unchanged. The optimized profile records
zero regex-compilation frames among 194 CPU samples, compared with 86.19% in
the earlier audit; the shorter sample is evidence of removed compilation cost,
not a timing guarantee for other hosts or complete-table client combinations.

Remaining information/reporting/remote command entries and broader Lima
startup/network paths remain open. Previous artifacts, active/original sources,
distinct nil and preserved call-out players remain the port's reference.

#### Lima frame-theme listing cost

The 2 October 2026 cost audit follows settings adoption on the same pinned Lima.
The full theme table uses the real [frame module][lima-frame] and
[colour/wrapping daemon][lima-xterm]. Three debug actions reach the unchanged
four-second limit and retain rollback/recovery. Three final optimized actions
complete in 0.94–1.11 seconds with all eleven themes and identical output, using
the fixture's plain terminal, ASCII frames and width eighty. The timeout is
build-sensitive; this is not a performance guarantee for other hosts or clients.

The optimized CPU profile attributes 86.19% of samples to regex compilation.
Gradients emit tokens per frame character; wrapping strips them for visible
widths and final conversion scans them again. At the audited revision, the
association simul called
[`regmatch`](efun/regmatch.md) for every match, recompiling the same patterns.
Separate ASCII/Unicode stage probes and an isolated reuse measurement preserve
identical text pieces; the latter excludes LPC/STM and cache lookup costs.

The debug reproduction and two optimized cases pass, with six quality gates and
184 predecessor artifact/report sets verified. An initial Unicode stage fixture
used ASCII match glyphs; its failure is preserved and the fixture corrected.
Native/original/active sources and the 4000 ms budget stay unchanged. No cache,
new regex API, mudlib patch, upstream execution or public fetch is introduced.

This audit led to the compiled regex reuse described above, preserving flags,
validation/errors, Unicode/nil/capture results and suffix-anchor behavior.
Remaining information/reporting/remote entries follow afterward.

#### Lima player-menu settings migration

The 2 October 2026 migration adopts the exact eleven-file settings patch over
the menu overlay. The full adopted library matches the frozen diagnostic byte
for byte. Width, mode, simplify, emoji, metric, frames, biff and snoopable now
have protected dispatch adapters; width retains its public main handler. Bare
width uses its numeric-zero query behavior for missing input. Widgets use
explicit integer conversions and truthy preference predicates; pshell admits
simplify/metric in both allowlists. Negative widths skip both receive wrapping
paths. Native type/null semantics and APIs are unchanged.

All twenty-six case bodies, assertions and normalized observations match the
audit. Eighteen cases use the adopted library; six retain previous-library
controls and two retain historical verbs libraries. Twelve fixture/control files
are unchanged; shared support changes only the adopted directory. All thirteen
focused compiles pass with identical diagnostics. Six gates, exact adoption and
183 predecessor artifact/report sets pass.

At the settings-migration revision, complete `frames themes` rendering exceeded
the four-second evaluation limit in the LPC regex/colour-wrapping path. The
frozen timeout case retains rollback and shell recovery; the subsequent native
cache change above replaces that expectation in the active regression harness.

Configuration changes only the library directory. Distinct nil, preserved
call-out players and four-second actions remain the target. Original Lima,
native sources, previous overlays and the diagnostic remain unchanged. Frozen
source evidence is reused without new public fetches, upstream execution, a
native suite rerun, historical matrix or preload survey. Actual mail/snooping,
remote completion/network interactions, live terminal negotiation, extreme
widths, every theme, general widget arithmetic, preference persistence, full
vocabulary and cold startup remain unestablished.

#### Lima player-menu UI and personal settings

The 2 October 2026 audit follows the menu migration on the same pinned Lima.
[Width][lima-width-command], mode, simplify, emoji, metric, frames, biff and
snoopable lack protected
dispatch adapters. An eleven-file diagnostic adds them, gives bare width's nil
argument its numeric-zero query behavior, and makes eight
[widget bar/slider][lima-widgets]
counter conversions explicit with [`to_int`](efun/to_int.md). Widget preference
predicates use truthiness for absent values. The
[shell's set/unset allowlists][lima-pshell]
also need `simplify` and `metric`; their omission is a defect in this Lima source.
Negative widths add `NO_WRAP` before both [receive branches][lima-user-messages],
fixing the original
per-word newlines in no-wrap mode; a control retains that failure. Native type
checking and null semantics are unchanged.

Actual shell/menu input exercises all eight commands and six UI prompts,
personal flag choices, real translations/emoji data, frame catalogue and
style/theme selection, account saves, caller/player isolation and rollback.
Adapters stay protected; width's existing public handler retains its visibility.
Auto width uses Lima's [79-column default][lima-ui-config] in this fixture,
independently of its
saved manual width of eighty. Numeric width, auto and no-wrap modes are checked.

Complete `frames themes` rendering remains blocked at the four-second evaluation
limit in the LPC regex/colour-wrapping path. An unchanged retry confirms the
boundary; a dedicated case verifies discarded output, unchanged state/saved
bytes and shell recovery. Theme catalogue/selection and style listing are
checked separately. The diagnostic does not solve the complete theme listing.

Twenty-four new cases and two unchanged verbs controls pass, with twenty-six
observations. Eighteen cases use the diagnostic, including that budget boundary.
Twenty-five of thirty-four focused compiles pass; nine original dispatch/type
blockers remain explicit, while all staged controls and diagnostic targets
compile. Initial compile inventories and fixture corrections are preserved.
An earlier body startup crossed four seconds and passed unchanged on retry;
the action budget is preserved. Six gates, exact patch reproduction and
182 predecessor artifact/report sets pass. Distinct nil, preserved call-out
players and four-second actions remain
the target. Original Lima, native sources and active overlay stay unchanged.

Pinned local originals match Git blobs; public width/widgets fetches failed,
and frozen menu evidence is reused. No upstream execution, native suite,
historical matrix or preload survey was run. Adopt the diagnostic next, then
investigate theme-listing evaluation cost before remaining information/reporting/
remote commands. Actual mail/snooping, remote completion, live terminal negotiation,
extreme widths, every theme, general widget arithmetic, preference persistence,
full vocabulary and cold startup remain unestablished.

#### Lima in-game player menu migration

The 2 October 2026 migration applies the exact two-file menu/plmenu patch from
the interaction audit over the tell/reply overlay. The full adopted library
matches the frozen diagnostic byte for byte. The protected command adapter,
explicit nil absence checks and native bare/bound callbacks now run from the
active overlay. Generic menu, renderer, modal input and native APIs are unchanged.

All eighteen case bodies, assertions and normalized observations match the audit.
Twelve cases use the adopted library; two retain previous-library controls, one
retains the combat zero check and one uses temporary biff/snoopable adapters.
Two historical verbs controls retain frozen libraries. Eight fixture/control
files are copied unchanged; shared support changes only the adopted directory.
All six focused compiles pass with unchanged diagnostics. Six gates, exact
adoption and 181 predecessor artifact/report sets pass.

Configuration changes only the library directory. Distinct nil, preserved
call-out players and four-second actions remain the target. Original Lima,
native sources, previous overlays and the diagnostic remain unchanged. Frozen
source evidence is reused without new public fetches, upstream execution,
a native suite rerun, historical matrix or preload survey.

Next, audit downstream player-menu command entries: sixteen helpers lack the
dispatch adapter, while `emoteapropos` and `adverbs` already compile. Temporary
biff/snoopable adapters remain controls. Remote completion, full mail/reporting/
network flows, all combat settings and terminal widths/themes, full vocabulary
delivery, other SOUL consumers, logging, login and cold startup remain
unestablished; the unused completion handler retains its documented limitation.

#### Lima in-game player menu interaction

The 2 October 2026 audit follows the tell/reply migration on the same pinned Lima
revision. This is the in-game [player menu][lima-semote-menu], separate from the
account/character-selection menu. Its [command][lima-game-menu-command] lacks the
protected CMD adapter, and plmenu passes integer zero to `complete_choice`'s
string argument. An adapter/nil-only control opens and navigates, but its prompt
and quit actions remain inert under native expression-closure semantics.

The two-file diagnostic adds the adapter, uses nil for remote completion and
converts sixty callback literals to bare/bound native functions. Eleven selected
keys are bound explicitly: the generic menu's zero-prompt branch calls actions
without arguments. This preserves generic dispatch while supplying `simple_cmd`
and biff/snoopable their choices. An absent combat shell preference needs another
nil check; comparing it with zero instead falls through to `explode(nil, "")`.
The old check is reproduced, and the correction toggles the real hide-misses
setting on and off through ordinary input.

Actual shell/modal input covers rendering, help, navigation, quit, SOUL preview
and listing, and personal-info setters/saves. Preview waits for Enter and returns
without other-user delivery or changes to SOUL save bytes/channel state. Nested
menus unwind independently; cancellation recovers the shell. Edit and quit
rollback preserve saved/account/output and clone/modal state. Fake command calls,
inline arguments and foreign menu-user reassignment are refused.

The real biff/snoopable commands work with temporary dispatch adapters, proving
bound yes/no choices; those adapters are controls, not adopted ports. The UI width
prompt works, but its unported downstream command prevents the change. No stub
replaces the menu, renderer, body or commands.

Sixteen new cases and two unchanged verbs controls pass, with eighteen recorded
observations. Thirty-four focused compile outcomes retain explicit original and
downstream failures: sixteen pass, sixteen helpers lack the dispatch adapter,
and the two original menu blockers remain visible. `emoteapropos` and `adverbs`
already compile; shared dependencies are unchanged. Six gates, exact patch
reproduction and 180 predecessor artifact/report sets pass. Fixture corrections
are preserved. A previous historical control exceeded the four-second startup
limit and passed unchanged on targeted retry; the final run includes both new
combat cases. Distinct nil, preserved call-out players and four-second actions
remain the target. Original Lima, native sources and active overlay stay unchanged.

Pinned local originals match Git blobs. New public command/menu fetches failed;
frozen semote evidence is reused, without upstream execution, a native suite,
historical matrix or preload survey. Adopt the two-file patch next, then audit
downstream player-menu command entries. Remote completion, full mail/reporting/
network flows, all combat settings and terminal widths/themes remain unchecked.
The unused completion handler retains its documented uninitialized menu. Full
vocabulary delivery, other SOUL consumers, logging, login and cold startup remain
unestablished.

#### Lima file metadata

Lima's `file_length` callers test existence or non-emptiness; none needs a line
count. Use [`file_size`](efun/file_size.md) with `>= 0` for a file, `> 0` for
a nonempty file, and `== -2` for a directory. These are deliberate changes:
upstream `file_length != -1` also accepts directories, while its `> 0` test
rejects nonempty files without an LF. Its implementation counts LF bytes and
does not count an unterminated final line. It authorizes under `file_size`.
[Source][fl-file-length].

Do not globally replace `stat` with a directory listing or a synthetic tuple.
Upstream regular-file results contain byte size, mtime, visible object's load
time or zero, and `st_ctime`; the last is metadata change time on Unix, despite
the reference calling it creation time. The reviewed implementation returns
this tuple even with flag `-1`. Other paths use `get_dir` behavior: a literal
directory without a trailing slash can yield a singleton containing its name,
which explains Lima `ls`'s length-one test. Native `get_dir` lists directory
contents instead. [Implementation][fl-stat-source], [tests][fl-stat-tests],
[directory handling][fl-get-dir-source].

Use `file_size` for the method, damage and crafting config guards, rejecting
directories explicitly. In error-context formatting, also catch failures from
the size probe and [`read_file`](efun/read_file.md); metadata availability does
not guarantee readable UTF-8 contents. Omit unavailable context while retaining
the original error report.

For help topics, compare [`file_time`](efun/file_time.md) with the saved read
timestamp only when it is not `-1`. An unavailable topic must not silently
become already read. For `ls`, collect size, modification time and a loaded
flag in a local record, then update all formatters together. Use
[`find_object`](efun/find_object.md) for the loaded marker after successful
metadata checks; passing a string to [`object_time`](efun/object_time.md)
would load the object. No native creation-time field or `stat` wrapper is
required by these callers.

Native `file_size` sees pending writes and removals, while `file_time` and
`get_dir` read disk state. Combining them does not provide one coherent
metadata snapshot, especially before pending changes commit. Native permission
checks use each efun's own name; retain the caller's path and privilege context
when adapting a source operation that previously authorized as `stat`.

Lima `update -z` needs a separate lifecycle adaptation. Native `object_time`
reports creation time, which [in-place recompilation](efun/request_object_recompile.md)
preserves. It cannot also serve as the current program's last-compilation
timestamp. Choose ordinary reload versus queued in-place upgrade behavior,
inherited-program handling and completion reporting explicitly.

#### Lima master startup

Lima's [master][lima-master] and [preload list][lima-preload] were reviewed on
**24 September 2026** at `eefaff66adcae9b01e16059574c925b0d63e70a1`.
The adapted master and complete simul library were exercised on lpc-rs. That
review established the bootstrap contracts; the later
[cold startup and login smoke](#lima-cold-startup-and-login) exercises the actual
driver. Neither establishes execution on FluffOS.

File policies need the native argument order:
`(string path, string func, object caller, string program)`. Return integer
permission decisions; a returned pathname does not redirect native file access.
Preserve the master-self exemption and the security daemon's restore exemption
without treating them as general load permission. Native paths are already
canonical. Compiler includes pass nil as their explicit caller argument and
require an explicit policy. Keep the bootstrap dependency grants narrow, then
apply the [source authorization policy below](#lima-source-authorization). See
[`valid_read`](apply/master/valid_read.md),
[`valid_write`](apply/master/valid_write.md),
[`valid_load`](apply/master/valid_load.md) and
[`valid_inherit`](apply/master/valid_inherit.md).

Lima's `preload` catches `load_object` without checking its result. Native
[`load_object`](efun/load_object.md) returns the configured null on failure:
test the returned object and report the failed path. An error in one
[`preload`](apply/master/preload.md) does not prevent later entries from running.
The reviewed list's `/secure/obj/shell` typo initially needs correcting;
the active preload list uses the concrete player/wizard shells described above.

Replace the master's structured-trace formatter with the native
[`error_handler`](apply/master/error_handler.md) mapping's rendered `diagnostic`;
do not expect `program`, `file`, `line`, `trace` or a caught-error argument.
`standard_trace` can retain its mudlib call shape while returning that diagnostic.
Use `this_interactive()` for the initiating user and retain wizard-only detail.
Store the error independently of optional log, user and channel delivery, and
avoid loading services from the error handler. The driver also logs uncaught
errors independently. The old `log_error` method is not a native compiler hook.

For virtual ancestor searches, stop when the final slash is at index zero:
`path[0..idx - 1]` otherwise becomes `path[0..-1]`, which retains the whole path
under native [slice semantics](lpc/indexing.md). This termination fix does not
adapt the virtual factory contract: native
[`compile_object`](apply/master/compile_object.md) requires a blueprint path,
whereas Lima returns a configured object. Preserve constructor and instance
state when porting those factories. Factory adaptation and lifecycle paths
beyond the tested startup/login remain separate porting work.

#### Lima security initialization

Lima's [security daemon][lima-security] and [`m_access`][lima-access] were
reviewed on **24 September 2026** at
`eefaff66adcae9b01e16059574c925b0d63e70a1`. The adapted daemon, full simuls and
master were exercised together on lpc-rs with distinct nil, using Lima's
`SENSIBLE_MODIFIERS` configuration. This is native execution and upstream source
review, not cross-driver execution or a complete Lima boot.

Convert the daemon's bound save/log callbacks to native partial application:
`unguarded(1, &save_object(path))` and `&write_file(path, text)`.
`(: save_object, path :)` is a native comma-expression closure that returns
`path` without saving. Use `m_delete` for the discarded-result mapping removals,
empty mapping literals for the two capacity-only allocations, and `deep_copy`
for returned access trees. Those trees use string keys. The daemon's clone
guard can inspect the `#` suffix in `file_name(this_object())`; that check does
not authorize cloning. See [function values](lpc/function_type.md),
[`m_delete`](efun/m_delete.md), [`deep_copy`](efun/deep_copy.md), and
[`file_name`](efun/file_name.md).

Initialize both `m_access` privilege fields explicitly to integer `0`, even in
distinct-null mode. Zero is a privilege level; nullable results use `nil`.
Return an integer predicate from `query_is_wizard`, and reject non-string,
non-integer inputs in `valid_privilege`. In the hierarchy's suffix comparison,
use `strindex(suffix, prefix) == 0` alongside the existing owner and length
checks: an empty prefix makes the original slice end at `-1`, selecting the
whole suffix under native [indexing](lpc/indexing.md).

Validate all six restored mappings, nested access/privilege structures, and
both directions of domain membership before completing initialization. A
successful `restore_object` does not mean every field was present or correctly
typed. Accept explicit empty access maps and zero/nil child sentinels, but abort
on missing fields, invalid structure, or malformed save text while preserving
the primary and backup files. Create defaults only when both save paths are
absent; a backup without its primary requires deliberate recovery. Keep
initialization idempotent so a later `create()` call cannot reload live policy.
See [`restore_object`](efun/restore_object.md) and [save format](save-format.md).

The `delete_wizard` instruction-budget preflight needs no native replacement:
uncaught errors and execution timeouts abort database changes, pending saves
and log output. A caught ordinary exception retains prior changes; `catch`
does not create a rollback boundary. The two save files are separate
commit-time writes, not a crash-atomic pair. See
[`save_object`](efun/save_object.md) and [`catch`](efun/catch.md).

Initialize the daemon before ordinary preloads with narrow, explicit master
grants: the daemon source for master-originated loading, its `m_access` inherit,
and its enumerated header dependencies. Permit its bootstrap `file_size` probes
only for the two save paths, alongside the existing restore exemption. A direct
call to the daemon's `query_privilege()` loads it while preserving initialization
errors that `load_object` would turn into null; authorize that specific
`call_other` load. Once the daemon is available, use the source policy below.

#### Lima source authorization

Source authorization was reviewed on **24 September 2026** at the listed
FluffOS revision and Lima revision above; the adapted master was exercised on
lpc-rs with distinct nil on **25 September 2026**. This is upstream source
review and native execution, not cross-driver execution or a complete Lima boot.
FluffOS's [file loader][fl-source-load] and [include reader][fl-source-include]
pass the master as the caller to [file permission checks][fl-file-permission].
Lima's master-self exemption therefore permits those reads. The optional
[`inherit_program` hook][fl-inherit-grammar] preserves default behavior when
absent, as in Lima. With `PACKAGE_UIDS`, loading additionally requires an
effective user; this does not describe every build's complete access policy.

After security initialization, use Lima's source read protections for native
`valid_load`, `valid_inherit`, and `valid_read` requests whose function is
`"include"`. Check them through the real security daemon with the preserved
requester chain. This deliberately enforces read restrictions that the
upstream master exemption bypasses. Retain the actual master's startup
authority, without granting it to every object or inherited function defined
under `/secure`. An include's `program` names its immediate includer, which
can be another header; its location must not substitute for the loader's
authority. Lima's `preload` is already private and cannot be invoked by an
ordinary remote LPC call.

Shared modules such as `m_access` must remain inheritable by less-privileged
objects. Lima caps `set_privilege` and `unguarded` to the receiving object's
write protection, so inheriting code does not confer its source owner's
privilege. This policy depends on retaining object-based checks; adding grants
based solely on an inherited `program` path requires revisiting inheritance
permission. See [Lima's access module][lima-access] and
[security daemon][lima-security].

Keep [`compile_string`](efun/compile_string.md)'s identity write check separate
from source read permission. Also distinguish cold loading from runtime access:
calls to resident objects and clones of resident prototypes do not ask
`valid_load` again. Load permission does not authorize destruction or other
lifecycle operations; review those policies independently.

#### Lima daemon persistence

Lima's [`m_daemon_data` module][lima-daemon-data] was reviewed on
**26 September 2026** at the pinned Lima revision above, with
`SENSIBLE_MODIFIERS` enabled. The adapted module, real master, full simuls and
security daemon were exercised together on lpc-rs with distinct nil. This is
native execution and upstream source review, not cross-driver execution or a
complete Lima boot.

Pass `this_object()` to `file_name`, give `create` an explicit `void` result,
and use native bound calls for saving, restoring and measuring the save file.
The original comma-expression callbacks silently skip persistence: the save
callback returns a path, and the size callback returns a string whose comparison
with integer zero is false. Drop `restore_object`'s flag `1`; native restoration
already preserves omitted values. See [function values](lpc/function_type.md),
[`file_name`](efun/file_name.md), and [`restore_object`](efun/restore_object.md).

Attempt restoration before probing file size. A size-first guard mistakes a
read refusal for a missing save and can start a daemon with defaults that later
overwrite unread data. Propagate restore errors; after a false result, permit
defaults only when the suffixed file probe returns `-1`, and reject directories
or other filesystem failures. This depends on consistent read policy for the
unsuffixed save name and suffixed probe. Missing and empty saves retain defaults;
malformed saves must abort initialization. See [`file_size`](efun/file_size.md)
and the [restoration contract](efun/restore_object.md).

An ordinary native `file_size` probe reads metadata without opening the contents;
pending changes to that path use the task's file view. A nonnegative size does
not prove the save's contents can be read, so retain the restore-first ordering.

Make `lima_save` private and `nosave`, and `set_lima_save` protected. The routing
choice belongs to constructor configuration: saved values must not redirect
later writes, and remote callers must not toggle it. On a mud named `LIMA`,
the default `/data/lima/daemons` write protection is stricter than the ordinary
daemon privilege. Configure an explicit directory grant when using that root;
retain the object's privilege ceiling. Save parent directories must exist.
Prototype names strip a terminal `_d`; clone names retain `_d#N` and use separate
save paths. See [Lima's security policy][lima-security] and
[`save_object`](efun/save_object.md).

All ten bundled saves have incompatible headers. Four (`alias`, `body`, `guild`
and `money`) also contain positional classes that need schema-aware migration.
The apparent class marker in `soul.o` is quoted text; that file instead exposes
the newline-encoding difference. Decode saved strings using the source format
and encode them in the native format, preserving escaped literal carriage
returns. Normalizing whole-file line endings can split a saved record. Parsing
success does not validate a daemon's state or prove complete data equivalence.
Keep schema checks with the concrete daemon and preserve original saves until
migration is validated. [Bundled data][lima-data], [source string codec][fl-object-persistence],
[native format](save-format.md).

Preserve saved numeric zeros explicitly when migrating. The two bundled guild
records contain ten zero-valued fields declared `string *`. Keeping those zeros
makes their native class records invalid in the Lima port's distinct nil
configuration. Loading them requires a deliberate schema or value conversion;
the migration must not silently reinterpret these zeros as `nil`. The other
nine bundled saves can be converted without this class-field conflict. The
reviewed migration retains existing content anomalies, including invalid dates
and inconsistent guild keys, for separate data cleanup. [Guild schema][lima-guild],
[class save validation](save-format.md#class-records).

#### Lima mapping helper callers

The reviewed `map_delete` callers discard their results, so they can use
`m_delete`; replace `mapp` with `mappingp` as well, including inherited modules.
Lima's [crafting daemon][lima-crafting] can count its material lists with
`map(materials, (: sizeof($2) :))`, preserving the input lists. Its
[party daemon][lima-party] uses `map_mapping` only to populate an inverse
mapping; use `foreach (mixed key, mixed value in map) new_map[value] = key`.
Duplicate values still collapse to one key, but the last inserted source entry
now wins. Party access-level ties therefore need an explicit policy if the
selected member matters. The [user news module][lima-newsdata] can likewise
remove obsolete groups with a `foreach` loop instead of collecting unused
callback results. Whole-daemon validation must still check saved state and
initialization; replacing these names alone does not establish a complete boot.

#### Lima copying callers

Lima's remaining `copy` calls were reviewed on **26 September 2026** at the
pinned revisions, with `PACKAGE_CONTRIB` enabled for FluffOS. Native tests cover
the adapted group daemon and extracted task/method getters, class, socket-table
and hook callers; no cross-driver execution was performed. The full
[method][lima-method], [party][lima-party] and [task][lima-task] daemons still
contain numeric-zero returns incompatible with their declared reference types
under distinct nil; those are remaining port blockers.

Use [`deep_copy`](efun/deep_copy.md) for these snapshots and working copies.
String and numeric table keys retain their comparisons. FTP's socket-object
keys and references also retain identity, as do hook callbacks; copying a
callback does not isolate its captured state. [FTP][lima-ftp],
[hooks][lima-hooks], [upstream copying][fl-copy].

This adopts native graph semantics for arbitrary nested values in task
attributes, effect arguments and other open payloads. Mutable container keys
are copied; lookup with the original key no longer finds them. Repeated
references remain shared within each independent copy, and cycles are retained.
Callers needing a related key and table should copy them together. This does
not make recursive mudlib traversals cycle-safe or promise that cyclic values
can be saved. [Task daemon][lima-task], [effects][lima-effects].

Keep each getter's contract explicit. In the reviewed task daemon,
`query_attributes` copies the attribute table, but `query_attribute` returns
the selected value directly. Likewise, `query_tasks()` copies the full task
tree, while `query_tasks(id)` returns that task's live subtask array.
Renaming the existing copy calls does not change those direct-return branches.
[Task getters][lima-task].

#### Lima JSON helpers

Lima's [JSON simul module][lima-json] uses byte buffers and the encoding efuns
above. Its encoder skips non-string mapping keys, substitutes `null` for
unsupported values and cyclic interior references, and accepts an optional
recursion-bookkeeping argument. Those are mudlib policies, not general FluffOS
JSON guarantees.

Remove this module's inherit to expose native [`json_encode`](efun/json_encode.md)
and [`json_decode`](efun/json_decode.md), then retire the unused source after
checking downstream callers. Bare calls and function pointers use the efuns;
explicit calls to the old module and the encoder's second argument need changes.
A conversion-only port still needs parser and escaping repairs: execution on
lpc-rs reproduced incorrect backslash handling, acceptance of malformed JSON,
NUL truncation and an 8,192-byte concatenation limit from the appended sentinel.
Renaming the encoding calls alone does not fix those behaviors.

Native encoding turns numeric mapping keys into text, rejects other non-string
keys, and encodes bytes as arrays of integers. Unsupported values and cycles
raise errors. Numeric and textual keys can collide after conversion; the last
value wins. Decoding enforces JSON syntax, resolves escapes, rejects trailing
NULs, and converts integers outside the native integer range to floats, which
may lose precision. The `-0` token retains floating negative zero.

JSON booleans decode to integer 1/0 and encode back as numbers. JSON null becomes
[distinct nil](lpc/null.md) in the Lima port and encodes back as `null`. Code
requiring the old coercion or filtering policies should implement them explicitly
around the native efuns. The inventory found no external JSON-helper consumers in the reviewed Lima tree; downstream
applications may have their own contracts.

#### FluffOS parser package

Visibility modifiers, nested scopes, multiword verbs, multiple `STR` captures
and caller-restricted parsing were reviewed on **23 September 2026**
at the same pinned FluffOS revision, with `PACKAGE_PARSER` and ordinary driver
environments (`NO_ENVIRONMENT` disabled). These are source comparisons, not
cross-driver execution results.

These notes concern the reviewed FluffOS implementation with `PACKAGE_PARSER`,
not every historical MudOS release. Consult its [source][fl-parser-source] and
[efun declarations][fl-parser-spec] alongside the native
[`parse_sentence`](efun/parse_sentence.md),
[`parse_add_rule`](efun/parse_add_rule.md), and
[handler contract](apply/object/parser_handlers.md).

| Topic | FluffOS behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Dispatch and actor | `parse_sentence` parses for the calling object, which must have called `parse_init`. | Defaults to the living `this_player()`; optional fifth and sixth arguments select the actor and command giver separately. The actor supplies scope and the initial `can_` target, while the giver supplies callback `this_player()`. In a user/body mudlib, pass body and user explicitly; actor remains immediate caller. Registered parser rules are also reached by ordinary command dispatch after the actor's own rules decline the verb. Review actor and dispatch ownership. Ordinary `command(line, actor)` uses that actor as player for vocabulary, permissions and actions, while retaining the entering interactive provenance. [Command context](efun/command.md#callback-context). Parser efun callbacks retain the invoking LPC frames behind the actor; review deeper caller-chain checks as well. [Context](efun/parse_sentence.md#callback-context). |
| Articles and possessives | Recognizes built-in English words including `a`, `any`, `the`, and `my`. | Supply accepted adjectives through the vocabulary applies; these words are not automatic articles or possessive selectors. |
| Error kinds | `ERR_IS_NOT` (1) distinguishes a recognized noun with no matches after qualification. `ERR_MANY_PATHS` (9) is intended to reject excessive object-pair combinations. Kinds 2–8 are also defined. [Noun matching][fl-parser-noun-mismatch], [pair guard][fl-parser-pair-limit]. | Retains the numbers 2–8, leaving 1 and 9 unused. A noun with no qualifying match uses kind 7; pair searches use the existing execution deadline rather than a separate pair-count diagnostic. Inspect each argument contract. The driver supplies no constants header. |
| Error hook and fallback | `get_the_error` calls the master with a kind-dependent argument count; a missing result becomes zero. | [`parser_error_message`](apply/master/parser_error_message.md) always receives four arguments, and runs only after all rules fail. No built-in English fallback: missing text leaves the silent result code, and command dispatch uses its ordinary failure fallback. |
| Visibility modifier | `:v` skips reachability filtering for named object matches while retaining the visible scope; handler names omit it. [Tokens][fl-parser-modifiers], [matching][fl-parser-visibility]. | Supports exactly `OBJ:v`, `OBS:v`, `LIV:v`, and `LVS:v`; each slot keeps its own access policy. The policy also applies to bare `all`. Other/repeated/combined suffixes are rejected. Normal permissions and living checks remain. |
| Multiple text captures | Accepts multiple nonempty `STR` captures. A non-final `STR` tries shorter spans first; the final one consumes the remainder. [Registration][fl-parser-str-rules], [split search][fl-parser-str-search]. | Accepts multiple nonempty `STR` captures, including adjacent ones. Earlier captures take longer spans first, with retries after refusal, up to 32 parses per rule/input. Rules with repeated separators or adjacent text slots can therefore reach handlers with different values; review permission and action assumptions. [Native contract](efun/parse_add_rule.md#text-captures-and-split-selection). |
| Ambiguity modifier | Recognizes `:c` to choose one match. | Does not recognize `:c`; resolve that choice in the mudlib or adapt the rule. |
| Debug argument | Nonzero debugging requires a `DEBUG` or `PARSE_DEBUG` build; otherwise it errors. | Accepts and ignores `debug`. |
| Name caching | `parse_refresh` invalidates cached parser information. | Rules and noun vocabulary are read afresh for each call; `parse_refresh` does nothing. |
| Nested scope arrays | Arrays describe the preceding parser-initialized object's contents, inheriting visibility, accessibility and ownership. Orphan arrays are skipped. [Array walk][fl-parser-scope-arrays]. | Arrays describe the immediately preceding live object's supplied contents; hooks govern visibility/accessibility, with native missing-hook defaults of true. Orphan arrays are errors. Flat lists remain reachable overrides. Duplicate identities keep first visible order and combine reachable paths. Existing lpc-rs callers using nested arrays merely for grouping must flatten them explicitly. [Contract](efun/parse_sentence.md#scope). |
| Living candidates | Uses the parser's `is_living` apply. | Uses `enable_commands` status, including extra candidates from [`parse_command_users`](apply/master/parse_command_users.md). |
| Handler names | Searches specific, literal-`word`, generic-shape, then generic-rule names. Plural action names depend on the actual match. [Construction][fl-parser-handler-args]. | Uses the same four tiers. Permission slugs are singular; `OBS`/`LVS` actions always use plural names and arrays, even for one match. Duplicate names retain their earliest layout. |
| Handler arguments | Middle tiers insert literal words; generic tiers prefix the base verb (and reconstructed rule for `verb_rule`). The original consumed verb phrase precedes source strings for every capture. [Construction][fl-parser-handler-args], [source strings][fl-parser-source-words]. | Uses those layouts with native capture text and the complete typed verb phrase, including internal whitespace. Generic rule text preserves registration whitespace and `:v` exactly; FluffOS reconstructs it without the visibility modifier. [Rule formatter][fl-parser-rule-text]. Earlier lpc-rs generic handlers need a typed-verb parameter before their source strings, and all tiers now append source text for `WRD`/`STR` as well as object captures. Update trailing positional parameters and variadic consumers together. [Contract](apply/object/parser_handlers.md). |
| Handler targets | Searches all tiers on the parsing object for `can_`, or noun candidate for direct/indirect, before the rule owner. Actions search only the owner. [Permission lookup][fl-parser-handler-targets]. | Uses the same target-first ordering with the selected native actor as the initial `can_` target, independently of the command giver. Only absence permits fallback; a present actor handler replaces the owner's check, and a candidate generic handler outranks an owner-specific one. |
| Missing handlers and replies | Missing permission handlers generate a parser error; an accepted parse with no action raises an error. Permission replies other than integers or strings do not supply an answer. [Permission lookup][fl-parser-handler-targets], [action lookup][fl-parser-action-lookup]. | Absent `can_` permits; missing object permissions exclude that candidate or pair; absent `do_` leaves the rule unhandled. Permission values other than zero, distinct nil or strings accept. Refusals stop method lookup, and exceptions propagate. |
| Two-object selection | Retains possible objects for a dependent relation pass. [Relations][fl-parser-relations]. | With at most one plural slot, checks pairs before resolving ambiguity or ordinary counts. Both orientations of one plural slot work; two plural slots retain aggregate checks and independent action arrays. |
| Permission arguments | Full relation checks pass individual objects; provisional indirect checks can receive a plural array. [Construction][fl-parser-handler-args], [relations][fl-parser-relations]. | Pair checks and their final rechecks always pass two individual objects, with no provisional null partner. Only `do_` receives the plural action array. Single-object rules and rules with two plural slots retain aggregate rechecks. Adapt hooks that require provisional or array calls. |
| Ordinal selection | Indirect ordinals are narrowed before the relation loop; direct ordinal handling occurs in that loop. Exact cross-driver outcomes are unverified. [Relations][fl-parser-relations]. | For pair rules, both slots pin ordinals to live noun matches satisfying their individual access requirements before permissions, without substituting another compatible object. Other rule shapes retain ordinals after initial filtering. |
| Partial plural refusal | Relation processing retains errors alongside matches. [Relations][fl-parser-relations]. | A pair's late refusal can remove one plural member without rejecting the accepted members. Actions receive raw plain refusal strings for the selected partner; soft reasons are omitted. No master error hook runs after an eventual successful rule. |
| Successful rule selection | Uses rule weights to select a successful match. [Selection][fl-parser-selection]. | Tries the longest matching verb phrase first, then shorter phrases if needed. Within each phrase, the first successful rule in registration order wins, even if a later rule is more specific. Earlier failures do not block later successes. |
| Error selection | Weighs rule matches and error counts. [Selection][fl-parser-selection]. | Within a rule, retains the parse that resolved the most object slots, taking the earliest tie. If every phrase fails, offers diagnostics in longest-phrase/registration order with each failure's own verb query context until the master returns a string; if none does, combines silent codes by severity (`-3`, then `-2`, then `-1`). Error text can therefore differ. |
| Verb matching | Tries progressively longer input prefixes after ASCII case folding and punctuation processing; rule weights determine selection, not a longest-verb guarantee. Registered names remain verbatim. [Search][fl-parser-verb-search], [registration][fl-parser-verb-register]. | Supports whole-word multiword verbs and synonyms. Normalizes Unicode whitespace in verb keys while preserving case and punctuation. Rejects empty keys. Match-dependent callbacks report the canonical full phrase through `query_verb()` and its raw span through `query_verb(1)`. Older lpc-rs callbacks assuming one-word parser verbs need review; actor/native/add_action behavior is unchanged. |
| Synonyms | The implementation accepts `parse_add_synonym(new, old, rule)` despite its reference showing two arguments. | Also supports the optional rule filter. Native synonyms copy current owner registrations, normalize verb-key whitespace, and keep exact rule-text filters. Review chaining and removal semantics in [`parse_add_synonym`](efun/parse_add_synonym.md) and [`parse_remove`](efun/parse_remove.md). |
| Caller-restricted parsing | Implemented `parse_my_rules(actor, line, flag=0)` uses only the caller's rules for the supplied actor. Zero flag returns the generic action arguments without action lookup; nonzero executes and returns 1. Both caller and actor require `parse_init`; nested parsing is rejected. [Entry point][fl-parser-owned], [owner filter][fl-parser-owner-filter]. | [`parse_my_rules`](efun/parse_my_rules.md) supplies both modes, using native selection, permission and failure policies above. Actor must be command-enabled; only rule registration requires `parse_init`. An optional fourth argument selects the callback command giver independently, while rules still belong to the object executing the efun; keep wrappers in that object or its inheritance tree. A missing action in execution mode leaves that parse unhandled. Nested parses are allowed up to the shared command depth of 16. Review initialization, failure handling and reentry assumptions. |
| Rule inspection | The upstream [`parse_my_rules` reference][fl-parser-owned-doc] describes a no-argument inspector, but the reviewed [declaration][fl-parser-spec] and [implementation][fl-parser-owned] instead expose the parsing operation above. | [`query_parser_rules()`](efun/query_parser_rules.md) lists the caller's registrations as `"verb rule"` strings; [`parse_dump()`](efun/parse_dump.md) lists all owners' registrations as text. [`query_parser_registry()`](efun/query_parser_registry.md) returns separate verb, rule, owner object and handler-base fields, globally by default or filtered by owner. |

Lima's `deep_useful_inv_parser_formatted` helper stops at inaccessible containers
before handing the driver its nested scope. Its stock container currently reports
accessibility as true. If a derived container makes visible contents inaccessible,
that helper must descend by visibility to supply them for `:v` rules; the driver
cannot restore omitted objects. [Lima helper][lima-parser-scope],
[container][lima-parser-container], reviewed at
`eefaff66adcae9b01e16059574c925b0d63e70a1` on 23 September 2026.

The reviewed FluffOS [guard for kind 9][fl-parser-pair-limit] counts freshly
cleared bitsets. Source inspection indicates it cannot fire there; this was
not execution-tested.

### DGD

| Topic | DGD behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Delayed user context | `this_user()` returns nil for delayed tasks. [Reference][dgd-callout-user]. | Leave `CALL_OUT_PRESERVE_PLAYER=false` for empty call-out player context; `this_interactive()` remains empty with either setting. DGD's user API and native `this_player` are distinct interfaces. |
| Nil mode | `typechecking=2` enables distinct nil; other modes use integer zero as nil. [Configuration][dgd-nil-config], [values][dgd-nil-values]. | Set `NULL_MODE=distinct` for a separate nil value. This setting is independent of compiler type-checking pragmas. The default is `zero`. Saved numeric zero always stays numeric; explicit saved nil collapses with a warning when restored in zero mode. [Reference](lpc/null.md). |
| Nil mapping values | Assigning nil deletes a mapping entry. [Implementation][dgd-nil-mapping]. | Nil is a storable value and key. Use `m_delete` for deletion and [`has_key`](efun/has_key.md) for membership, independently of the stored value. |
| Atomic function boundaries | The `atomic` modifier gives a function an error-rollback boundary. [Language reference][dgd-lpc]. | Transactions belong to driver tasks; there is no `atomic` function modifier. Calls across objects share their owning transaction. Function-level rollback assumptions need review. [Runtime](../README.md#how-execution-works). |
| Snapshot restoration | `dump_state` captures system state after the task, for subsequent snapshot restart. [Snapshot API][dgd-dump], [restart lifecycle][dgd-intro]. | Object/map save files serialise selected data; they do not restore the live world, its programs and scheduled work as a driver snapshot. Snapshot-dependent startup requires redesign. [Save format](save-format.md). |
| `parse_string` alternatives | The third argument controls additional parse alternatives in the result. [Reference][dgd-parse]. | Only `0` is accepted; other values raise an error. Code consuming multiple alternatives needs changes. See the [parser details below](#dgd-string-parsing). |
| Grammar caching | Generated parser machinery is cached per object until that object is destructed. [Reference][dgd-parse]. | A driver-wide LRU cache holds 64 grammar texts. Cache ownership and eviction differ. [Reference](efun/parse_string.md). |
| `save_object` visibility | Private and static globals are excluded, as are zero and object values. [Reference][dgd-save]. | Private globals are saved unless marked `nosave`/`static`. Ported objects may therefore save data they previously omitted. [Reference](efun/save_object.md). |

#### DGD string parsing

Compare the [DGD reference][dgd-parse], [parser][dgd-parser-source], and
[grammar compiler][dgd-grammar-source] with the native
[`parse_string`](efun/parse_string.md) contract.

| Topic | DGD behaviour | lpc-rs behaviour and porting impact |
|---|---|---|
| Failure result | Returns `nil` when no parse succeeds. | Returns the selected null value: `0` by default, `nil` in distinct mode. |
| Alternative trees | Can return nested arrays of alternatives at ambiguous branches. | Accepts only `alternatives = 0`. Actions may reject a derivation so another is tried, but the efun does not return multiple trees. |
| Ordering | Ordering by the topmost differing rule is **not checked** against the pinned revision. | Tries productions as written and the longest span first. Do not depend on cross-driver agreement for ambiguous grammars without a probe. |
| Cache and construction | Keeps generated parser machinery per object and expands automata as sentences require. | Keeps 64 complete compiled grammar texts in a driver-wide LRU cache. Review assumptions about cache lifetime and compilation costs. |
| Limits | Has rule, grammar, and regex size errors, and charges parsing against ticks. Additional nesting limits are **not checked**. | Has no separate grammar-text size limit; regex groups nest at most 250 levels. Parsing has a step budget, tries at most 64 derivations, and permits at most 4096 production levels. LPC actions still use the task's execution allowance. |
| Character units | Tokenizes bytes. | Tokenizes UTF-8 characters; byte-oriented grammars may accept different text. |
| `< func` | Stores the grammar annotation. | Accepts it and ignores it. |
| Special objects | The reference excludes special objects from calling `parse_string`. | Has no corresponding special-object category. |

## Coverage still to assess

The initial comparison does not establish complete efun/apply coverage, complete
mudlib compatibility or comparative performance. Further useful comparisons are:

- Error handling inside a task: caught errors, nested calls and effect ordering.
- Inheritance, access modifiers, shadowing and function-pointer lifetime details.
- Execution limits, scheduling precision and caller/player context for every callback.
- Security policy defaults and the full set of master/apply signatures.
- Save-file interchange with representative data, including cycles and object references.
- Database packages, outbound sockets, native extensions and remaining client protocols.
- Operating-system support and development tooling across particular release builds.

## Maintaining this comparison

When a compared feature changes, review its row alongside the canonical efun,
apply or language reference. Keep the detailed contract in that reference and
keep all driver-specific migration advice in this page's per-driver sections;
shared examples belong in the common porting topics above. Link from this page
to native references instead of adding porting sections to each efun or apply.
Update this page's description of the difference. Record the reviewed revision
and date when changing an upstream claim; use publicly accessible, pinned
sources and state any required build options.

Review each named capability across every driver before adding it to a matrix
row. Credit equivalent support even when its API name or implementation layer
differs, state optional packages and defaults, and keep the level of detail
comparable across cells. Use a separate row for each protocol or other feature
whose omission could imply lack of support. Record gaps as not checked instead
of leaving an ambiguous blank or treating an unreviewed feature as absent.

A new comparison starts as source-reviewed. Mark behaviour as execution-tested
only when a reproducible probe has run against the specified builds, and link
the probe, expected results and relevant configuration. API inventories can
identify missing names and signature changes; they cannot establish matching
semantics. Leave unresolved questions explicitly not checked.

[cd-backend]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/backend.c
[cd-save]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/efun/save_object
[cd-functions]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/function_references/functions
[cd-parser]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/efun/parse_command
[cd-master]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/master.n
[cd-network]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/comm1.c
[cd-telnet]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/telnet.c
[cd-actions]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/efun/add_action
[cd-readme]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/README.md
[cd-config]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/config.h
[cd-api]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/func_spec.c
[cd-alarm]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/efun/set_alarm
[cd-callouts]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/call_out.c
[cd-parse-source]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/parse.c
[cd-regexp]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/regexp.c
[cd-explode]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/array.c
[ld-backend]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/src/backend.c
[ld-async]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/LPC/async
[ld-save]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/save_object
[ld-types]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/LPC/types
[ld-closures]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/LPC/closures
[ld-actions]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/add_action
[ld-settings]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/src/settings/default
[ld-replace]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/replace_program
[ld-parser]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/parse_command
[ld-hooks]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/concepts/hooks
[ld-simul]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/concepts/simul_efun
[ld-tls]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/concepts/tls
[ld-telnet]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/hook/telnet_neg
[ld-binary]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/binary_message
[ld-mccp]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/concepts/mccp
[ld-install]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/INSTALL
[ld-invocation]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/driver/invocation
[ld-refs]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/LPC/references
[ld-regex]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/concepts/regexp
[ld-colour]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/terminal_colour
[ld-explode]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/explode
[ld-explode-source]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/src/array.c
[fl-async]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/concepts/general/async.md
[fl-pragmas]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/preprocessor/pragma.md
[fl-predefined]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/lexer_rules_pp.cc
[fl-pragma-lexer]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/lexer_utils.cc
[fl-pragma-compiler]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/compiler.cc
[fl-callouts]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/call_out.cc
[fl-callout-reference]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/calls/call_out.md
[fl-find-callout]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/system/find_call_out.md
[lima-help-timers]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/help_d.c
[lima-timer-config]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/adm/dist/config.mud#L226-L229
[fl-save]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/save_object.md
[fl-restore-object]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/restore_object.md
[fl-object-persistence]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/object.cc
[fl-save-variable]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/general/save_variable.md
[fl-restore-variable]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/general/restore_variable.md
[fl-fetch-variable]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/contrib/fetch_variable.md
[fl-store-variable]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/contrib/store_variable.md
[fl-named-globals]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L131-L179
[fl-global-lookup]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/object.cc#L1294-L1323
[fl-global-assignment]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/svalue.cc#L17-L56
[fl-global-flags]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/program.h#L96-L120
[fl-bind-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L94-L153
[fl-bound-context]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L1924-L1953
[fl-strings]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/types/strings.md
[fl-buffer]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/types/buffer.md
[fl-functions]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/types/function.md
[fl-refs]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/constructs/ref.md
[fl-reload]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/recompile_object.md
[fl-parser]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/parsing/parse_sentence.md
[fl-actions]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/interactive/add_action.md
[fl-patterns]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/parsing/parse_command.md
[fl-options]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/local_options
[fl-packages]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/CMakeLists.txt
[fl-telnet]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/net/telnet.cc
[fl-driver]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/concepts/general/fluffos_driver.md
[fl-simul]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/concepts/general/simul_efun.md
[fl-readme]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/README.md
[fl-config]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/driver/config.md
[fl-children]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/children.md
[fl-children-table]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/otable.cc#L19-L64
[fl-clone]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/clone_object.md
[fl-refresh]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/parsing/parse_refresh.md
[fl-regexp]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/regexp.cc
[fl-pcre]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/pcre/pcre_match.md
[fl-oldcrypt]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/strings/oldcrypt.md
[fl-explode]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc
[fl-explode-entry]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/string.cc
[dgd-readme]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/README.md
[dgd-build]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/doc/compiling.md
[dgd-config]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/mud.dgd
[dgd-comm]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/comm.cpp
[dgd-extensions]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/doc/extensions.md
[dgd-lpc]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/LPC.md
[dgd-dump]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/dump_state
[dgd-intro]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/Introduction
[dgd-compile]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/compile_object
[dgd-parse]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/parse_string
[dgd-save]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/save_object
[dgd-send]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/send_message
[dgd-explode]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/kfun/extra.cpp

[fl-inherit-grammar]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar_rules.cc

[fl-ranges]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/ops/ops.cc
[fl-indexing]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc

[fl-variadic-declarations]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar_rules_types.cc
[fl-call-arguments]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar.y#L1050-L1064
[fl-parameter-grammar]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar.y
[fl-foreach]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/lpc/constructs/foreach.md
[fl-variadic-runtime]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc

[fl-nosave-keywords]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/lexer_utils.cc
[fl-nosave-storage]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/object.cc

[fl-catch]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/calls/catch.md
[fl-catch-jumps]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar_rules_loops.cc
[fl-copy]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L202-L301
[fl-contrib-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.spec#L10
[cd-pointerp]: https://github.com/cotillion/cd-gamedriver/blob/03a98d78db215e40d33642828aad7a43a65e35f7/doc/efun/pointerp
[ld-pointerp]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/pointerp
[fl-sscanf]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/strings/sscanf.md
[fl-sscanf-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L5310-L5624
[fl-colour-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L577-L1102
[fl-parser-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc
[fl-parser-selection]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2825
[fl-parser-rule-weights]: https://raw.githubusercontent.com/fluffos/fluffos/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc
[fl-parser-relations]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2478
[fl-parser-handler-args]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2084
[fl-parser-pair-limit]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2675
[fl-parser-noun-mismatch]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L1659
[fl-parser-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.spec
[dgd-parser-source]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/parser/parse.cpp
[dgd-grammar-source]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/parser/grammar.cpp

[fl-strsrch-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L2665-L2750
[fl-replace-string-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L1931-L2227
[fl-string-search]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/base/internal/strutils.cc#L71-L162
[fl-core-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/core.spec
[fl-member-string]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L1035-L1071
[fl-member-tests]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/testsuite/single/tests/efuns/member_array.lpc
[fl-map-string]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc#L1035-L1111
[fl-map-reference]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/general/map.md
[fl-map-tests]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/testsuite/single/tests/efuns/map.lpc
[fl-map-array]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc#L1001-L1032
[fl-filter-array]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc#L590-L631
[fl-array-callbacks]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L562-L612
[fl-filter-zero]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.h#L99
[fl-repeat-string]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L2014-L2054
[fl-repeat-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.spec#L29
[fl-repeat-reference]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/contrib/repeat_string.md
[fl-strsrch-tests]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/testsuite/single/tests/efuns/strsrch.lpc

[fl-evaluate]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/functions/evaluate.md
[fl-evaluate-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/ops/ops.cc#L1296-L1312
[fl-origin]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/calls/origin.md
[fl-origin-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc
[fl-pointer-dispatch]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/function.cc

[fl-undefined]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L3080-L3093
[fl-undefined-alias]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/core.spec#L143-L144
[dgd-nil-config]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/config.cpp#L1718
[dgd-nil-values]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/data.cpp#L33-L43
[dgd-nil-mapping]: https://github.com/dworkin/dgd/blob/733ea01eeeb571721c35511cb7f51a7f9d32fb6c/src/array.cpp#L1818-L1864

[fl-get-char]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/interactive/get_char.md
[fl-input-mode]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/net/telnet.cc#L620-L649

[fl-valid-override]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/apply/master/valid_override.md
[fl-valid-override-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar_rules_exprs.cc

[fl-parser-source-words]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L1938-L1956
[fl-parser-handler-targets]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2204-L2303
[fl-parser-action-lookup]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L2943-L2993

[fl-parser-modifiers]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L779-L870
[fl-parser-visibility]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L1559-L1768
[fl-parser-scope-arrays]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L1089-L1132
[fl-parser-rule-text]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L1959-L2004
[lima-parser-scope]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/objcalls.c#L84-L103
[lima-parser-container]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/container.c#L839-L848

[fl-parser-verb-search]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3256-L3383
[fl-parser-verb-register]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3597-L3687

[fl-parser-str-rules]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L889-L913
[fl-parser-str-search]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3051-L3072

[fl-parser-owned]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3506-L3573
[fl-parser-owner-filter]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3146-L3172
[fl-parser-owned-doc]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/parsing/parse_my_rules.md

[fl-array-spread-codegen]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/icode.cc#L225-L255

[fl-class-grammar]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar.y#L715-L724
[fl-class-declarations]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar_rules.cc#L343-L399
[fl-class-inheritance]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/compiler.cc#L900-L939
[fl-slice-assignment]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L993
[fl-string-unlink]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L536
[fl-string-write-ranges]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L878
[fl-string-write-replacement]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L1273
[fl-string-index-lvalue]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L695
[fl-string-index-replacement]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L1150

[fl-snoop]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/comm.cc#L1054-L1175
[fl-master-getter]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L967-L973
[fl-master-destruct]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/simulate.cc#L1494-L1540
[fl-math-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/math/math.spec
[fl-round]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/math/math.cc#L170
[fl-numeric-comparisons]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/ops/ops.cc#L95
[fl-call-stack]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/calls/call_stack.md
[fl-type-predicates]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc
[fl-typeof]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/general/typeof.md
[fl-typeof-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L3071-L3077
[fl-reg-assoc]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/strings/reg_assoc.md
[fl-shuffle]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/arrays/shuffle.md
[fl-shuffle-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L2653-L2687
[fl-shuffle-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.spec#L46
[fl-random]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L1686-L1693
[fl-grapheme-length]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L2516-L2561
[fl-string-read]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/interpret.cc#L4096-L4137
[fl-string-read-scalar]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/base/internal/strutils.cc#L164-L185
[fl-reg-assoc-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/regexp.cc#L1428-L1592
[fl-pcre-assoc]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/pcre/pcre.cc#L683-L889

[fl-class-assembly]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L2561-L2589
[fl-byte-read]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L1707-L1728
[fl-byte-read-range]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/file.cc#L492-L565
[fl-utf8-sanitize]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/base/internal/strutils.cc#L46

[fl-text-encoding]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/encoding.cc
[lima-json]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/json.c
[lima-master]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/master.c
[lima-startup-verbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/verb_d.c
[lima-startup-last-login]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/last_login_d.c
[lima-rst-generation]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/rst_d.c
[lima-imud-mail]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/imail_d.c
[lima-imud-mail-protocol]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/imud/mail.c
[lima-imud-auth]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/imud/auth.c
[lima-imud-who]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/imud/who.c
[lima-imud-socket]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/socket.c
[lima-imud-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/imud_d.c
[lima-imud-oob]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/imud/oob.c
[fl-network-sockets]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/sockets/socket_efuns.cc
[fl-network-values]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/object.cc
[fl-network-signatures]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/sockets/sockets.spec
[fl-network-external]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/external/external.cc
[lima-troll-race]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/race/troll.c
[lima-drop-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/drop.c
[lima-get-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/get.c
[lima-write-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/write.c
[lima-security]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/secure_d.c
[lima-access]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/modules/m_access.c
[lima-daemon-data]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/modules/m_daemon_data.c
[lima-data]: https://github.com/limalib/lima/tree/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/data
[lima-guild]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/guild_d.c
[fl-map-delete]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L932-L950
[fl-mapping-transform]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/mapping.cc#L1040-L1078
[lima-crafting]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/crafting_d.c#L770-L773
[lima-party]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/party_d.c#L128-L140
[lima-newsdata]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/newsdata.c#L136-L158
[lima-ftp]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/ftp_d.c
[lima-hooks]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/hooks.c
[lima-method]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/method_d.c
[lima-task]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/task_d.c
[lima-effects]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/living/effects.c
[lima-preload]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/data/config/preload
[fl-source-load]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/simulate.cc#L479-L487
[fl-source-include]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/lexer_utils.cc#L2224-L2251
[fl-file-permission]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/file.cc#L642-L675

[ld-trim]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/trim
[ld-trim-header]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/mudlib/sys/strings.h
[fl-trim]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/trim/trim.cc
[fl-trim-helper]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/base/internal/strutils.cc
[fl-trim-build]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/CMakeLists.txt

[fl-stat-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L2609-L2662
[fl-stat-tests]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/testsuite/single/tests/efuns/stat.lpc
[fl-file-length]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/contrib/contrib.cc#L1618-L1666
[fl-get-dir-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/file.cc#L149-L283

[fl-objects]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/objects.md
[fl-objects-implementation]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc#L2102-L2169
[fl-object-enumeration]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/object.cc#L2253-L2288

[fl-clonep]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/objects/clonep.md
[lima-state-objects]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/state_d.c
[lima-domain-objects]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/domain_d.c
[lima-heartbeat-config]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/adm/dist/config.mud#L114-L115
[lima-stateful-module]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_stateful.c

[lima-command-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/cmd.c
[lima-wish]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/obj/wish.c
[lima-shell-getopt]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/shell/getopt.c
[lima-command-stdio]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/cmd/stdio.c

[lima-command-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/cmd_d.c

[lima-save-module]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/modules/m_save.c
[lima-shell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/shell.c

[lima-overrides]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/overrides.c
[lima-user-shell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/shell.c
[lima-memcheck]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/memcheck_d.c
[lima-shell-prompt]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/shell/prompt.c
[lima-shell-history]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/shell/history.c
[lima-input-forwarding]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/modules/m_input.c
[lima-xterm]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/xterm256_d.c
[lima-ansi]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/ansi_d.c
[lima-inputsys]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/inputsys.c
[lima-login]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/login.c
[lima-user-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/user_d.c
[lima-did-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/did_d.c
[lima-user-gmcp]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/gmcp.c
[lima-frame]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_frame.c
[lima-menu]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/menu.c
[lima-player-menu]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/usermenu/usermenu.c
[lima-pshell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/mudlib/pshell.c
[lima-user-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/messages.c
[lima-menu-config]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/config/user_menu.h
[lima-user-bodies]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/bodies.c
[lima-body-switching]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/sw_body.c
[lima-body]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body.c
[lima-object]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object.c
[lima-description]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/description.c
[lima-names]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/names.c
[lima-flags]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/flags.c
[lima-recipient]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/msg_recipient.c
[lima-stats]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/stats/lima.c
[lima-object-verbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/vsupport.c
[fl-object-receive]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/efuns_main.cc#L1750-L1773
[fl-object-receive-spec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/core.spec
[lima-container]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/container.c
[lima-container-move]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/move.c
[lima-container-user]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user.c
[fl-container-userp]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/efuns_main.cc#L2928-L2936
[fl-container-exec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/interactive.cc#L3-L29
[lima-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_messages.c
[lima-living-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/living/messages.c
[lima-message-grammar]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_grammar.c
[lima-message-strings]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/string.c
[lima-message-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/messages_d.c
[lima-message-effects]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/living/effects.c
[lima-message-colours]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_colours.c

[lima-effect-class]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/effect.c
[lima-effect-disease]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/effect/base_disease.c
[lima-effect-combat]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/main.c
[lima-event-info-types]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/event_info/types.c
[lima-event-info-simple]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/event_info/simple.c
[lima-event-producer]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/blows/class_types.c
[lima-combat-config]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/combat_modules.h
[lima-combat-blows]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/blows/base.c
[lima-combat-damage-sink]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_damage_sink.c
[lima-damage-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/damage_d.c
[fl-empty-array]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/array.cc#L89-L99
[fl-array-equality]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/ops/ops.cc#L121-L126
[lima-durability]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_durability.c
[lima-combat-damage-source]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_damage_source.c
[fl-durability-conversions]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/compiler/internal/compiler.cc
[fl-durability-assignment]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/compiler/internal/grammar.y
[lima-combat-behaviors]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/behaviors.c
[lima-combat-condition]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/condition.c
[lima-combat-targets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/target.c
[lima-combat-corpses]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/death/corpses.c
[lima-combat-pulse]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/pulse/heart_beat.c
[lima-combat-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/messages.c
[lima-combat-armour]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/armour/armour_base.c
[lima-combat-wield]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/wield/wield_base.c
[lima-stats-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/stats/base.c
[lima-monster-setup]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/monster.c
[fl-stats-filter-spec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/core.spec#L290-L292
[fl-stats-filter-mapping]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/mapping.cc#L1024-L1107
[lima-drunk]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/drunk.h
[lima-body-definitions]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/data/daemons/body.o
[lima-body-skills]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/skills.c
[lima-adversary-skills]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/skills.c
[lima-skill-class]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/skill.c
[lima-trainer]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_trainer.c
[lima-skill-formula]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/formula/skills.c
[lima-adversary]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary.c
[lima-setup-overrides]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/overrides.c
[lima-setup-follow]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_follow.c
[lima-follow-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/follow.c
[lima-follow-monster]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/following_monster.c
[lima-follow-wolf]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/monster/wolf.c
[lima-follow-smartmove]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_smartmove.c
[lima-follow-actions]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_actions.c
[lima-actions-quiet]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/quiet.c
[lima-actions-naming]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/naming.c
[lima-death-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/death/death_messages.c
[lima-loot-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/loot_d.c
[lima-corpse-object]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/corpse.c
[lima-decay]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_decay.c
[lima-health-limbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/health/limbs.c
[lima-advancement-simple]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/advancement/simple.c
[lima-body-quests]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/quests.c
[lima-party-advancement]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/party_d.c
[lima-body-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/body_d.c
[lima-wield-limbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/wield/limbs.c
[lima-wieldable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_wieldable.c
[lima-wield-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/wield.c
[lima-dualwield-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/dualwield.c
[lima-armour-limbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/armour/limbs.c
[lima-wear-info]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/wear_info.c
[lima-wearable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_wearable.c

[fl-runtime-settings]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/base/internal/rc.cc
[ld-callout-context]: https://github.com/ldmud/ldmud/blob/e1cbe804944c1f5865ad1beeeff220b43c48c7e3/doc/efun/call_out
[dgd-callout-user]: https://github.com/dworkin/lpc-doc/blob/4bf768ac6f3a20561e0ebdc8ad85ddc7c00d8ca4/kfun/this_user

[lima-armour-object]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/armour.c
[lima-weapon-object]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/weapon.c
[fl-type-qualifiers]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/compiler/internal/grammar.y#L3305-L3319
[fl-implode-strings]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/array.cc#L341-L370

[lima-crafting-registry]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/crafting_d.c
[lima-crafting-materials]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/data/config/crafting-materials
[lima-crafting-module]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_crafting.c
[lima-repair-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/repair.c
[lima-craft-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/craft.c
[lima-salvage-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/salvage.c
[lima-verb-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/verb_ob.c
[lima-complex-exit]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_complex_exit.c
[lima-simple-exit]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_exit.c
[lima-furniture]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/furniture.c
[lima-crafting-station]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/crafting_station.c
[lima-complex-exit-object]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/complex_exit_obj.c
[lima-clean-array]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/misc.c#L47-L73
[lima-mail-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/mail_d.c
[lima-group-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/group_d.c

[lima-money-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/money_d.c
[lima-body-pockets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/pockets.c

[lima-door]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/door.c
[lima-sibling]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_sibling.c
[lima-lockable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_lockable.c
[lima-openable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_openable.c
[lima-hidden-door]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/hidden_door.c
[lima-secret-door]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/secret_door.c
[lima-open-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/open.c
[lima-pick-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/pick.c
[lima-oak-door]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/object/large_oak_door.c
[lima-elevator-door]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/elevator_door.c
[lima-gate]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/object/gate.c
[lima-moving-room]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/moving_room.c
[lima-elevator]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/elevator.c
[lima-elevator-button]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/elevator_button.c
[lima-elevator-call-button]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/elevator_call_button.c
[lima-stock-elevator]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/room/elevator.c
[lima-base-room]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/base_room.c
[lima-roomdesc]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/room/roomdesc.c
[lima-room-state]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/room/state.c
[lima-object-grouping]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/objcalls.c
[lima-cmp]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/misc.c#L92-L127
[lima-school-room-chat]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/school/R/room_chat01.c
[lima-throwable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_throwable.c
[lima-stick]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/weapon/stick.c
[lima-throw-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/throw.c
[lima-club]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/weapon/club.c
[lima-rock]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/object/rock.c
[lima-grenade]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/baseball_grenade.c
[lima-food]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/food.c
[lima-edible]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_edible.c
[lima-healing]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_healing.c
[lima-twinkie]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/misc/twinkie.c
[lima-cobbler]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/mongoose_cobbler.c
[lima-hamburger]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/consumable/hamburger.c
[lima-poison]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/transient/poison.c
[lima-transient]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/transient.c
[lima-transient-speed]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/transient/speed.c
[lima-transient-shield]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/transient/shield.c
[lima-transient-spell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/transient_spell.c
[lima-transient-true-strike]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/transient/true_strike.c
[lima-transient-acid-arrow]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/transient/acid_arrow.c
[lima-spell-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/spell.c
[lima-spell-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/spell_d.c
[lima-skill-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/skill_d.c
[lima-cast-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/cast.c
[lima-combat-spell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/combat_spell.c
[lima-acid-spell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/acid_arrow.c
[lima-fire-beam]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/fire_beam.c
[lima-fireball]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/fireball.c
[lima-dancing-lights]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/dancing_lights.c
[lima-dancing-lights-item]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/item/dancing_lights.c
[lima-companion]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_companion.c
[lima-stateful]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_stateful.c
[lima-state-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/state_d.c
[lima-object-hooks]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/hooks.c
[lima-ale-spell]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/spell/ale.c
[lima-ale-item]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/ale.c
[lima-fluid]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/fluid.c
[lima-drinkable]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_drinkable.c
[lima-fluid-container]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_fluid_container.c
[lima-fluid-source]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_fluid_source.c
[lima-drink-source]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_drink_source.c
[lima-drink-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/drink.c
[lima-drink-container]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_drink_container.c
[lima-beer]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/consumable/beer.c
[lima-chemo]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/consumable/chemo_satchel.c
[lima-dirty-water]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/consumable/dirty_water.c
[lima-blackberry-cider]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/blackberry_cider.c
[lima-boiled-water]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/boiled_water.c
[lima-bottle]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/bottle.c
[lima-keg]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/keg.c
[lima-fill-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/fill.c
[lima-drink-verb]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/verbs/drink.c
[lima-water-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/water.c
[lima-water-item]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/water.c
[lima-flask]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/flask.c
[lima-tankard]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/tankard.c
[lima-river]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/river.c
[lima-lockpick-simple]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/electric_lock_pick.c
[lima-lockpick-mediocre]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/mediocre_electric_lock_pick.c
[lima-lockpick-advanced]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/crafting/advanced_electric_lock_pick.c
[lima-lockpick-module]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_lockpick.c

[lima-naming-title]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/title.c
[lima-naming-describe]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/describe.c
[lima-naming-nickname]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/nickname.c
[fl-query-idle]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/interactive/query_idle.md
[fl-query-idle-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/comm.cc#L1176-L1181

[lima-player-guilds]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/guilds.c
[lima-player-guild-master]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_guild_master.c#L189-L224
[fl-guild-filter-entry]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/efuns_main.cc#L598-L607
[fl-guild-filter-mapping]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/mapping.cc#L1078-L1142

[lima-body-mass]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/object/mass.c
[lima-body-combat]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/combat_modules.h
[lima-body-start]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/start.c
[fl-body-save-spec]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/core.spec
[fl-body-save-source]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/core/save.cc

[lima-entry-user]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user.c

[lima-entry-mailbox]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/mailbox.c

[lima-entry-channel]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/channel/cmd.c

[lima-entry-snoop]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/snoop_d.c

[fl-entry-function-grammar]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar.y#L581-L600

[fl-entry-implode]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/docs/efun/strings/implode.md

[fl-entry-fold]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/vm/internal/base/array.cc#L423-L461

[lima-channel-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/channel_d.c
[lima-channel-moderation]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/channel/moderation.c
[lima-channel-user]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/channel.c
[fl-channel-members]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/compiler/internal/grammar.y
[lima-entry-snoop-dummy]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/snoop_dummy.c

[lima-speaker-context]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/userfuncs.c
[lima-speaker-soul]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/soul_d.c

[lima-emote-feelings]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/feelings.c

[lima-emote-remove]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/rmemote.c

[lima-emote-move]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/mvemote.c

[lima-emote-add]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/addemote.c

[lima-emote-log]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/log_d.c

[lima-log-header]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/log.h

[lima-listing-adverbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/adverbs.c
[lima-listing-apropos]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/emoteapropos.c
[lima-listing-show]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/showemote.c
[lima-listing-verbs]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/verbs.c
[lima-shorthand-stupid]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/stupidemote.c
[lima-shorthand-target]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/targetemote.c

[fl-parser-dump]: https://github.com/fluffos/fluffos/blob/2c27287500daf48a87a1f89d69a97d8b9603028d/src/packages/parser/parser.cc#L3819

[lima-semote-preview]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/semote.c
[lima-semote-menu]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/mudlib/plmenu.c

[lima-random-emote]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/random.c
[lima-random-generator]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/random2.c
[lima-tell-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/tell.c
[lima-reply-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/reply.c
[lima-tell-history]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/history.c
[lima-game-menu-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/menu.c
[lima-width-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/width.c
[lima-widgets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_widgets.c
[lima-ui-config]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/config.h
[fl-random-config-query]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/rc.cc
[fl-random-mapping-limit]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/mapping.cc#L557-L562

[lima-news-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/news.c
[lima-who-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/who.c
[lima-news-reader]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/mudlib/newsreader.c
[lima-news-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/news_d.c
[fl-news-save-source]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/vm/internal/base/object.cc
[fl-uptime-spec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/core.spec#L274
[fl-uptime-source]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/efuns_main.cc#L2924-L2926
[fl-uptime-reference]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/docs/efun/system/uptime.md

[lima-news-user-state]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/newsdata.c
[lima-news-number-sets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/modules/m_set.c
[lima-pager]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/obj/more_ob.c
[lima-text-editor]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/obj/edit_ob.c
[lima-more-helpers]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/simul_efun/more.c
[lima-user-switch]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/sw_user.c
[lima-ed-session]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/obj/ed_session.c
[lima-ed-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/cmds/ed.c
[lima-ed-codesay]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/codesay.c
[lima-ed-find]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/cmds/find.c
[lima-ed-grep]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/cmds/grep.c
[lima-ed-locate]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/cmds/locate.c
[lima-ed-replace]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/trans/cmds/replace.c
[fluffos-object-editor]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/ed.cc
[fluffos-object-editor-spec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/core.spec
[fluffos-object-editor-mode]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/efuns_main.cc
[fluffos-file-write-flags]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/file.cc
[lima-editor-account]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/userinfo.c

[lima-did-ports]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/ports.h

[lima-did-user]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/misc.c

[lima-did-post-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/I.c

[lima-did-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/wiz/didlog.c
[lima-mailbox-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/mailbox_d.c
[lima-mailer-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/mailers/mailer.c
[lima-body-mail-composition]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body.c
[lima-body-mailbase]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/mailbase.c
[lima-body-mail-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/body/cmd.c
[lima-portable-board]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/domains/std/object/portable_board.c
[lima-contrib-board]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/contrib/board.c
[lima-contrib-replynote]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/contrib/boards/replynote.c
[lima-contrib-board-installation]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/contrib/boards/installation
[lima-mailer-standard]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/mailers/standard.c
[lima-mailer-mh]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/secure/mailers/Mh.c
[lima-mail-command]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/mail.c
[lima-user-mailbase]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/user/mailbase.c

[fl-configured-port]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/compiler/internal/lex.cc#L3003-L3006

[fl-scalar-regexp]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/efuns_main.cc#L1797-L1827

[fl-scalar-regexp-spec]: https://github.com/fluffos/fluffos/blob/44532e0228f34be2184c701a361df4079371b8b0/src/packages/core/core.spec#L152

[lima-mail-message-class]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/classes/mailmsg.c
[lima-mail-biff]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/biff.c

[lima-http]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/http_d.c
[lima-http-form]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/WWW/cgi/formtest.c
[lima-http-webed]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/cgi/webed.c
[lima-ftp-header]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/ftp_d.h
[lima-rcp]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/secure/daemons/rcp_d.dontuse
[lima-player-help]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/help.c
[lima-player-brief]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/brief.c
[lima-player-verbose]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/verbose.c
[lima-player-save]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/save.c
[lima-help-viewer]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/obj/mudlib/helpsys.c
[lima-rst-viewer]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_rstview.c
[lima-player-hp]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/hp.c
[lima-player-score]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/score.c
[lima-widgets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/modules/m_widgets.c
[lima-party-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/party_d.c
[lima-player-equip]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/equip.c
[lima-player-skills]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/skills.c
[lima-player-materials]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/materials.c
[lima-player-money]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/money.c
[lima-player-rows]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/rows.c
[lima-player-frames]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/frames.c
[lima-player-version]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/version.c
[lima-player-timezone]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/timezone.c
[lima-player-time]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/time.c
[lima-localtime-header]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/localtime.h
[lima-event-daemon]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/daemons/event_d.c
[lima-player-title]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/title.c
[lima-player-describe]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/describe.c
[lima-player-nickname]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/nickname.c
[lima-player-plan]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/cmds/player/plan.c
