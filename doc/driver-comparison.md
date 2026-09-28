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
| Execution | Parallel LPC tasks; snapshot reads, atomic commits and conflict retries. [Runtime](../README.md#how-execution-works). | Tasks evaluated serially by the [backend loop][cd-backend]. | Sequential [backend evaluation][ld-backend]; explicit [coroutines][ld-async]. | Event-driven LPC; [async functions and promises][fl-async], with worker-thread I/O in the async package. | Explicit [atomic functions][dgd-lpc] roll back on errors; multicore acceleration is a separate [Hydra offering][dgd-readme]. |
| Saving and restarting | [Object and mapping save files, including class records](save-format.md), with [`nosave` globals](lpc/nosave.md); [`save_value`](efun/save_value.md) / [`restore_value`](efun/restore_value.md) serialize individual values as strings. Whole-world snapshot/restart is not implemented. | [Object variable saves][cd-save]. | [Object saves][ld-save] with selectable formats and a serialised-string form. | [Object saves][fl-save] with flags, optional compression and a serialised-string form; [`nosave` globals][fl-nosave-keywords] with `SENSIBLE_MODIFIERS`; individual values through [`save_variable`][fl-save-variable] / [`restore_variable`][fl-restore-variable]. | Object saves plus [whole-system snapshots][dgd-dump] and [restart from a snapshot][dgd-intro]. |
| Language features | [Caller inspection](efun/calling_object.md); [Exact int/float comparisons](lpc/comparisons.md); [Configurable zero or distinct nil](lpc/null.md) with [`nilp`](efun/nilp.md); [`has_key` for mapping membership](efun/has_key.md); [UTF-8 strings, immutable bytes, function values and closures](lpc/types.md); [`evaluate` for values or callbacks](efun/evaluate.md); explicit [value captures](lpc/function_type.md#explicit-value-captures) and [reference parameters](lpc/references.md); [macro argument stringification](lpc/macros.md#stringification) and [predefined source macros](lpc/macros.md#predefined-macros); [warning pragmas and retained function types](lpc/pragmas.md); [inheritance visibility, basename calls, and checked sibling/child function resolution](lpc/inheritance.md); [reverse indices, slices and array/string writes](lpc/indexing.md); [array literal spreading](lpc/array_spreading.md) and [trailing call commas](lpc/anatomy_of_a_program.md#function-calls); [`foreach` with `in` or `:`](lpc/foreach.md); [expression and block catches](efun/catch.md); [named variadic parameters with `mixed` or array collectors](lpc/variadic_parameters.md) and [unnamed prototype parameters](lpc/anatomy_of_a_program.md#function-prototypes); [nominal classes with declaration visibility, shared instances, guarded members, shallow copying and native persistence](lpc/classes.md); [`deep_copy` of mutable container graphs](efun/deep_copy.md); literal [`strindex`](efun/strindex.md), [`replace_string`](efun/replace_string.md) and [`trim`](efun/trim.md); scalar-based string [`member_array`](efun/member_array.md) and [`map`](efun/map.md). | [Function values and partial application][cd-functions]. | [Unicode strings, bytes, structs, lightweight objects and coroutines][ld-types]; [closures][ld-closures], [reference arguments][ld-refs] and [trimming][ld-trim]. | [Unicode text][fl-strings], mutable [buffers][fl-buffer], [function values and closures][fl-functions], [reference parameters][fl-refs], [class declarations][fl-class-grammar], [named variadic parameters][fl-variadic-declarations], [array literal spreading][fl-parameter-grammar], [promises][fl-async], [`copy` with `PACKAGE_CONTRIB`][fl-copy] and [trimming with `PACKAGE_TRIM`][fl-trim]. | [DGD LPC types and modifiers][dgd-lpc]; function pointers and bound arguments with the [CLOSURES build define][dgd-build]. |
| File metadata | [`file_size`](efun/file_size.md), [`file_time`](efun/file_time.md) and [`get_dir`](efun/get_dir.md); [`rm`](efun/rm.md) checks its task's pending entry changes without reading contents. See [Lima adaptations](#lima-file-metadata). | **Not checked**. | **Not checked**. | Core [`stat`][fl-stat-source] combines metadata with loaded-object information or directory results; [`file_length`][fl-file-length] counts LF bytes with `PACKAGE_CONTRIB`. | **Not checked**. |
| Updating live objects | [Queued in-place recompilation](efun/request_object_recompile.md) upgrades a prototype and its current clones atomically, preserving globals by declaring source, name and type; system prototypes can also be upgraded. | In-place program migration: **not checked**. | [`replace_program`][ld-replace] selects an already inherited program and drops extra code/data; it does not recompile source. | [`recompile_object`][fl-reload] updates a prototype and its clones in place, carrying globals by name; inheritors need separate updates. | [`compile_object`][dgd-compile] upgrades an existing object and its clones when the object has no inheritors; matching variable names and types retain values. |
| Commands and grammars | [`command`](efun/command.md#callback-context), [`add_action`](efun/add_action.md), [`parse_command`](efun/parse_command.md), [`parse_sentence`](efun/parse_sentence.md) with [separate actor and command giver](efun/parse_sentence.md#callback-context), caller-owned [`parse_my_rules`](efun/parse_my_rules.md) with [retained LPC callers](efun/parse_sentence.md#callback-context), [multiword verbs](efun/parse_add_rule.md#the-verb), [multiple text captures](efun/parse_add_rule.md#text-captures-and-split-selection), [visible-only slots](efun/parse_add_rule.md), [structured scopes](efun/parse_sentence.md#scope), and [four handler tiers](apply/object/parser_handlers.md), and [`parse_string`](efun/parse_string.md), plus [native command patterns](command-rules.md). | [`add_action` commands][cd-actions] and object-aware [`parse_command` patterns][cd-parser]. | [`add_action` commands][ld-actions] and optional [`parse_command` patterns][ld-parser]. | [`add_action` commands][fl-actions], [`parse_command` patterns][fl-patterns] and [`parse_sentence` rules and handlers][fl-parser]; build options below. | [`parse_string`][dgd-parse] accepts grammar text and produces parse results. |
| Driver–mudlib interface | [Master, object, living and connection applies](../ulib/doc/applies.md), including [constructor arguments](efun/clone_object.md), explicit [load](apply/master/valid_load.md), [inherit](apply/master/valid_inherit.md) and [destruction](apply/master/valid_destruct.md) permissions; configurable simul-efuns and automatic inheritance in [default.env](../default.env), with optional [`valid_override`](apply/master/valid_override.md) control of explicit efun calls. | [Master callbacks][cd-master] define policy and connections; [simulated efuns][cd-functions] expose mudlib functions. | [Driver hooks][ld-hooks] configure movement, identity and creation; [simul-efuns][ld-simul] expose mudlib functions and structs. | [Master and object applies][fl-driver] for connections and lifecycle; [simul-efuns][fl-simul] extend or wrap efuns. [`valid_override`][fl-valid-override] controls explicit efun bypasses. | A [driver object and automatically inherited auto object][dgd-intro]; the auto object can wrap kfuns. |
| Call origin | [`origin`](efun/origin.md) identifies eight dispatch mechanisms independently of caller identity; named simul-efun calls report `"simul_efun"`, scheduled callbacks `"call_out"`, and explicit named pointers `"function_pointer"`. | **Not checked**. | **Not checked**. | Eight [origin categories][fl-origin]; named local pointers report `"local"` in the reviewed [implementation][fl-pointer-dispatch]. | **Not checked**. |
| Timer scheduling | [`call_out`](efun/call_out.md) takes a function, delay and optional repeat interval; bind callback data into the function. | **Not checked**. | **Not checked**. | [`call_out`][fl-callout-reference] accepts a string or function callback followed by delay and callback arguments. | **Not checked**. |
| Timer inspection | [`query_call_outs()`](efun/query_call_outs.md) lists the current owner's timers; an object selects that owner and `-1` selects all owners. Rows include the function pointer, milliseconds remaining, repeat interval and ID. | **Not checked**. | **Not checked**. | [`call_out_info()`][fl-callouts] lists all live owners' timers with callback labels and seconds remaining. | **Not checked**. |
| Object enumeration | [`objects()`](efun/objects.md) lists all loaded objects from the current transaction, including its own placements, in unspecified order. [`object_clones`](efun/object_clones.md) lists one compiled program's clones. [`all_inventory`](efun/all_inventory.md) and [`deep_inventory`](efun/deep_inventory.md) exclude objects destroyed in that transaction. | **Not checked**. | **Not checked**. | [`objects`][fl-objects] lists loaded objects with optional filtering, visibility rules and an array-size cap; [`children`][fl-children] groups by base filename. | **Not checked**. |
| Named global access | [`variable_info`](efun/variable_info.md) reads all declarations; [`store_variable`](efun/store_variable.md) replaces one by name and optional declaring source, with retained-type checks and separate master authorization for foreign writes. | **Not checked**. | **Not checked**. | [`fetch_variable`][fl-fetch-variable] / [`store_variable`][fl-store-variable] with `PACKAGE_CONTRIB`; [lookup and access restrictions][fl-named-globals]. | **Not checked**. |
| Connection observation | Authorized one-to-one [`snoop`](efun/snoop.md), asynchronous typed callbacks, bounded delivery. | Not reviewed. | Not reviewed. | One-to-one snooping; optional callback delivery. [Source][fl-snoop]. | Not reviewed. |
| Connection model | [Telnet sessions](efun/query_connection.md) with [line](efun/input_to.md) and [character](efun/get_char.md) callbacks and optional [TLS listeners](secure-connections.md). | Driver-managed [Telnet negotiation][cd-telnet]. | Telnet with [mudlib negotiation hooks][ld-telnet] and optional [TLS][ld-tls]. | Configurable [Telnet, binary, WebSocket and TLS listeners][fl-config]. | Configured [Telnet and binary ports][dgd-config]; additional facilities can come from [extension modules][dgd-extensions]. |
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
| Regex association | `reg_assoc` returns parallel arrays of alternating gaps/matches and their labels. Earliest matches win, with pattern order breaking ties. Each search uses the remaining suffix; the default gap label is integer zero. `pcre_assoc` uses PCRE. [Reference][fl-reg-assoc], [core source][fl-reg-assoc-source], [PCRE source][fl-pcre-assoc]. | A mudlib simul can build the association arrays with [`regmatch`](efun/regmatch.md) and `RE_MATCH_SUBS`; there is no native association efun. Preserve empty gaps and advance empty matches by a Unicode scalar without dropping intervening text. Use an explicit `0` default label when callers require numeric zero. Searching a suffix retains the legacy anchor context; an offset into the original text preserves its anchors instead. Review patterns for the [native regex dialect](efun/regexp.md#dialect), scalar indexing and embedded NUL support. This does not supply full PCRE compatibility. Repeated `regmatch` calls recompile their patterns, so measure frequent calls or inputs with many matches. |
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
| Explicit closure captures | `$(expression)` evaluates at closure creation and retains the result for later calls. Named and bound pointer literals use `(: name :)` and `(: name, arg :)`. [Function reference][fl-functions]. | Supports `$(expression)` alongside existing shared variable captures. Captured references remain shared; the captured value itself cannot be assigned. Named/bound FluffOS literals still need adaptation to `name` and `&name(arg)`. [Reference](lpc/function_type.md#explicit-value-captures). |
| Class copying and conversion | `assemble_class` creates positional class storage from an array, without a named definition; `disassemble_class` returns a shallow field array. Their round trip copies the outer record while sharing nested references. Requires `PACKAGE_CONTRIB`. [Implementation][fl-class-assembly]. | Replace copy-only round trips with [`copy_class`](efun/copy_class.md), which preserves nominal identity and retained field views. Converting between different class types requires explicit named construction, even when their layouts match. For example, build `new(class limb, health: data.health, max_health: data.max_health, parent: data.parent, flags: data.flags)` and let omitted integer fields default to zero. Replace layout-count repair checks with a guarded nominal cast and presence check; compatible [field views](lpc/classes.md) do not require resetting a record merely because fields changed. Generic assembly/disassembly and foreign class-save import remain unsupported. |
| Class visibility | Accepts visibility modifiers on class definitions but discards them; inheritance copies classes regardless of the modifier. [Grammar][fl-class-grammar], [declarations][fl-class-declarations], [inheritance][fl-class-inheritance]. | Enforces `private class`: descendants cannot name the class or access its members, including through `mixed`. Public and protected classes remain inheritable, subject to inherit visibility. Defining code retains access. If descendants rely on an upstream private declaration being exposed, make it public/protected or include its definition explicitly. [Reference](lpc/classes.md#declaration-visibility). |
| Inheritance visibility | Supports access modifiers on inherits; private functions and globals become hidden when inherited again. The reviewed build enables `SENSIBLE_MODIFIERS`, so explicit public declarations do not defeat a private inherit. [Grammar rules][fl-inherit-grammar], [compiler][fl-pragma-compiler], [defaults][fl-options]. | Supports public, protected and private inheritance, including local access and restricted simul-efun exports. Other inherit modifiers remain unsupported. [Reference](lpc/inheritance.md). |
| Inherited function qualifiers | Named qualifiers match direct-parent path suffixes with the source extension removed. Both named qualifiers and unqualified `::foo()` search direct parents in declaration order for the first defining the function. Qualified calls through a private direct inherit are rejected. [Compiler, `arrange_call_inherited`][fl-pragma-compiler]. | Source basenames and unqualified `::foo()` search direct parents in reverse declaration order, giving later inherits precedence. Explicit aliases select one parent and take precedence over automatic names; path-suffix qualifiers are unsupported. Private imports remain callable within the importing source. [Reference](lpc/inheritance.md#calls-and-aliases). |
| Predefined source macros | `__FILE__` names the current in-game source; `__DIR__` includes a trailing `/`. Both expand at use time, including in macro bodies. `__VERSION__` is the build's version string. These names cannot be redefined or undefined. [Expansion and protection][fl-predefined], [version registration][fl-pragma-lexer]. | Provides `__FILE__`, `__DIR__`, and `__VERSION__`; the version identifies `lpc-rs` and its package version. Driver-specific configuration checks still need adaptation. [Reference](lpc/macros.md#predefined-macros). |
| Warning and type-retention pragmas | `warnings` / `no_warnings` switch warning reporting as compilation proceeds; includes share the setting. `save_types` / `no_save_types` control retention of argument types. Both positive flags are enabled by the reviewed defaults. Unknown pragmas warn. [Pragmas][fl-pragmas], [lexer][fl-pragma-lexer], [compiler][fl-pragma-compiler], [defaults][fl-options]. | `warnings` / `no_warnings` follow each warning's primary source location, including repeated includes and macro invocations. This can differ from FluffOS's reporting-time setting for delayed warnings. `save_types` is accepted silently because function types are always retained. `no_save_types` and other unknown pragmas remain errors even with warnings disabled. [Reference](lpc/pragmas.md). |
| Constructor arguments | `clone_object(path, ...)` forwards extra arguments to the new object's `create`. [Reference][fl-clone]. | `clone_object` forwards extra arguments after global initialisation. Constructors also run without arguments when loading prototypes; use defaults or handle `0`. Supplied declared arguments are type-checked. [Reference](efun/clone_object.md). |
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
| String repetition | `repeat_string(text, count)` in `PACKAGE_CONTRIB` repeats the whole string, reducing a positive count to fit the configured byte limit. Nonpositive counts return an empty string. [Implementation][fl-repeat-string], [reference][fl-repeat-reference]. | Replace calls with `text * count`; parenthesize compound operands. Nonpositive counts also produce an empty string. Native results above 8,192 UTF-8 bytes raise an error instead of returning fewer repetitions. Oversized constant expressions can fail compilation; dynamic overflow is a catchable runtime error. Review callers relying on capped output. [Strings](lpc/types.md). |
| Mixed numeric comparisons | Ordering and equality compare int and float operands using C++ numeric promotion, which can round large integers. [Implementation][fl-numeric-comparisons]. | [All six comparisons](lpc/comparisons.md) accept ints and floats without warnings and compare their exact numeric values. For example, `9007199254740993 > 9007199254740992.0` is true. Natural sorting, min/max and array membership use the same numeric rule. Mapping keys, array set operations and grouping remain type-sensitive. |
| Rounding | `round(float)` in `PACKAGE_MATH` returns a float using the C math rounding operation, with halfway values rounded away from zero. [Declaration][fl-math-spec], [implementation][fl-round]. | [`round(int \| float)`](efun/round.md) also returns a float and rounds halfway values away from zero, alongside [`floor`](efun/floor.md) and [`ceil`](efun/ceil.md). Integer arguments are promoted to float and can lose precision at large magnitudes. Use `to_int(round(value))` where a declared integer result is required; native typing does not implicitly convert the result to int. |
| String membership search | `member_array(integer, text, start)` searches bytes from a byte offset. A negative start or one beyond the byte length raises an error. Search stops at NUL, and integer zero can match the terminator. [Implementation][fl-member-string]. | [`member_array`](efun/member_array.md) searches integer Unicode scalars using scalar offsets. A negative start raises an error; a start at or past the end returns `-1`. Zero matches only actual NUL data, and search continues past embedded NULs. Review byte offsets, especially around file I/O. The optional fourth flags argument is unsupported. |
| Array mapping and filtering | `map_array` and `filter_array` alias `map` and `filter` with array declarations. Callbacks can be function values or method names, with an optional receiver for named methods; extra arguments follow the element. [Declarations][fl-core-spec], [dispatch][fl-array-callbacks]. | Use [`map`](efun/map.md) and [`filter`](efun/filter.md) with function values. Translate `(: name :)` to `name`, bound literals to `&name(args)`, and named-method calls to function values for the intended receiver. Native `filter` accepts arrays and mappings; string filtering needs an explicit loop or character-array conversion. No native aliases are provided. |
| Mapping deletion | `map_delete(mapping, key)` removes an entry in place and returns void. [Declaration][fl-core-spec], [implementation][fl-map-delete]. | Use [`m_delete`](efun/m_delete.md) when the result is discarded. It removes the entry in place but returns the same mapping, so a wrapper that promises void should discard its result. Missing keys are harmless. |
| Mapping callbacks | `map_mapping` aliases `map`; callbacks receive the key, value, then extra arguments. Entries are traversed through the hash table. [Declaration][fl-core-spec], [implementation][fl-mapping-transform]. | Use [`map`](efun/map.md) with function values to produce new values under the same keys, or [`foreach`](lpc/foreach.md) for side effects. Native traversal follows insertion order. Review callbacks whose outcome depends on order, especially inversion of mappings with repeated values. |
| Array callback edge cases | Empty arrays skip callback setup. `filter` accepts non-integer results, including `0.0`. Both loops read live array slots; `filter` copies an accepted slot after its callback returns. [Map][fl-map-array], [filter][fl-filter-array], [zero test][fl-filter-zero]. | Native callbacks must be function values even for empty inputs. `filter` uses native truthiness, rejecting `0.0`, nil and dead objects. Both efuns snapshot the input slots before callbacks; nested mutable values remain shared. Audit callbacks that edit the traversed array. Array `map` retains every callback result, including configured nil from void functions; use `foreach` when only side effects are wanted. |
| String mapping | Calls the callback with each byte up to the first NUL. A nonzero integer result replaces that byte through a `char` conversion; zero or a non-integer result preserves it. Supports function pointers and named callbacks. [Implementation][fl-map-string], [reference][fl-map-reference]. | [`map`](efun/map.md) passes integer Unicode scalars and requires valid integer scalar results. Zero inserts NUL; invalid results raise errors before the next callback. Use explicit identity returns for missing table entries and `foreach` for void callbacks used for side effects. Update byte-indexed translation tables and named callback syntax; native callbacks use function values. |
| Text length | String length and ordinary indexing use extended grapheme clusters. [Strings][fl-strings]. The string overloads of `member_array` and `map` operate on bytes; see above. | String lengths and positions use Unicode scalar values. A combining sequence or multi-code-point emoji can therefore occupy several positions. [Types](lpc/types.md), [length implementation](../lpc-rs-utils/src/lpc_string.rs). |
| Binary values | Mutable `buffer` values, including indexed/range assignment. [Buffers][fl-buffer]. | Immutable `bytes`, with explicit text/byte conversions. Buffer mutation and implicit conversions need adaptation. [Types](lpc/types.md). |
| Text encoding | `string_encode(text, encoding)` returns a buffer; `string_decode(buffer, encoding)` returns a string. ICU supplies the converters; the reviewed decoder trims trailing zero bytes while more than one byte remains before converting. [Implementation][fl-text-encoding]. | Use [`to_bytes`](efun/to_bytes.md) and [`to_text`](efun/to_text.md) with an explicit encoding. Native support is UTF-8 and Latin-1, rejects invalid input, and preserves NULs. Byte offsets and Unicode scalar offsets differ; decode complete sequences. Audit other encodings and error handling separately. |
| Byte-file reads used as text | `read_bytes` returns zero on read failure or EOF; successful reads return a string after replacing invalid UTF-8. The underlying read seeks by byte offset and enforces the configured transfer limit. [Wrapper][fl-byte-read], [sanitizer][fl-utf8-sanitize], [range read][fl-byte-read-range]. | [`read_bytes`](efun/read_bytes.md) returns immutable bytes or configured nil at EOF; missing files and permission failures raise errors. Use explicit [`to_text`](efun/to_text.md) decoding. For a UTF-8 tail helper, find LF boundaries in bytes and decode the selected suffix once: decoding arbitrary chunks can split characters. Count unterminated final lines, preserve blank lines and delimiters, and handle empty files explicitly. Piped command input is already text and must not be passed as a path. Native range reads currently materialize the whole file before slicing, so a chunk loop does not provide bounded-memory tailing. |
| Regular expressions | Traditional `regexp` and separate `pcre_*` efuns. [Implementation][fl-regexp], [PCRE reference][fl-pcre]. | Rust `regex` syntax; no `pcre_*` API, look-around or back-references. [Reference](efun/regexp.md). |
| `save_object` options | File form returns `1` for success, `0` for failure; flags control saving zero values and compression; a no-file form returns data. [Reference][fl-save]. | Requires a path, returns the path and saves zero-valued globals. No flag or no-file form. Permission/serialisation errors raise errors; delivery failures after commit are logged. [Reference](efun/save_object.md). |
| `restore_object` options | The optional flag `1` preserves globals omitted from the file; otherwise non-static globals are cleared before restoration. [Reference][fl-restore-object]. | Takes only the path and always preserves omitted globals. Remove flag `1`; code relying on the upstream clearing default needs explicit initialization. Permission refusal raises an error; a missing or unreadable file returns `0`. [Reference](efun/restore_object.md). |
| Saved-file migration | The reader skips `#` lines, decodes unescaped carriage returns inside strings as newlines, and reads positional `(/.../)` class records. [Implementation][fl-object-persistence]. | Headers need removal, string values need decoding and re-encoding, and classes need nominal identities and typed named fields. Header removal alone does not establish equivalence. See the [native format](save-format.md) and [Lima persistence notes](#lima-daemon-persistence). |
| Value serialization | `save_variable(mixed)` returns a string and `restore_variable(string)` reads one, using the driver's object-save format. [Save][fl-save-variable], [restore][fl-restore-variable]. | Use [`save_value`](efun/save_value.md) and [`restore_value`](efun/restore_value.md). They use the native [save format](save-format.md), including class and bytes encodings and configured nil handling. Renaming calls does not establish compatibility with existing saved strings; audit and migrate persisted data separately. Cross-driver data interchange is **not checked**. |
| Named global reads and writes | `fetch_variable(name, object?)` and `store_variable(name, value, object?)` default to the current object. Lookup takes the first match, searching inherits before the object's own globals. The implementation rejects a private match on a foreign target; it does not enforce the reference's public-only description. Self-access includes private globals, and `nosave` globals remain accessible. The setter copies the value without a declared-type check. [References][fl-fetch-variable], [setter][fl-store-variable], [implementation][fl-named-globals], [lookup][fl-global-lookup], [assignment][fl-global-assignment]. | Read through [`variable_info`](efun/variable_info.md); no native `fetch_variable` is needed for a name lookup. The native setter takes the target **first**, requires a live object, and accepts an optional exact declaring-source path. Duplicate names require that selector; it does not choose the first match. Writes check retained types, configured nil semantics and scalar class identity. Foreign writes require [`valid_store_variable`](apply/master/valid_store_variable.md), including public fields, separately from inspection permission. Update argument order, duplicate-name handling, type assumptions, and master policy together. |
| Rebinding named-global helpers | `bind` can change an eligible function pointer's owner after `valid_bind` permission. A rebound efun pointer therefore performs named-variable operations as its new owner. [Implementation][fl-bind-source], [pointer context][fl-bound-context]. | Pass the target explicitly to `store_variable` or `variable_info`. Translate the original privilege checks into the native master policies; removing rebinding must not remove its authorization. An inherited helper runs as its containing object, but a simul-efun wrapper runs as the simul-efun object and needs permission to access its caller. General pointer rebinding remains unsupported. |
| Legacy password hashes | `oldcrypt` supports the older MudOS algorithm. [Reference][fl-oldcrypt]. | `crypt` does not recognise that format. Existing password data needs a migration plan; renaming the efun is insufficient. See [stored passwords](#stored-passwords) and the native [`crypt` reference](efun/crypt.md). |

#### Lima port configuration

The Lima port targets **`LPC_NULL_MODE=distinct`**. Its adaptations and active
validation use that configuration only. Earlier local runs also exercised
zero mode; those results are historical evidence, not a Lima compatibility
requirement. The driver supports both settings for different mudlibs, with
separate native tests for each. Distinct nil does not emulate FluffOS
`undefinedp` metadata; mapping membership and omitted arguments still need
the adaptations described below.

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

Native callbacks start without `this_player()`. Help already retains its
initiator and can notify that object directly. Use `nil` when clearing the
object or returning an absent topic array, and `!topics` to detect an
uninitialized index in either null mode. Other timer callers that depend on
saved player context need their own adaptation; an ordinary simul wrapper also
changes timer ownership. [Callback context](efun/call_out.md),
[null modes](lpc/null.md).

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

Call-outs have no current player. A timeout notice sent through Lima's `write`
wrapper therefore misses the connection. Use [`write_socket`](efun/write_socket.md)
from the user object for that notice and its former `receive` output calls.
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
mudlib; native call-outs still start without a current player.

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
compilation into the combat code. Default message lookup remains blocked by the
[message daemon's][lima-message-daemon] dependency on
[`m_colours`][lima-message-colours], which assigns a `floor` float result to an
integer. Default-registry tests use an explicit daemon fixture; a separate real
lookup confirms the blocker. Message-file loading, full body entry and combat
remain unvalidated.

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

The producer's weaponless fallback damage type is a string, while ordinary
weapons supply an array of names. Searching that fallback with a string needle
in the [resistance code][lima-combat-blows] fails under native `member_array`;
normalize the representation or handle both shapes when adapting those
consumers. Other consumers, including natural armour and the
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
the source comment describing zero. Native timer callbacks have no player;
spell and material actions using Lima's `this_body()` still need their caller
context adapted. The checks exercise the real module, hooks and native timers
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
The [corpse module][lima-combat-corpses]'s literal `stop_fight(0)` still needs
caller adaptation. Reciprocal `stop_hitting_me` uses
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
and real inventory traversal. The [heartbeat pulse][lima-combat-pulse] check
records scheduler requests and calls the heartbeat manually; it does not
establish native heartbeat scheduling. Pulse-module adaptation remains pending.
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
recording `do_wear` callback. The helper now compiles independently; full armour,
worn-state behavior and body entry remain unvalidated. The
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
human compilation reaches the [stats base][lima-stats-base], where `filter_mapping`
and literal-zero string resets require adaptation.

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
The adapted master and complete simul library were exercised on lpc-rs. This
establishes the tested startup contracts, not a complete Lima
boot or execution on FluffOS.

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
The reviewed list's `/secure/obj/shell` entry must be `/obj/secure/shell`.

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
state when porting those factories. Lifecycle permissions, factory adaptation
and user login remain separate porting work.

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
| Rule inspection | The upstream [`parse_my_rules` reference][fl-parser-owned-doc] describes a no-argument inspector, but the reviewed [declaration][fl-parser-spec] and [implementation][fl-parser-owned] instead expose the parsing operation above. | [`query_parser_rules()`](efun/query_parser_rules.md) lists the caller's registrations as `"verb rule"` strings; [`parse_dump()`](efun/parse_dump.md) lists all owners' registrations as text. |

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
[lima-combat-behaviors]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/behaviors.c
[lima-combat-condition]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/condition.c
[lima-combat-targets]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/target.c
[lima-combat-corpses]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/death/corpses.c
[lima-combat-pulse]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/pulse/heart_beat.c
[lima-combat-messages]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/messages.c
[lima-combat-armour]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/armour/armour_base.c
[lima-combat-wield]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/wield/wield_base.c
[lima-stats-base]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/std/adversary/stats/base.c
[lima-drunk]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/include/drunk.h
[lima-body-definitions]: https://github.com/limalib/lima/blob/eefaff66adcae9b01e16059574c925b0d63e70a1/lib/data/daemons/body.o
