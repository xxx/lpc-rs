# Keep ulib aligned with applies

Use this guide when changing a callback the driver invokes in LPC. ulib is a
distributed example of the current driver API, so its runnable code, disabled
examples and explanations belong in the same change as the API update.

## Update the contract and examples

1. Change the driver and add or adjust a behaviour test for the changed
   contract. Update the apply name constants for additions, removals or renames.
2. Update the relevant reference in [doc/apply](apply). Include changes to
   arguments, return values, missing-hook defaults, call order, transaction
   context, permissions and `this_player()`/`previous_object()` as applicable.
3. Find the apply in the [ulib index](../ulib/doc/applies.md). Update its live
   implementation or disabled example, the documentation beside the function,
   and the index entry. Check the [lifecycle explanation](../ulib/doc/how-it-works.md)
   and [command tutorial](../ulib/doc/add-a-command.md) if their lessons depend
   on the changed behaviour. Remove obsolete examples when removing an apply.
4. For parser-handler changes, review all four families and their generic
   fallbacks in [examples/verb.c](../ulib/examples/verb.c), as well as the
   [handler contract](apply/object/parser_handlers.md) and
   [rule documentation](efun/parse_add_rule.md).

The contracts in `doc/apply/` describe the driver; they must be updated even
when a change requires no adjustment to ulib's executable code. For example,
changing the meaning of a `0` return or the body receiving a callback can leave
every LPC signature valid while making an example's explanation wrong.

## Check and record the review

From the repository root, run:

```sh
cargo test --workspace --test ulib_test --test ulib
```

The checks compile every live file and enabled example, compare apply names
and signatures against the reference, validate the copied guide's links, and
exercise login and chat through real connections. The TCP test runs on Unix.
The normal workspace test job runs these checks in CI too.

The review check reports any added, changed or removed reference file since
the last ulib review. After reviewing and updating the affected examples and
prose, record those paths explicitly, for example:

```sh
python3 tests/update_ulib_apply_review.py doc/apply/special/logon.md
```

Pass multiple paths to record a related change. For a rename, pass both the old
and new paths; a reviewed deleted path is removed from the record. The script
updates only the named entries in
[tests/fixtures/ulib_apply_review.json](../tests/fixtures/ulib_apply_review.json).
It does not edit or verify documentation, so review comes first. A spelling
correction may need only a review-record update; a contract change usually
needs changes to source comments, the index, and sometimes executable LPC.

Rerun the targeted checks, then the workspace checks in [AGENTS.md](../AGENTS.md),
and include the review record in the same commit as the change.

The record is a reminder to review meaning, not proof of correctness. A driver
change that leaves its reference untouched cannot be inferred from these
digests. Keeping the canonical reference current and testing the changed
behaviour remain part of the apply-maintenance rule.
