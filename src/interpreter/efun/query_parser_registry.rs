//! Structured inspection of parser-package registrations.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::registry::VerbRules,
    interpreter::{efun::efun_context::EfunContext, lpc_mapping::LpcMapping, lpc_ref::LpcRef},
};

/// List live parser registrations, globally or restricted to an owner.
pub fn query_parser_registry<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let selected = match context.arg(0) {
        value @ LpcRef::Object(_) => {
            let Some(owner) = value.live_object(context.txn()) else {
                context.return_array([]);
                return Ok(());
            };
            Some(owner)
        }
        value if value == &context.txn().nil() => None,
        _ => return Err(context.runtime_error("query_parser_registry: expected an object or nil")),
    };
    let rules = VerbRules::new(context.task_context()).all();
    let entries = rules.iter().filter_map(|rule| {
        let protocol = rule.protocol()?;
        let owner = rule.owner()?;
        if !owner.is_live(context.txn())
            || selected
                .as_ref()
                .is_some_and(|selected| !Arc::ptr_eq(selected, &owner))
        {
            return None;
        }
        let fields = [
            ("verb", rule.verb.as_str().into()),
            ("rule", protocol.rule.as_str().into()),
            ("owner", Arc::downgrade(&owner).into()),
            ("handler_verb", protocol.verb.as_str().into()),
        ];
        let mapping = LpcMapping::new(
            fields
                .into_iter()
                .map(|(key, value)| (key.into(), value))
                .collect(),
        );
        Some(LpcRef::Mapping(
            context.txn().with(|t| t.mint_mapping(mapping)),
        ))
    });
    let result = context.mint_array(entries);
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            process::Process,
            stm::{Transaction, TxnHandle, commit_changeset, start_txn},
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::permissive_master,
    };

    const OWNER: &str = r#"
        void create() {
            parse_init();
            parse_add_rule("look", "");
            parse_add_rule("look", "at OBJ");
            parse_add_synonym("examine", "look", "at OBJ");
        }
        void drop() { parse_remove("look"); }
    "#;

    async fn vm(mode: NullMode) -> Vm {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        permissive_master(&vm.global_state.object_space).await;
        vm
    }

    async fn load(vm: &Vm, name: &str, source: &str) -> Arc<Process> {
        vm.initialize_process_from_code(name, source)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process
    }

    async fn call(vm: &Vm, process: Arc<Process>, name: &str) -> Result<LpcRef, String> {
        apply_function_by_name(
            name,
            &[],
            process,
            TaskTemplate::from(vm.global_state.clone()),
            Some(4000),
        )
        .await
        .expect("test method exists")
        .map_err(|e| e.diagnostic_string())
    }

    #[tokio::test]
    async fn empty_queries_return_arrays_through_direct_and_pointer_calls() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                int check() {
                    function query = query_parser_registry;
                    mapping *direct = query_parser_registry();
                    mapping *pointer = query();
                    return arrayp(direct) && !sizeof(direct)
                        && arrayp(pointer) && !sizeof(pointer)
                        && !sizeof(query_parser_registry(nil))
                        && !sizeof(query_parser_registry(this_object()));
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn records_preserve_multiword_boundaries_whitespace_and_owner_identity() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                void create() {
                    parse_init();
                    parse_add_rule("go", "to OBJ");
                    parse_add_rule("go to", "OBJ");
                    parse_add_rule(" \tLook\t at  ", "  with\tOBJ\n ");
                    parse_add_rule("bare word", "");
                }
                int check() {
                    mapping *rows = query_parser_registry();
                    if (sizeof(rows) != 4) return 0;
                    foreach (mapping row : rows) {
                        if (sizeof(row) != 4 || row["owner"] != this_object()
                            || row["handler_verb"] != row["verb"]) return 0;
                    }
                    return rows[0]["verb"] == "go" && rows[0]["rule"] == "to OBJ"
                        && rows[1]["verb"] == "go to" && rows[1]["rule"] == "OBJ"
                        && rows[2]["verb"] == "Look at" && rows[2]["rule"] == "  with\tOBJ\n "
                        && rows[3]["verb"] == "bare word" && rows[3]["rule"] == "";
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn global_and_owner_queries_preserve_duplicate_registration_order() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            load(&vm, "/first.c", OWNER).await;
            load(&vm, "/second.c", OWNER).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                int check() {
                    object first = find_object("/first"), second = find_object("/second");
                    mapping *rows = query_parser_registry();
                    mapping *owned = query_parser_registry(first);
                    function query = query_parser_registry;
                    if (sizeof(rows) != 6 || sizeof(owned) != 3
                        || sizeof(query(nil)) != 6 || sizeof(query(second)) != 3
                        || sizeof(query_parser_registry(this_object()))) return 0;
                    for (int i = 0; i < 6; i++) {
                        if (rows[i]["owner"] != (i < 3 ? first : second)) return 0;
                        if (i < 3 && owned[i]["verb"] != rows[i]["verb"]) return 0;
                    }
                    return rows[0]["verb"] == "look" && rows[0]["rule"] == ""
                        && rows[1]["verb"] == "look" && rows[1]["rule"] == "at OBJ"
                        && rows[2]["verb"] == "examine" && rows[2]["handler_verb"] == "look";
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn synonym_chains_report_the_original_handler_verb() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                void create() {
                    parse_init();
                    parse_add_rule("base word", "OBJ");
                    parse_add_synonym("alias word", "base word");
                    parse_add_synonym("last alias", "alias word");
                }
                int check() {
                    mapping *rows = query_parser_registry(this_object());
                    return sizeof(rows) == 3 && rows[0]["verb"] == "base word"
                        && rows[1]["verb"] == "alias word" && rows[2]["verb"] == "last alias"
                        && rows[1]["handler_verb"] == "base word"
                        && rows[2]["handler_verb"] == "base word"
                        && rows[2]["rule"] == "OBJ";
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn actor_action_and_native_rule_tables_are_excluded() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                int tick() { return 1; }
                void create() {
                    enable_commands();
                    set_this_player(this_object());
                    add_action(tick, "action");
                    add_rule("'native'", tick);
                    parse_init();
                    parse_add_rule("parser", "OBJ");
                }
                int check() {
                    mapping *rows = query_parser_registry();
                    return sizeof(rows) == 1 && rows[0]["verb"] == "parser";
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn changing_arrays_records_and_strings_does_not_change_the_registry() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                void create() {
                    parse_init();
                    parse_add_rule("look", "at OBJ");
                    parse_add_synonym("see", "look");
                }
                int check() {
                    string dump = parse_dump();
                    string *owned = query_parser_rules();
                    mapping *first = query_parser_registry();
                    mapping *second = query_parser_registry();
                    if (first == second || first[0] == second[0] || first[0] == first[1]) return 0;
                    first[0]["verb"] = "changed";
                    first[0]["rule"][0] = 'X';
                    first[0]["owner"] = nil;
                    first[0]["handler_verb"] = "changed";
                    first[1] = ([ "verb": "replacement" ]);
                    mapping *fresh = query_parser_registry();
                    return sizeof(fresh) == 2 && fresh[0]["verb"] == "look"
                        && fresh[0]["rule"] == "at OBJ" && fresh[1]["verb"] == "see"
                        && fresh[1]["rule"] == "at OBJ" && fresh[1]["handler_verb"] == "look"
                        && fresh[0]["owner"] == this_object()
                        && second[0]["verb"] == "look" && second[0]["rule"] == "at OBJ"
                        && parse_dump() == dump && query_parser_rules()[0] == owned[0]
                        && query_parser_rules()[1] == owned[1];
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn pending_registration_and_owner_removal_are_visible_before_and_after_commit() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            load(&vm, "/first.c", OWNER).await;
            load(&vm, "/second.c", OWNER).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                void create() {
                    parse_init();
                    parse_add_rule("pending", "");
                    if (sizeof(query_parser_registry()) != 7) throw("missing pending rule");
                    find_object("/first")->drop();
                    if (!check()) throw("wrong pending removal view");
                }
                int check() {
                    mapping *rows = query_parser_registry();
                    return sizeof(rows) == 4 && rows[0]["owner"] == find_object("/second")
                        && rows[3]["verb"] == "pending"
                        && !sizeof(query_parser_registry(find_object("/first")));
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn destroyed_owner_filters_stay_empty_instead_of_becoming_global_queries() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let _held = load(&vm, "/first.c", OWNER).await;
            load(&vm, "/second.c", OWNER).await;
            let process = load(
                &vm,
                "/caller.c",
                r#"
                object dead;
                void create() {
                    dead = find_object("/first");
                    destruct(dead);
                    if (!check()) throw("destroyed owner is still visible");
                }
                int check() {
                    mapping *rows = query_parser_registry();
                    function query = query_parser_registry;
                    return sizeof(rows) == 3 && rows[0]["owner"] == find_object("/second")
                        && !sizeof(query_parser_registry(dead)) && !sizeof(query(dead))
                        && sizeof(query_parser_registry(nil)) == 3;
                }
            "#,
            )
            .await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn registrations_after_self_destruction_are_not_exposed() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            load(&vm, "/other.c", OWNER).await;
            let task = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r#"
                int create() {
                    parse_init();
                    parse_add_rule("before", "");
                    destruct(this_object());
                    parse_add_rule("after", "OBJ");
                    mapping *rows = query_parser_registry();
                    return sizeof(rows) == 3 && rows[0]["owner"] == find_object("/other")
                        && !sizeof(query_parser_registry(this_object()));
                }
            "#,
                )
                .await
                .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn aborted_registration_removal_and_destruction_leave_the_registry_unchanged() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            load(&vm, "/other.c", OWNER).await;
            let process = load(&vm, "/caller.c", r#"
                void create() { parse_init(); }
                void fail() {
                    parse_add_rule("aborted", "OBJ");
                    object other = find_object("/other");
                    other->drop();
                    destruct(other);
                    mapping *rows = query_parser_registry();
                    if (sizeof(rows) != 1 || rows[0]["verb"] != "aborted") throw("wrong pending view");
                    throw("abort parser registry changes");
                }
                int check() {
                    object other = find_object("/other");
                    mapping *rows = query_parser_registry();
                    return objectp(other) && sizeof(rows) == 3
                        && sizeof(query_parser_registry(other)) == 3
                        && rows[2]["verb"] == "examine"
                        && !sizeof(query_parser_registry(this_object()));
                }
            "#).await;
            let error = call(&vm, process.clone(), "fail").await.unwrap_err();
            assert!(error.contains("abort parser registry changes"), "{error}");
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn runtime_rejects_non_object_selectors_with_mode_specific_nil() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let process = load(&vm, "/caller.c", r#"
                void create() { parse_init(); parse_add_rule("look", ""); }
                int tick() { return 1; }
                int check() {
                    function query = query_parser_registry;
                    foreach (mixed value : ({ -1, 1, 1.0, "owner", ({}), ([]), tick })) {
                        mixed direct_error = catch(query_parser_registry(value));
                        if (!stringp(direct_error) || strindex(direct_error, "expected an object or nil") < 0)
                            return 0;
                        if (!catch(query(value))) return 0;
                    }
                    mixed zero = 0;
                    mixed zero_error = catch(query_parser_registry(zero));
                    return sizeof(query_parser_registry(nil)) == 1
                        && ((nil == 0) ? !zero_error : !!zero_error);
                }
            "#).await;
            assert_eq!(
                call(&vm, process, "check").await.unwrap(),
                LpcRef::from(1),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn global_and_filtered_queries_conflict_with_a_later_registry_edit() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            let reader = load(
                &vm,
                "/reader.c",
                &format!(
                    "{OWNER}
                    int global() {{ return sizeof(query_parser_registry()); }}
                    int owned() {{ return sizeof(query_parser_registry(this_object())); }}"
                ),
            )
            .await;
            let writer = load(
                &vm,
                "/writer.c",
                "void create() { parse_init(); }
                 void add() { parse_add_rule(\"later\", \"OBJ\"); }",
            )
            .await;
            for method in ["global", "owned"] {
                let mut live = start_txn(&vm.global_state.committer_tx).await.unwrap();
                let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
                let mut template = TaskTemplate::from(vm.global_state.clone());
                template.txn = txn.clone();
                let result =
                    apply_function_by_name(method, &[], reader.clone(), template, Some(4000))
                        .await
                        .unwrap()
                        .unwrap();
                assert_eq!(result, LpcRef::from(3));
                call(&vm, writer.clone(), "add").await.unwrap();
                let changeset = txn.with(|t| t.take_changeset());
                live.disarm();
                let conflict = commit_changeset(&vm.global_state.committer_tx, changeset)
                    .await
                    .unwrap()
                    .unwrap_err();
                assert_eq!(
                    conflict.cell(),
                    Some(vm.global_state.object_space.verb_rules.id),
                    "{mode}, {method}: {conflict}"
                );
            }
        }
    }

    #[tokio::test]
    async fn compiler_checks_selector_types_arity_and_mapping_array_result() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = vm(mode).await;
            for (source, expected) in [
                (
                    "mixed create() { return query_parser_registry(nil, nil); }",
                    "incorrect argument count",
                ),
                (
                    "mixed create() { return query_parser_registry(\"owner\"); }",
                    "unexpected argument type",
                ),
                (
                    "mixed create() { return query_parser_registry(1.0); }",
                    "unexpected argument type",
                ),
                (
                    "mixed create() { return query_parser_registry(({})); }",
                    "unexpected argument type",
                ),
                (
                    "mixed create() { return query_parser_registry(([])); }",
                    "unexpected argument type",
                ),
                (
                    "mixed create() { string *bad = query_parser_registry(); return bad; }",
                    "mismatched types",
                ),
            ] {
                let error = vm
                    .initialize_process_from_code("/bad.c", source)
                    .await
                    .unwrap_err()
                    .to_string();
                assert!(error.contains(expected), "{mode}, {source}: {error}");
            }
        }
    }
}
