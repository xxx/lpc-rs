use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::native::compile,
        registry::{Family, Rule},
    },
    interpreter::{
        efun::{add_action::handler_from, efun_context::EfunContext},
        lpc_ref::LpcRef,
        stm::MergeOp,
    },
};

/// `add_rule`, an efun that registers a pattern on `this_player()`: one rule
/// per leading verb, all under the returned id.
pub fn add_rule<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(player) = context.this_player().load_full() else {
        return Err(context.runtime_error("add_rule: no this_player() to attach to"));
    };
    if !player.commands_enabled(context.txn()) {
        return Err(context.runtime_error("add_rule: this_player() is not a living object"));
    }

    let LpcRef::String(pattern) = context.resolve_local_register(1 as RegisterSize).clone() else {
        return Err(context.runtime_error("add_rule: the pattern must be a string"));
    };
    let compiled =
        compile(pattern.to_str()).map_err(|e| context.runtime_error(format!("add_rule: {e}")))?;
    let handler = handler_from(
        context,
        context.resolve_local_register(2 as RegisterSize).clone(),
        "add_rule",
    )?;

    let Some((first_verb, other_verbs)) = compiled.verbs.split_first() else {
        return Err(context.runtime_bug("add_rule: a compiled pattern has no verb"));
    };
    let first = Rule::new(
        &context.frame().process,
        *first_verb,
        Family::Native {
            compiled: compiled.clone(),
            pointer: handler,
        },
    );
    let id = i64::try_from(first.id.0)
        .map_err(|_| context.runtime_bug("add_rule: rule ids exceeded the int range"))?;
    let siblings: Vec<Rule> = other_verbs
        .iter()
        .map(|verb| first.sibling(*verb))
        .collect();
    context.txn().with(|t| {
        t.merge(player.rules.id, MergeOp::RulesAppend(first));
        for rule in siblings {
            t.merge(player.rules.id, MergeOp::RulesAppend(rule));
        }
    });
    context.return_efun_result(LpcRef::from(id));
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use crate::{
        command::registry::{Family, Rule, RuleId, VerbMatch},
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn a_pattern_registers_one_rule_per_verb_under_the_returned_id() {
        let code = indoc! { r#"
            int id;
            void create() {
                set_this_player(this_object());
                enable_commands();
                id = add_rule("'give' / 'hand' %w 'to' %w", "do_give");
            }
            int do_give(string what, string whom) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let rules = vm.global_state.committed_rules(&proc);
        let verbs: Vec<&str> = rules.iter().map(|r| r.verb.as_str()).collect();
        assert_eq!(verbs, vec!["give", "hand"]);
        let LpcRef::Int(id) = vm.global_state.committed_global(&proc, 0u16) else {
            panic!("add_rule returns an int");
        };
        let id = RuleId(u64::try_from(id.0).unwrap());
        assert!(rules.iter().all(|r| r.id == id));
        assert!(
            rules
                .iter()
                .all(|r| matches!(r.family, Family::Native { .. }))
        );
        assert!(rules.iter().all(|r| r.matching() == VerbMatch::Exact));
        let compiled_of = |r: &Rule| match &r.family {
            Family::Native { compiled, .. } => compiled.clone(),
            _ => panic!("a native rule"),
        };
        assert!(Arc::ptr_eq(
            &compiled_of(&rules[0]),
            &compiled_of(&rules[1])
        ));
        assert!(
            rules
                .iter()
                .all(|r| r.owner().is_some_and(|o| std::ptr::eq(&*o, &*proc)))
        );
    }

    #[tokio::test]
    async fn a_function_pointer_handler_is_accepted() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'wave'", (: do_wave :));
            }
            int do_wave() { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_rules(&proc).len(), 1);
    }

    async fn error_of(code: &str) -> String {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err()
            .to_string()
    }

    #[tokio::test]
    async fn a_pattern_fault_reaches_the_caller_with_its_text() {
        let unterminated = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'get", "do_get");
            }
            int do_get() { return 1; }
        "# };
        let err = error_of(unterminated).await;
        assert!(err.contains("add_rule: a quote is not closed"), "{err}");
    }

    #[tokio::test]
    async fn a_noun_capture_with_nothing_in_scope_is_no_match() {
        let code = indoc! { r#"
            int id; int matched;
            void create() {
                set_this_player(this_object());
                enable_commands();
                id = add_rule("'get' %o", "do_get");
                matched = command("get sword");
            }
            int do_get(mixed ob) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let rules = vm.global_state.committed_rules(&proc);
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].verb.as_str(), "get");
        let LpcRef::Int(id) = vm.global_state.committed_global(&proc, 0u16) else {
            panic!("add_rule returns an int");
        };
        assert!(id.0 > 0, "the rule id should be a positive int");
        assert_eq!(
            vm.global_state.committed_global(&proc, 1u16),
            LpcRef::from(0),
            "the rule fails because nothing in scope answers to `sword`"
        );
    }

    #[tokio::test]
    async fn a_missing_function_names_add_rule() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'look'", "nope");
            }
        "# };
        let err = error_of(code).await;
        assert!(err.contains("add_rule: no function `nope`"), "{err}");
    }

    #[tokio::test]
    async fn the_verb_attached_cell_starts_empty() {
        let vm = Vm::new(test_config());
        assert!(vm.global_state.committed_verb_rules().is_empty());
    }

    #[tokio::test]
    async fn no_player_or_a_non_living_player_is_an_error() {
        let no_player = indoc! { r#"
            void create() { add_rule("'look'", "do_look"); }
            int do_look() { return 1; }
        "# };
        let err = error_of(no_player).await;
        assert!(err.contains("add_rule: no this_player()"), "{err}");

        let non_living = indoc! { r#"
            void create() { set_this_player(this_object()); add_rule("'look'", "do_look"); }
            int do_look() { return 1; }
        "# };
        let err = error_of(non_living).await;
        assert!(
            err.contains("add_rule: this_player() is not a living object"),
            "{err}"
        );
    }
}
