use std::{collections::HashSet, sync::Arc};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::registry::Rule,
    interpreter::{
        efun::efun_context::EfunContext, function_type::function_address::FunctionAddress,
        lpc_ref::LpcRef, process::Process, stm::MergeOp,
    },
};

/// `remove_action`, an efun that unregisters this object's rules for a verb:
/// `(verb)` from `this_player()`, `(function, verb)` the MudOS shape, or
/// `(verb, object)` the LDMud shape. Returns how many were removed.
pub fn remove_action<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let first = context.resolve_local_register(1 as RegisterSize).clone();
    let LpcRef::String(first) = first else {
        return Err(context.runtime_error("remove_action: the first argument must be a string"));
    };
    let this_object = context.process().clone();

    // An omitted default arg is still a live register, filled with NULL
    // rather than left absent.
    let second = context
        .try_resolve_local_register(2 as RegisterSize)
        .filter(|r| !r.is_null());
    let (verb, function_name, target): (String, Option<String>, Arc<Process>) = match second {
        None => (first.to_str().to_owned(), None, player(context)?),
        Some(LpcRef::String(verb)) => (
            verb.to_str().to_owned(),
            Some(first.to_str().to_owned()),
            player(context)?,
        ),
        Some(LpcRef::Object(weak)) => {
            let Some(target) = weak.upgrade() else {
                context.return_efun_result(LpcRef::from(0));
                return Ok(());
            };
            (first.to_str().to_owned(), None, target)
        }
        Some(_) => {
            return Err(context
                .runtime_error("remove_action: the second argument must be a verb or an object"));
        }
    };

    let rules = target.rules_of(context.txn());
    let doomed: HashSet<_> = rules
        .iter()
        .filter(|rule| rule.verb.as_str() == verb)
        .filter(|rule| std::ptr::eq(rule.owner.as_ptr(), Arc::as_ptr(&this_object)))
        .filter(|rule| {
            function_name
                .as_deref()
                .is_none_or(|name| handles(rule, name))
        })
        .map(|rule| rule.id)
        .collect();
    // A native registration shares one id across every verb, so a doomed id can match several rules here.
    let removed = rules
        .iter()
        .filter(|rule| doomed.contains(&rule.id))
        .count();
    context.txn().with(|t| {
        for id in &doomed {
            t.merge(target.rules.id, MergeOp::RulesRemove(*id));
        }
    });
    context.return_efun_result(LpcRef::from(removed as i64));
    Ok(())
}

/// The living the rules hang on: `this_player()`, or a runtime error.
fn player<const N: usize>(context: &EfunContext<'_, N>) -> Result<Arc<Process>> {
    context
        .this_player()
        .load_full()
        .ok_or_else(|| context.runtime_error("remove_action: no this_player()"))
}

/// Whether the rule's handler is this object's function named `name`.
fn handles(rule: &Rule, name: &str) -> bool {
    let Some(pointer) = rule.pointer() else {
        return false;
    };
    matches!(&pointer.address, FunctionAddress::Local(_, f) if f.prototype.name.as_ref() == name)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    async fn verbs_after(code: &str) -> (Vec<String>, LpcRef) {
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let verbs = vm
            .global_state
            .committed_rules(&proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        (verbs, vm.global_state.committed_global(&proc, 0u16))
    }

    #[tokio::test]
    async fn one_argument_removes_this_objects_rules_for_the_verb() {
        let code = indoc! { r#"
            int removed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action("do_look", "look");
                add_action("do_get", "get");
                removed = remove_action("look");
            }
            int do_look(string s) { return 1; }
            int do_get(string s) { return 1; }
        "# };
        let (verbs, removed) = verbs_after(code).await;
        assert_eq!(verbs, vec!["get"]);
        assert_eq!(removed, LpcRef::from(2));
    }

    #[tokio::test]
    async fn the_mudos_shape_removes_by_function_and_verb() {
        let code = indoc! { r#"
            int removed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action("do_peek", "look");
                removed = remove_action("do_peek", "look");
            }
            int do_look(string s) { return 1; }
            int do_peek(string s) { return 1; }
        "# };
        let (verbs, removed) = verbs_after(code).await;
        assert_eq!(verbs, vec!["look"]);
        assert_eq!(removed, LpcRef::from(1));
    }

    #[tokio::test]
    async fn the_ldmud_shape_removes_from_a_named_object() {
        let code = indoc! { r#"
            int removed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                removed = remove_action("look", this_object());
            }
            int do_look(string s) { return 1; }
        "# };
        let (verbs, removed) = verbs_after(code).await;
        assert!(verbs.is_empty());
        assert_eq!(removed, LpcRef::from(1));
    }

    #[tokio::test]
    async fn removing_one_verb_of_a_native_registration_removes_every_alternative() {
        let code = indoc! { r#"
            int removed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'give' / 'hand' %w", "do_give");
                removed = remove_action("give");
            }
            int do_give(string what) { return 1; }
        "# };
        let (verbs, removed) = verbs_after(code).await;
        assert!(verbs.is_empty());
        assert_eq!(removed, LpcRef::from(2));
    }

    #[tokio::test]
    async fn removing_an_unknown_verb_removes_nothing() {
        let code = indoc! { r#"
            int removed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                removed = remove_action("dance");
            }
            int do_look(string s) { return 1; }
        "# };
        let (verbs, removed) = verbs_after(code).await;
        assert_eq!(verbs, vec!["look"]);
        assert_eq!(removed, LpcRef::from(0));
    }
}
