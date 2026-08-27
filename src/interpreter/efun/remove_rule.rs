use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::registry::RuleId,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, stm::MergeOp},
};

/// `remove_rule`, an efun that unregisters the calling object's rule with
/// this id from `this_player()`, every verb of it; 1 when one was there.
pub async fn remove_rule<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(id) = context.resolve_local_register(1 as RegisterSize).clone() else {
        return Err(context.runtime_error("remove_rule: the id must be an int"));
    };
    let Some(player) = context.this_player().load_full() else {
        return Err(context.runtime_error("remove_rule: no this_player()"));
    };
    let Ok(id) = u64::try_from(id.0) else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    let id = RuleId(id);
    let this_object = &context.frame().process;
    let held = player
        .rules_of(context.txn())
        .iter()
        .any(|rule| rule.id == id && rule.owned_by(this_object));
    if held {
        context
            .txn()
            .with(|t| t.merge(player.rules.id, MergeOp::RulesRemove(id)));
    }
    context.return_efun_result(LpcRef::from(held));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn removing_by_id_drops_every_verb_of_the_registration_once() {
        let code = indoc! { r#"
            int first; int second;
            void create() {
                set_this_player(this_object());
                enable_commands();
                int id = add_rule("'give' / 'hand' %w", "do_give");
                add_rule("'look'", "do_look");
                first = remove_rule(id);
                second = remove_rule(id);
            }
            int do_give(string what) { return 1; }
            int do_look() { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        let verbs: Vec<String> = vm
            .global_state
            .committed_rules(&proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        assert_eq!(verbs, vec!["look"]);
        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(
            vm.global_state.committed_global(&proc, 1u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn only_the_registering_object_can_remove_a_rule() {
        let other = "int zap(int id) { return remove_rule(id); }";
        let code = indoc! { r#"
            int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                int id = add_rule("'look'", "do_look");
                r = "/other"->zap(id);
            }
            int do_look() { return 1; }
        "# };
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/other.c", other)
            .await
            .unwrap();
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_rules(&proc).len(), 1);
        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn no_player_is_an_error() {
        let code = "void create() { remove_rule(1); }";
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err();
        assert!(
            err.to_string().contains("remove_rule: no this_player()"),
            "{err}"
        );
    }
}
