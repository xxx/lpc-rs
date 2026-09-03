use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;
use ustr::Ustr;

use crate::{
    command::registry::{Family, Rule, VerbMatch},
    interpreter::{
        efun::efun_context::EfunContext,
        function_type::{
            function_address::FunctionAddress,
            function_ptr::{FunctionPtr, FunctionPtrBuilder},
        },
        lpc_ref::LpcRef,
        stm::MergeOp,
    },
};

/// `add_action`, an efun that registers a verb on `this_player()`, handled
/// by a function of this object or a bound function pointer.
pub fn add_action<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(player) = context.this_player().load_full() else {
        return Err(context.runtime_error("add_action: no this_player() to attach to"));
    };
    if !player.commands_enabled(context.txn()) {
        return Err(context.runtime_error("add_action: this_player() is not a living object"));
    }

    let handler = handler_from(
        context,
        context.resolve_local_register(1 as RegisterSize).clone(),
        "add_action",
    )?;

    let verb_ref = context.resolve_local_register(2 as RegisterSize);
    let verbs: Vec<Ustr> = match verb_ref {
        LpcRef::String(s) => vec![Ustr::from(s.to_str())],
        LpcRef::Array(_) => {
            let items: Vec<LpcRef> =
                verb_ref.with_array(context.txn(), |array| array.array.to_vec())?;
            let mut verbs = Vec::with_capacity(items.len());
            for item in items {
                let LpcRef::String(s) = item else {
                    return Err(context.runtime_error("add_action: verbs must be strings"));
                };
                verbs.push(Ustr::from(s.to_str()));
            }
            verbs
        }
        _ => {
            return Err(
                context.runtime_error("add_action: the verb must be a string or string array")
            );
        }
    };

    // An omitted default arg is still a live register, filled with NULL
    // rather than left absent.
    let flag_ref = context
        .try_resolve_local_register(3 as RegisterSize)
        .filter(|r| !r.is_null());
    let flag = match flag_ref {
        None => 0,
        Some(LpcRef::Int(i)) => i.0,
        Some(_) => return Err(context.runtime_error("add_action: the flag must be an int")),
    };
    let Some(matching) = VerbMatch::from_flag(flag) else {
        return Err(context.runtime_error(format!("add_action: unknown flag {flag}")));
    };

    let owner = &context.frame().process;
    let rules: Vec<Rule> = verbs
        .into_iter()
        .map(|verb| {
            Rule::new(
                owner,
                verb,
                Family::AddAction {
                    matching,
                    pointer: handler.clone(),
                },
            )
        })
        .collect();
    context.txn().with(|t| {
        for rule in rules {
            t.merge(player.rules.id, MergeOp::RulesAppend(rule));
        }
    });

    Ok(())
}

/// The handler for a rule: a function of this object by name, or a bound
/// function pointer as `call_out` requires; `efun` prefixes the messages.
pub(crate) fn handler_from<const N: usize>(
    context: &EfunContext<'_, N>,
    arg: LpcRef,
    efun: &str,
) -> Result<Arc<FunctionPtr>> {
    match arg {
        LpcRef::String(name) => {
            let this_object = &context.frame().process;
            let Some(function) = this_object.program.unmangled_functions.get(name.to_str()) else {
                return Err(context.runtime_error(format!(
                    "{efun}: no function `{}` in {}",
                    name.to_str(),
                    this_object.filename()
                )));
            };
            let owner = Arc::downgrade(this_object);
            let ptr = FunctionPtrBuilder::default()
                .owner(owner.clone())
                .address(FunctionAddress::Local(owner, function.clone()))
                .build()
                .map_err(|e| context.runtime_bug(format!("{efun}: {e}")))?;
            Ok(Arc::new(ptr))
        }
        LpcRef::Function(ptr) => {
            if !ptr.receiver_bound() {
                return Err(context.runtime_error(format!(
                    "{efun}: the receiver of a dynamic function pointer must be bound"
                )));
            }
            Ok(ptr)
        }
        _ => Err(context.runtime_error(format!(
            "{efun}: the handler must be a function name or a function pointer"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        command::registry::{ArgSpan, Reported, VerbMatch},
        interpreter::{CommittedReader, vm::Vm},
        test_support::test_config,
    };

    #[tokio::test]
    async fn every_shape_registers_on_this_player() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action("do_say", "'", 1);
                add_action((: do_get :), ({ "get", "take" }));
                add_action("do_whisper", "whisper", 3);
            }
            int do_look(string s) { return 1; }
            int do_say(string s) { return 1; }
            int do_get(string s) { return 1; }
            int do_whisper(string s) { return 1; }
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
        assert_eq!(verbs, vec!["look", "'", "get", "take", "whisper"]);
        assert_eq!(rules[0].matching(), VerbMatch::Exact);
        assert_eq!(
            rules[1].matching(),
            VerbMatch::Prefix {
                reports: Reported::Full,
                args: ArgSpan::RestOfLine
            }
        );
        assert_eq!(
            rules[4].matching(),
            VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfWord
            }
        );
        assert!(
            rules
                .iter()
                .all(|r| r.owner().is_some_and(|o| std::ptr::eq(&*o, &*proc)))
        );
    }

    #[tokio::test]
    async fn a_missing_function_is_an_error() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("nope", "look");
            }
        "# };
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err();
        assert!(
            err.to_string().contains("add_action: no function `nope`"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_non_living_player_is_an_error() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                add_action("do_look", "look");
            }
            int do_look(string s) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("not a living object"), "{err}");
    }

    #[tokio::test]
    async fn no_player_is_an_error() {
        let code = indoc! { r#"
            void create() { add_action("do_look", "look"); }
            int do_look(string s) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("no this_player()"), "{err}");
    }

    #[tokio::test]
    async fn an_unknown_flag_is_an_error() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look", 9);
            }
            int do_look(string s) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let err = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("unknown flag 9"), "{err}");
    }

    #[tokio::test]
    async fn a_rejected_attempt_registers_nothing() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                int j = 0;
                int x = 10 / j;
            }
            int do_look(string s) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/player.c", code)
            .await
            .expect_err("the initializer divides by zero");
        let proc = vm.global_state.object_space.lookup("/player").unwrap();
        assert!(vm.global_state.committed_rules(&proc).is_empty());
    }
}
