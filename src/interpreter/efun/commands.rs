use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::registry::Family,
    interpreter::{
        efun,
        efun::efun_context::EfunContext,
        lpc_ref::{LpcRef, NULL},
    },
};

/// `commands([ob])`: the rules the object (the caller by default) can
/// command, one `({ verb, flag, owner, function })` per rule in
/// registration order — the `add_action` flag (0 for another surface),
/// the registering object (0 once destructed), the handler's name (0 for
/// a parser rule, whose handlers are named by protocol).
pub async fn commands<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(process) = efun::arg_or_this_object(context.arg(0), context).await? else {
        return Err(context.runtime_error("commands: the argument must be an object"));
    };
    let rules = process.rules_of(context.txn());
    let rows: Vec<LpcRef> = rules
        .iter()
        .map(|rule| {
            let flag = match &rule.family {
                Family::AddAction { matching, .. } => matching.flag(),
                Family::Native { .. } | Family::Parser(_) => 0,
            };
            let owner = rule
                .owner()
                .map_or(NULL, |owner| LpcRef::from(Arc::downgrade(&owner)));
            let function = rule
                .pointer()
                .map_or(NULL, |pointer| LpcRef::from(pointer.name()));
            context.mint_array([
                LpcRef::from(rule.verb.as_str()),
                LpcRef::from(flag),
                owner,
                function,
            ])
        })
        .collect();
    let result = context.mint_array(rows);
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{committed_global, run_prog, test_config, try_run_prog},
    };

    #[tokio::test]
    async fn every_rule_is_a_row() {
        let code = indoc! { r#"
            string summary = "";
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action("do_say", "'", 1);
                add_action(&do_get(), ({ "get", "take" }));
                add_action("do_whisper", "whisper", 3);
                add_action((: 1 :), "nop");
                add_rule("'frob' %s", "do_frob");
                mixed *rows = commands();
                for (int i = 0; i < sizeof(rows); i++) {
                    mixed *r = rows[i];
                    summary += sprintf("%s:%d:%d:%s ", r[0], r[1], r[2] == this_object(), r[3]);
                }
            }
            int do_look(string s) { return 1; }
            int do_say(string s) { return 1; }
            int do_get(string s) { return 1; }
            int do_whisper(string s) { return 1; }
            int do_frob(string s) { return 1; }
        "# };
        let task = run_prog(code).await;
        assert_eq!(
            committed_global(&task, "summary"),
            LpcRef::from(
                "look:0:1:do_look ':1:1:do_say get:0:1:do_get take:0:1:do_get whisper:3:1:do_whisper nop:0:1:closure-0 frob:0:1:do_frob "
            )
        );
    }

    #[tokio::test]
    async fn an_object_without_rules_has_none() {
        let result = run_prog("int create() { return sizeof(commands()); }")
            .await
            .result();
        assert_eq!(result, Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn the_argument_names_another_object_by_path_too() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code(
            "/p.c",
            indoc! { r#"
                void create() {
                    set_this_player(this_object());
                    enable_commands();
                    add_action("do_look", "look");
                }
                int do_look(string s) { return 1; }
            "# },
        )
        .await
        .unwrap();
        let result = vm
            .initialize_process_from_code(
                "/asker.c",
                r#"int create() { return sizeof(commands("/p")) * 10 + sizeof(commands(find_object("/p"))); }"#,
            )
            .await
            .unwrap()
            .result();
        assert_eq!(result, Some(LpcRef::from(11)));
    }

    #[tokio::test]
    async fn a_non_object_is_an_error() {
        let err = try_run_prog("int create() { mixed x = 1; return sizeof(commands(x)); }")
            .await
            .expect_err("1 is not an object")
            .to_string();
        assert!(
            err.contains("commands: the argument must be an object"),
            "{err}"
        );
    }
}
