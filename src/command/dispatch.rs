//! One command line through a living's rules: the pre-hook, the trial in
//! precedence order, and (for a line nothing handled) the fallback.

use std::sync::Arc;

use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    command::{
        frontend::add_action::{argument, reported_verb, verb_matches},
        grammar::parse,
        registry::{Frontend, Rule, scope_of},
    },
    interpreter::{
        PROCESS_INPUT,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        task::apply_function::apply_function,
        task_context::{CommandState, TaskContext},
    },
};

/// Whether a line was handled by a rule or the pre-hook.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    /// A rule (or the pre-hook, by consuming the line) handled it.
    Handled,
    /// Nothing handled it.
    Unhandled,
}

/// Run `line` as `actor` inside `ctx`'s transaction.
pub async fn dispatch(ctx: &TaskContext, actor: Arc<Process>, line: &str) -> Result<Outcome> {
    if !actor.commands_enabled(ctx.txn()) {
        return Ok(Outcome::Unhandled);
    }
    let Some(line) = pre_hook(ctx, &actor, line).await? else {
        return Ok(Outcome::Handled);
    };
    let first_word = line.split_whitespace().next().unwrap_or("").to_owned();
    ctx.command.lock().push(CommandState {
        line: line.clone(),
        verb_typed: first_word.clone(),
        verb_reported: first_word.clone(),
        notify_fail: None,
    });
    let outcome = trial(ctx, &actor, &line, &first_word).await;
    ctx.command.lock().pop();
    outcome
}

/// `process_input`: a string replaces the line, `0` or no hook passes it
/// through, anything else consumes it (`None`).
async fn pre_hook(ctx: &TaskContext, actor: &Arc<Process>, line: &str) -> Result<Option<String>> {
    let Some(hook) = actor
        .program
        .unmangled_functions
        .get(PROCESS_INPUT)
        .cloned()
    else {
        return Ok(Some(line.to_owned()));
    };
    let result = apply_on(ctx, actor, actor, hook, &[LpcString::from(line).into()]).await?;
    Ok(match result {
        LpcRef::String(replacement) => Some(replacement.to_string()),
        LpcRef::Int(LpcInt(0)) => Some(line.to_owned()),
        _ => None,
    })
}

/// Rules in precedence order, each tried until one handles the line.
async fn trial(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    first_word: &str,
) -> Result<Outcome> {
    let rules = actor.rules_of(ctx.txn());
    let scope = scope_of(ctx.txn(), actor);
    let mut candidates: Vec<&Rule> = rules
        .iter()
        .filter(|rule| {
            rule.owner()
                .is_some_and(|owner| owner.is_live(ctx.txn()) && scope.contains(&owner))
        })
        .filter(|rule| verb_matches(rule.verb.as_str(), rule.matching, first_word))
        .collect();
    candidates.sort_by_key(|rule| std::cmp::Reverse(rule.id));

    for rule in candidates {
        let Some(parsed) = parse(&rule.grammar, line).next() else {
            continue;
        };
        let args = match rule.source {
            Frontend::AddAction => {
                let arg = argument(rule.verb.as_str(), rule.matching, &parsed, line);
                vec![LpcString::from(arg.as_str()).into()]
            }
        };
        let reported = reported_verb(rule.verb.as_str(), rule.matching, &parsed, line);
        ctx.with_command(|state| {
            if let Some(state) = state {
                state.verb_reported = reported;
            }
        });

        let handler_ctx = ctx.clone().with_process(actor.clone());
        handler_ctx.this_player.store(Some(actor.clone()));
        let Some(resolved) = rule.handler.prepare_call(&args, &handler_ctx).await? else {
            continue;
        };
        let timeout = ctx.config().max_execution_time;
        let result = apply_function(
            resolved.function,
            &resolved.args,
            handler_ctx.with_process(resolved.process),
            Some(timeout),
        )
        .await?;
        if !matches!(result, LpcRef::Int(LpcInt(0))) {
            return Ok(Outcome::Handled);
        }
    }
    Ok(Outcome::Unhandled)
}

/// Apply `function` on `target` with `this_player` set, joining `ctx`'s
/// transaction.
pub(crate) async fn apply_on(
    ctx: &TaskContext,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
    function: Arc<ProgramFunction>,
    args: &[LpcRef],
) -> Result<LpcRef> {
    let nested = ctx.clone().with_process(target.clone());
    nested.this_player.store(Some(this_player.clone()));
    let timeout = ctx.config().max_execution_time;
    apply_function(function, args, nested, Some(timeout)).await
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, lpc_string::LpcString, vm::Vm},
        test_support::test_config,
    };

    fn s(text: &str) -> LpcRef {
        LpcString::from(text).into()
    }

    /// Initializes `/player.c` from `code` and returns its committed globals in
    /// declaration order.
    async fn globals(code: &str, count: u16) -> Vec<LpcRef> {
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        (0..count)
            .map(|slot| vm.global_state.committed_global(&proc, slot))
            .collect()
    }

    #[tokio::test]
    async fn the_most_recent_rule_is_tried_first_and_zero_falls_through() {
        let code = indoc! { r#"
            string look_arg; string look_verb; int nope_tried; int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action("do_nope", "look");
                r = command("look   at   me");
            }
            int do_look(string arg) { look_arg = arg; look_verb = query_verb(); return 1; }
            int do_nope(string arg) { nope_tried = 1; return 0; }
        "# };
        let g = globals(code, 4).await;
        assert_eq!(
            g,
            vec![s("at   me"), s("look"), LpcRef::from(1), LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn a_prefix_verb_reports_the_typed_word() {
        let code = indoc! { r#"
            string arg; string verb; string full; string line;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_say", "'", 1);
                command("'hello there");
            }
            int do_say(string a) { arg = a; verb = query_verb(); full = query_verb(1); line = query_command(); return 1; }
        "# };
        let g = globals(code, 4).await;
        assert_eq!(
            g,
            vec![
                s("hello there"),
                s("'hello"),
                s("'hello"),
                s("'hello there")
            ]
        );
    }

    #[tokio::test]
    async fn no_matching_rule_is_unhandled() {
        let code = indoc! { r#"
            int dance; int lookat;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                dance = command("dance");
                lookat = command("lookat me");
            }
            int do_look(string arg) { return 1; }
        "# };
        assert_eq!(
            globals(code, 2).await,
            vec![LpcRef::from(0), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn a_nested_command_restores_the_outer_verb() {
        let code = indoc! { r#"
            string outer; string inner; string after;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_outer", "outer");
                add_action("do_inner", "inner");
                command("outer x");
            }
            int do_outer(string a) { outer = query_verb(); command("inner y"); after = query_verb(); return 1; }
            int do_inner(string a) { inner = query_verb(); return 1; }
        "# };
        assert_eq!(
            globals(code, 3).await,
            vec![s("outer"), s("inner"), s("outer")]
        );
    }

    #[tokio::test]
    async fn query_verb_outside_a_command_is_zero() {
        let code = indoc! { r#"
            mixed verb; mixed line;
            void create() { verb = query_verb(); line = query_command(); }
        "# };
        assert_eq!(
            globals(code, 2).await,
            vec![LpcRef::from(0), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn a_non_living_actor_is_unhandled() {
        let code = indoc! { r#"
            int r;
            void create() { set_this_player(this_object()); r = command("look"); }
        "# };
        assert_eq!(globals(code, 1).await, vec![LpcRef::from(0)]);
    }

    #[tokio::test]
    async fn an_explicit_actor_runs_its_own_rules() {
        let code = indoc! { r#"
            int r; string arg;
            void create() {
                enable_commands();
                set_this_player(this_object());
                add_action("do_look", "look");
                set_this_player(0);
                r = command("look here", this_object());
            }
            int do_look(string a) { arg = a; return 1; }
        "# };
        assert_eq!(globals(code, 2).await, vec![LpcRef::from(1), s("here")]);
    }

    #[tokio::test]
    async fn process_input_rewrites_passes_or_consumes() {
        let rewrite = indoc! { r#"
            string arg; int r;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); r = command("at me"); }
            string process_input(string line) { return "look " + line; }
            int do_look(string a) { arg = a; return 1; }
        "# };
        assert_eq!(globals(rewrite, 2).await, vec![s("at me"), LpcRef::from(1)]);

        let consume = indoc! { r#"
            int called; int r;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); r = command("look"); }
            int process_input(string line) { return 1; }
            int do_look(string a) { called = 1; return 1; }
        "# };
        assert_eq!(
            globals(consume, 2).await,
            vec![LpcRef::from(0), LpcRef::from(1)]
        );

        let pass = indoc! { r#"
            int called; int r;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); r = command("look"); }
            int process_input(string line) { return 0; }
            int do_look(string a) { called = 1; return 1; }
        "# };
        assert_eq!(
            globals(pass, 2).await,
            vec![LpcRef::from(1), LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn a_handler_error_aborts_the_command() {
        let code = indoc! { r#"
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_boom", "boom"); command("boom"); }
            int do_boom(string a) { int j; return 1 / j; }
        "# };
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/player.c", code)
            .await
            .expect_err("the handler divides by zero");
    }
}
