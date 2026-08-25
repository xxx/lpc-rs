//! One command line through a living's rules: the pre-hook, the trial in
//! precedence order, and (for a line nothing handled) the fallback.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::{
    command::{
        frontend::{add_action::verb_matches, arguments_and_verb},
        grammar::parse,
        registry::{Rule, scope_of},
    },
    compile_time_config::MAX_COMMAND_DEPTH,
    interpreter::{
        CATCH_TELL, COMMAND_NOT_FOUND, PROCESS_INPUT,
        function_type::function_ptr::FunctionPtr,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        stm::Effect,
        task::apply_function::apply_function,
        task_context::{CommandState, TaskContext},
    },
    telnet::ops::ConnectionOp,
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
    if !actor.is_live(ctx.txn()) || !actor.commands_enabled(ctx.txn()) {
        return Ok(Outcome::Unhandled);
    }
    if ctx.command.lock().len() >= MAX_COMMAND_DEPTH {
        return Err(lpc_error!(
            "command: nesting deeper than {MAX_COMMAND_DEPTH}"
        ));
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
    let outcome = match outcome {
        Ok(Outcome::Unhandled) => {
            // The trial left the last candidate's verb behind; the fallback reports the typed one.
            ctx.with_command(|state| {
                if let Some(state) = state {
                    state.verb_reported = state.verb_typed.clone();
                }
            });
            fallback(ctx, &actor, &line)
                .await
                .map(|()| Outcome::Unhandled)
        }
        other => other,
    };
    ctx.command.lock().pop();
    outcome
}

/// Run `line` as `actor` for a connection: a body that never called
/// `enable_commands()` still gets the pre-hook and the fallback.
pub(crate) async fn dispatch_from_connection(
    ctx: &TaskContext,
    actor: Arc<Process>,
    line: &str,
) -> Result<Outcome> {
    if !actor.is_live(ctx.txn()) {
        return Ok(Outcome::Unhandled);
    }
    if actor.commands_enabled(ctx.txn()) {
        return dispatch(ctx, actor, line).await;
    }
    if pre_hook(ctx, &actor, line).await?.is_none() {
        return Ok(Outcome::Handled);
    }
    let message = default_message(ctx, &actor);
    deliver(ctx, &actor, &message).await?;
    Ok(Outcome::Unhandled)
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
        if !rule.handler.receiver_is_live(ctx.txn()) {
            continue;
        }
        let (args, reported) = arguments_and_verb(rule, &parsed, line);
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

/// The message for a line nothing handled: the pending `notify_fail`, else
/// the master's `command_not_found`, else the driver default.
async fn fallback(ctx: &TaskContext, actor: &Arc<Process>, line: &str) -> Result<()> {
    let pending = ctx.with_command(|state| state.and_then(|state| state.notify_fail.take()));
    let message = match pending {
        Some(LpcRef::String(message)) => Some(message.to_string()),
        Some(LpcRef::Function(closure)) => notify_closure(ctx, actor, &closure).await?,
        Some(_) => None,
        None => master_message(ctx, actor, line).await?,
    };
    if let Some(message) = message {
        deliver(ctx, actor, &message).await?;
    }
    Ok(())
}

/// A `notify_fail` closure's string is the message; any other result means
/// it reported the failure itself.
async fn notify_closure(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    closure: &FunctionPtr,
) -> Result<Option<String>> {
    if !closure.receiver_is_live(ctx.txn()) {
        return Ok(None);
    }
    let handler_ctx = ctx.clone().with_process(actor.clone());
    handler_ctx.this_player.store(Some(actor.clone()));
    let Some(resolved) = closure.prepare_call(&[], &handler_ctx).await? else {
        return Ok(None);
    };
    let timeout = ctx.config().max_execution_time;
    let result = apply_function(
        resolved.function,
        &resolved.args,
        handler_ctx.with_process(resolved.process),
        Some(timeout),
    )
    .await?;
    Ok(match result {
        LpcRef::String(message) => Some(message.to_string()),
        _ => None,
    })
}

/// `master->command_not_found(actor, line)`: a string is the message, `0`
/// is silence; an undefined hook yields the driver default.
async fn master_message(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
) -> Result<Option<String>> {
    let master = ctx.object_space().master_object();
    let hook = master.as_ref().and_then(|master| {
        master
            .program
            .unmangled_functions
            .get(COMMAND_NOT_FOUND)
            .cloned()
    });
    let (Some(master), Some(hook)) = (master, hook) else {
        return Ok(Some(default_message(ctx, actor)));
    };
    let args = [
        LpcRef::Object(Arc::downgrade(actor)),
        LpcString::from(line).into(),
    ];
    Ok(match apply_on(ctx, &master, actor, hook, &args).await? {
        LpcRef::String(message) => Some(message.to_string()),
        _ => None,
    })
}

const NOT_IMPLEMENTED_HINT: &str = concat!(
    "Error: *I* received your command, but the game hasn't implemented any way to handle it. ",
    "Please tell the game's owner to implement `process_input` (and call `enable_commands()`) ",
    "in your body.\n"
);

/// `What?`, or the implementation hint for a body with neither a pre-hook
/// nor any rule.
fn default_message(ctx: &TaskContext, actor: &Arc<Process>) -> String {
    let bare = actor
        .program
        .unmangled_functions
        .get(PROCESS_INPUT)
        .is_none()
        && actor.rules_of(ctx.txn()).is_empty();
    if bare {
        NOT_IMPLEMENTED_HINT.to_owned()
    } else {
        "What?\n".to_owned()
    }
}

/// Deliver `message` to `actor` through `catch_tell`, else straight to its
/// connection, else the debug log — as effects, so nothing reaches the
/// player unless the command commits.
pub(crate) async fn deliver(ctx: &TaskContext, actor: &Arc<Process>, message: &str) -> Result<()> {
    if let Some(catch_tell) = actor.program.unmangled_functions.get(CATCH_TELL).cloned() {
        apply_on(
            ctx,
            actor,
            actor,
            catch_tell,
            &[LpcString::from(message).into()],
        )
        .await?;
        return Ok(());
    }
    let connection = ctx.txn().with(|t| t.read_connection(actor.connection.id));
    let effect = match connection {
        Some(connection) => Effect::Socket {
            op: ConnectionOp::SendMessage(message.to_owned()),
            tx: connection.tx.clone(),
        },
        None => Effect::DebugLog(message.to_owned()),
    };
    ctx.txn().with(|t| t.record_effect(effect));
    Ok(())
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

    #[tokio::test]
    async fn notify_fail_is_delivered_when_nothing_handles_the_line() {
        let code = indoc! { r#"
            string heard; int r;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_open", "open"); r = command("open door"); }
            int do_open(string a) { notify_fail("The door is stuck.\n"); return 0; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(
            globals(code, 2).await,
            vec![s("The door is stuck.\n"), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn the_last_notify_fail_wins_and_returns_zero() {
        let code = indoc! { r#"
            string heard; int first; mixed pending;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_a", "go"); add_action("do_b", "go"); command("go"); }
            int do_b(string a) { first = notify_fail("b\n"); return 0; }
            int do_a(string a) { notify_fail("a\n"); pending = query_notify_fail(); return 0; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(
            globals(code, 3).await,
            vec![s("a\n"), LpcRef::from(0), s("a\n")]
        );
    }

    #[tokio::test]
    async fn a_notify_fail_closure_supplies_the_message() {
        let code = indoc! { r#"
            string heard;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_open", "open"); command("open door"); }
            int do_open(string a) { notify_fail((: "You can't " + query_verb() + " that.\n" :)); return 0; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(globals(code, 1).await, vec![s("You can't open that.\n")]);
    }

    #[tokio::test]
    async fn a_notify_fail_closure_sees_the_typed_verb_after_other_rules_were_tried() {
        let code = indoc! { r#"
            string heard;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_open", "open");
                add_action("do_other", "op", 1);
                command("open door");
            }
            int do_other(string a) { return 0; }
            int do_open(string a) { notify_fail((: query_verb() + "?\n" :)); return 0; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(globals(code, 1).await, vec![s("open?\n")]);
    }

    #[tokio::test]
    async fn a_handled_line_delivers_nothing() {
        let code = indoc! { r#"
            mixed heard;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_open", "open"); command("open door"); }
            int do_open(string a) { notify_fail("never\n"); return 1; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(globals(code, 1).await, vec![LpcRef::from(0)]);
    }

    #[tokio::test]
    async fn without_notify_fail_the_master_supplies_the_message() {
        let master = indoc! { r#"
            string seen;
            string command_not_found(object who, string line) { seen = line; return "Huh? " + line + "\n"; }
        "# };
        let player = indoc! { r#"
            string heard;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); command("xyzzy plugh"); }
            int do_look(string a) { return 1; }
            void catch_tell(string m) { heard = m; }
        "# };
        let vm = Vm::new(test_config());
        let master_proc = vm
            .initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap()
            .context
            .process;
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&master_proc, 0u16),
            s("xyzzy plugh")
        );
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            s("Huh? xyzzy plugh\n")
        );
    }

    #[tokio::test]
    async fn a_zero_from_the_master_is_silent() {
        let master = indoc! { r#"
            int command_not_found(object who, string line) { return 0; }
        "# };
        let player = indoc! { r#"
            mixed heard;
            void create() { set_this_player(this_object()); enable_commands(); command("xyzzy"); }
            void catch_tell(string m) { heard = m; }
        "# };
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn the_driver_default_is_what_or_the_hint() {
        let with_rules = indoc! { r#"
            string heard;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); command("xyzzy"); }
            int do_look(string a) { return 1; }
            void catch_tell(string m) { heard = m; }
        "# };
        assert_eq!(globals(with_rules, 1).await, vec![s("What?\n")]);

        let bare = indoc! { r#"
            string heard;
            void create() { set_this_player(this_object()); enable_commands(); command("xyzzy"); }
            void catch_tell(string m) { heard = m; }
        "# };
        let heard = globals(bare, 1).await.remove(0).to_string();
        assert!(
            heard.starts_with("Error: *I* received your command"),
            "{heard}"
        );
        assert!(heard.ends_with("in your body.\n"), "{heard}");
    }

    #[tokio::test]
    async fn a_destructed_actor_runs_nothing() {
        let victim = indoc! { r#"
            int ran;
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_look", "look"); }
            int do_look(string a) { ran = 1; return 1; }
        "# };
        let player = indoc! { r#"
            int r;
            void create() { object v = find_object("/victim"); destruct(v); r = command("look", v); }
        "# };
        let vm = Vm::new(test_config());
        let victim_proc = vm
            .initialize_process_from_code("/victim.c", victim)
            .await
            .unwrap()
            .context
            .process;
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            LpcRef::from(0)
        );
        assert_eq!(
            vm.global_state.committed_global(&victim_proc, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn a_rule_whose_handler_receiver_is_destructed_is_skipped() {
        let third = "int do_look(string a) { return 1; }";
        let player = indoc! { r#"
            string arg; int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_action(&(find_object("/third"))->do_look(), "look");
                destruct(find_object("/third"));
                r = command("look here");
            }
            int do_look(string a) { arg = a; return 1; }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/third.c", third)
            .await
            .unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            s("here")
        );
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 1u16),
            LpcRef::from(1)
        );
    }

    /// One nested command costs ~85KB of native stack in a debug build, so the
    /// recursion runs where the cap has room to fire.
    #[test]
    fn nesting_deeper_than_the_cap_is_an_error() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_again", "again");
                command("again");
            }
            int do_again(string a) { command("again"); return 1; }
        "# };
        let runner = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(move || {
                tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("a current-thread runtime")
                    .block_on(async {
                        let vm = Vm::new(test_config());
                        vm.initialize_process_from_code("/player.c", code)
                            .await
                            .expect_err("the handler recurses forever")
                            .to_string()
                    })
            })
            .expect("a thread with room for the recursion");
        let err = runner.join().expect("the runner panicked");
        assert!(err.contains("nesting deeper than 16"), "{err}");
    }
}
