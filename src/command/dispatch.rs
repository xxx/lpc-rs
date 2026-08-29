//! One command line's pipeline: the pre-hook, the command frame, the trial
//! in `trial`, and (for a line nothing handled) the fallback.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::trial,
    interpreter::{
        COMMAND_NOT_FOUND, PROCESS_INPUT,
        apply::{apply_hook, apply_pointer, deliver},
        function_type::function_ptr::FunctionPtr,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        lpc_string::LpcString,
        process::Process,
        task_context::TaskContext,
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
    if !actor.is_live(ctx.txn()) || !actor.commands_enabled(ctx.txn()) {
        return Ok(Outcome::Unhandled);
    }
    trial::depth_guard(ctx, "command")?;
    let Some(line) = pre_hook(ctx, &actor, line).await? else {
        return Ok(Outcome::Handled);
    };
    let first_word = line.split_whitespace().next().unwrap_or("").to_owned();
    let _frame = trial::Frame::push(ctx, &line, &first_word);
    if trial::run(ctx, &actor, &line, &first_word).await? {
        return Ok(Outcome::Handled);
    }
    // The trial left the last candidate's verb behind.
    ctx.with_command(|state| {
        if let Some(state) = state {
            state.verb_reported = state.verb_typed.clone();
        }
    });
    fallback(ctx, &actor, &line).await?;
    Ok(Outcome::Unhandled)
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
    deliver(ctx, &actor, Some(&actor), &message).await?;
    Ok(Outcome::Unhandled)
}

/// `process_input`: a string replaces the line, `0` or no hook passes it
/// through, anything else consumes it (`None`).
async fn pre_hook(ctx: &TaskContext, actor: &Arc<Process>, line: &str) -> Result<Option<String>> {
    Ok(
        match apply_hook(
            ctx,
            actor,
            actor,
            PROCESS_INPUT,
            &[LpcString::from(line).into()],
        )
        .await?
        {
            None => Some(line.to_owned()),
            Some(LpcRef::String(replacement)) => Some(replacement.to_string()),
            Some(LpcRef::Int(LpcInt(0))) => Some(line.to_owned()),
            Some(_) => None,
        },
    )
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
        deliver(ctx, actor, Some(actor), &message).await?;
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
    Ok(match apply_pointer(ctx, actor, closure, &[]).await? {
        Some(LpcRef::String(message)) => Some(message.to_string()),
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
    let Some(master) = ctx.object_space().master_object() else {
        return Ok(Some(default_message(ctx, actor)));
    };
    let args = [
        LpcRef::Object(Arc::downgrade(actor)),
        LpcString::from(line).into(),
    ];
    Ok(
        match apply_hook(ctx, &master, actor, COMMAND_NOT_FOUND, &args).await? {
            None => Some(default_message(ctx, actor)),
            Some(LpcRef::String(message)) => Some(message.to_string()),
            Some(_) => None,
        },
    )
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

    #[tokio::test]
    async fn a_native_rule_passes_each_capture_as_its_own_argument() {
        let code = indoc! { r#"
            string item; string target; string verb; int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'give' / 'hand' %w 'to' %s", "do_give");
                r = command("hand sword to the  guard");
            }
            int do_give(string what, string whom) { item = what; target = whom; verb = query_verb(); return 1; }
        "# };
        assert_eq!(
            globals(code, 4).await,
            vec![s("sword"), s("the  guard"), s("hand"), LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn a_number_capture_arrives_as_an_int_and_a_word_does_not_match_it() {
        let code = indoc! { r#"
            mixed count; int words; int r; int overflowed;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'take' %d", "do_take");
                words = command("take five");
                r = command("take 5");
                overflowed = command("take 99999999999999999999");
            }
            int do_take(int n) { count = n; return 1; }
        "# };
        assert_eq!(
            globals(code, 4).await,
            vec![
                LpcRef::from(5),
                LpcRef::from(0),
                LpcRef::from(1),
                LpcRef::from(0)
            ]
        );
    }

    #[tokio::test]
    async fn native_and_add_action_rules_share_one_precedence_order() {
        let code = indoc! { r#"
            string native_arg; string action_arg;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                add_rule("'look' [at] %w", "do_look_at");
                command("look at bob");
            }
            int do_look_at(string who) { native_arg = who; return 0; }
            int do_look(string arg) { action_arg = arg; return 1; }
        "# };
        assert_eq!(globals(code, 2).await, vec![s("bob"), s("at bob")]);
    }

    #[tokio::test]
    async fn a_room_rule_registered_from_init_dispatches_after_the_move() {
        let room = indoc! { r#"
            string seen;
            void init() { add_rule("'look' [at] %w", "do_look"); }
            int do_look(string what) { seen = what; return 1; }
        "# };
        let player = indoc! { r#"
            int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                move_object("/room");
                r = command("look at sign");
            }
        "# };
        let vm = Vm::new(test_config());
        let room_proc = vm.create_process_from_code("/room.c", room).await.unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&room_proc, 0u16),
            s("sign")
        );
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn a_native_handler_returning_zero_falls_through_to_the_next_native_rule() {
        let code = indoc! { r#"
            int earlier_ran; int later_ran; int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'look'", "do_earlier");
                add_rule("'look'", "do_later");
                r = command("look");
            }
            int do_earlier() { earlier_ran = 1; return 1; }
            int do_later() { later_ran = 1; return 0; }
        "# };
        assert_eq!(
            globals(code, 3).await,
            vec![LpcRef::from(1), LpcRef::from(1), LpcRef::from(1)]
        );
    }

    /// Initializes `master` as the master, each of `objects` at its path in
    /// order, then `/player.c` from `player`; returns the player's first
    /// `count` committed globals.
    async fn scenario(
        master: &str,
        objects: &[(&str, &str)],
        player: &str,
        count: u16,
    ) -> Vec<LpcRef> {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        for (path, code) in objects {
            vm.initialize_process_from_code(path, code).await.unwrap();
        }
        let proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        (0..count)
            .map(|slot| vm.global_state.committed_global(&proc, slot))
            .collect()
    }

    const SWORD: (&str, &str) = (
        "/sword.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        string *parse_command_plural_id_list() { return ({ "swords" }); }
        string *parse_command_adjectiv_id_list() { return ({ "red" }); }
        void go(object dest) { move_object(dest); }
    "# },
    );

    const BAG: (&str, &str) = (
        "/bag.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "bag" }); }
        string *parse_command_adjectiv_id_list() { return ({ "old" }); }
        void go(object dest) { move_object(dest); }
    "# },
    );

    const SWORD2: (&str, &str) = (
        "/sword2.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        string *parse_command_plural_id_list() { return ({ "swords" }); }
        void go(object dest) { move_object(dest); }
    "# },
    );

    const ROOM: (&str, &str) = ("/room.c", "");

    #[tokio::test]
    async fn an_items_capture_hands_the_handler_a_numeral_and_the_objects() {
        let player = indoc! { r#"
            int r; int n; mixed numeral; int got;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                add_rule("'get' %i", "do_get");
                r = command("get red sword");
            }
            int do_get(mixed *items) { n = sizeof(items); numeral = items[0]; got = items[1] == find_object("/sword"); return 1; }
        "# };
        assert_eq!(
            scenario("", &[SWORD], player, 4).await,
            vec![
                LpcRef::from(1),
                LpcRef::from(2),
                LpcRef::from(1),
                LpcRef::from(1)
            ]
        );
    }

    #[tokio::test]
    async fn an_unresolved_phrase_falls_through_to_the_next_rule() {
        let player = indoc! { r#"
            int r; string any; int items_tried;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                add_rule("'get' %s", "do_any");
                add_rule("'get' %i", "do_get");
                r = command("get xyzzy");
            }
            int do_get(mixed *items) { items_tried = 1; return 1; }
            int do_any(string s) { any = s; return 1; }
        "# };
        assert_eq!(
            scenario("", &[SWORD], player, 3).await,
            vec![LpcRef::from(1), s("xyzzy"), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn numerals_and_the_all_word_come_from_the_master() {
        let master = indoc! { r#"
            int parse_command_numeral(string w) { if (w == "two") return 2; if (w == "second") return -2; return 0; }
            string parse_command_all_word() { return "alles"; }
        "# };
        let player = indoc! { r#"
            mixed two; mixed second; mixed alles; int alles_count;
            mixed last; int last_count;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                add_rule("'get' %i", "do_get");
                command("get two swords"); two = last;
                command("get second sword"); second = last;
                command("get alles"); alles = last; alles_count = last_count;
            }
            int do_get(mixed *items) { last = items[0]; last_count = sizeof(items) - 1; return 1; }
        "# };
        assert_eq!(
            scenario(master, &[SWORD], player, 4).await,
            vec![
                LpcRef::from(2),
                LpcRef::from(-2),
                LpcRef::from(0),
                LpcRef::from(2)
            ]
        );
    }

    #[tokio::test]
    async fn plurals_come_from_the_object_else_the_master_else_nowhere() {
        let master = indoc! { r#"
            string *parse_command_pluralize(string *s) { string *out = ({}); int i; for (i = 0; i < sizeof(s); i++) out += ({ s[i] + "s" }); return out; }
        "# };
        let bagless = (
            "/bag.c",
            indoc! { r#"
            string *parse_command_id_list() { return ({ "bag" }); }
            void go(object dest) { move_object(dest); }
        "# },
        );
        let player = indoc! { r#"
            mixed swords; mixed bags;
            mixed last;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                "/bag"->go(this_object());
                add_rule("'get' %i", "do_get");
                command("get swords"); swords = last;
                last = 7;
                command("get bags"); bags = last;
            }
            int do_get(mixed *items) { last = items[0]; return 1; }
        "# };
        assert_eq!(
            scenario(master, &[SWORD, bagless], player, 2).await,
            vec![LpcRef::from(0), LpcRef::from(0)]
        );
        assert_eq!(
            scenario("", &[SWORD, bagless], player, 2).await,
            vec![LpcRef::from(0), LpcRef::from(7)],
            "no pluralize apply: `bags` is unmatched, so `last` keeps its sentinel"
        );
    }

    #[tokio::test]
    async fn an_object_capture_is_the_first_match_and_living_takes_only_livings() {
        let player = indoc! { r#"
            int got; int poked_me; int poked_sword;
            string *parse_command_id_list() { return ({ "me" }); }
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                add_rule("'look' %o", "do_look");
                add_rule("'poke' %l", "do_poke");
                command("look sword");
                poked_me = command("poke me");
                poked_sword = command("poke sword");
            }
            int do_look(object ob) { got = ob == find_object("/sword"); return 1; }
            int do_poke(mixed *who) { return who[1] == this_object(); }
        "# };
        assert_eq!(
            scenario("", &[SWORD], player, 3).await,
            vec![LpcRef::from(1), LpcRef::from(1), LpcRef::from(0)]
        );
    }

    const OWN_THINGS_BEFORE_ROOMS_PLAYER: &str = indoc! { r#"
        int mine; string order;
        void create() {
            set_this_player(this_object());
            enable_commands();
            move_object("/room");
            "/sword2"->go(find_object("/room"));
            "/sword"->go(this_object());
            add_rule("'look' %o", "do_look");
            add_rule("'get' %i", "do_get");
            command("look sword");
            command("get swords");
        }
        int do_look(object ob) { mine = ob == find_object("/sword"); return 1; }
        int do_get(mixed *obs) { order = file_name(obs[1]) + " " + file_name(obs[2]); return 1; }
    "# };

    #[tokio::test]
    async fn an_object_capture_finds_the_actors_own_things_before_the_rooms() {
        assert_eq!(
            scenario(
                "",
                &[SWORD, SWORD2, ROOM],
                OWN_THINGS_BEFORE_ROOMS_PLAYER,
                1
            )
            .await,
            vec![LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn an_items_capture_lists_the_actors_own_things_before_the_rooms() {
        assert_eq!(
            scenario(
                "",
                &[SWORD, SWORD2, ROOM],
                OWN_THINGS_BEFORE_ROOMS_PLAYER,
                2
            )
            .await[1],
            s("/sword /sword2")
        );
    }

    #[tokio::test]
    async fn a_preposition_capture_is_the_matched_entry_of_the_masters_list() {
        let master = indoc! { r#"
            string *parse_command_prepos_list() { return ({ "in", "in front of" }); }
        "# };
        let player = indoc! { r#"
            string prep; string what; int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_rule("'look' %p %w", "do_look");
                r = command("look in front of box");
            }
            int do_look(string p, string w) { prep = p; what = w; return 1; }
        "# };
        assert_eq!(
            scenario(master, &[], player, 3).await,
            vec![s("in front of"), s("box"), LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn the_next_parse_is_tried_when_a_phrase_fails_to_resolve() {
        let player = indoc! { r#"
            int first; string word; int second;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword"->go(this_object());
                "/bag"->go(this_object());
                add_rule("'put' %i %w %i", "do_put");
                command("put red sword in old bag");
            }
            int do_put(mixed *a, string w, mixed *b) { first = a[1] == find_object("/sword"); word = w; second = b[1] == find_object("/bag"); return 1; }
        "# };
        assert_eq!(
            scenario("", &[SWORD, BAG], player, 3).await,
            vec![LpcRef::from(1), s("in"), LpcRef::from(1)]
        );
    }

    #[tokio::test]
    async fn an_object_without_lists_is_asked_id() {
        let rock = (
            "/rock.c",
            indoc! { r#"
            int id(string s) { return s == "rock"; }
            void go(object dest) { move_object(dest); }
        "# },
        );
        let player = indoc! { r#"
            int plain; int adorned;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/rock"->go(this_object());
                add_rule("'get' %i", "do_get");
                plain = command("get rock");
                adorned = command("get big rock");
            }
            int do_get(mixed *items) { return items[1] == find_object("/rock"); }
        "# };
        assert_eq!(
            scenario("", &[rock], player, 2).await,
            vec![LpcRef::from(1), LpcRef::from(0)]
        );
    }

    #[tokio::test]
    async fn a_list_apply_that_returns_no_array_contributes_nothing() {
        let mute = (
            "/mute.c",
            indoc! { r#"
            mixed parse_command_id_list() { return "thing"; }
            void go(object dest) { move_object(dest); }
        "# },
        );
        let player = indoc! { r#"
            int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/mute"->go(this_object());
                add_rule("'get' %i", "do_get");
                r = command("get thing");
            }
            int do_get(mixed *items) { return 1; }
        "# };
        assert_eq!(
            scenario("", &[mute], player, 1).await,
            vec![LpcRef::from(0)]
        );
    }

    const TAKE_VERB: (&str, &str) = (
        "/take_verb.c",
        indoc! { r#"
        void create() { parse_init(); parse_add_rule("take", "OBJ"); }
        void do_take_obj(object o, string w) { }
    "# },
    );
    const SWORD_ITEM: (&str, &str) = (
        "/sword_item.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        mixed direct_take_obj(object ob, string w) { return 1; }
        void go(object dest) { move_object(dest); }
    "# },
    );

    #[tokio::test]
    async fn leading_whitespace_before_the_verb_still_dispatches() {
        let player = indoc! { r#"
            int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                "/sword_item"->go(this_object());
                r = command("  take sword");
            }
        "# };
        assert_eq!(
            scenario("", &[TAKE_VERB, SWORD_ITEM], player, 1).await,
            vec![LpcRef::from(1)]
        );
    }

    const GROESSE_VERB: (&str, &str) = (
        "/groesse_verb.c",
        indoc! { r#"
        void create() { parse_init(); parse_add_rule("größe", "OBJ"); }
    "# },
    );

    /// Before the fix, slicing the untrimmed line by the verb's byte length
    /// landed inside `ß`'s two-byte encoding and panicked.
    #[tokio::test]
    async fn a_multibyte_verb_does_not_panic_on_leading_whitespace() {
        let player = indoc! { r#"
            int r;
            void create() {
                set_this_player(this_object());
                enable_commands();
                r = command("  größe sword");
            }
        "# };
        assert_eq!(
            scenario("", &[GROESSE_VERB], player, 1).await,
            vec![LpcRef::from(0)]
        );
    }
}
