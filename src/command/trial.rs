//! One line tried against rules in precedence order inside one command
//! frame, shared by command dispatch and the parser efuns.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use ustr::Ustr;

use crate::{
    command::{
        frontend::{add_action, native},
        parser::{self, Completion, Nickname, Options, Rejection, RejectionCode, Verdict},
        registry::{ActorRules, Family, Rule, VerbRules},
        resolve::{LpcVocabulary, Resolver},
        scope::{SuppliedScope, neighbourhood},
    },
    compile_time_config::MAX_COMMAND_DEPTH,
    interpreter::{
        apply::{apply_pointer, as_actor, deliver},
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
        stm::TxnHandle,
        task_context::{CommandState, TaskContext},
    },
};

/// Fail when another frame would exceed `MAX_COMMAND_DEPTH`; `entry` names
/// the caller in the message (`command`, `parse_sentence`). Call before the
/// pre-hook; push the `Frame` after it, so `process_input` sees the outer
/// frame.
pub(crate) fn depth_guard(ctx: &TaskContext, entry: &str) -> Result<()> {
    if ctx.command.lock().len() >= MAX_COMMAND_DEPTH {
        return Err(lpc_error!(
            "{entry}: nesting deeper than {MAX_COMMAND_DEPTH}"
        ));
    }
    Ok(())
}

/// One line's `CommandState`, pushed for its lifetime and popped on drop —
/// on the error path too.
pub(crate) struct Frame<'a> {
    ctx: &'a TaskContext,
}

impl<'a> Frame<'a> {
    /// Push the frame for `line`, whose verb is `first_word` — after
    /// `depth_guard` and the pre-hook.
    pub(crate) fn push(ctx: &'a TaskContext, line: &str, first_word: &str) -> Self {
        ctx.command.lock().push(CommandState {
            line: line.to_owned(),
            verb_typed: first_word.to_owned(),
            verb_reported: first_word.to_owned(),
            notify_fail: None,
        });
        Self { ctx }
    }
}

impl Drop for Frame<'_> {
    fn drop(&mut self) {
        self.ctx.command.lock().pop();
    }
}

/// A parser phrase's query context, restored on success, refusal or unwind.
struct VerbFrame<'a> {
    ctx: &'a TaskContext,
    previous: Option<(String, String)>,
}

impl<'a> VerbFrame<'a> {
    fn enter(ctx: &'a TaskContext, registered: &str, typed: &str) -> Self {
        let previous = ctx.with_command(|state| {
            state.map(|state| {
                (
                    std::mem::replace(&mut state.verb_reported, registered.to_owned()),
                    std::mem::replace(&mut state.verb_typed, typed.to_owned()),
                )
            })
        });
        Self { ctx, previous }
    }
}

impl Drop for VerbFrame<'_> {
    fn drop(&mut self) {
        if let Some((registered, typed)) = self.previous.take() {
            self.ctx.with_command(|state| {
                if let Some(state) = state {
                    state.verb_reported = registered;
                    state.verb_typed = typed;
                }
            });
        }
    }
}

/// The phrase context retained until every parser rule has failed.
struct PendingRejection<'a> {
    verb: Ustr,
    typed: &'a str,
    rejection: Rejection,
}

/// Rules in precedence order — the actor's own, then the verb-attached
/// parser phrases — each tried until one handles the line;
/// `true` when one did. The verb-attached cell is read only when the actor's
/// own rules did not handle the line — it stays out of most transactions'
/// read sets.
pub(crate) async fn run(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    first_word: &str,
) -> Result<bool> {
    let scope = neighbourhood(ctx.txn(), actor);
    let candidates = ActorRules::new(ctx.txn(), actor).matching(first_word, &scope);

    let mut resolver = Resolver::new(
        LpcVocabulary::new(ctx, as_actor(ctx, actor), scope.members()),
        None,
    );
    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for — `command` calls `dispatch` unboxed.
    if Box::pin(try_actor_rules(ctx, actor, line, &mut resolver, candidates)).await? {
        return Ok(true);
    }
    match Box::pin(sentence_trial(ctx, actor, line, None, Options::default())).await? {
        Sentence::Handled => Ok(true),
        Sentence::Message(message) => {
            deliver(ctx, || as_actor(ctx, actor), actor, Some(actor), &message).await?;
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Try the actor's native and add_action rules before reading the parser registry.
async fn try_actor_rules(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    resolver: &mut Resolver<LpcVocabulary<'_>>,
    candidates: Vec<Rule>,
) -> Result<bool> {
    for rule in candidates {
        if rule
            .pointer()
            .is_some_and(|pointer| !pointer.receiver_is_live(ctx.txn()))
        {
            continue;
        }
        let (pointer, args, reported) = match &rule.family {
            Family::AddAction { matching, pointer } => {
                let Some((args, reported)) =
                    add_action::arguments_and_verb(rule.verb.as_str(), *matching, line)
                else {
                    continue;
                };
                (pointer, args, reported)
            }
            Family::Native { compiled, pointer } => {
                let Some(args) = Box::pin(native::arguments(compiled, line, resolver)).await?
                else {
                    continue;
                };
                (pointer, args, rule.verb.to_string())
            }
            Family::Parser(_) => continue,
        };
        ctx.with_command(|state| {
            if let Some(state) = state {
                state.verb_reported = reported;
            }
        });

        let Some(result) = apply_pointer(ctx, actor, pointer, &args).await? else {
            continue;
        };
        if !matches!(result, LpcRef::Nil | LpcRef::Int(LpcInt(0))) {
            return Ok(true);
        }
    }
    Ok(false)
}

/// A parser efun's outcome.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Sentence {
    /// A verb-attached rule's `do_` ran.
    Handled,
    /// A restricted parse's generic action arguments, without executing the action.
    Arguments(Vec<LpcRef>),
    /// No registered parser verb matches a leading phrase of the line.
    NoVerb,
    /// No verb-attached rule's grammar parsed the rest of the line.
    NoParse,
    /// A handler refused and the master gave no message.
    Refused,
    /// An object phrase did not resolve and the master gave no message.
    Unresolved,
    /// The master's message for the failure.
    Message(String),
}

impl Sentence {
    /// The parser efuns share status codes, message strings and argument arrays.
    pub(crate) fn into_value(self, txn: &TxnHandle) -> LpcRef {
        match self {
            Self::Handled => LpcRef::from(1),
            Self::Arguments(args) => {
                LpcRef::Array(txn.with(|t| t.mint_array(args.into_iter().collect())))
            }
            Self::NoVerb => LpcRef::from(0),
            Self::NoParse => LpcRef::from(-1),
            Self::Refused => LpcRef::from(-2),
            Self::Unresolved => LpcRef::from(-3),
            Self::Message(message) => LpcRef::from(message.as_str()),
        }
    }

    /// The outcome when no rule handled the line or gave a message:
    /// `Unresolved` over `Refused` over `NoParse`; `NoParse` for none.
    pub(crate) fn worst(codes: &[RejectionCode]) -> Sentence {
        if codes.contains(&RejectionCode::Unresolved) {
            Sentence::Unresolved
        } else if codes.contains(&RejectionCode::Refused) {
            Sentence::Refused
        } else {
            Sentence::NoParse
        }
    }
}

/// Run `line` for `actor` over the parser rules only, as `parse_sentence`
/// does: the same transaction, depth limit and command state as
/// `dispatch`, but no pre-hook, no fallback, nothing delivered.
pub(crate) async fn sentence(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    scope: Option<SuppliedScope>,
    nicknames: &[Nickname],
) -> Result<Sentence> {
    depth_guard(ctx, "parse_sentence")?;
    let first_word = line.split_whitespace().next().unwrap_or("").to_owned();
    let _frame = Frame::push(ctx, line, &first_word);
    sentence_trial(
        ctx,
        actor,
        line,
        None,
        Options {
            scope: scope.as_ref(),
            nicknames,
            ..Options::default()
        },
    )
    .await
}

/// Parse only the caller's registrations, with an explicit actor for every callback.
pub(crate) async fn owned_sentence(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    owner: &Arc<Process>,
    line: &str,
    completion: Completion,
) -> Result<Sentence> {
    depth_guard(ctx, "parse_my_rules")?;
    let ctx = ctx.clone();
    ctx.this_player.store(Some(actor.clone()));
    let first_word = line.split_whitespace().next().unwrap_or("");
    let _frame = Frame::push(&ctx, line, first_word);
    sentence_trial(
        &ctx,
        actor,
        line,
        Some(owner),
        Options {
            completion,
            ..Options::default()
        },
    )
    .await
}

/// Try every matching parser phrase before reporting any failure.
async fn sentence_trial(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    only_owner: Option<&Arc<Process>>,
    options: Options<'_>,
) -> Result<Sentence> {
    let mut candidates = VerbRules::new(ctx).for_line(line);
    if let Some(owner) = only_owner {
        candidates.retain(|matched| matched.rule.owned_by(owner));
    }
    if candidates.is_empty() {
        return Ok(Sentence::NoVerb);
    }
    let mut rejections = Vec::new();
    for matched in candidates {
        let rule = &matched.rule;
        let Some(owner) = rule.owner().filter(|owner| owner.is_live(ctx.txn())) else {
            continue;
        };
        let Some(parser) = rule.protocol() else {
            continue;
        };
        let _verb = VerbFrame::enter(ctx, rule.verb.as_str(), matched.typed);
        let verdict = Box::pin(parser::run(
            ctx,
            actor,
            &owner,
            parser,
            matched.rest,
            options,
        ))
        .await?;
        match verdict {
            Verdict::Handled => return Ok(Sentence::Handled),
            Verdict::Arguments(args) => return Ok(Sentence::Arguments(args)),
            Verdict::Rejected(rejection) => rejections.push(PendingRejection {
                verb: rule.verb,
                typed: matched.typed,
                rejection,
            }),
            Verdict::NoParse => {}
        }
    }
    report_rejections(ctx, actor, rejections).await
}

/// Return the first message in trial order, else combine silent failure codes.
async fn report_rejections(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    rejections: Vec<PendingRejection<'_>>,
) -> Result<Sentence> {
    let mut codes = Vec::with_capacity(rejections.len());
    for pending in rejections {
        let _verb = VerbFrame::enter(ctx, pending.verb.as_str(), pending.typed);
        codes.push(pending.rejection.code);
        if let Some(message) = pending.rejection.report(ctx, actor).await? {
            return Ok(Sentence::Message(message));
        }
    }
    Ok(Sentence::worst(&codes))
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{interpreter::vm::Vm, test_support::test_config};

    #[test]
    fn unresolved_outranks_refused_outranks_no_parse() {
        assert_eq!(
            Sentence::worst(&[RejectionCode::Refused]),
            Sentence::Refused
        );
        assert_eq!(
            Sentence::worst(&[
                RejectionCode::Refused,
                RejectionCode::Unresolved,
                RejectionCode::Refused,
            ]),
            Sentence::Unresolved
        );
    }

    #[test]
    fn no_verdicts_is_no_parse() {
        assert_eq!(Sentence::worst(&[]), Sentence::NoParse);
    }

    /// The 16MiB thread gives the cap room to fire before a debug-build stack runs out.
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
