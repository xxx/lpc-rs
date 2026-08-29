//! One line tried against rules in precedence order inside one command
//! frame; `dispatch` and `parse_sentence` are its two callers.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};

use crate::{
    command::{
        frontend::{
            add_action::{self, verb_matches},
            native,
        },
        parser::{self, Nickname, Verdict},
        registry::{Family, Rule, VerbRules},
        resolve::{LpcVocabulary, Resolver},
        scope::neighbourhood,
    },
    compile_time_config::MAX_COMMAND_DEPTH,
    interpreter::{
        apply::{apply_pointer, deliver},
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
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
    /// Push the frame for `line`, whose verb is `first_word`.
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

/// `line` after its first word, spacing trimmed. `first_word` came from
/// `split_whitespace`, so it starts exactly where the trimmed line does —
/// slicing by its byte length off the untrimmed `line` would misplace the
/// cut (or land mid-character) whenever `line` has leading whitespace.
fn rest_of<'a>(line: &'a str, first_word: &str) -> &'a str {
    let trimmed = line.trim_start();
    trimmed[first_word.len()..].trim_start()
}

/// Rules in precedence order — the actor's own, then the verb-attached
/// parser rules for `first_word` — each tried until one handles the line;
/// `true` when one did. The verb-attached cell is read only when the actor's
/// own rules did not handle the line — it stays out of most transactions'
/// read sets.
pub(crate) async fn run(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    first_word: &str,
) -> Result<bool> {
    let rules = actor.rules_of(ctx.txn());
    let scope = neighbourhood(ctx.txn(), actor);
    let mut candidates: Vec<Rule> = rules
        .iter()
        .filter(|rule| {
            rule.owner()
                .is_some_and(|owner| owner.is_live(ctx.txn()) && scope.contains(&owner))
        })
        .filter(|rule| verb_matches(rule.verb.as_str(), rule.matching(), first_word))
        .cloned()
        .collect();
    candidates.sort_by_key(|rule| std::cmp::Reverse(rule.id));

    let mut resolver = Resolver::new(LpcVocabulary::new(ctx, scope.members()), None);
    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for — `command` calls `dispatch` unboxed.
    if Box::pin(try_rules(
        ctx,
        actor,
        line,
        first_word,
        &mut resolver,
        candidates,
    ))
    .await?
    {
        return Ok(true);
    }
    let verb_rules = VerbRules::new(ctx).for_verb(first_word);
    if Box::pin(try_rules(
        ctx,
        actor,
        line,
        first_word,
        &mut resolver,
        verb_rules,
    ))
    .await?
    {
        return Ok(true);
    }
    Ok(false)
}

/// Try `candidates` in turn against `line`; `true` as soon as one handles
/// it (delivering a `Protocol` rule's message first).
async fn try_rules(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    first_word: &str,
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
            Family::Parser(parser) => {
                let Some(owner) = rule.owner() else { continue };
                let rest = rest_of(line, first_word);
                ctx.with_command(|state| {
                    if let Some(state) = state {
                        state.verb_reported = rule.verb.to_string();
                    }
                });
                match Box::pin(parser::run(ctx, actor, &owner, parser, rest, None, &[])).await? {
                    Verdict::Handled => return Ok(true),
                    Verdict::Message(message) => {
                        deliver(ctx, actor, &message).await?;
                        return Ok(true);
                    }
                    Verdict::NoParse | Verdict::Refused | Verdict::Unresolved => continue,
                }
            }
        };
        ctx.with_command(|state| {
            if let Some(state) = state {
                state.verb_reported = reported;
            }
        });

        let Some(result) = apply_pointer(ctx, actor, pointer, &args).await? else {
            continue;
        };
        if !matches!(result, LpcRef::Int(LpcInt(0))) {
            return Ok(true);
        }
    }
    Ok(false)
}

/// `parse_sentence`'s outcome.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Sentence {
    /// A verb-attached rule's `do_` ran.
    Handled,
    /// No verb-attached rule is registered for the line's first word.
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
    /// The outcome when no rule handled the line or gave a message:
    /// `Unresolved` over `Refused` over `NoParse`; `NoParse` for none.
    pub(crate) fn worst(verdicts: &[Verdict]) -> Sentence {
        let rank = |v: &Verdict| match v {
            Verdict::Unresolved => 2,
            Verdict::Refused => 1,
            _ => 0,
        };
        match verdicts.iter().max_by_key(|v| rank(v)) {
            Some(Verdict::Unresolved) => Sentence::Unresolved,
            Some(Verdict::Refused) => Sentence::Refused,
            _ => Sentence::NoParse,
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
    scope: Option<Vec<Arc<Process>>>,
    nicknames: &[Nickname],
) -> Result<Sentence> {
    depth_guard(ctx, "parse_sentence")?;
    let first_word = line.split_whitespace().next().unwrap_or("").to_owned();
    let _frame = Frame::push(ctx, line, &first_word);
    sentence_trial(ctx, actor, line, &first_word, scope, nicknames).await
}

/// The verb-attached rules for `first_word`, tried in registration order:
/// none registered is `NoVerb`; the first `Handled` wins; a `Message`
/// returns at once; otherwise the outcomes fold by severity, `Unresolved`
/// over `Refused` over `NoParse`.
async fn sentence_trial(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    line: &str,
    first_word: &str,
    scope: Option<Vec<Arc<Process>>>,
    nicknames: &[Nickname],
) -> Result<Sentence> {
    let candidates = VerbRules::new(ctx).for_verb(first_word);
    if candidates.is_empty() {
        return Ok(Sentence::NoVerb);
    }
    let rest = rest_of(line, first_word);

    let mut verdicts = Vec::new();
    for rule in &candidates {
        let Some(owner) = rule.owner() else { continue };
        let Some(parser) = rule.protocol() else {
            continue;
        };
        let verdict = Box::pin(parser::run(
            ctx,
            actor,
            &owner,
            parser,
            rest,
            scope.clone(),
            nicknames,
        ))
        .await?;
        match verdict {
            Verdict::Handled => return Ok(Sentence::Handled),
            Verdict::Message(message) => return Ok(Sentence::Message(message)),
            other => verdicts.push(other),
        }
    }
    Ok(Sentence::worst(&verdicts))
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{interpreter::vm::Vm, test_support::test_config};

    #[test]
    fn unresolved_outranks_refused_outranks_no_parse() {
        assert_eq!(
            Sentence::worst(&[Verdict::NoParse, Verdict::Refused, Verdict::NoParse]),
            Sentence::Refused
        );
        assert_eq!(
            Sentence::worst(&[Verdict::Refused, Verdict::Unresolved, Verdict::Refused]),
            Sentence::Unresolved
        );
        assert_eq!(Sentence::worst(&[Verdict::NoParse]), Sentence::NoParse);
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
