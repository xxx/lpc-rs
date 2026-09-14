//! The rule registry: `Rule` and its list, held in transactional cells — one
//! on each living's `Process` for what it can command, one on `ObjectSpace`
//! for the verb-attached rules — and changed only through merge ops.

mod actor;
mod payload;

pub(crate) use actor::ActorRules;
pub(crate) use payload::RuleEdit;
pub use payload::RuleList;

use std::sync::{
    Arc, Weak,
    atomic::{AtomicU64, Ordering},
};

use ustr::Ustr;

use crate::{
    command::{
        frontend::{native::Compiled, parser::ParserRule},
        scope::Scope,
    },
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        process::Process,
        stm::{MergeOp, TxnHandle, VarId},
        task_context::TaskContext,
    },
};

/// A rule's identity; ids increase with registration order, which is the
/// precedence order (most recent first).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuleId(pub u64);

static NEXT_RULE_ID: AtomicU64 = AtomicU64::new(1);

impl RuleId {
    /// The next unused id, driver-wide.
    pub fn next() -> RuleId {
        RuleId(NEXT_RULE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

/// What `query_verb()` reports for a prefix verb.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Reported {
    /// The whole first word as typed.
    Full,
    /// The verb as registered.
    Registered,
}

/// What a prefix verb's handler receives as its argument.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArgSpan {
    /// Everything after the first word.
    AfterWord,
    /// The rest of the first word plus everything after it.
    RestOfLine,
    /// Only the rest of the first word.
    RestOfWord,
}

/// How a rule's verb matches the first word of a line.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VerbMatch {
    /// The first word equals the verb.
    Exact,
    /// The first word starts with the verb.
    Prefix { reports: Reported, args: ArgSpan },
}

impl VerbMatch {
    /// The add_action flag mapping shared by every dialect: `0` exact,
    /// `1`/`AA_SHORT`, `2`/`AA_NOSPACE`, `3`/`AA_IMM_ARGS`.
    pub fn from_flag(flag: i64) -> Option<VerbMatch> {
        match flag {
            0 => Some(VerbMatch::Exact),
            1 => Some(VerbMatch::Prefix {
                reports: Reported::Full,
                args: ArgSpan::AfterWord,
            }),
            2 => Some(VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfLine,
            }),
            3 => Some(VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfWord,
            }),
            _ => None,
        }
    }

    /// The `add_action` flag this matching came from; `Full` with any
    /// span answers 1, `Registered` with `AfterWord`, which no flag makes,
    /// answers 2.
    pub fn flag(self) -> i64 {
        match self {
            VerbMatch::Exact => 0,
            VerbMatch::Prefix {
                reports: Reported::Full,
                ..
            } => 1,
            VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfLine | ArgSpan::AfterWord,
            } => 2,
            VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfWord,
            } => 3,
        }
    }
}

/// Which surface registered a rule: the one thing that owns both how a
/// line is matched and how the rule runs once it matches.
#[derive(Clone, Debug)]
pub enum Family {
    /// `add_action()`: the verb, then the rest of the line as one string.
    AddAction {
        /// How the verb matches the first word.
        matching: VerbMatch,
        /// The handler.
        pointer: Arc<FunctionPtr>,
    },
    /// `add_rule()`: a native pattern, one argument per capture.
    Native {
        /// The pattern, shared by every verb it was registered under.
        compiled: Arc<Compiled>,
        /// The handler.
        pointer: Arc<FunctionPtr>,
    },
    /// `parse_add_rule()`: the `can_`/`direct_`/`indirect_`/`do_` protocol.
    Parser(Arc<ParserRule>),
}

/// One registered command rule.
#[derive(Clone, Debug)]
pub struct Rule {
    /// This rule's identity.
    pub id: RuleId,
    /// The object whose `init()` (or `add_rule`) registered it.
    pub owner: Weak<Process>,
    /// The registered verb: the dispatch pre-filter and `query_verb()`.
    pub verb: Ustr,
    /// How the rule matches a line and runs.
    pub family: Family,
}

impl Rule {
    /// A rule with a fresh id, owned by `owner`.
    pub fn new(owner: &Arc<Process>, verb: Ustr, family: Family) -> Rule {
        Rule {
            id: RuleId::next(),
            owner: Arc::downgrade(owner),
            verb,
            family,
        }
    }

    /// How the verb matches the first word of a line; only `add_action`
    /// rules match by prefix.
    pub fn matching(&self) -> VerbMatch {
        match &self.family {
            Family::AddAction { matching, .. } => *matching,
            Family::Native { .. } | Family::Parser(_) => VerbMatch::Exact,
        }
    }

    /// The handler, for a rule that calls one function.
    pub fn pointer(&self) -> Option<&Arc<FunctionPtr>> {
        match &self.family {
            Family::AddAction { pointer, .. } | Family::Native { pointer, .. } => Some(pointer),
            Family::Parser(_) => None,
        }
    }

    /// The parser rule, for a `parse_add_rule` rule.
    pub fn protocol(&self) -> Option<&Arc<ParserRule>> {
        match &self.family {
            Family::Parser(rule) => Some(rule),
            Family::AddAction { .. } | Family::Native { .. } => None,
        }
    }

    /// The owner, if it has not been dropped.
    pub fn owner(&self) -> Option<Arc<Process>> {
        self.owner.upgrade()
    }

    /// Whether `owner` registered this rule, by pointer identity; `true`
    /// even if `owner` has since been destructed elsewhere.
    pub fn owned_by(&self, owner: &Arc<Process>) -> bool {
        std::ptr::eq(self.owner.as_ptr(), Arc::as_ptr(owner))
    }

    /// The same registration under another verb: it shares this rule's id,
    /// so a removal by id drops them all.
    pub fn sibling(&self, verb: Ustr) -> Rule {
        Rule {
            verb,
            ..self.clone()
        }
    }
}

impl PartialEq for Rule {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Rule {}

/// The verb-attached rules — every verb object's, driver-wide — read and
/// changed through one transaction.
pub(crate) struct VerbRules<'a> {
    txn: &'a TxnHandle,
    cell: VarId,
}

impl<'a> VerbRules<'a> {
    /// The verb-attached rules as `ctx`'s transaction sees them.
    pub(crate) fn new(ctx: &'a TaskContext) -> Self {
        VerbRules {
            txn: ctx.txn(),
            cell: ctx.object_space().verb_rules.id,
        }
    }

    /// Every rule in registration order; a tracked read.
    pub(crate) fn all(&self) -> RuleList {
        self.txn.with(|t| t.read_rules(self.cell))
    }

    /// The rules for `verb` (exact) whose owner is live.
    pub(crate) fn for_verb(&self, verb: &str) -> Vec<Rule> {
        self.all()
            .iter()
            .filter(|rule| rule.verb.as_str() == verb)
            .filter(|rule| rule.owner().is_some_and(|owner| owner.is_live(self.txn)))
            .cloned()
            .collect()
    }

    /// The rules `owner` registered, in registration order.
    pub(crate) fn owned_by(&self, owner: &Arc<Process>) -> Vec<Rule> {
        self.all()
            .iter()
            .filter(|rule| rule.owned_by(owner))
            .cloned()
            .collect()
    }

    /// Register a parser rule without reading the shared cell.
    pub(crate) fn register(&self, owner: &Arc<Process>, parser: ParserRule) {
        self.append(vec![Rule::new(
            owner,
            parser.verb,
            Family::Parser(Arc::new(parser)),
        )]);
    }

    /// Copy matching typed-verb registrations with fresh identities and the same base handlers.
    pub(crate) fn add_synonym(
        &self,
        owner: &Arc<Process>,
        new_verb: Ustr,
        old_verb: &str,
        rule_filter: Option<&str>,
    ) -> bool {
        let found: Vec<_> = self
            .all()
            .iter()
            .filter(|rule| rule.owned_by(owner) && rule.verb.as_str() == old_verb)
            .filter(|rule| {
                rule_filter.is_none_or(|wanted| {
                    rule.protocol().is_some_and(|parser| parser.rule == wanted)
                })
            })
            .map(|rule| Rule::new(owner, new_verb, rule.family.clone()))
            .collect();
        let matched = !found.is_empty();
        self.append(found);
        matched
    }

    /// Remove an owner's base verb and its synonyms, also purging dropped owners.
    pub(crate) fn remove_verb(&self, owner: &Arc<Process>, verb: &str) {
        let rules = self.all();
        let edit = RuleEdit::remove_ids(
            rules
                .iter()
                .filter(|rule| {
                    rule.owner().is_none()
                        || (rule.owned_by(owner)
                            && rule
                                .protocol()
                                .is_some_and(|parser| parser.verb.as_str() == verb))
                })
                .map(|rule| rule.id),
        );
        drop(rules);
        if let Some(edit) = edit {
            self.edit(edit);
        }
    }

    /// Remove every rule `owner` registered without reading the shared cell.
    pub(crate) fn remove_owner(&self, owner: &Arc<Process>) {
        self.edit(RuleEdit::remove_owners(Scope::new([owner.clone()])));
    }

    fn append(&self, rules: Vec<Rule>) {
        if !rules.is_empty() {
            self.edit(RuleEdit::append(rules));
        }
    }

    fn edit(&self, edit: RuleEdit) {
        self.txn.with(|t| t.merge(self.cell, MergeOp::Rules(edit)));
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::{collections::HashSet, sync::Arc};

    use super::*;
    use crate::interpreter::{
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
        process::Process,
    };

    /// A rule for `verb` owned by `owner`, with a dynamic (receiver-less)
    /// handler; enough for identity and scope tests.
    pub(crate) fn rule(owner: &Arc<Process>, verb: &str) -> Rule {
        let pointer = Arc::new(
            FunctionPtrBuilder::default()
                .owner(Arc::downgrade(owner))
                .address(FunctionAddress::Dynamic(verb.into()))
                .build()
                .unwrap(),
        );
        Rule::new(
            owner,
            verb.into(),
            Family::AddAction {
                matching: VerbMatch::Exact,
                pointer,
            },
        )
    }

    /// A parser rule for `verb` with rule text `text`, owned by `owner`.
    pub(crate) fn parser_rule(owner: &Arc<Process>, verb: &str, text: &str) -> Rule {
        let parser = Arc::new(ParserRule {
            verb: verb.into(),
            rule: text.to_owned(),
            can_slug: text.to_lowercase().replace(' ', "_").into(),
            do_slug: text.to_lowercase().replace(' ', "_").into(),
            compiled: crate::command::frontend::native::compile_pattern("%w").unwrap(),
        });
        Rule::new(owner, verb.into(), Family::Parser(parser))
    }

    #[test]
    fn a_family_answers_for_its_pointer_and_protocol() {
        let owner = Arc::new(Process::default());
        let pointer = rule(&owner, "look");
        let protocol = parser_rule(&owner, "look", "at OBJ");
        assert!(pointer.pointer().is_some());
        assert!(pointer.protocol().is_none());
        assert!(protocol.pointer().is_none());
        assert_eq!(protocol.protocol().unwrap().rule, "at OBJ");
    }

    #[test]
    fn only_add_action_carries_its_own_matching() {
        let owner = Arc::new(Process::default());
        let prefix = VerbMatch::Prefix {
            reports: Reported::Full,
            args: ArgSpan::AfterWord,
        };
        let mut short = rule(&owner, "'");
        if let Family::AddAction { matching, .. } = &mut short.family {
            *matching = prefix;
        }
        assert_eq!(short.matching(), prefix);
        assert_eq!(
            parser_rule(&owner, "look", "at OBJ").matching(),
            VerbMatch::Exact
        );
    }

    #[test]
    fn rule_ids_increase_per_rule() {
        let owner = Arc::new(Process::default());
        let a = rule(&owner, "a");
        let b = rule(&owner, "b");
        assert!(a.id < b.id);
    }

    #[test]
    fn rules_are_equal_by_id_only() {
        let owner = Arc::new(Process::default());
        let a = rule(&owner, "a");
        let same = Rule {
            verb: "different".into(),
            ..a.clone()
        };
        assert_eq!(a, same);
        assert_ne!(a, rule(&owner, "a"));
    }

    #[test]
    fn flags_map_to_verb_matching() {
        assert_eq!(VerbMatch::from_flag(0), Some(VerbMatch::Exact));
        assert_eq!(
            VerbMatch::from_flag(1),
            Some(VerbMatch::Prefix {
                reports: Reported::Full,
                args: ArgSpan::AfterWord
            })
        );
        assert_eq!(
            VerbMatch::from_flag(2),
            Some(VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfLine
            })
        );
        assert_eq!(
            VerbMatch::from_flag(3),
            Some(VerbMatch::Prefix {
                reports: Reported::Registered,
                args: ArgSpan::RestOfWord
            })
        );
        assert_eq!(VerbMatch::from_flag(4), None);
        assert_eq!(VerbMatch::from_flag(-1), None);
    }

    #[test]
    fn a_flag_round_trips_through_its_matching() {
        for flag in 0..4 {
            assert_eq!(VerbMatch::from_flag(flag).unwrap().flag(), flag);
        }
    }

    #[test]
    fn a_dropped_owner_is_absent() {
        let owner = Arc::new(Process::default());
        let r = rule(&owner, "a");
        assert!(r.owner().is_some());
        drop(owner);
        assert!(r.owner().is_none());
    }

    struct World {
        committer: crate::interpreter::stm::Committer,
        tx: flume::Sender<crate::interpreter::stm::CommitProtocol>,
        _rx: flume::Receiver<crate::interpreter::stm::CommitProtocol>,
    }

    impl World {
        fn new() -> Self {
            let (tx, rx) = flume::unbounded();
            Self {
                committer: crate::interpreter::stm::Committer::new(),
                tx,
                _rx: rx,
            }
        }

        fn begin(&mut self) -> (crate::interpreter::stm::LiveSnapshot, TxnHandle) {
            let (reply, rx) = flume::bounded(1);
            self.committer.process(
                crate::interpreter::stm::CommitProtocol::Start { reply },
                &self.tx,
            );
            let live = rx.recv().unwrap();
            let txn = TxnHandle::new(crate::interpreter::stm::Transaction::new(
                live.inner.clone(),
            ));
            (live, txn)
        }

        fn commit(&mut self, txn: &TxnHandle) -> Result<(), crate::interpreter::stm::Conflict> {
            use crate::interpreter::stm::CommitProtocol;
            let (reply, rx) = flume::bounded(1);
            self.committer.process(
                CommitProtocol::Commit {
                    changeset: txn.with(|t| t.take_changeset()),
                    releases_base: false,
                    reply,
                },
                &self.tx,
            );
            rx.recv().unwrap()
        }
    }

    fn register(actions: &ActorRules<'_>, owner: &Arc<Process>, verb: &str) {
        actions.register_actions(
            owner,
            vec![verb.into()],
            VerbMatch::Exact,
            rule(owner, verb).pointer().unwrap().clone(),
        );
    }

    fn verbs(rules: &RuleList) -> Vec<&str> {
        rules.iter().map(|rule| rule.verb.as_str()).collect()
    }

    #[test]
    fn actor_edits_preserve_snapshots_and_registration_groups() {
        let mut world = World::new();
        let actor = Arc::new(Process::default());
        let other = Arc::new(Process::default());
        let (_live, txn) = world.begin();
        let actions = ActorRules::new(&txn, &actor);
        let compiled = crate::command::frontend::native::compile("'give' / 'hand' %w").unwrap();
        let id = actions
            .register_native(
                &actor,
                compiled,
                rule(&actor, "give").pointer().unwrap().clone(),
            )
            .unwrap();
        register(&actions, &other, "look");
        let first = actions.all();
        assert_eq!(verbs(&first), ["give", "hand", "look"]);
        assert_eq!(first[0].id, first[1].id);
        assert!(!actions.remove(&other, RuleId(id as u64)));
        assert_eq!(actions.remove_actions(&actor, "give", None), 2);
        register(&actions, &actor, "wave");
        actions.retain_owners(Scope::new([actor.clone()]));
        register(&actions, &other, "return");
        assert_eq!(verbs(&actions.all()), ["wave", "return"]);
        assert_eq!(verbs(&first), ["give", "hand", "look"]);
        world.commit(&txn).unwrap();

        let (_live, txn) = world.begin();
        let actions = ActorRules::new(&txn, &actor);
        let committed = actions.all();
        ActorRules::forget_owner(&txn, &other, std::slice::from_ref(&actor));
        register(&actions, &other, "again");
        assert_eq!(verbs(&actions.all()), ["wave", "again"]);
        actions.clear();
        register(&actions, &actor, "after_clear");
        world.commit(&txn).unwrap();
        assert_eq!(verbs(&committed), ["wave", "return"]);
        let (_live, txn) = world.begin();
        assert_eq!(verbs(&ActorRules::new(&txn, &actor).all()), ["after_clear"]);
    }

    #[test]
    fn a_removal_does_not_consume_a_later_sibling_with_the_same_identity() {
        use crate::interpreter::stm::WorldValue;
        let owner = Arc::new(Process::default());
        let first = rule(&owner, "give");
        let sibling = first.sibling("hand".into());
        let edits = [
            MergeOp::Rules(RuleEdit::append(vec![first.clone()])),
            MergeOp::Rules(RuleEdit::remove_id(first.id)),
            MergeOp::Rules(RuleEdit::append(vec![sibling])),
        ];
        let value = MergeOp::fold_onto(None, &edits)
            .unwrap()
            .unwrap()
            .into_rules()
            .unwrap();
        assert_eq!(verbs(&value), ["hand"]);
        assert_eq!(value[0].id, first.id);
        assert!(edits[0].apply_to(Some(&WorldValue::Ref(1.into()))).is_err());
    }

    #[test]
    fn concurrent_registrations_keep_actor_precedence_and_parser_commit_order() {
        let mut world = World::new();
        let actor = Arc::new(Process::default());
        let cell = VarId::new();
        let (_older_live, older) = world.begin();
        let (_newer_live, newer) = world.begin();
        for txn in [&older, &newer] {
            register(&ActorRules::new(txn, &actor), &actor, "look");
            let parser = parser_rule(&actor, "look", "WRD")
                .protocol()
                .unwrap()
                .as_ref()
                .clone();
            VerbRules { txn, cell }.register(&actor, parser);
        }
        world.commit(&newer).unwrap();
        world.commit(&older).unwrap();
        let (_live, txn) = world.begin();
        let actions = ActorRules::new(&txn, &actor);
        let inspected = actions.all();
        assert_eq!(inspected.len(), 2);
        assert!(inspected[0].id > inspected[1].id);
        let selected = actions.matching("look", &Scope::new([actor.clone()]));
        assert_eq!(
            selected.iter().map(|rule| rule.id).collect::<Vec<_>>(),
            inspected.iter().map(|rule| rule.id).collect::<Vec<_>>()
        );
        let parsers = VerbRules { txn: &txn, cell }.for_verb("look");
        assert_eq!(parsers.len(), 2);
        assert!(parsers[0].id > parsers[1].id);
    }

    #[test]
    fn observed_removal_conflicts_and_an_abandoned_edit_never_commits() {
        let mut world = World::new();
        let actor = Arc::new(Process::default());
        let (_reader_live, reader) = world.begin();
        let (_writer_live, writer) = world.begin();
        assert_eq!(
            ActorRules::new(&reader, &actor).remove_actions(&actor, "look", None),
            0
        );
        register(&ActorRules::new(&reader, &actor), &actor, "rejected");
        register(&ActorRules::new(&writer, &actor), &actor, "look");
        world.commit(&writer).unwrap();
        assert!(world.commit(&reader).is_err());
        {
            let (_live, txn) = world.begin();
            ActorRules::new(&txn, &actor).clear();
            register(&ActorRules::new(&txn, &actor), &actor, "abandoned");
        }
        let (_live, txn) = world.begin();
        assert_eq!(verbs(&ActorRules::new(&txn, &actor).all()), ["look"]);
    }

    #[test]
    fn parser_synonyms_keep_base_handlers_and_removal_preserves_other_owners() {
        let mut world = World::new();
        let owner = Arc::new(Process::default());
        let other = Arc::new(Process::default());
        let cell = VarId::new();
        let (_live, txn) = world.begin();
        let parsers = VerbRules { txn: &txn, cell };
        for process in [&owner, &other] {
            let parser = parser_rule(process, "look", "WRD")
                .protocol()
                .unwrap()
                .as_ref()
                .clone();
            parsers.register(process, parser);
        }
        assert!(parsers.add_synonym(&owner, "peek".into(), "look", Some("WRD")));
        assert!(parsers.add_synonym(&owner, "glance".into(), "peek", None));
        assert!(!parsers.add_synonym(&owner, "missing".into(), "look", Some("OBJ")));
        let snapshot = parsers.all();
        assert_eq!(verbs(&snapshot), ["look", "look", "peek", "glance"]);
        assert_eq!(
            snapshot
                .iter()
                .map(|rule| rule.id)
                .collect::<HashSet<_>>()
                .len(),
            4
        );
        assert!(
            snapshot
                .iter()
                .all(|rule| rule.protocol().unwrap().verb.as_str() == "look")
        );
        parsers.remove_verb(&owner, "peek");
        assert_eq!(parsers.all().len(), 4);
        parsers.remove_verb(&owner, "look");
        assert_eq!(parsers.all().len(), 1);
        assert!(parsers.all()[0].owned_by(&other));
        parsers.remove_owner(&other);
        assert!(parsers.all().is_empty());
        assert_eq!(snapshot.len(), 4);
        world.commit(&txn).unwrap();
    }
}
