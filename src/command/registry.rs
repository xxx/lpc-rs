//! The rule registry: what a living can command, held as a transactional
//! cell on its `Process` and changed only through merge ops.

use std::sync::{
    Arc, Weak,
    atomic::{AtomicU64, Ordering},
};

use ustr::Ustr;

use crate::{
    command::frontend::{native::Compiled, parser::ParserRule},
    interpreter::{function_type::function_ptr::FunctionPtr, process::Process, stm::TxnHandle},
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
                args: ArgSpan::RestOfLine,
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

/// A living's rule list; copy-on-write like an array payload.
pub type RuleList = Arc<[Rule]>;

/// The objects whose rules a living may use, as weak references.
#[derive(Clone, Debug, Default)]
pub struct Scope(Vec<Weak<Process>>);

impl Scope {
    /// A scope over `members`, held weakly.
    pub fn new(members: impl IntoIterator<Item = Arc<Process>>) -> Scope {
        Scope(members.into_iter().map(|p| Arc::downgrade(&p)).collect())
    }

    /// Whether `process` is a member, by pointer identity.
    pub fn contains(&self, process: &Arc<Process>) -> bool {
        self.0
            .iter()
            .any(|w| std::ptr::eq(w.as_ptr(), Arc::as_ptr(process)))
    }

    /// Whether `owner` is a member, by pointer identity.
    pub fn contains_weak(&self, owner: &Weak<Process>) -> bool {
        self.0.iter().any(|w| Weak::ptr_eq(w, owner))
    }

    /// The members not yet dropped, in scope order.
    pub fn members(&self) -> Vec<Arc<Process>> {
        self.0.iter().filter_map(Weak::upgrade).collect()
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len()
            && self.0.iter().zip(&other.0).all(|(a, b)| Weak::ptr_eq(a, b))
    }
}

/// The objects whose rules `living` may use: itself, its environment, that
/// environment's contents, and its own contents.
pub(crate) fn scope_of(txn: &TxnHandle, living: &Arc<Process>) -> Scope {
    let mut members = vec![living.clone()];
    if let Some(environment) = Process::environment_of(txn, living) {
        members.extend(
            Process::inventory_of(txn, &environment)
                .into_iter()
                .filter(|ob| !Arc::ptr_eq(ob, living)),
        );
        members.push(environment);
    }
    members.extend(Process::inventory_of(txn, living));
    Scope::new(members)
}

#[cfg(test)]
pub(crate) mod tests {
    use std::sync::Arc;

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
            args: ArgSpan::RestOfLine,
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
                args: ArgSpan::RestOfLine
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
    fn scope_membership_is_by_identity() {
        let a = Arc::new(Process::default());
        let b = Arc::new(Process::default());
        let scope = Scope::new([a.clone()]);
        assert!(scope.contains(&a));
        assert!(!scope.contains(&b));
        assert!(scope.contains_weak(&Arc::downgrade(&a)));
        assert_eq!(scope, Scope::new([a.clone()]));
        assert_ne!(scope, Scope::new([b]));
    }

    #[test]
    fn a_dropped_owner_is_absent() {
        let owner = Arc::new(Process::default());
        let r = rule(&owner, "a");
        assert!(r.owner().is_some());
        drop(owner);
        assert!(r.owner().is_none());
    }
}
