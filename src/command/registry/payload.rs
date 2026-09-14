//! Immutable rule snapshots and their ordered transactional edits.

use std::{collections::HashSet, ops::Deref, sync::Arc};

use super::{Rule, RuleId};
use crate::command::scope::Scope;

/// An immutable snapshot of rules in registration order.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuleList(Arc<Vec<Rule>>);

impl Deref for RuleList {
    type Target = [Rule];

    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl From<Vec<Rule>> for RuleList {
    fn from(rules: Vec<Rule>) -> Self {
        Self(Arc::new(rules))
    }
}

impl FromIterator<Rule> for RuleList {
    fn from_iter<T: IntoIterator<Item = Rule>>(iter: T) -> Self {
        Self::from(iter.into_iter().collect::<Vec<_>>())
    }
}

/// A rule mutation interpreted by the registry when STM materializes a cell.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct RuleEdit(Edit);

#[derive(Clone, Debug, PartialEq)]
enum Edit {
    Append(Vec<Rule>),
    RemoveId(RuleId),
    RemoveIds(HashSet<RuleId>),
    RemoveOwners(Scope),
    RetainOwners(Scope),
}

impl RuleEdit {
    pub(super) fn append(rules: Vec<Rule>) -> Self {
        Self(Edit::Append(rules))
    }

    pub(super) fn remove_id(id: RuleId) -> Self {
        Self(Edit::RemoveId(id))
    }

    pub(super) fn remove_ids(ids: impl IntoIterator<Item = RuleId>) -> Option<Self> {
        let mut ids = ids.into_iter();
        let first = ids.next()?;
        let Some(second) = ids.next() else {
            return Some(Self::remove_id(first));
        };
        Some(Self(Edit::RemoveIds(
            [first, second].into_iter().chain(ids).collect(),
        )))
    }

    pub(super) fn remove_owners(scope: Scope) -> Self {
        Self(Edit::RemoveOwners(scope))
    }

    pub(super) fn retain_owners(scope: Scope) -> Self {
        Self(Edit::RetainOwners(scope))
    }

    pub(crate) fn apply(&self, rules: &mut RuleList) {
        let rules = Arc::make_mut(&mut rules.0);
        match &self.0 {
            Edit::Append(added) => rules.extend(added.iter().cloned()),
            Edit::RemoveId(id) => rules.retain(|rule| rule.id != *id),
            Edit::RemoveIds(ids) => rules.retain(|rule| !ids.contains(&rule.id)),
            Edit::RemoveOwners(scope) => rules.retain(|rule| !scope.contains_weak(&rule.owner)),
            Edit::RetainOwners(scope) => rules.retain(|rule| scope.contains_weak(&rule.owner)),
        }
    }

    pub(crate) fn fold(&mut self, next: Self) -> Option<Self> {
        match (&mut self.0, next.0) {
            (Edit::Append(first), Edit::Append(next)) => {
                first.extend(next);
                None
            }
            (_, next) => Some(Self(next)),
        }
    }
}
