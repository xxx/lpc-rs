//! A living's transactional command rules.

use std::{collections::HashSet, sync::Arc};

use ustr::Ustr;

use super::{Family, Rule, RuleEdit, RuleId, RuleList, VerbMatch};
use crate::{
    command::{
        frontend::{add_action::verb_matches, native::Compiled},
        scope::Scope,
    },
    interpreter::{
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        process::Process,
        stm::{MergeOp, TxnHandle, VarId},
    },
};

/// A living's rules as seen and changed by one transaction.
pub(crate) struct ActorRules<'a> {
    txn: &'a TxnHandle,
    cell: VarId,
}

impl<'a> ActorRules<'a> {
    pub(crate) fn new(txn: &'a TxnHandle, actor: &Process) -> Self {
        Self {
            txn,
            cell: actor.rules.id,
        }
    }

    /// Every registration in list order; a tracked read, including dropped owners.
    pub(crate) fn all(&self) -> RuleList {
        self.txn.with(|t| t.read_rules(self.cell))
    }

    /// Register each action under its own identity without reading the cell.
    pub(crate) fn register_actions(
        &self,
        owner: &Arc<Process>,
        verbs: Vec<Ustr>,
        matching: VerbMatch,
        pointer: Arc<FunctionPtr>,
    ) {
        let rules = verbs
            .into_iter()
            .map(|verb| {
                Rule::new(
                    owner,
                    verb,
                    Family::AddAction {
                        matching,
                        pointer: pointer.clone(),
                    },
                )
            })
            .collect();
        self.append(rules);
    }

    /// Register all leading verbs under one LPC removal ID without reading the cell.
    pub(crate) fn register_native(
        &self,
        owner: &Arc<Process>,
        compiled: Arc<Compiled>,
        pointer: Arc<FunctionPtr>,
    ) -> Result<i64, &'static str> {
        let Some((verb, others)) = compiled.verbs.split_first() else {
            return Err("a compiled pattern has no verb");
        };
        let first = Rule::new(
            owner,
            *verb,
            Family::Native {
                compiled: compiled.clone(),
                pointer,
            },
        );
        let id = i64::try_from(first.id.0).map_err(|_| "rule ids exceeded the int range")?;
        let mut rules = Vec::with_capacity(compiled.verbs.len());
        rules.extend(others.iter().map(|verb| first.sibling(*verb)));
        rules.insert(0, first);
        self.append(rules);
        Ok(id)
    }

    /// Remove a registration only when `owner` holds it; checks the transaction-visible list.
    pub(crate) fn remove(&self, owner: &Arc<Process>, id: RuleId) -> bool {
        let held = self
            .all()
            .iter()
            .any(|rule| rule.id == id && rule.owned_by(owner));
        if held {
            self.edit(RuleEdit::remove_id(id));
        }
        held
    }

    /// Remove matching registrations and count every alternative sharing their identities.
    pub(crate) fn remove_actions(
        &self,
        owner: &Arc<Process>,
        verb: &str,
        function: Option<&str>,
    ) -> usize {
        let rules = self.all();
        let ids: HashSet<_> = rules.iter()
            .filter(|rule| rule.owned_by(owner) && rule.verb.as_str() == verb)
            .filter(|rule| function.is_none_or(|name| {
                rule.pointer().is_some_and(|pointer| {
                    matches!(&pointer.address, FunctionAddress::Local(_, f) if f.prototype.name.as_ref() == name)
                })
            }))
            .map(|rule| rule.id)
            .collect();
        let count = rules.iter().filter(|rule| ids.contains(&rule.id)).count();
        drop(rules);
        if let Some(edit) = RuleEdit::remove_ids(ids) {
            self.edit(edit);
        }
        count
    }

    /// Matching, live, in-scope registrations, newest identity first.
    pub(crate) fn matching(&self, word: &str, scope: &Scope) -> Vec<Rule> {
        let mut rules: Vec<_> = self
            .all()
            .iter()
            .filter(|rule| {
                rule.owner()
                    .is_some_and(|owner| owner.is_live(self.txn) && scope.contains(&owner))
            })
            .filter(|rule| verb_matches(rule.verb.as_str(), rule.matching(), word))
            .cloned()
            .collect();
        // Concurrent registrations can commit in a different order from their IDs.
        rules.sort_by_key(|rule| std::cmp::Reverse(rule.id));
        rules
    }

    /// Keep owners in the new scope without reading the rule cell.
    pub(crate) fn retain_owners(&self, scope: Scope) {
        self.edit(RuleEdit::retain_owners(scope));
    }

    /// Forget one departing owner's registrations on all affected livings, without reads.
    pub(crate) fn forget_owner(txn: &TxnHandle, owner: &Arc<Process>, actors: &[Arc<Process>]) {
        if actors.is_empty() {
            return;
        }
        let edit = RuleEdit::remove_owners(Scope::new([owner.clone()]));
        txn.with(|t| {
            for actor in actors {
                t.merge(actor.rules.id, MergeOp::Rules(edit.clone()));
            }
        });
    }

    /// Clear the cell without observing it.
    pub(crate) fn clear(&self) {
        self.txn.with(|t| t.drop_var(self.cell));
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
