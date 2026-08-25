//! Commutative writes: operations the committer applies to the committed
//! value at commit time, in commit order. Recording one tracks no read, so
//! merges never conflict with each other; a tracked reader of the cell
//! still conflicts with a committed merge.

use std::sync::Arc;

use lpc_rs_core::LpcIntInner;

use crate::{
    command::registry::{Rule, RuleId, Scope},
    interpreter::{
        lpc_array::LpcArray, lpc_int::LpcInt, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
        stm::WorldValue,
    },
};

/// One commutative mutation of a cell.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MergeOp {
    /// Add to an int slot; an absent cell applies onto `NULL`'s 0.
    IntAdd(LpcIntInner),
    /// Append values to an array cell; an absent cell is the empty array.
    ArrayAppend(Vec<LpcRef>),
    /// Remove every element equal to the value (an object matches by
    /// identity); an absent cell stays the empty array.
    ArrayRemoveValue(LpcRef),
    /// Insert or overwrite one key; an absent cell is the empty mapping.
    MapInsert(LpcRef, LpcRef),
    /// Append one rule; an absent cell is the empty list.
    RulesAppend(Rule),
    /// Remove the rule with this id.
    RulesRemove(RuleId),
    /// Remove every rule registered by an owner in the scope.
    #[cfg_attr(not(test), expect(dead_code, reason = "used once dispatch lands"))]
    RulesRemoveOwners(Scope),
    /// Keep only rules registered by an owner in the scope.
    #[cfg_attr(not(test), expect(dead_code, reason = "used once dispatch lands"))]
    RulesRetainOwners(Scope),
}

/// The committed value no longer has the type the op needs. The commit is
/// rejected as a conflict; the re-run peeks the fresh snapshot and takes the
/// typed path, so the mismatch cannot recur.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MergeMismatch;

impl MergeOp {
    /// Apply this op onto a committed value; `None` is the absent cell.
    pub(crate) fn apply_to(&self, base: Option<&WorldValue>) -> Result<WorldValue, MergeMismatch> {
        match self {
            MergeOp::IntAdd(n) => match base {
                None => Ok(WorldValue::Ref(LpcRef::Int(LpcInt(*n)))),
                Some(WorldValue::Ref(LpcRef::Int(i))) => {
                    Ok(WorldValue::Ref(LpcRef::Int(i.wrapping_add(*n).into())))
                }
                Some(_) => Err(MergeMismatch),
            },
            MergeOp::ArrayAppend(values) => {
                let mut array = base_array(base)?;
                array.array.extend(values.iter().cloned());
                Ok(WorldValue::Array(Arc::new(array)))
            }
            MergeOp::ArrayRemoveValue(value) => {
                let mut array = base_array(base)?;
                array.array.retain(|item| item != value);
                Ok(WorldValue::Array(Arc::new(array)))
            }
            MergeOp::MapInsert(key, value) => {
                let mut mapping = base_mapping(base)?;
                mapping.insert(key.clone(), value.clone());
                Ok(WorldValue::Mapping(Arc::new(mapping)))
            }
            MergeOp::RulesAppend(rule) => {
                let mut rules = base_rules(base)?;
                rules.push(rule.clone());
                Ok(WorldValue::Rules(Arc::from(rules)))
            }
            MergeOp::RulesRemove(id) => {
                let mut rules = base_rules(base)?;
                rules.retain(|rule| rule.id != *id);
                Ok(WorldValue::Rules(Arc::from(rules)))
            }
            MergeOp::RulesRemoveOwners(scope) => {
                let mut rules = base_rules(base)?;
                rules.retain(|rule| !scope.contains_weak(&rule.owner));
                Ok(WorldValue::Rules(Arc::from(rules)))
            }
            MergeOp::RulesRetainOwners(scope) => {
                let mut rules = base_rules(base)?;
                rules.retain(|rule| scope.contains_weak(&rule.owner));
                Ok(WorldValue::Rules(Arc::from(rules)))
            }
        }
    }

    /// Fold `next` into this op when the kinds compose; a returned op did
    /// not fold and keeps its own slot.
    pub(crate) fn fold(&mut self, next: MergeOp) -> Option<MergeOp> {
        match (self, next) {
            (MergeOp::IntAdd(a), MergeOp::IntAdd(b)) => {
                *a = a.wrapping_add(b);
                None
            }
            (MergeOp::ArrayAppend(a), MergeOp::ArrayAppend(b)) => {
                a.extend(b);
                None
            }
            (_, next) => Some(next),
        }
    }
}

/// The array a container op starts from: the committed contents, or empty
/// for an absent cell.
fn base_array(base: Option<&WorldValue>) -> Result<LpcArray, MergeMismatch> {
    match base {
        None => Ok(LpcArray::default()),
        Some(WorldValue::Array(arc)) => Ok((**arc).clone()),
        Some(_) => Err(MergeMismatch),
    }
}

/// The mapping a container op starts from, as in [`base_array`].
fn base_mapping(base: Option<&WorldValue>) -> Result<LpcMapping, MergeMismatch> {
    match base {
        None => Ok(LpcMapping::default()),
        Some(WorldValue::Mapping(arc)) => Ok((**arc).clone()),
        Some(_) => Err(MergeMismatch),
    }
}

/// The rule list a rules op starts from: the committed list, or empty for
/// an absent cell.
fn base_rules(base: Option<&WorldValue>) -> Result<Vec<Rule>, MergeMismatch> {
    match base {
        None => Ok(Vec::new()),
        Some(WorldValue::Rules(rules)) => Ok(rules.to_vec()),
        Some(_) => Err(MergeMismatch),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        command::registry::{Scope, tests::rule},
        interpreter::process::Process,
    };

    fn rules_of(value: &WorldValue) -> Vec<&str> {
        match value {
            WorldValue::Rules(rules) => rules.iter().map(|r| r.verb.as_str()).collect(),
            other => panic!("expected rules, got {other:?}"),
        }
    }

    #[test]
    fn append_onto_an_absent_cell_starts_the_list() {
        let owner = Arc::new(Process::default());
        let value = MergeOp::RulesAppend(rule(&owner, "look"))
            .apply_to(None)
            .unwrap();
        assert_eq!(rules_of(&value), vec!["look"]);
    }

    #[test]
    fn append_keeps_registration_order() {
        let owner = Arc::new(Process::default());
        let first = MergeOp::RulesAppend(rule(&owner, "a"))
            .apply_to(None)
            .unwrap();
        let second = MergeOp::RulesAppend(rule(&owner, "b"))
            .apply_to(Some(&first))
            .unwrap();
        assert_eq!(rules_of(&second), vec!["a", "b"]);
    }

    #[test]
    fn remove_by_id_drops_only_that_rule() {
        let owner = Arc::new(Process::default());
        let a = rule(&owner, "a");
        let b = rule(&owner, "b");
        let value = WorldValue::Rules(Arc::from(vec![a.clone(), b.clone()]));
        let after = MergeOp::RulesRemove(a.id).apply_to(Some(&value)).unwrap();
        assert_eq!(rules_of(&after), vec!["b"]);
    }

    #[test]
    fn remove_owners_drops_every_rule_of_those_owners() {
        let room = Arc::new(Process::default());
        let sign = Arc::new(Process::default());
        let value = WorldValue::Rules(Arc::from(vec![rule(&room, "look"), rule(&sign, "read")]));
        let after = MergeOp::RulesRemoveOwners(Scope::new([sign.clone()]))
            .apply_to(Some(&value))
            .unwrap();
        assert_eq!(rules_of(&after), vec!["look"]);
    }

    #[test]
    fn retain_owners_keeps_only_rules_in_scope() {
        let room = Arc::new(Process::default());
        let sign = Arc::new(Process::default());
        let value = WorldValue::Rules(Arc::from(vec![rule(&room, "look"), rule(&sign, "read")]));
        let after = MergeOp::RulesRetainOwners(Scope::new([sign.clone()]))
            .apply_to(Some(&value))
            .unwrap();
        assert_eq!(rules_of(&after), vec!["read"]);
    }

    #[test]
    fn a_rules_op_on_a_non_rules_cell_is_a_mismatch() {
        let owner = Arc::new(Process::default());
        let base = WorldValue::Ref(LpcRef::from(1));
        assert_eq!(
            MergeOp::RulesAppend(rule(&owner, "a")).apply_to(Some(&base)),
            Err(MergeMismatch)
        );
    }

    #[test]
    fn rules_ops_do_not_fold() {
        let owner = Arc::new(Process::default());
        let mut first = MergeOp::RulesAppend(rule(&owner, "a"));
        let next = MergeOp::RulesAppend(rule(&owner, "b"));
        assert!(first.fold(next).is_some());
    }
}
