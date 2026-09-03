//! Commutative writes: operations the committer applies to the committed
//! value at commit time, in commit order. Recording one tracks no read, so
//! merges never conflict with each other; a tracked reader of the cell
//! still conflicts with a committed merge.

use std::sync::Arc;

use lpc_rs_core::LpcIntInner;

use crate::{
    command::{
        registry::{Rule, RuleId},
        scope::Scope,
    },
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
    RulesRemoveOwners(Scope),
    /// Keep only rules registered by an owner in the scope.
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
        let mut value = base.cloned().unwrap_or_else(|| self.identity());
        self.apply_in_place(&mut value)?;
        Ok(value)
    }

    /// Fold `ops` onto `base` in record order, cloning a shared payload
    /// once — a loop over `apply_to` clones it per op.
    pub(crate) fn fold_onto(
        base: Option<WorldValue>,
        ops: &[MergeOp],
    ) -> Result<Option<WorldValue>, MergeMismatch> {
        let Some(first) = ops.first() else {
            return Ok(base);
        };
        let mut value = base.unwrap_or_else(|| first.identity());
        for op in ops {
            op.apply_in_place(&mut value)?;
        }
        Ok(Some(value))
    }

    /// Apply this op onto `value` in place; a payload shared with the
    /// committed world is cloned, never mutated.
    pub(crate) fn apply_in_place(&self, value: &mut WorldValue) -> Result<(), MergeMismatch> {
        match (self, value) {
            (MergeOp::IntAdd(n), WorldValue::Ref(LpcRef::Int(i))) => {
                *i = i.wrapping_add(*n).into();
            }
            (MergeOp::ArrayAppend(values), WorldValue::Array(array)) => {
                Arc::make_mut(array).array.extend(values.iter().cloned());
            }
            (MergeOp::ArrayRemoveValue(value), WorldValue::Array(array)) => {
                Arc::make_mut(array).array.retain(|item| item != value);
            }
            (MergeOp::MapInsert(key, value), WorldValue::Mapping(mapping)) => {
                Arc::make_mut(mapping).insert(key.clone(), value.clone());
            }
            (MergeOp::RulesAppend(rule), WorldValue::Rules(rules)) => {
                let mut list = rules.to_vec();
                list.push(rule.clone());
                *rules = Arc::from(list);
            }
            (MergeOp::RulesRemove(id), WorldValue::Rules(rules)) => {
                *rules = rules
                    .iter()
                    .filter(|rule| rule.id != *id)
                    .cloned()
                    .collect();
            }
            (MergeOp::RulesRemoveOwners(scope), WorldValue::Rules(rules)) => {
                *rules = rules
                    .iter()
                    .filter(|rule| !scope.contains_weak(&rule.owner))
                    .cloned()
                    .collect();
            }
            (MergeOp::RulesRetainOwners(scope), WorldValue::Rules(rules)) => {
                *rules = rules
                    .iter()
                    .filter(|rule| scope.contains_weak(&rule.owner))
                    .cloned()
                    .collect();
            }
            _ => return Err(MergeMismatch),
        }
        Ok(())
    }

    /// What this op applies onto for an absent cell.
    fn identity(&self) -> WorldValue {
        match self {
            MergeOp::IntAdd(_) => WorldValue::Ref(LpcRef::Int(LpcInt(0))),
            MergeOp::ArrayAppend(_) | MergeOp::ArrayRemoveValue(_) => {
                WorldValue::Array(Arc::new(LpcArray::default()))
            }
            MergeOp::MapInsert(..) => WorldValue::Mapping(Arc::new(LpcMapping::default())),
            MergeOp::RulesAppend(_)
            | MergeOp::RulesRemove(_)
            | MergeOp::RulesRemoveOwners(_)
            | MergeOp::RulesRetainOwners(_) => WorldValue::Rules(Arc::from(Vec::new())),
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        command::{registry::tests::rule, scope::Scope},
        interpreter::process::Process,
    };

    fn rules_of(value: &WorldValue) -> Vec<&str> {
        match value {
            WorldValue::Rules(rules) => rules.iter().map(|r| r.verb.as_str()).collect(),
            other => panic!("expected rules, got {other:?}"),
        }
    }

    #[test]
    fn apply_in_place_leaves_a_shared_payload_untouched() {
        let shared = Arc::new(LpcMapping::default());
        let mut value = WorldValue::Mapping(shared.clone());
        MergeOp::MapInsert("a".into(), 1.into())
            .apply_in_place(&mut value)
            .expect("a mapping takes an insert");

        assert!(shared.is_empty());
        let WorldValue::Mapping(own) = value else {
            panic!("a mapping stays a mapping");
        };
        assert!(!Arc::ptr_eq(&shared, &own));
        assert_eq!(own.get(&"a".into()), Some(&1.into()));
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
