//! Transactional class instances and guarded access through compiled definitions.

use std::{collections::HashSet, sync::Arc};

use lpc_rs_core::class::{ClassDefinition, ClassField, ClassId, ClassScope};
use lpc_rs_errors::{LpcError, Result};
use ustr::Ustr;

use crate::interpreter::{
    lpc_ref::{LpcRef, NULL},
    stm::{SVar, TxnHandle},
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Slot {
    field: ClassField,
    value: LpcRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct View {
    definition: Arc<ClassDefinition>,
    slots: Box<[usize]>,
}

/// A shared instance whose retained views reuse storage by field name and type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LpcClass {
    pub(crate) id: ClassId,
    slots: Vec<Slot>,
    views: Vec<View>,
}

impl LpcClass {
    pub(crate) fn new(definition: Arc<ClassDefinition>) -> Self {
        let mut instance = Self {
            id: definition.id,
            slots: Vec::new(),
            views: Vec::new(),
        };
        instance.acquire_view(definition);
        instance
    }

    /// The nominal source/name identity, independent of retained field views.
    pub fn id(&self) -> ClassId {
        self.id
    }

    /// All retained field storage, including slots used only by older views.
    pub fn stored_fields(&self) -> impl Iterator<Item = (&ClassField, &LpcRef)> {
        self.slots.iter().map(|slot| (&slot.field, &slot.value))
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &LpcRef> {
        self.slots.iter().map(|slot| &slot.value)
    }

    /// Transform all retained values without acquiring views; callers must preserve field types.
    pub(crate) fn try_map_values(
        &mut self,
        mut transform: impl FnMut(&LpcRef) -> Result<LpcRef>,
    ) -> Result<()> {
        for slot in &mut self.slots {
            slot.value = transform(&slot.value)?;
        }
        Ok(())
    }

    pub(crate) fn stored_views(&self) -> impl Iterator<Item = &[usize]> {
        self.views.iter().map(|view| view.slots.as_ref())
    }

    pub(crate) fn from_saved(
        id: ClassId,
        fields: Vec<(ClassField, LpcRef)>,
        views: Vec<Box<[usize]>>,
        txn: &TxnHandle,
    ) -> Result<Self> {
        let invalid = |message: &str| LpcError::runtime(format!("save file: {message}"));
        let mut unique = HashSet::new();
        for (field, value) in &fields {
            if !unique.insert(field) {
                return Err(invalid("duplicate class slot"));
            }
            if !value.passes_cast(field.type_, txn) {
                return Err(invalid(&format!(
                    "invalid value for {id}.{}: expected {}",
                    field.name, field.type_
                )));
            }
        }
        if views.is_empty() {
            return Err(invalid("class requires at least one view"));
        }
        let mut used = vec![false; fields.len()];
        let mut unique_views = HashSet::new();
        let mut restored_views = Vec::with_capacity(views.len());
        for slots in views {
            let mut names = HashSet::new();
            let mut declarations = Vec::with_capacity(slots.len());
            for &index in &slots {
                let Some((field, _)) = fields.get(index) else {
                    return Err(invalid("class view slot index is out of range"));
                };
                if !names.insert(field.name) {
                    return Err(invalid("duplicate field name in class view"));
                }
                used[index] = true;
                declarations.push(field.clone());
            }
            let definition = Arc::new(ClassDefinition {
                id,
                fields: declarations.into_boxed_slice(),
            });
            if !unique_views.insert(definition.clone()) {
                return Err(invalid("duplicate class view"));
            }
            restored_views.push(View { definition, slots });
        }
        if used.contains(&false) {
            return Err(invalid("class slot is not used by any view"));
        }
        Ok(Self {
            id,
            slots: fields
                .into_iter()
                .map(|(field, value)| Slot { field, value })
                .collect(),
            views: restored_views,
        })
    }

    fn view(&self, definition: &ClassDefinition) -> Option<&View> {
        self.views
            .iter()
            .find(|view| *view.definition == *definition)
    }

    fn acquire_view(&mut self, definition: Arc<ClassDefinition>) -> &View {
        if let Some(index) = self
            .views
            .iter()
            .position(|view| *view.definition == *definition)
        {
            return &self.views[index];
        }
        let slots = definition
            .fields
            .iter()
            .map(|field| {
                self.slots
                    .iter()
                    .position(|slot| slot.field == *field)
                    .unwrap_or_else(|| {
                        self.slots.push(Slot {
                            field: field.clone(),
                            value: NULL,
                        });
                        self.slots.len() - 1
                    })
            })
            .collect();
        self.views.push(View { definition, slots });
        self.views.last().expect("view just inserted")
    }
}

pub(crate) fn copy_class(value: &LpcRef, txn: &TxnHandle) -> Result<LpcRef> {
    let LpcRef::Class(cell) = value else {
        return Err(LpcError::runtime(format!(
            "copy_class: expected a class, found {}",
            value.type_name()
        )));
    };
    txn.with(|t| {
        let instance = t
            .read_class(cell.id)
            .ok_or_else(|| LpcError::runtime("class is not in this transaction's world"))?;
        Ok(LpcRef::Class(t.mint_class((*instance).clone())))
    })
}

fn resolve(
    receiver: &LpcRef,
    txn: &TxnHandle,
    scope: &ClassScope,
    name: Ustr,
) -> Result<(SVar<LpcClass>, Arc<ClassDefinition>, usize)> {
    let LpcRef::Class(cell) = receiver else {
        return Err(LpcError::runtime(format!(
            "member `{name}` requires a class, found {}",
            receiver.type_name()
        )));
    };
    let instance = txn
        .with(|t| t.read_class(cell.id))
        .ok_or_else(|| LpcError::runtime("class is not in this transaction's world"))?;
    let definition = scope
        .definitions
        .iter()
        .find(|definition| definition.id == instance.id)
        .ok_or_else(|| LpcError::runtime(format!("no visible definition for {}", instance.id)))?;
    let index = definition
        .fields
        .iter()
        .position(|field| field.name == name)
        .ok_or_else(|| LpcError::runtime(format!("unknown field `{name}` in {}", instance.id)))?;
    Ok((cell.clone(), definition.clone(), index))
}

pub(crate) fn read_member(
    receiver: &LpcRef,
    txn: &TxnHandle,
    scope: &ClassScope,
    name: Ustr,
) -> Result<LpcRef> {
    let (cell, definition, index) = resolve(receiver, txn, scope, name)?;
    txn.with(|t| {
        let current = t
            .read_class(cell.id)
            .ok_or_else(|| LpcError::runtime("class is not in this transaction's world"))?;
        if let Some(view) = current.view(&definition) {
            return Ok(current.slots[view.slots[index]].value.clone());
        }
        drop(current);
        t.with_class_cow(cell.id, |instance| {
            let slot = instance.acquire_view(definition).slots[index];
            instance.slots[slot].value.clone()
        })
    })
}

pub(crate) fn write_member(
    receiver: &LpcRef,
    txn: &TxnHandle,
    scope: &ClassScope,
    name: Ustr,
    value: LpcRef,
) -> Result<()> {
    let (cell, definition, index) = resolve(receiver, txn, scope, name)?;
    let target = definition.fields[index].type_;
    // Validate before touching an already-owned changeset payload, including its views.
    if !value.passes_cast(target, txn) {
        return Err(LpcError::runtime(format!(
            "invalid value for {}.{name}: expected {target}, found {}",
            definition.id,
            value.as_lpc_type(txn)
        )));
    }
    txn.with(|t| {
        t.with_class_cow(cell.id, |instance| {
            let slot = instance.acquire_view(definition).slots[index];
            instance.slots[slot].value = value;
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::Compiler,
        test_support::{run_prog, try_run_prog},
    };

    async fn returns(code: &str, expected: i64) {
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(expected)));
    }

    async fn compile_error(code: &str, expected: &str) {
        let error = Compiler::default()
            .compile_string("/records.c", code)
            .await
            .unwrap_err();
        assert!(error.to_string().contains(expected), "{error}");
    }

    #[tokio::test]
    async fn construction_defaults_order_and_aliases() {
        returns(
            r#"
            class record { int x, y, unused; }
            int counter;
            int next() { return ++counter; }
            int create() {
                class record r = new(class record, y: next(), x: next());
                class record alias = r;
                alias.x += 3;
                return r.x == 5 && r.y == 1 && r.unused == 0 && counter == 2;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn recursive_chains_indexed_receivers_and_function_fields() {
        returns(r#"
            class record { int count; class record next; function callback; }
            int create() {
                class record r = new(class record, next: new(class record, count: 4), callback: (: 7 :));
                mapping records = ([ "root": r ]);
                records["root"].next.count++;
                return records["root"].next.count + r.callback();
            }
        "#, 12).await;
    }

    #[tokio::test]
    async fn member_mutations_evaluate_receivers_once() {
        returns(
            r#"
            class record { int count; }
            class record record = new(class record, count: 10);
            int calls;
            class record get_record() { calls++; return record; }
            int create() {
                int a = get_record().count++;
                int b = ++get_record().count;
                int c = get_record().count--;
                int d = --get_record().count;
                get_record().count += 5;
                return a == 10 && b == 12 && c == 12 && d == 10 && record.count == 15 && calls == 5;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn compound_arithmetic_and_bitwise_member_writes() {
        returns(
            r#"
            class record { int n; }
            int create() {
                class record r = new(class record, n: 8);
                r.n *= 3; r.n /= 2; r.n -= 2; r.n %= 7;
                r.n <<= 3; r.n >>= 1; r.n |= 3; r.n &= 14; r.n ^= 5;
                return r.n;
            }
        "#,
            11,
        )
        .await;
    }

    #[tokio::test]
    async fn logical_compounds_short_circuit_and_return_the_written_value() {
        returns(
            r#"
            class record { int n; }
            int calls;
            int rhs() { calls++; return 5; }
            int create() {
                class record r = new(class record);
                r.n &&= rhs();
                int a = r.n ||= rhs();
                r.n ||= rhs();
                int b = r.n &&= rhs();
                return a == 5 && b == 5 && calls == 2;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn assignment_keeps_receiver_when_rhs_replaces_the_variable() {
        returns(
            r#"
            class record { int n; }
            class record r = new(class record, n: 1);
            int change() { r = new(class record, n: 99); return 4; }
            int create() {
                class record old = r;
                r.n = change();
                return old.n == 4 && r.n == 99;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn compound_keeps_receiver_and_prior_value_across_rhs_effects() {
        returns(
            r#"
            class record { int n; }
            class record r = new(class record, n: 2);
            int change() { r.n = 50; r = new(class record, n: 99); return 4; }
            int create() {
                class record old = r;
                r.n += change();
                return old.n == 6 && r.n == 99;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn efun_outputs_use_guarded_members_and_freeze_receivers() {
        returns(
            r#"
            class record { int n; }
            class record r = new(class record);
            int calls;
            class record get_record() { calls++; return r; }
            int create() {
                int matched = sscanf("42", "%d", get_record().n);
                return matched == 1 && r.n == 42 && calls == 1;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn rejected_efun_output_preserves_the_field() {
        returns(
            r#"
            class record { int n; }
            int create() {
                class record r = new(class record, n: 8);
                mixed alias = r;
                mixed error = catch(sscanf("wrong", "%s", alias.n));
                return stringp(error) && r.n == 8;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn all_field_replacements_validate_runtime_values() {
        returns(
            r#"
            class record { int n; }
            int create() {
                class record r = new(class record, n: 8);
                mixed wrong = "wrong";
                int misleading = wrong;
                mixed a = catch(r.n = wrong);
                mixed b = catch(r.n = misleading);
                mixed alias = r;
                mixed c = catch(alias.n += wrong);
                mixed d = catch(new(class record, n: misleading));
                return stringp(a) && stringp(b) && stringp(c) && stringp(d) && r.n == 8;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn class_fields_check_nominal_identity_and_accept_zero() {
        returns(
            r#"
            class a { int n; }
            class b { int n; }
            class box { class a value; }
            int create() {
                class box r = new(class box, value: new(class a));
                mixed wrong = new(class b);
                mixed error = catch(r.value = wrong);
                r.value = 0;
                return stringp(error) && r.value == 0;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn arrays_in_fields_keep_shallow_checks_and_shared_mutation() {
        returns(
            r#"
            class a { int n; }
            class b { int n; }
            class box { class a *values; }
            int create() {
                mixed *values = ({ new(class b) });
                class box r = new(class box, values: values);
                mixed error = catch(r.values[0].n);
                values[0] = new(class a, n: 7);
                return stringp(error) && r.values[0].n == 7;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn nominal_assignments_arguments_returns_and_casts_are_guarded() {
        returns(
            r#"
            class a { int n; }
            class b { int n; }
            mixed wrong = new(class b);
            class a result() { return wrong; }
            int accept(class a value) { return 1; }
            int create() {
                class a value;
                mixed a = catch(value = wrong);
                mixed b = catch(accept(wrong));
                mixed c = catch(result());
                mixed d = catch((class a) wrong);
                return stringp(a) && stringp(b) && stringp(c) && stringp(d) && value == 0;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn scalar_class_initializers_are_guarded() {
        let error = try_run_prog(
            r#"
            class a {}
            class b {}
            mixed wrong = new(class b);
            class a value = wrong;
        "#,
        )
        .await
        .unwrap_err();
        assert!(error.to_string().contains("expected class a"), "{error}");
    }

    #[tokio::test]
    async fn ref_aliases_cannot_hide_wrong_nominal_values() {
        returns(
            r#"
            class a { int n; }
            class b { int n; }
            void replace(mixed ref value) { value = new(class b, n: 9); }
            int create() {
                class a value = new(class a);
                replace(ref value);
                mixed error = catch(value.n);
                value = new(class a, n: 4);
                return stringp(error) && value.n == 4;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn typed_foreach_checks_even_unused_elements() {
        let error = try_run_prog(
            r#"
            class a {}
            class b {}
            void create() {
                mixed values = ({ new(class b) });
                foreach (class a value : values) { }
            }
        "#,
        )
        .await
        .unwrap_err();
        assert!(error.to_string().contains("expected class a"), "{error}");
    }

    #[tokio::test]
    async fn identity_equality_mapping_keys_and_type_reporting() {
        returns(r#"
            class record { int n; }
            int create() {
                class record a = new(class record, n: 4);
                class record b = new(class record, n: 4);
                mixed alias = a;
                mapping values = ([ a: 7, b: 9 ]);
                return a != b && a == alias && values[alias] == 7 && values[b] == 9 && typeof(a) == 9 && !arrayp(a) && !!a;
            }
        "#, 1).await;
    }

    #[tokio::test]
    async fn missing_members_and_zero_receivers_are_catchable() {
        returns(
            r#"
            class record { int n; }
            int create() {
                mixed r = new(class record);
                mixed a = catch(r.missing);
                mixed b = catch(r.missing = 8);
                class record zero;
                mixed c = catch(zero.n);
                return stringp(a) && stringp(b) && stringp(c) && r.n == 0;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn duplicate_initializers_are_rejected() {
        compile_error(
            "class r { int n; } mixed x = new(class r, n: 1, n: 2);",
            "duplicate initializer",
        )
        .await;
    }

    #[tokio::test]
    async fn unknown_initializers_are_rejected() {
        compile_error(
            "class r { int n; } mixed x = new(class r, missing: 1);",
            "unknown field",
        )
        .await;
    }

    #[tokio::test]
    async fn unknown_typed_members_are_rejected() {
        compile_error(
            "class r { int n; } int f(class r x) { return x.missing; }",
            "unknown field",
        )
        .await;
    }

    #[tokio::test]
    async fn non_class_member_receivers_are_rejected() {
        compile_error(
            "int f(int x) { return x.n; }",
            "member access requires a class or mixed",
        )
        .await;
    }

    #[tokio::test]
    async fn incompatible_initializers_are_rejected_statically() {
        compile_error(
            "class r { int n; } mixed x = new(class r, n: \"wrong\");",
            "initializer `n` expects int",
        )
        .await;
    }

    #[tokio::test]
    async fn explicit_ref_member_arguments_remain_unsupported() {
        compile_error(
            "class r { int n; } void f(int ref x) {} void g(class r x) { f(ref x.n); }",
            "ref",
        )
        .await;
    }
    fn definition(fields: &[(&str, lpc_rs_core::lpc_type::LpcType)]) -> Arc<ClassDefinition> {
        Arc::new(ClassDefinition {
            id: ClassId {
                source: ustr::ustr("/record.h"),
                name: ustr::ustr("record"),
            },
            fields: fields
                .iter()
                .map(|(name, type_)| ClassField {
                    name: ustr::ustr(name),
                    type_: *type_,
                })
                .collect(),
        })
    }

    fn scope(definition: Arc<ClassDefinition>) -> ClassScope {
        ClassScope {
            definitions: vec![definition].into_boxed_slice(),
        }
    }

    #[test]
    fn copying_preserves_all_views_without_acquiring_one_or_changing_the_source() {
        use lpc_rs_core::lpc_type::LpcType;
        let txn = TxnHandle::empty();
        let old = scope(definition(&[
            ("shared", LpcType::Int(false)),
            ("changed", LpcType::Int(false)),
            ("removed", LpcType::Int(false)),
        ]));
        let new = scope(definition(&[
            ("changed", LpcType::String(false)),
            ("shared", LpcType::Int(false)),
            ("added", LpcType::Int(false)),
        ]));
        let cell = txn.with(|t| t.mint_class(LpcClass::new(old.definitions[0].clone())));
        let original = LpcRef::Class(cell.clone());
        for (scope, field, value) in [
            (&old, "shared", 7.into()),
            (&old, "changed", 8.into()),
            (&old, "removed", 9.into()),
            (&new, "changed", "new".into()),
            (&new, "added", 10.into()),
        ] {
            write_member(&original, &txn, scope, ustr::ustr(field), value).unwrap();
        }
        let before = txn.with(|t| t.read_class(cell.id)).unwrap();
        let copy = copy_class(&original, &txn).unwrap();
        let LpcRef::Class(copy_cell) = &copy else {
            panic!("class copy")
        };
        assert_ne!(original, copy);
        assert_eq!(txn.with(|t| t.read_class(copy_cell.id)).unwrap(), before);
        assert_eq!(txn.with(|t| t.read_class(cell.id)).unwrap(), before);
        write_member(&copy, &txn, &new, ustr::ustr("shared"), 70.into()).unwrap();
        write_member(&copy, &txn, &old, ustr::ustr("removed"), 90.into()).unwrap();
        assert_eq!(
            read_member(&copy, &txn, &old, ustr::ustr("shared")).unwrap(),
            LpcRef::from(70)
        );
        assert_eq!(
            read_member(&copy, &txn, &old, ustr::ustr("changed")).unwrap(),
            LpcRef::from(8)
        );
        assert_eq!(
            read_member(&copy, &txn, &new, ustr::ustr("changed")).unwrap(),
            LpcRef::from("new")
        );
        assert_eq!(
            read_member(&copy, &txn, &new, ustr::ustr("added")).unwrap(),
            LpcRef::from(10)
        );
        assert_eq!(txn.with(|t| t.read_class(cell.id)).unwrap(), before);
    }

    #[test]
    fn removed_or_type_reverted_fields_reuse_their_retained_values() {
        use lpc_rs_core::lpc_type::LpcType;
        let txn = TxnHandle::empty();
        let old = scope(definition(&[("count", LpcType::Int(false))]));
        let interim = scope(definition(&[
            ("count", LpcType::String(false)),
            ("added", LpcType::Int(false)),
        ]));
        let reintroduced = scope(definition(&[
            ("added", LpcType::Int(false)),
            ("count", LpcType::Int(false)),
        ]));
        let instance =
            LpcRef::Class(txn.with(|t| t.mint_class(LpcClass::new(old.definitions[0].clone()))));
        write_member(&instance, &txn, &old, ustr::ustr("count"), 7.into()).unwrap();
        write_member(
            &instance,
            &txn,
            &interim,
            ustr::ustr("count"),
            LpcRef::from("text"),
        )
        .unwrap();
        assert_eq!(
            read_member(&instance, &txn, &reintroduced, ustr::ustr("count")).unwrap(),
            LpcRef::from(7)
        );
        write_member(
            &instance,
            &txn,
            &reintroduced,
            ustr::ustr("count"),
            LpcRef::from(8),
        )
        .unwrap();
        assert_eq!(
            read_member(&instance, &txn, &old, ustr::ustr("count")).unwrap(),
            LpcRef::from(8)
        );
        assert_eq!(
            read_member(&instance, &txn, &interim, ustr::ustr("count")).unwrap(),
            LpcRef::from("text")
        );
    }

    #[test]
    fn aborted_view_acquisition_does_not_publish_new_fields() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let old = definition(&[("n", LpcType::Int(false))]);
        let new = scope(definition(&[
            ("added", LpcType::Int(false)),
            ("n", LpcType::Int(false)),
        ]));
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let cell = seed.with(|t| t.mint_class(LpcClass::new(old)));
        let instance = LpcRef::Class(cell.clone());
        let before = seed.with(|t| t.read_class(cell.id)).unwrap();
        committer.commit(seed.with(|t| t.take_changeset())).unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            read_member(&instance, &writer, &new, ustr::ustr("added")).unwrap(),
            NULL
        );
        write_member(&instance, &writer, &new, ustr::ustr("added"), 42.into()).unwrap();
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(reader.with(|t| t.read_class(cell.id)).unwrap(), before);
        drop(writer);
        assert_eq!(
            read_member(&instance, &reader, &new, ustr::ustr("added")).unwrap(),
            NULL
        );
        write_member(&instance, &reader, &new, ustr::ustr("added"), 9.into()).unwrap();
        committer
            .commit(reader.with(|t| t.take_changeset()))
            .unwrap();
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            read_member(&instance, &reader, &new, ustr::ustr("added")).unwrap(),
            LpcRef::from(9)
        );
    }

    #[test]
    fn view_acquisition_conflicts_with_field_writes_and_retries_from_current_values() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let old = scope(definition(&[("n", LpcType::Int(false))]));
        let new = scope(definition(&[
            ("added", LpcType::Int(false)),
            ("n", LpcType::Int(false)),
        ]));
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let instance =
            LpcRef::Class(seed.with(|t| t.mint_class(LpcClass::new(old.definitions[0].clone()))));
        committer.commit(seed.with(|t| t.take_changeset())).unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let adapting = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            read_member(&instance, &adapting, &new, ustr::ustr("n")).unwrap(),
            NULL
        );
        write_member(&instance, &writer, &old, ustr::ustr("n"), 7.into()).unwrap();
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        assert!(
            committer
                .commit(adapting.with(|t| t.take_changeset()))
                .is_err()
        );
        let retry = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            read_member(&instance, &retry, &new, ustr::ustr("n")).unwrap(),
            LpcRef::from(7)
        );
        assert_eq!(
            read_member(&instance, &retry, &new, ustr::ustr("added")).unwrap(),
            NULL
        );
        committer
            .commit(retry.with(|t| t.take_changeset()))
            .unwrap();
    }

    #[test]
    fn copying_reads_pending_values_and_aborted_copies_are_absent() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let scope = scope(definition(&[("n", LpcType::Int(false))]));
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let instance =
            LpcRef::Class(seed.with(|t| t.mint_class(LpcClass::new(scope.definitions[0].clone()))));
        committer.commit(seed.with(|t| t.take_changeset())).unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        write_member(&instance, &writer, &scope, ustr::ustr("n"), 7.into()).unwrap();
        let copy = copy_class(&instance, &writer).unwrap();
        assert_eq!(
            read_member(&copy, &writer, &scope, ustr::ustr("n")).unwrap(),
            LpcRef::from(7)
        );
        drop(writer);
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(read_member(&copy, &reader, &scope, ustr::ustr("n")).is_err());
        assert_eq!(
            read_member(&instance, &reader, &scope, ustr::ustr("n")).unwrap(),
            NULL
        );
    }

    #[test]
    fn copying_conflicts_when_its_source_changes_before_commit() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let scope = scope(definition(&[("n", LpcType::Int(false))]));
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let instance =
            LpcRef::Class(seed.with(|t| t.mint_class(LpcClass::new(scope.definitions[0].clone()))));
        committer.commit(seed.with(|t| t.take_changeset())).unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let copier = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let abandoned = copy_class(&instance, &copier).unwrap();
        write_member(&instance, &writer, &scope, ustr::ustr("n"), 7.into()).unwrap();
        committer
            .commit(writer.with(|t| t.take_changeset()))
            .unwrap();
        assert!(
            committer
                .commit(copier.with(|t| t.take_changeset()))
                .is_err()
        );
        let retry = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert!(copy_class(&abandoned, &retry).is_err());
        let copy = copy_class(&instance, &retry).unwrap();
        assert_eq!(
            read_member(&copy, &retry, &scope, ustr::ustr("n")).unwrap(),
            LpcRef::from(7)
        );
        committer
            .commit(retry.with(|t| t.take_changeset()))
            .unwrap();
    }

    #[tokio::test]
    async fn independently_compiled_views_and_generic_copying_share_only_matching_fields() {
        use crate::{
            interpreter::vm::Vm,
            test_support::{lib_holding, temp_lib_config},
        };
        let header = r#"
            #ifdef UPDATED
            class record { string changed; int renamed, shared, added; }
            #else
            class record { int shared, removed, changed; }
            #endif
        "#;
        let root = lib_holding("class-evolution-programs", &[("record.h", header)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/old.c",
            r#"
            #include "record.h"
            mixed make() { return new(class record, shared: 7, removed: 8, changed: 9); }
            int check(class record r, int shared) {
                return r.shared == shared && r.removed == 8 && r.changed == 9;
            }
            int update(mixed r) { r.shared++; return ++r.removed; }
        "#,
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/new.c",
            r#"
            #define UPDATED
            #include "record.h"
            int adapt(class record r) {
                if (r.shared != 7 || r.changed != 0 || r.added != 0 || r.renamed != 0) return 0;
                r.shared = 70; r.changed = "new"; r.added = 10;
                return 1;
            }
            int check(mixed r, int shared) {
                return r.shared == shared && r.changed == "new" && r.added == 10 && r.renamed == 0;
            }
            int rejected(mixed r) {
                mixed wrong = catch(r.changed = 123);
                mixed removed = catch(r.removed);
                return stringp(wrong) && stringp(removed) && r.changed == "new";
            }
        "#,
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/generic.c",
            r#"
            mixed duplicate(mixed r) { return classp(r) ? copy_class(r) : 0; }
        "#,
        )
        .await
        .unwrap();
        let result = vm
            .initialize_process_from_code(
                "/runner.c",
                r#"
            int create() {
                mixed original = "/old"->make();
                if (!"/new"->adapt(original) || !"/old"->check(original, 70)) return 0;
                mixed copy = "/generic"->duplicate(original);
                if (!classp(copy) || copy == original || !"/old"->check(copy, 70)) return 0;
                if ("/old"->update(copy) != 9) return 0;
                return "/new"->check(copy, 71) && "/new"->check(original, 70) &&
                    "/old"->check(original, 70) && "/new"->rejected(copy);
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[test]
    fn mixed_access_requires_the_actual_nominal_definition() {
        use lpc_rs_core::lpc_type::LpcType;
        let txn = TxnHandle::empty();
        let definition = definition(&[("n", LpcType::Int(false))]);
        let instance =
            LpcRef::Class(txn.with(|txn| txn.mint_class(LpcClass::new(definition.clone()))));
        let mut unrelated = (*definition).clone();
        unrelated.id.source = ustr::ustr("/other.h");
        let error = read_member(
            &instance,
            &txn,
            &scope(Arc::new(unrelated)),
            ustr::ustr("n"),
        )
        .unwrap_err();
        assert!(error.to_string().contains("no visible definition"));
    }

    #[test]
    fn class_updates_are_isolated_and_aborted_allocations_are_absent() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let definition = definition(&[("n", LpcType::Int(false))]);
        let scope = scope(definition.clone());
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let instance =
            LpcRef::Class(seed.with(|txn| txn.mint_class(LpcClass::new(definition.clone()))));
        committer
            .commit(seed.with(|txn| txn.take_changeset()))
            .unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        write_member(&instance, &writer, &scope, ustr::ustr("n"), 7.into()).unwrap();
        let orphan = writer.with(|txn| txn.mint_class(LpcClass::new(definition)));
        assert_eq!(
            read_member(&instance, &writer, &scope, ustr::ustr("n")).unwrap(),
            LpcRef::from(7)
        );
        assert_eq!(
            read_member(&instance, &reader, &scope, ustr::ustr("n")).unwrap(),
            NULL
        );
        drop(writer);
        assert!(reader.with(|txn| txn.read_class(orphan.id)).is_none());
        assert_eq!(
            read_member(&instance, &reader, &scope, ustr::ustr("n")).unwrap(),
            NULL
        );
    }

    #[test]
    fn caught_invalid_writes_preserve_owned_payloads_and_do_not_create_views() {
        use lpc_rs_core::lpc_type::LpcType;
        let txn = TxnHandle::empty();
        let old = definition(&[("n", LpcType::Int(false))]);
        let new = definition(&[
            ("n", LpcType::Int(false)),
            ("added", LpcType::String(false)),
        ]);
        let cell = txn.with(|txn| txn.mint_class(LpcClass::new(old.clone())));
        let instance = LpcRef::Class(cell.clone());
        write_member(&instance, &txn, &scope(old), ustr::ustr("n"), 7.into()).unwrap();
        let before = txn.with(|txn| txn.read_class(cell.id)).unwrap();
        assert!(
            write_member(&instance, &txn, &scope(new), ustr::ustr("added"), 42.into()).is_err()
        );
        assert_eq!(txn.with(|txn| txn.read_class(cell.id)).unwrap(), before);
    }

    #[tokio::test]
    async fn independent_programs_use_nominal_visibility_and_shared_header_identity() {
        use crate::{
            interpreter::{
                task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
                vm::Vm,
            },
            test_support::{lib_holding, temp_lib_config},
        };
        let root = lib_holding(
            "class-program-visibility",
            &[("record.h", "class record { int n; }")],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let owner = vm
            .initialize_process_from_code(
                "/owner.c",
                "#include \"record.h\"\n mixed make() { return new(class record, n: 7); }",
            )
            .await
            .unwrap()
            .context
            .process;
        let helper = vm
            .initialize_process_from_code(
                "/helper.c",
                "#include \"record.h\"\n int read(class record r) { return r.n; }",
            )
            .await
            .unwrap()
            .context
            .process;
        let unrelated = vm.initialize_process_from_code("/unrelated.c", "class record { int n; } int read(mixed r) { return r.n; } int typed(class record r) { return r.n; }").await.unwrap().context.process;
        let generic = vm
            .initialize_process_from_code("/generic.c", "int read(mixed r) { return r.n; }")
            .await
            .unwrap()
            .context
            .process;
        let value = apply_function_by_name(
            "make",
            &[],
            owner,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        let args = [value];
        let result = apply_function_by_name(
            "read",
            &args,
            helper,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(7));
        for process in [generic, unrelated.clone()] {
            let error = apply_function_by_name(
                "read",
                &args,
                process,
                TaskTemplate::from(vm.global_state.clone()),
                Some(5000),
            )
            .await
            .unwrap()
            .unwrap_err();
            assert!(
                error.to_string().contains("no visible definition"),
                "{error}"
            );
        }
        let error = apply_function_by_name(
            "typed",
            &args,
            unrelated,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap_err();
        assert!(
            error.to_string().contains("unexpected argument type"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn class_values_inside_closures_keep_shared_storage() {
        returns(
            r#"
            class record { int n; }
            function reader(class record r) { return (: ++r.n :); }
            int create() {
                class record r = new(class record, n: 4);
                function f = reader(r);
                return f() == 5 && r.n == 5;
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn unsupported_instance_operations_fail_without_changing_the_record() {
        returns(
            r#"
            class record { int n; }
            int create() {
                mixed r = new(class record, n: 8);
                mixed a = catch(r[0]);
                mixed b = catch(r + 1);
                mixed c = catch(r[0..1]);
                return stringp(a) && stringp(b) && stringp(c) && sizeof(r) == 0 && r.n == 8;
            }
        "#,
            1,
        )
        .await;
    }

    #[test]
    fn saving_nested_classes_restores_fresh_instances() {
        use crate::interpreter::{
            lpc_array::LpcArray,
            save_format::{read_value, write_value},
        };
        let txn = TxnHandle::empty();
        let instance =
            LpcRef::Class(txn.with(|txn| txn.mint_class(LpcClass::new(definition(&[])))));
        let array =
            LpcRef::Array(txn.with(|txn| txn.mint_array(LpcArray::new([instance.clone()]))));
        let mut text = String::new();
        write_value(&mut text, &array, &txn, 0).unwrap();
        let restored = read_value(&text, &txn, &|_| None).unwrap();
        let item = restored.with_array(&txn, |items| items[0].clone()).unwrap();
        let LpcRef::Class(cell) = &item else {
            panic!("restored class")
        };
        assert_ne!(item, instance);
        assert_eq!(
            txn.with(|txn| txn.read_class(cell.id)).unwrap().id(),
            definition(&[]).id
        );
    }
    #[tokio::test]
    async fn scalar_class_defaults_spreads_and_ref_parameters_are_guarded() {
        returns(
            r#"
            class a {}
            class b {}
            mixed wrong = new(class b);
            int defaults(class a value = wrong) { return 1; }
            int spread(class a value) { return 1; }
            int by_ref(class a ref value) { return 1; }
            int create() {
                mixed a = catch(defaults());
                mixed b = catch(spread(({ wrong })...));
                mixed c = catch(by_ref(ref wrong));
                return stringp(a) && stringp(b) && stringp(c);
            }
        "#,
            1,
        )
        .await;
    }

    #[tokio::test]
    async fn assignment_to_positional_closure_arguments_still_works() {
        returns(
            "int create() { function f = (: $1 = 7 :); return f(3); }",
            7,
        )
        .await;
    }

    #[test]
    fn writes_to_different_fields_of_one_instance_conflict() {
        use crate::interpreter::stm::{Committer, Transaction};
        use lpc_rs_core::lpc_type::LpcType;
        let mut committer = Committer::new();
        let definition = definition(&[("x", LpcType::Int(false)), ("y", LpcType::Int(false))]);
        let seed = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let instance =
            LpcRef::Class(seed.with(|txn| txn.mint_class(LpcClass::new(definition.clone()))));
        committer
            .commit(seed.with(|txn| txn.take_changeset()))
            .unwrap();
        let scope = scope(definition);
        let first = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        let second = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        write_member(&instance, &first, &scope, ustr::ustr("x"), 1.into()).unwrap();
        write_member(&instance, &second, &scope, ustr::ustr("y"), 2.into()).unwrap();
        committer
            .commit(first.with(|txn| txn.take_changeset()))
            .unwrap();
        assert!(
            committer
                .commit(second.with(|txn| txn.take_changeset()))
                .is_err()
        );
        let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        assert_eq!(
            read_member(&instance, &reader, &scope, ustr::ustr("x")).unwrap(),
            LpcRef::from(1)
        );
        assert_eq!(
            read_member(&instance, &reader, &scope, ustr::ustr("y")).unwrap(),
            NULL
        );
    }
    #[tokio::test]
    async fn class_construction_keeps_new_available_as_an_identifier() {
        returns(
            r#"
            class record { int n; }
            int new(int value) { return value + 1; }
            int accept(int new) { return new; }
            int create() {
                class record r = new(class record, n: 8);
                return new(r.n) + accept(3);
            }
        "#,
            12,
        )
        .await;
    }

    #[tokio::test]
    async fn another_function_cannot_use_class_constructor_syntax() {
        compile_error(
            "class r {} mixed value = other(class r);",
            "class construction requires `new`",
        )
        .await;
    }
}
