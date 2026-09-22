use std::collections::HashMap;

use lpc_rs_errors::{LpcError, Result, lpc_bug};

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::LpcRef,
    stm::{Transaction, VarId},
};

/// Copy the mutable container graph, preserving cycles and shared references.
pub fn deep_copy<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let copy = context.txn().with(|txn| copy_graph(context.arg(0), txn))?;
    context.return_efun_result(copy);
    Ok(())
}

fn container_id(value: &LpcRef) -> Option<VarId> {
    match value {
        LpcRef::Array(cell) => Some(cell.id),
        LpcRef::Mapping(cell) => Some(cell.id),
        LpcRef::Class(cell) => Some(cell.id),
        LpcRef::Nil
        | LpcRef::Int(_)
        | LpcRef::Float(_)
        | LpcRef::String(_)
        | LpcRef::Bytes(_)
        | LpcRef::Object(_)
        | LpcRef::Function(_) => None,
    }
}

fn copied_ref(value: &LpcRef, copies: &HashMap<VarId, LpcRef>) -> Result<LpcRef> {
    match container_id(value) {
        Some(id) => copies
            .get(&id)
            .cloned()
            .ok_or_else(|| lpc_bug!("deep_copy: undiscovered container")),
        None => Ok(value.clone()),
    }
}

fn copy_graph(value: &LpcRef, txn: &mut Transaction) -> Result<LpcRef> {
    let mut copies = HashMap::new();
    let mut pending = vec![value.clone()];

    // Allocate every reachable container before replacing references, so cycles resolve locally.
    while let Some(source) = pending.pop() {
        let Some(id) = container_id(&source) else {
            continue;
        };
        if copies.contains_key(&id) {
            continue;
        }
        let missing =
            || LpcError::runtime("deep_copy: container is not in this transaction's world");
        let copy = match source {
            LpcRef::Array(cell) => {
                let array = txn.read_array(cell.id).ok_or_else(missing)?;
                pending.extend(array.iter().cloned());
                LpcRef::Array(txn.mint_array((*array).clone()))
            }
            LpcRef::Mapping(cell) => {
                let mapping = txn.read_mapping(cell.id).ok_or_else(missing)?;
                for (key, value) in mapping.iter() {
                    pending.push(key.clone());
                    pending.push(value.clone());
                }
                LpcRef::Mapping(txn.mint_mapping((*mapping).clone()))
            }
            LpcRef::Class(cell) => {
                let instance = txn.read_class(cell.id).ok_or_else(missing)?;
                pending.extend(instance.values().cloned());
                LpcRef::Class(txn.mint_class((*instance).clone()))
            }
            _ => return Err(lpc_bug!("deep_copy: expected a mutable container")),
        };
        copies.insert(id, copy);
    }

    for copy in copies.values() {
        match copy {
            LpcRef::Array(cell) => txn.with_array_cow(cell.id, |array| {
                for value in array.iter_mut() {
                    *value = copied_ref(value, &copies)?;
                }
                Ok(())
            })?,
            LpcRef::Mapping(cell) => txn.with_mapping_cow(cell.id, |mapping| {
                mapping.mapping = mapping
                    .iter()
                    .map(|(key, value)| {
                        Ok((copied_ref(key, &copies)?, copied_ref(value, &copies)?))
                    })
                    .collect::<Result<_>>()?;
                Ok(())
            })?,
            LpcRef::Class(cell) => txn.with_class_cow(cell.id, |instance| {
                instance.try_map_values(|value| copied_ref(value, &copies))
            })??,
            _ => return Err(lpc_bug!("deep_copy: expected a mutable copy")),
        }
    }

    copied_ref(value, &copies)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::{
        class::{ClassDefinition, ClassField, ClassId, ClassScope},
        lpc_type::LpcType,
    };
    use ustr::ustr;

    use super::*;
    use crate::{
        interpreter::{
            lpc_array::LpcArray,
            lpc_class::{LpcClass, read_member, write_member},
            lpc_mapping::LpcMapping,
            stm::{Committer, SVar, TxnHandle},
            vm::Vm,
        },
        test_support::{lib_holding, run_prog, temp_lib_config},
    };

    #[tokio::test]
    async fn immutable_values_and_object_and_function_identities_are_preserved() {
        let result = run_prog(
            r#"
            int create() {
                int counter;
                function callback = (: ++counter :);
                mixed *values = ({ 0, 42, -7, 1.25, "text", to_bytes("x", "utf8"),
                    this_object(), callback });
                foreach (mixed value : values) {
                    if (deep_copy(value) != value) return 0;
                }
                function copied = deep_copy(callback);
                return copied() == 1 && callback() == 2;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn nested_containers_are_independent_and_classes_keep_their_nominal_type() {
        let result = run_prog(
            r#"
            class record { int *items; mapping labels; class record child; int n; }
            class other { int n; }
            int accept(class record value) { return value.n; }
            int create() {
                class record original = new(class record, items: ({ 1 }),
                    labels: ([ "x": ({ 2 }) ]), child: new(class record, n: 3), n: 4);
                class record copy = deep_copy(original);
                copy.items[0] = 10;
                copy.labels["x"][0] = 20;
                copy.child.n = 30;
                mixed wrong = catch((class other)deep_copy(original));
                return copy != original && copy.items != original.items &&
                    copy.labels != original.labels && copy.child != original.child &&
                    original.items[0] == 1 && original.labels["x"][0] == 2 &&
                    original.child.n == 3 && accept(copy) == 4 && stringp(wrong);
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn repeated_references_share_one_copy_per_call() {
        let result = run_prog(
            r#"
            class record { mixed value; }
            int create() {
                mixed *shared = ({ 1 });
                mapping map = ([ "item": shared ]);
                class record record = new(class record, value: shared);
                mixed *original = ({ shared, shared, map, map, record, record });
                mixed *copy = deep_copy(original);
                mixed *second = deep_copy(original);
                copy[0][0] = 9;
                return copy != original && copy[0] != shared && copy[0] == copy[1] &&
                    copy[2] == copy[3] && copy[4] == copy[5] &&
                    copy[2]["item"] == copy[0] && copy[4].value == copy[0] &&
                    copy[1][0] == 9 && shared[0] == 1 && second[0][0] == 1 &&
                    second[0] != copy[0] && second[2] != copy[2] && second[4] != copy[4];
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn cycles_across_arrays_mappings_and_classes_point_into_the_copy() {
        let result = run_prog(
            r#"
            class record { mixed next; }
            int create() {
                mixed *array = ({ 0, 0 });
                mapping map = ([]);
                class record record = new(class record, next: array);
                array[0] = array;
                array[1] = map;
                map["record"] = record;
                map[map] = map;
                mixed *copy = deep_copy(array);
                mapping copied_map = copy[1];
                class record copied_record = copied_map["record"];
                return copy != array && copy[0] == copy && copied_map != map &&
                    copied_record != record && copied_record.next == copy &&
                    copied_map[copied_map] == copied_map && copied_map[map] == 0 &&
                    array[0] == array && map[map] == map && record.next == array;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn mutable_keys_are_copied_with_values_and_keep_insertion_order() {
        let result = run_prog(
            r#"
            class record { int n; }
            int create() {
                mixed *array = ({ 1 });
                mapping map = ([ "n": 2 ]);
                class record record = new(class record, n: 3);
                function callback = (: 4 :);
                mapping original = ([ array: array, map: map, record: record,
                    this_object(): 4, callback: 5, "text": 6 ]);
                mixed *graph = deep_copy(({ original, array, map, record }));
                mapping copy = graph[0];
                mixed *ordered = keys(copy);
                graph[1][0] = 10;
                graph[2]["n"] = 20;
                graph[3].n = 30;
                return sizeof(copy) == 6 && copy[graph[1]] == graph[1] &&
                    copy[graph[2]] == graph[2] && copy[graph[3]] == graph[3] &&
                    copy[array] == 0 && copy[map] == 0 && copy[record] == 0 &&
                    copy[this_object()] == 4 && copy[callback] == 5 && copy["text"] == 6 &&
                    ordered[0] == graph[1] && ordered[1] == graph[2] &&
                    ordered[2] == graph[3] && ordered[3] == this_object() &&
                    ordered[4] == callback && ordered[5] == "text" &&
                    array[0] == 1 && map["n"] == 2 && record.n == 3;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn empty_containers_get_fresh_identities_through_an_efun_pointer() {
        let result = run_prog(
            r#"
            class empty {}
            int create() {
                function copier = &deep_copy();
                mixed *array = ({});
                mapping map = ([]);
                class empty instance = new(class empty);
                mixed *copy = copier(({ array, map, instance }));
                return arrayp(copy[0]) && sizeof(copy[0]) == 0 && copy[0] != array &&
                    mappingp(copy[1]) && sizeof(copy[1]) == 0 && copy[1] != map &&
                    classp(copy[2]) && copy[2] != instance;
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn generic_copying_preserves_old_and_new_class_views_and_nested_aliases() {
        let root = lib_holding(
            "deep-copy-class-views",
            &[(
                "record.h",
                r#"
                #ifdef UPDATED
                class record { mapping changed; mixed *shared; }
                #else
                class record { mixed *shared, removed, changed; }
                #endif
            "#,
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/old.c",
            r#"
            #include "record.h"
            mixed make() {
                mixed *shared = ({ 7 });
                return new(class record, shared: shared, removed: shared, changed: ({ 8 }));
            }
            int check(class record r, int expected) {
                return r.shared == r.removed && r.removed[0] == expected && r.changed[0] == 8;
            }
            void update(class record r) { r.removed[0] = 70; }
        "#,
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/new.c",
            r#"
            #define UPDATED
            #include "record.h"
            void adapt(class record r) { r.changed = ([ "shared": r.shared ]); }
            int check(class record r, int expected) {
                return r.changed["shared"] == r.shared && r.shared[0] == expected;
            }
        "#,
        )
        .await
        .unwrap();
        let result = vm
            .initialize_process_from_code(
                "/generic.c",
                r#"
                int create() {
                    mixed original = "/old"->make();
                    "/new"->adapt(original);
                    mixed copy = deep_copy(original);
                    if (!"/old"->check(copy, 7) || !"/new"->check(copy, 7)) return 0;
                    "/old"->update(copy);
                    return "/old"->check(copy, 70) && "/new"->check(copy, 70) &&
                        "/old"->check(original, 7) && "/new"->check(original, 7);
                }
            "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[test]
    fn deeply_nested_graphs_do_not_require_recursive_traversal() {
        let txn = TxnHandle::empty();
        txn.with(|txn| {
            let mut original = LpcRef::from(42);
            for _ in 0..10_000 {
                original = LpcRef::Array(txn.mint_array(LpcArray::new(vec![original])));
            }
            let mut copy = copy_graph(&original, txn).unwrap();
            for _ in 0..10_000 {
                assert_ne!(copy, original);
                let LpcRef::Array(source) = original else {
                    panic!("expected array")
                };
                let LpcRef::Array(dest) = copy else {
                    panic!("expected copied array")
                };
                original = txn.read_array(source.id).unwrap()[0].clone();
                copy = txn.read_array(dest.id).unwrap()[0].clone();
            }
            assert_eq!(copy, LpcRef::from(42));
        });
    }

    fn record_scope() -> ClassScope {
        ClassScope {
            definitions: vec![Arc::new(ClassDefinition {
                id: ClassId {
                    source: ustr("/record.h"),
                    name: ustr("record"),
                },
                fields: vec![ClassField {
                    name: ustr("value"),
                    type_: LpcType::Mixed(false),
                }]
                .into_boxed_slice(),
            })]
            .into_boxed_slice(),
        }
    }

    #[test]
    fn pending_writes_are_copied_and_aborted_allocations_are_absent() {
        let mut committer = Committer::new();
        let scope = record_scope();
        let mut seed = Transaction::new(committer.snapshot_clone());
        let array = seed.mint_array(LpcArray::new(vec![1.into()]));
        let mapping = seed.mint_mapping(LpcMapping::default());
        let class = seed.mint_class(LpcClass::new(scope.definitions[0].clone()));
        let root = LpcRef::Array(seed.mint_array(LpcArray::new(vec![
            LpcRef::Array(array.clone()),
            LpcRef::Mapping(mapping.clone()),
            LpcRef::Class(class.clone()),
        ])));
        committer.commit(seed.take_changeset()).unwrap();
        let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
        writer.with(|txn| {
            txn.with_array_cow(array.id, |a| {
                a[0] = 2.into();
                Ok(())
            })
            .unwrap();
            txn.with_mapping_cow(mapping.id, |m| {
                m.insert("key".into(), 3.into());
                Ok(())
            })
            .unwrap();
        });
        write_member(
            &LpcRef::Class(class),
            &writer,
            &scope,
            ustr("value"),
            4.into(),
        )
        .unwrap();
        let copy = writer.with(|txn| copy_graph(&root, txn)).unwrap();
        let children = copy.with_array(&writer, |a| a.to_vec()).unwrap();
        assert_eq!(
            children[0].with_array(&writer, |a| a[0].clone()).unwrap(),
            LpcRef::from(2)
        );
        assert_eq!(
            children[1]
                .with_mapping(&writer, |m| m.get(&"key".into()).cloned())
                .unwrap(),
            Some(3.into())
        );
        assert_eq!(
            read_member(&children[2], &writer, &scope, ustr("value")).unwrap(),
            LpcRef::from(4)
        );
        drop(writer);
        let mut reader = Transaction::new(committer.snapshot_clone());
        for value in children.iter().chain(std::iter::once(&copy)) {
            assert!(reader.read_value(container_id(value).unwrap()).is_none());
        }
        assert_eq!(reader.read_array(array.id).unwrap()[0], LpcRef::from(1));
        assert!(reader.read_mapping(mapping.id).unwrap().is_empty());
    }

    #[test]
    fn changes_to_nested_containers_including_keys_conflict_and_retry() {
        let scope = record_scope();
        for changed in 0..3 {
            let mut committer = Committer::new();
            let mut seed = Transaction::new(committer.snapshot_clone());
            let key = seed.mint_array(LpcArray::new(vec![1.into()]));
            let class = seed.mint_class(LpcClass::new(scope.definitions[0].clone()));
            let map = seed.mint_mapping(LpcMapping::new(
                [(LpcRef::Array(key.clone()), LpcRef::Class(class.clone()))]
                    .into_iter()
                    .collect(),
            ));
            let root =
                LpcRef::Array(seed.mint_array(LpcArray::new(vec![LpcRef::Mapping(map.clone())])));
            committer.commit(seed.take_changeset()).unwrap();
            let mut copier = Transaction::new(committer.snapshot_clone());
            let abandoned = copy_graph(&root, &mut copier).unwrap();
            let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
            match changed {
                0 => writer
                    .with(|txn| {
                        txn.with_array_cow(key.id, |a| {
                            a[0] = 9.into();
                            Ok(())
                        })
                    })
                    .unwrap(),
                1 => writer
                    .with(|txn| {
                        txn.with_mapping_cow(map.id, |m| {
                            m.insert("added".into(), 9.into());
                            Ok(())
                        })
                    })
                    .unwrap(),
                _ => write_member(
                    &LpcRef::Class(class),
                    &writer,
                    &scope,
                    ustr("value"),
                    9.into(),
                )
                .unwrap(),
            }
            committer
                .commit(writer.with(|txn| txn.take_changeset()))
                .unwrap();
            assert!(committer.commit(copier.take_changeset()).is_err());
            let retry = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
            assert!(retry.with(|txn| copy_graph(&abandoned, txn)).is_err());
            let copy = retry.with(|txn| copy_graph(&root, txn)).unwrap();
            let map = copy.with_array(&retry, |a| a[0].clone()).unwrap();
            let (key, value) = map
                .with_mapping(&retry, |m| {
                    let (key, value) = m.get_index(0).unwrap();
                    (key.clone(), value.clone())
                })
                .unwrap();
            match changed {
                0 => assert_eq!(
                    key.with_array(&retry, |a| a[0].clone()).unwrap(),
                    LpcRef::from(9)
                ),
                1 => assert_eq!(
                    map.with_mapping(&retry, |m| m.get(&"added".into()).cloned())
                        .unwrap(),
                    Some(9.into())
                ),
                _ => assert_eq!(
                    read_member(&value, &retry, &scope, ustr("value")).unwrap(),
                    LpcRef::from(9)
                ),
            }
            committer
                .commit(retry.with(|txn| txn.take_changeset()))
                .unwrap();
        }
    }

    #[test]
    fn missing_container_cells_return_errors() {
        let txn = TxnHandle::empty();
        for missing in [
            LpcRef::Array(SVar::new()),
            LpcRef::Mapping(SVar::new()),
            LpcRef::Class(SVar::new()),
        ] {
            txn.with(|txn| {
                assert!(
                    copy_graph(&missing, txn)
                        .unwrap_err()
                        .to_string()
                        .contains("not in this transaction")
                );
                let nested = LpcRef::Array(txn.mint_array(LpcArray::new(vec![missing])));
                assert!(copy_graph(&nested, txn).is_err());
            });
        }
    }
}
