use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `m_delete(m, key)`: remove `key` from the mapping and return the same
/// mapping; the rest keep their order. A destructed-object key names 0.
pub fn m_delete<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mapping = context.arg(0);
    if !matches!(mapping, LpcRef::Mapping(_)) {
        return Err(context.runtime_error(format!(
            "m_delete: {} is not a mapping",
            mapping.type_name()
        )));
    }
    let txn = context.txn();
    let key = context.arg(1).mapping_key(txn);
    // An absent key writes nothing, so a retry conflicts on nothing.
    if mapping.with_mapping(txn, |m| m.contains_key(&key))? {
        mapping.with_mapping_cow(txn, |m| {
            m.shift_remove(&key);
            Ok(())
        })?;
    }
    let result = mapping.clone();
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, strings_of, try_run_prog},
    };

    #[tokio::test]
    async fn m_delete_removes_the_key() {
        let code = r#"mixed *create() { return keys(m_delete(([ "a": 1, "b": 2 ]), "a")); }"#;
        assert_eq!(strings_of(code).await, ["b"]);
    }

    #[tokio::test]
    async fn m_delete_returns_the_same_mapping() {
        let code = r#"int create() { mapping m = ([ "a": 1 ]); return m_delete(m, "a") == m; }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn the_deletion_is_seen_through_every_reference() {
        let code = r#"int create() { mapping m = ([ "a": 1, "b": 2 ]); m_delete(m, "a"); return sizeof(m); }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_absent_key_changes_nothing() {
        let code =
            r#"int create() { mapping m = ([ "a": 1 ]); m_delete(m, "z"); return sizeof(m); }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn the_rest_keep_their_order() {
        let code = r#"
            mixed *create() {
                mapping m = ([ "a": 1, "b": 2, "c": 3 ]);
                m_delete(m, "b");
                return keys(m);
            }
        "#;
        assert_eq!(strings_of(code).await, ["a", "c"]);
    }

    #[tokio::test]
    async fn a_destructed_object_key_names_the_zero_key() {
        let code = r#"
            int create() {
                mapping m = ([ 0: 1 ]);
                object o = clone_object("/clone_target");
                destruct(o);
                m_delete(m, o);
                return sizeof(m);
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn m_delete_of_a_non_mapping_is_an_error() {
        let err = try_run_prog(r#"mixed create() { mixed x = 1; return m_delete(x, "a"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("m_delete: int is not a mapping"), "{err}");
    }
}
