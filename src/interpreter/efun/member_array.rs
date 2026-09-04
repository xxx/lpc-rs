use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `member_array(x, arr [, start])`: the index of the first element equal
/// to `x` (as `==` sees it: a destructed object is 0) at or past `start`;
/// -1 when there is none.
pub fn member_array<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let haystack = context.arg(1);
    if !matches!(haystack, LpcRef::Array(_)) {
        return Err(context.runtime_error(format!(
            "member_array: {} is not an array",
            haystack.type_name()
        )));
    }
    let start = match context.arg(2) {
        LpcRef::Int(start) if start.0 < 0 => {
            return Err(context.runtime_error(format!("member_array: negative start {start}")));
        }
        LpcRef::Int(start) => start.0 as usize,
        other => {
            return Err(
                context.runtime_error(format!("member_array: {} is not an int", other.type_name()))
            );
        }
    };
    let needle = context.arg(0);
    let txn = context.txn();
    let found = haystack.with_array(txn, |a| {
        a.iter()
            .skip(start)
            .position(|item| item.eq_in(needle, txn))
    })?;
    let index = found.map_or(-1, |i| (i + start) as LpcIntInner);
    context.return_efun_result(LpcRef::from(index));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    async fn int_of(code: &str) -> LpcRef {
        run_prog(code).await.result().expect("a result")
    }

    #[tokio::test]
    async fn member_array_is_the_index_of_the_first_match() {
        let r = int_of("int create() { return member_array(2, ({ 1, 2, 3, 2 })); }").await;
        assert_eq!(r, LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_absent_item_is_minus_one() {
        let r = int_of("int create() { return member_array(9, ({ 1, 2, 3 })); }").await;
        assert_eq!(r, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn the_search_starts_at_start() {
        let r = int_of("int create() { return member_array(2, ({ 1, 2, 3, 2 }), 2); }").await;
        assert_eq!(r, LpcRef::from(3));
    }

    #[tokio::test]
    async fn a_start_past_the_end_is_minus_one() {
        let r = int_of("int create() { return member_array(1, ({ 1 }), 5); }").await;
        assert_eq!(r, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn strings_match_by_value() {
        let r = int_of(r#"int create() { return member_array("b", ({ "a", "b" })); }"#).await;
        assert_eq!(r, LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_destructed_object_matches_zero() {
        let code = r#"
            int create() {
                object o = clone_object("/clone_target");
                destruct(o);
                return member_array(0, ({ 1, o }));
            }
        "#;
        assert_eq!(int_of(code).await, LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_negative_start_is_an_error() {
        let err = try_run_prog("int create() { return member_array(1, ({ 1 }), -1); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("member_array: negative start -1"), "{err}");
    }

    #[tokio::test]
    async fn a_non_array_haystack_is_an_error() {
        let err = try_run_prog(r#"int create() { mixed s = "s"; return member_array(1, s); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("member_array: string is not an array"),
            "{err}"
        );
    }
}
