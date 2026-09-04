use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// `allocate(n [, init])`: an array of `n` copies of `init` (0 when
/// absent). An array or mapping `init` is one value shared by every slot.
pub fn allocate<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::Int(size) = context.arg(0) else {
        return Err(context.runtime_error(format!(
            "allocate: {} is not an int",
            context.arg(0).type_name()
        )));
    };
    let n = size.0;
    if n < 0 {
        return Err(context.runtime_error(format!("allocate: negative size {n}")));
    }
    let init = context.arg(1).clone();
    context.return_array(std::iter::repeat_n(init, n as usize));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, try_run_prog},
    };

    async fn strings_of(code: &str) -> Vec<String> {
        let task = run_prog(code).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                arr.iter().map(|x| x.to_string()).collect()
            })
            .unwrap()
    }

    #[tokio::test]
    async fn allocate_makes_n_zeros() {
        let items = strings_of("mixed *create() { return allocate(3); }").await;
        assert_eq!(items, ["0", "0", "0"]);
    }

    #[tokio::test]
    async fn allocate_fills_every_slot_with_the_init_value() {
        let items = strings_of(r#"mixed *create() { return allocate(2, "x"); }"#).await;
        assert_eq!(items, ["x", "x"]);
    }

    #[tokio::test]
    async fn allocate_of_zero_is_empty() {
        let items = strings_of("mixed *create() { return allocate(0); }").await;
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn an_array_init_value_is_shared_by_every_slot() {
        let code = r#"
            int create() {
                mixed *a = allocate(2, ({ 1 }));
                a[0][0] = 9;
                return a[1][0];
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(9)));
    }

    #[tokio::test]
    async fn a_negative_size_is_an_error() {
        let err = try_run_prog("mixed *create() { return allocate(-1); }")
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("allocate: negative size -1"), "{err}");
    }
}
