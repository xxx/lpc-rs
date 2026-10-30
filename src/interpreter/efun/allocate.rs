use std::alloc::Layout;

use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

fn allocation_len(size: LpcIntInner) -> Option<usize> {
    let length = usize::try_from(size).ok()?;
    let elements = Layout::array::<LpcRef>(length).ok()?;
    // ThinVec stores two header words before its elements, as memory accounting records.
    let _ = Layout::new::<[usize; 2]>().extend(elements).ok()?;
    Some(length)
}

/// `allocate(n [, init])`: an array of `n` copies of `init` (the selected
/// null value when absent). An array or mapping `init` is shared by every slot.
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
    let length = allocation_len(n)
        .ok_or_else(|| context.runtime_error(format!("allocate: size {n} is too large")))?;
    let init = context.arg(1).clone();
    context.return_array(std::iter::repeat_n(init, length));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::lpc_ref::LpcRef,
        test_support::{run_prog, strings_of, try_run_prog},
    };

    #[test]
    fn size_validation_preserves_platform_width_without_allocating() {
        assert_eq!(super::allocation_len(-1), None);
        assert_eq!(super::allocation_len(0), Some(0));
        assert_eq!(super::allocation_len(3), Some(3));
        assert_eq!(super::allocation_len(i64::MAX), None);
        #[cfg(target_pointer_width = "32")]
        assert_eq!(super::allocation_len(1_i64 << 32), None);
        #[cfg(target_pointer_width = "64")]
        assert_eq!(super::allocation_len(1_i64 << 32), Some(1_usize << 32));
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

    #[tokio::test]
    async fn an_unrepresentable_size_is_a_runtime_error() {
        let err = try_run_prog("mixed *create() { return allocate(9223372036854775807); }")
            .await
            .unwrap_err();
        assert_eq!(err.severity(), lpc_rs_errors::LpcErrorSeverity::Error);
        let err = err.to_string();
        assert!(
            err.contains("allocate: size 9223372036854775807 is too large"),
            "{err}"
        );
    }
}
