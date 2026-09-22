//! `typeof`: the tag of a value's type; `doc/efun/typeof.md` lists the
//! tags a mudlib header defines.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// The tag of an int; also used for destructed objects in zero mode.
pub const T_INT: i64 = 1;
/// The tag of a float.
pub const T_FLOAT: i64 = 2;
/// The tag of a string.
pub const T_STRING: i64 = 3;
/// The tag of a live object.
pub const T_OBJECT: i64 = 4;
/// The tag of an array.
pub const T_ARRAY: i64 = 5;
/// The tag of a mapping.
pub const T_MAPPING: i64 = 6;
/// The tag of a function pointer.
pub const T_FUNCTION: i64 = 7;
/// The tag of a `bytes`.
pub const T_BYTES: i64 = 8;
/// The tag of a class instance.
pub const T_CLASS: i64 = 9;
/// The tag of a distinct nil value.
pub const T_NIL: i64 = 10;

/// `typeof(x)`: the tag of `x`'s type.
pub fn r#typeof<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let tag = match context.arg(0) {
        LpcRef::Nil => T_NIL,
        LpcRef::Int(_) => T_INT,
        LpcRef::Float(_) => T_FLOAT,
        LpcRef::String(_) => T_STRING,
        object @ LpcRef::Object(_) => match object.live_object(context.txn()) {
            Some(_) => T_OBJECT,
            None => {
                if context.txn().null_mode() == NullMode::Distinct {
                    T_NIL
                } else {
                    T_INT
                }
            }
        },
        LpcRef::Array(_) => T_ARRAY,
        LpcRef::Mapping(_) => T_MAPPING,
        LpcRef::Function(_) => T_FUNCTION,
        LpcRef::Bytes(_) => T_BYTES,
        LpcRef::Class(_) => T_CLASS,
    };
    context.return_efun_result(LpcRef::from(tag));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};

    const TAGS: [i64; 7] = [
        T_INT, T_FLOAT, T_STRING, T_OBJECT, T_ARRAY, T_MAPPING, T_FUNCTION,
    ];

    #[tokio::test]
    async fn each_variant_has_its_own_tag() {
        let code = indoc! { r#"
            int *create() {
                return ({
                    typeof(1), typeof(1.5), typeof("s"), typeof(this_object()),
                    typeof(({ })), typeof(([ ])), typeof((: 1 :)),
                });
            }
        "# };
        let task = run_prog(code).await;
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                assert_eq!(arr, TAGS.as_slice());
            })
            .unwrap();
    }

    #[tokio::test]
    async fn a_destructed_object_is_an_int() {
        let code = r#"
            int create() {
                object o = clone_object("/clone_target");
                destruct(o);
                return typeof(o);
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(T_INT)));
    }
}
