use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    stm::TxnHandle,
};

/// The item count of an array or mapping, the character length of a string,
/// the byte count of a `bytes`, `0` for anything else.
pub(crate) fn size_of(lpc_ref: &LpcRef, txn: &TxnHandle) -> Result<LpcRef> {
    Ok(match lpc_ref {
        LpcRef::Array(_) => {
            let l = lpc_ref.with_array(txn, |a| a.len())?;
            LpcRef::Int(LpcInt(l as LpcIntInner))
        }
        LpcRef::Mapping(_) => {
            lpc_ref.drop_dead_keys(txn)?;
            let l = lpc_ref.with_mapping(txn, |m| m.len())?;
            LpcRef::Int(LpcInt(l as LpcIntInner))
        }
        LpcRef::String(_) => {
            let l = lpc_ref.with_string(|s| s.char_count())?;
            LpcRef::Int(LpcInt(l as LpcIntInner))
        }
        LpcRef::Bytes(b) => LpcRef::Int(LpcInt(b.len() as LpcIntInner)),
        LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::Object(_) | LpcRef::Function(_) => NULL,
    })
}

/// `sizeof` fired through a pointer; a direct call compiles to its opcode.
pub fn sizeof<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = size_of(context.arg(0), context.txn())?;
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::lpc_ref::{LpcRef, test_bytes},
        test_support::run_prog,
    };

    #[test]
    fn a_bytes_is_measured_in_bytes() {
        let sized = size_of(&test_bytes(b"abc"), &TxnHandle::empty()).unwrap();
        assert_eq!(sized, LpcRef::from(3));
    }

    #[tokio::test]
    async fn a_sizeof_pointer_measures_like_the_operator() {
        let code = r#"
            int create() {
                function f = &sizeof();
                return f(({ 1, 2, 3 })) * 100 + f(([ 1: 2 ])) * 10 + f("ab");
            }
        "#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(312)));
    }

    #[tokio::test]
    async fn a_string_is_measured_in_characters() {
        let code = r#"int create() { return sizeof("héllo") * 10 + sizeof(""); }"#;
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(50)));
    }
}
