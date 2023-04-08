use lpc_rs_errors::Result;
use qcell::QCellOwner;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_value::LpcValue};
use crate::interpreter::lpc_ref::LpcRef;

/// `papplyv`, an efun to partially apply a function to arguments taken from an array
pub fn papplyv<const N: usize>(
    context: &mut EfunContext<N>,
    _cell_key: &mut QCellOwner,
) -> Result<()> {
    let LpcRef::Function(f) = context.resolve_local_register(1_usize) else {
        return Err(context.runtime_error("non-function argument sent to `papplyv`"));
    };
    let LpcValue::Function(func) = &*f.borrow() else {
        return Err(context.runtime_error("non-function argument sent to `papplyv`"));
    };

    let LpcRef::Array(a) = context.resolve_local_register(2_usize) else {
        return Err(context.runtime_error("non-array argument sent to `papplyv`"));
    };
    let LpcValue::Array(arr) = &*a.borrow() else {
        return Err(context.runtime_error("non-array argument sent to `papplyv`"));
    };

    // TODO: this clone is unnecessarily heavy
    let mut ptr = func.clone_with_new_id();
    ptr.partially_apply(&arr);

    let v = LpcValue::Function(ptr);
    let result = context.value_to_ref(v);

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use lpc_rs_utils::config::Config;
    use crate::interpreter::call_outs::CallOuts;
    use crate::interpreter::gc::gc_bank::GcBank;
    use crate::interpreter::memory::Memory;
    use crate::interpreter::object_space::ObjectSpace;
    use crate::interpreter::task::Task;
    use crate::test_support::compile_prog;
    use super::*;

    #[test]
    fn test_papplyv() {
        let mut cell_key = QCellOwner::new();

        let code = r##"
            function create() {
                return papplyv(dump, ({ "foo", "bar" }));
            }
        "##;

        let (tx, _) = std::sync::mpsc::channel();
        let (program, _, _) = compile_prog(code, &mut cell_key);
        let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
        let result = Task::<5>::initialize_program(
            program,
            Config::default(),
            cell_key.cell(ObjectSpace::default()),
            Memory::default(),
            cell_key.cell(GcBank::default()),
            call_outs,
            tx,
            &mut cell_key,
        );

        let b = result.unwrap();
        let r = b.result().unwrap();

        let LpcRef::Function(f) = r else {
            panic!("expected function ref");
        };

        let LpcValue::Function(func) = &*f.borrow() else {
            panic!("expected function value");
        };

        assert_eq!(func.name(), "dump");

        assert_eq!(
            func.partial_args.iter().map(|a| a.as_ref().unwrap().to_string()).collect::<Vec<_>>(),
            vec!["\"foo\"", "\"bar\""].iter().map(ToString::to_string).collect::<Vec<_>>()
        );
    }
}