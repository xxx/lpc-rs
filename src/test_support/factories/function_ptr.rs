use crate::interpreter::function_type::function_ptr::FunctionPtr;
use qcell::QCellOwner;
use std::rc::Rc;
use factori::factori;
use crate::interpreter::process::Process;
use crate::interpreter::function_type::function_address::FunctionAddress;
use lpc_rs_core::function_arity::FunctionArity;
use crate::interpreter::gc::unique_id::UniqueId;


factori!(FunctionPtr, {
    default {
        owner = Rc::downgrade(&Rc::new(QCellOwner::new().cell(Process::default()))),
        address = FunctionAddress::Efun("dump".to_string()),
        arity = FunctionArity::default(),
        partial_args = vec![],
        call_other = false,
        upvalue_ptrs = vec![],
        unique_id = UniqueId::new(),
    }
});
