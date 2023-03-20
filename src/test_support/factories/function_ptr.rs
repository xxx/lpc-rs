use std::rc::Rc;

use factori::factori;
use lpc_rs_core::function_arity::FunctionArity;
use qcell::QCellOwner;

use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    gc::unique_id::UniqueId,
    process::Process,
};

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
