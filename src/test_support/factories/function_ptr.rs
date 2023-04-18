use std::sync::Arc;

use factori::factori;
use ustr::ustr;

use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    gc::unique_id::UniqueId,
    process::Process,
};

factori!(FunctionPtr, {
    default {
        owner = Arc::downgrade(&Arc::new(Process::default())),
        address = FunctionAddress::Efun(ustr("dump")),
        partial_args = vec![],
        call_other = false,
        upvalue_ptrs = vec![],
        unique_id = UniqueId::new(),
    }
});
