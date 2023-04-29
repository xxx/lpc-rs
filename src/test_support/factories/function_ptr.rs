use std::sync::Arc;

use factori::factori;
use ustr::ustr;
use thin_vec::thin_vec;

use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    gc::unique_id::UniqueId,
    process::Process,
};

factori!(FunctionPtr, {
    default {
        owner = Arc::downgrade(&Arc::new(Process::default())),
        address = FunctionAddress::Efun(ustr("dump")),
        partial_args = thin_vec![],
        call_other = false,
        upvalue_ptrs = thin_vec![],
        unique_id = UniqueId::new(),
    }
});
