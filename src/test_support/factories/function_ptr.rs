use std::sync::{Arc, Weak};

use factori::factori;
use lpc_rs_core::register::Register;
use thin_vec::{ThinVec, thin_vec};
use ustr::ustr;

use crate::interpreter::{
    function_type::{
        function_address::FunctionAddress,
        function_ptr::{FunctionPtr, FunctionPtrBuilder},
    },
    gc::unique_id::UniqueId,
    lpc_ref::LpcRef,
    process::Process,
};

factori!(FunctionPtr, {
    default {
        owner: Weak<Process> = Arc::downgrade(&Arc::new(Process::default())),
        address: FunctionAddress = FunctionAddress::Efun(ustr("dump")),
        partial_args: ThinVec<Option<LpcRef>> = thin_vec![],
        upvalue_ptrs: ThinVec<Register> = thin_vec![],
        unique_id: UniqueId = UniqueId::new(),
    }

    builder {
        FunctionPtrBuilder::default()
            .owner(owner)
            .address(address)
            .partial_args(partial_args)
            .upvalue_ptrs(upvalue_ptrs)
            .unique_id(unique_id)
            .build()
            .unwrap()
    }
});
