use std::sync::{Arc, Weak};

use factori::factori;
use thin_vec::{ThinVec, thin_vec};
use ustr::ustr;

use crate::interpreter::stm::VarId;
use crate::interpreter::{
    function_type::{
        function_address::FunctionAddress,
        function_ptr::{FunctionPtr, FunctionPtrBuilder},
    },
    lpc_ref::LpcRef,
    process::Process,
};

factori!(FunctionPtr, {
    default {
        owner: Weak<Process> = Arc::downgrade(&Arc::new(Process::default())),
        address: FunctionAddress = FunctionAddress::Efun(ustr("dump")),
        partial_args: ThinVec<Option<LpcRef>> = thin_vec![],
        upvalue_ptrs: ThinVec<VarId> = thin_vec![],
    }

    builder {
        FunctionPtrBuilder::default()
            .owner(owner)
            .address(address)
            .partial_args(partial_args)
            .upvalue_ptrs(upvalue_ptrs)
            .build()
            .unwrap()
    }
});
