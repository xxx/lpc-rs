use crate::compiler::semantic::symbol::Symbol;
use factori::factori;
use fake::Fake;
use lpc_rs_core::{global_var_flags::GlobalVarFlags, lpc_type::LpcType};

factori!(Symbol, {
    default {
        name = format!("sym_{}", (0..100000).fake::<usize>()),
        type_ = LpcType::Int(false),
        location = None,
        scope_id = 0,
        span = None,
        flags = GlobalVarFlags::default()
    }
});
