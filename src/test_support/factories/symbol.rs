use factori::factori;
use fake::Fake;
use lpc_rs_core::global_var_flags::GlobalVarFlags;
use lpc_rs_core::lpc_type::LpcType;
use crate::compiler::semantic::symbol::Symbol;

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
