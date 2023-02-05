use lpc_rs_core::{global_var_flags::GlobalVarFlags, lpc_type::LpcType};
use lpc_rs_function_support::symbol::Symbol;

beaver::define! {
    pub SymbolFactory (Symbol) {
        name -> |n| format!("sym_{n}"),
        type_ -> |_| LpcType::Int(false),
        location -> |_| None,
        scope_id -> |_| None,
        span -> |_| None,
        flags -> |_| GlobalVarFlags::default(),
        upvalue -> |_| false,
    }
}
