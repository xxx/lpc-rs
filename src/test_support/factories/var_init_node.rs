use factori::factori;
use fake::Fake;
use lpc_rs_core::lpc_type::LpcType;
use ustr::ustr;

use crate::compiler::ast::var_init_node::VarInitNode;

factori!(VarInitNode, {
    default {
        type_ = LpcType::Int(false),
        name = ustr(&format!("var-{}", (0..100000).fake::<usize>())),
        value = None,
        array = false,
        global = false,
        span = None,
        flags = None,
    }
});
