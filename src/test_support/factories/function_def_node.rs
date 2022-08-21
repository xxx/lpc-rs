use factori::factori;
use fake::Fake;
use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};

use crate::compiler::ast::function_def_node::FunctionDefNode;

factori!(FunctionDefNode, {
    default {
        name = format!("function-{}", (0..100000).fake::<usize>()),
        return_type = LpcType::Mixed(false),
        parameters = vec![],
        flags = FunctionFlags::default(),
        body = vec![],
        span = None,
    }
});
