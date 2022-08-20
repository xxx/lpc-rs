use factori::factori;
use fake::Fake;
use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};

use crate::compiler::ast::closure_node::ClosureNode;

factori!(ClosureNode, {
    default {
        name = format!("closure-{}", (0..100000).fake::<usize>()),
        return_type = LpcType::Mixed(false),
        parameters = None,
        flags = FunctionFlags::default(),
        body = vec![],
        span = None,
        scope_id = None
    }
});
