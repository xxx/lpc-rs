use factori::factori;
use fake::Fake;
use lpc_rs_core::call_namespace::CallNamespace;

use crate::compiler::ast::call_node::CallNode;

factori!(CallNode, {
    default {
        receiver = None,
        arguments = vec![],
        name = format!("function-{}", (0..100000).fake::<usize>()),
        span = None,
        namespace = CallNamespace::default()
    }
});