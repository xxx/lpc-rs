use factori::factori;
use fake::Fake;

use crate::compiler::ast::var_node::VarNode;

factori!(VarNode, {
    default {
        name = format!("function-{}", (0..100000).fake::<usize>()),
        span = None,
        global = false,
        function_name = false,
    }
});
