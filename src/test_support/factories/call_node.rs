use factori::factori;
use fake::Fake;
use lpc_rs_core::call_namespace::CallNamespace;
use ustr::ustr;

use crate::compiler::ast::call_node::{CallChain, CallNode};

factori!(CallNode, {
    default {
        chain = CallChain::Root {
            receiver: None,
            namespace: CallNamespace::default(),
            name: ustr(&format!("function-{}", (0..100000).fake::<usize>())),
        },
        arguments = vec![],
        span = None,
    }
});
