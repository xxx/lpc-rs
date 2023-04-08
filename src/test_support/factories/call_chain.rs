use factori::factori;
use fake::Fake;
use lpc_rs_core::call_namespace::CallNamespace;
use ustr::{ustr, Ustr};

use crate::compiler::ast::{
    call_node::{CallChain, CallNode},
    expression_node::ExpressionNode,
};

factori!(CallChain, {
    default {
        node: Option<CallNode> = None,
        receiver: Option<Box<ExpressionNode>> = None,
        namespace: CallNamespace = CallNamespace::default(),
        name: Ustr = ustr(&format!("function-{}", (0..100000).fake::<usize>())),
    }

    builder {
        if node.is_some() {
            CallChain::Node(Box::new(node.unwrap()))
        } else {
            CallChain::Root { receiver, namespace, name }
        }
    }
});
