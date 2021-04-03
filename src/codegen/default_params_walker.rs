use crate::{
    ast::function_def_node::FunctionDefNode, codegen::tree_walker::TreeWalker,
};
use crate::{codegen::tree_walker::ContextHolder, context::Context};
use crate::errors::LpcError;

/// A walker to collect function argument lists, so codegen can access them for default arguments.
#[derive(Debug, Default)]
pub struct DefaultParamsWalker {
    /// The compilation context
    context: Context,
}

impl DefaultParamsWalker {
    pub fn new(context: Context) -> Self {
        Self { context }
    }
}

impl ContextHolder for DefaultParamsWalker {
    fn into_context(self) -> Context {
        self.context
    }
}

impl TreeWalker for DefaultParamsWalker {
    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), LpcError> {
        let vec = node
            .parameters
            .iter()
            .map(|p| p.value.clone())
            .collect::<Vec<_>>();
        self.context.function_params.insert(node.name.clone(), vec);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{expression_node::ExpressionNode, var_init_node::VarInitNode},
        semantic::lpc_type::LpcType,
    };

    use super::*;

    #[test]
    fn test_visit_function_def_populates_the_functions() {
        let context = Context::default();
        let mut walker = DefaultParamsWalker::new(context);

        let parameters = vec![
            VarInitNode {
                type_: LpcType::Int(false),
                name: "i".to_string(),
                value: None,
                array: false,
                global: false,
                span: None,
            },
            VarInitNode {
                type_: LpcType::String(false),
                name: "s".to_string(),
                value: Some(ExpressionNode::from("marf")),
                array: false,
                global: false,
                span: None,
            },
        ];

        let mut node = FunctionDefNode {
            return_type: LpcType::Void,
            name: "foo".to_string(),
            parameters,
            body: vec![],
            span: None,
        };

        let _ = walker.visit_function_def(&mut node);

        let params = walker.context.function_params.get("foo").unwrap();

        let expected = vec![None, Some(ExpressionNode::from("marf"))];

        for (idx, param) in params.into_iter().enumerate() {
            assert_eq!(*param, expected[idx]);
        }
    }
}
