use async_trait::async_trait;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::{
        closure_node::ClosureNode, function_def_node::FunctionDefNode, var_init_node::VarInitNode,
    },
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
};

/// A walker to collect function argument lists, so codegen can access them for
/// default arguments.
#[derive(Debug, Default)]
pub struct DefaultParamsWalker {
    /// The compilation context
    context: CompilationContext,
}

impl DefaultParamsWalker {
    pub fn new(context: CompilationContext) -> Self {
        Self { context }
    }

    fn insert_params<T>(&mut self, name: T, parameters: &[VarInitNode])
    where
        T: Into<String>,
    {
        let vec = parameters
            .iter()
            .map(|p| p.value.clone())
            .collect::<Vec<_>>();
        self.context
            .default_function_params
            .insert(name.into(), vec);
    }
}

impl ContextHolder for DefaultParamsWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

#[async_trait]
impl TreeWalker for DefaultParamsWalker {
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        if let Some(parameters) = &node.parameters {
            self.insert_params(&*node.name, parameters);
        }

        Ok(())
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        self.insert_params(&*node.name, &node.parameters);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use factori::create;
    use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};
    use ustr::ustr;

    use super::*;
    use crate::{
        compiler::ast::{expression_node::ExpressionNode, var_init_node::VarInitNode},
        test_support::factories::*,
    };

    #[tokio::test]
    async fn test_visit_closure_populates_the_functions() {
        let context = CompilationContext::default();
        let mut walker = DefaultParamsWalker::new(context);

        let parameters = vec![
            create!(
                VarInitNode,
                type_: LpcType::Int(false),
                name: ustr("i"),
            ),
            create!(
                VarInitNode,
                type_: LpcType::String(false),
                name: ustr("s"),
                value: Some(ExpressionNode::from("marf")),
            ),
        ];

        let mut node = create!(
            ClosureNode,
            name: ustr("foo"),
            parameters: Some(parameters),
        );

        let _ = walker.visit_closure(&mut node).await;

        let params = walker.context.default_function_params.get("foo").unwrap();

        let expected = vec![None, Some(ExpressionNode::from("marf"))];

        for (idx, param) in params.iter().enumerate() {
            assert_eq!(*param, expected[idx]);
        }
    }

    #[tokio::test]
    async fn test_visit_function_def_populates_the_functions() {
        let context = CompilationContext::default();
        let mut walker = DefaultParamsWalker::new(context);

        let parameters = vec![
            VarInitNode {
                type_: LpcType::Int(false),
                name: ustr("i"),
                value: None,
                array: false,
                global: false,
                span: None,
                flags: None,
            },
            VarInitNode {
                type_: LpcType::String(false),
                name: ustr("s"),
                value: Some(ExpressionNode::from("marf")),
                array: false,
                global: false,
                span: None,
                flags: None,
            },
        ];

        let mut node = FunctionDefNode {
            return_type: LpcType::Void,
            name: ustr("foo"),
            parameters,
            flags: FunctionFlags::default(),
            body: vec![],
            span: None,
        };

        let _ = walker.visit_function_def(&mut node).await;

        let params = walker.context.default_function_params.get("foo").unwrap();

        let expected = vec![None, Some(ExpressionNode::from("marf"))];

        for (idx, param) in params.iter().enumerate() {
            assert_eq!(*param, expected[idx]);
        }
    }
}
