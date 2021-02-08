use std::collections::HashMap;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::function_def_node::FunctionDefNode;
use crate::errors::compiler_error::CompilerError;
use crate::ast::expression_node::ExpressionNode;

/// A walker to collect function argument lists, so codegen can access them for default arguments.
#[derive(Debug)]
pub struct DefaultParamsWalker {
    functions: HashMap<String, Vec<Option<ExpressionNode>>>
}

impl DefaultParamsWalker {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new()
        }
    }

    /// Consume this struct and return the functions
    pub fn into_functions(self) -> HashMap<String, Vec<Option<ExpressionNode>>> {
        self.functions
    }
}

impl TreeWalker for DefaultParamsWalker {
    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), CompilerError> {
        let vec = node.parameters.iter().map(|p| p.value.clone()).collect::<Vec<_>>();
        self.functions.insert(node.name.clone(), vec);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::lpc_type::LPCType;
    use crate::ast::var_init_node::VarInitNode;

    #[test]
    fn test_visit_function_def_populates_the_functions() {
        let mut walker = DefaultParamsWalker::new();

        let parameters = vec![
            VarInitNode {
                type_: LPCType::Int(false),
                name: "i".to_string(),
                value: None,
                array: false,
                span: None
            },
            VarInitNode {
                type_: LPCType::String(false),
                name: "s".to_string(),
                value: Some(ExpressionNode::from("marf")),
                array: false,
                span: None
            },
        ];

        let node = FunctionDefNode {
            return_type: LPCType::Void,
            name: "foo".to_string(),
            parameters,
            body: vec![],
            span: None
        };

        let _ = walker.visit_function_def(&node);

        let params = walker.functions.get("foo").unwrap();

        let expected = vec![
            None,
            Some(ExpressionNode::from("marf"))
        ];

        for (idx, param) in params.into_iter().enumerate() {
            assert_eq!(*param, expected[idx]);
        }
    }
}