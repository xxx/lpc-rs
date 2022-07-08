use lpc_rs_core::function_arity::FunctionArity;
use crate::{
    ast::function_def_node::FunctionDefNode,
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
    semantic::function_prototype::FunctionPrototype,
    Result,
};

/// A walker to collect all of the function definitions. This runs early on to allow for forward references.
#[derive(Debug, Default)]
pub struct FunctionPrototypeWalker {
    /// The compilation context
    context: CompilationContext,
}

impl FunctionPrototypeWalker {
    pub fn new(context: CompilationContext) -> Self {
        Self { context }
    }
}

impl ContextHolder for FunctionPrototypeWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for FunctionPrototypeWalker {
    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        // Store the prototype now, to allow for forward references.
        let num_args = node.parameters.len();
        let num_default_args = node.parameters.iter().filter(|p| p.value.is_some()).count();

        let arg_types = node
            .parameters
            .iter()
            .map(|parm| parm.type_)
            .collect::<Vec<_>>();
        self.context.function_prototypes.insert(
            node.name.clone(),
            FunctionPrototype {
                name: node.name.clone().into(),
                return_type: node.return_type,
                arity: FunctionArity {
                    num_args,
                    num_default_args,
                    ellipsis: node.flags.ellipsis(),
                    varargs: node.flags.varargs(),
                },
                arg_types,
                span: node.span,
                arg_spans: {
                    node.parameters
                        .iter()
                        .flat_map(|n| n.span)
                        .collect::<Vec<_>>()
                },
                flags: node.flags,
            },
        );

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_type::LpcType;
    use crate::{ast::var_init_node::VarInitNode};

    use super::*;
    use crate::semantic::function_flags::FunctionFlags;

    #[test]
    fn stores_the_prototype() {
        let mut walker = FunctionPrototypeWalker::default();
        let mut node = FunctionDefNode {
            return_type: LpcType::Mixed(false),
            name: "marf".to_string(),
            flags: FunctionFlags::default(),
            parameters: vec![
                VarInitNode::new("foo", LpcType::Int(false)),
                VarInitNode::new("bar", LpcType::Mapping(true)),
            ],
            body: vec![],
            span: None,
        };

        let _ = walker.visit_function_def(&mut node);

        let proto = walker
            .context
            .function_prototypes
            .get("marf")
            .expect("prototype not found!");

        assert_eq!(
            *proto,
            FunctionPrototype {
                name: "marf".into(),
                return_type: LpcType::Mixed(false),
                arity: FunctionArity {
                    num_args: 2,
                    num_default_args: 0,
                    ellipsis: false,
                    varargs: false
                },
                arg_types: vec![LpcType::Int(false), LpcType::Mapping(true)],
                span: None,
                arg_spans: vec![],
                flags: FunctionFlags::default(),
            }
        )
    }
}
