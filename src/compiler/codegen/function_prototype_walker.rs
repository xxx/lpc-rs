use lpc_rs_core::function_arity::FunctionArity;
use lpc_rs_errors::Result;
use lpc_rs_function_support::function_prototype::{FunctionKind, FunctionPrototypeBuilder};
use lpc_rs_utils::string::closure_arg_number;
use qcell::QCellOwner;

use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait, closure_node::ClosureNode, function_def_node::FunctionDefNode,
        var_node::VarNode,
    },
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
};

/// A walker to collect all of the function definitions. This runs early on to
/// allow for forward references.
#[derive(Debug, Default)]
pub struct FunctionPrototypeWalker {
    /// The compilation context
    context: CompilationContext,

    /// Track the max number used in any `$\d` vars within closures
    max_closure_arg_reference: usize,
}

impl FunctionPrototypeWalker {
    /// Create a new instance
    #[inline]
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            max_closure_arg_reference: 0,
        }
    }

    /// Am I walking the simul efuns? Used in codegen.
    #[inline]
    fn is_simul_efuns(&self) -> bool {
        let Some(simul_efun_file) = &self.context.config.simul_efun_file else {
            return false;
        };

        self.context.filename.ends_with(&simul_efun_file)
    }
}

impl ContextHolder for FunctionPrototypeWalker {
    #[inline]
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for FunctionPrototypeWalker {
    fn visit_closure(&mut self, node: &mut ClosureNode, cell_key: &mut QCellOwner) -> Result<()> {
        let mut num_args = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.len())
            .unwrap_or(0);
        let num_default_args = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.iter().filter(|p| p.value.is_some()).count())
            .unwrap_or(0);

        let arg_types = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.iter().map(|parm| parm.type_).collect::<Vec<_>>())
            .unwrap_or_else(Vec::new);

        let arg_spans = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.iter().flat_map(|n| n.span).collect::<Vec<_>>())
            .unwrap_or_else(Vec::new);

        // look for cases of closures-within-closures
        if let Some(parameters) = &mut node.parameters {
            for param in parameters {
                param.visit(self, cell_key)?;
            }
        }

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }

        if self.max_closure_arg_reference > num_args {
            num_args = self.max_closure_arg_reference;
        }

        self.context.function_prototypes.insert(
            node.name.clone(),
            FunctionPrototypeBuilder::default()
                .name(node.name.clone())
                .filename(self.context.filename.clone())
                .return_type(node.return_type)
                .arity(FunctionArity {
                    num_args,
                    num_default_args,
                    ellipsis: node.flags.ellipsis(),
                    varargs: node.flags.varargs(),
                })
                .arg_types(arg_types)
                .span(node.span)
                .arg_spans(arg_spans)
                .flags(node.flags)
                .build()
                .expect("Failed to build function prototype"),
        );

        Ok(())
    }

    fn visit_function_def(
        &mut self,
        node: &mut FunctionDefNode,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        // Store the prototype now, to allow for forward references.
        let num_args = node.parameters.len();
        let num_default_args = node.parameters.iter().filter(|p| p.value.is_some()).count();

        let kind = if self.is_simul_efuns() { FunctionKind::SimulEfun } else { FunctionKind::Local };

        let arg_types = node
            .parameters
            .iter()
            .map(|parm| parm.type_)
            .collect::<Vec<_>>();
        self.context.function_prototypes.insert(
            node.name.clone(),
            FunctionPrototypeBuilder::default()
                .name(node.name.clone())
                .filename(self.context.filename.clone())
                .return_type(node.return_type)
                .kind(kind)
                .arity(FunctionArity {
                    num_args,
                    num_default_args,
                    ellipsis: node.flags.ellipsis(),
                    varargs: node.flags.varargs(),
                })
                .arg_types(arg_types)
                .span(node.span)
                .arg_spans({
                    node.parameters
                        .iter()
                        .flat_map(|n| n.span)
                        .collect::<Vec<_>>()
                })
                .flags(node.flags)
                .build()
                .expect("Failed to build function prototype"),
        );

        // walk the contents of the function, in case any closures are defined.
        for parameter in &mut node.parameters {
            parameter.visit(self, cell_key)?;
        }

        for expression in &mut node.body {
            expression.visit(self, cell_key)?;
        }

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode, _cell_key: &mut QCellOwner) -> Result<()> {
        if node.is_closure_arg_var() {
            let idx = closure_arg_number(&node.name)?;

            if idx > self.max_closure_arg_reference {
                self.max_closure_arg_reference = idx;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::{function_flags::FunctionFlags, lpc_path::LpcPath, lpc_type::LpcType};

    use super::*;
    use crate::compiler::ast::{
        ast_node::AstNode, expression_node::ExpressionNode, var_init_node::VarInitNode,
    };

    #[test]
    fn function_def_stores_the_prototype() {
        let mut cell_key = QCellOwner::new();
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

        let _ = walker.visit_function_def(&mut node, &mut cell_key);

        let proto = walker
            .context
            .function_prototypes
            .get("marf")
            .expect("prototype not found!");

        assert_eq!(
            *proto,
            FunctionPrototypeBuilder::default()
                .name("marf")
                .filename(Arc::new(LpcPath::new_server("/")))
                .return_type(LpcType::Mixed(false))
                .arity(FunctionArity::new(2))
                .arg_types(vec![LpcType::Int(false), LpcType::Mapping(true)])
                .build()
                .expect("Failed to build function prototype"),
        )
    }

    #[test]
    fn closure_stores_the_prototype() {
        let mut cell_key = QCellOwner::new();
        let mut walker = FunctionPrototypeWalker::default();
        let mut node = ClosureNode {
            name: "closure-123".into(),
            return_type: LpcType::Mixed(false),
            flags: FunctionFlags::default(),
            parameters: Some(vec![
                VarInitNode::new("foo", LpcType::Int(false)),
                VarInitNode::new("bar", LpcType::Mapping(true)),
            ]),
            body: vec![AstNode::Expression(ExpressionNode::Var(VarNode::new("$4")))],
            span: None,
            scope_id: None,
        };

        let _ = walker.visit_closure(&mut node, &mut cell_key);

        let proto = walker
            .context
            .function_prototypes
            .get("closure-123")
            .expect("prototype not found!");

        assert_eq!(
            *proto,
            FunctionPrototypeBuilder::default()
                .name("closure-123")
                .filename(Arc::new(LpcPath::new_server("/")))
                .return_type(LpcType::Mixed(false))
                .arity(FunctionArity::new(4))
                .arg_types(vec![LpcType::Int(false), LpcType::Mapping(true)])
                .build()
                .expect("Failed to build function prototype"),
        )
    }
}
