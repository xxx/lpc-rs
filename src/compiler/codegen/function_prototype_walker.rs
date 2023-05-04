use async_trait::async_trait;
use lpc_rs_core::{call_namespace::CallNamespace, function_arity::FunctionArity, RegisterSize};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::function_prototype::{FunctionKind, FunctionPrototypeBuilder};
use lpc_rs_utils::string::closure_arg_number;

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
    max_closure_arg_reference: RegisterSize,
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

        self.context.filename.ends_with(simul_efun_file.as_str())
    }
}

impl ContextHolder for FunctionPrototypeWalker {
    #[inline]
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

#[async_trait]
impl TreeWalker for FunctionPrototypeWalker {
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        let num_args = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.len())
            .unwrap_or(0);
        let mut num_args = RegisterSize::try_from(num_args)?;

        let num_default_args = node
            .parameters
            .as_ref()
            .map(|nodes| nodes.iter().filter(|p| p.value.is_some()).count())
            .unwrap_or(0);
        let num_default_args = RegisterSize::try_from(num_default_args)?;

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
                param.visit(self).await?;
            }
        }

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        if self.max_closure_arg_reference > num_args {
            num_args = self.max_closure_arg_reference;
        }

        self.context.function_prototypes.insert(
            node.name.to_owned(),
            FunctionPrototypeBuilder::default()
                .name(node.name.to_owned())
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

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let proto_opt = self
            .context
            .lookup_function_complete(node.name, &CallNamespace::default());

        if let Some(function_like) = proto_opt {
            let prototype = function_like.as_ref();
            // If we find another prototype with this name, it's not ours.
            if prototype.flags.nomask() {
                let e = LpcError::new(format!(
                    "attempt to redefine nomask function `{}`",
                    node.name
                ))
                .with_span(node.span)
                .with_label("defined here", prototype.span)
                .into();

                return Err(e);
            }
        }

        // Store the prototype now, to allow for forward references.
        let num_args = RegisterSize::try_from(node.parameters.len())?;
        let num_default_args =
            RegisterSize::try_from(node.parameters.iter().filter(|p| p.value.is_some()).count())?;

        let kind = if self.is_simul_efuns() {
            FunctionKind::SimulEfun
        } else {
            FunctionKind::Local
        };

        let arg_types = node
            .parameters
            .iter()
            .map(|parm| parm.type_)
            .collect::<Vec<_>>();
        self.context.function_prototypes.insert(
            node.name.to_owned(),
            FunctionPrototypeBuilder::default()
                .name(node.name.to_owned())
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
            parameter.visit(self).await?;
        }

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        Ok(())
    }

    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        if node.is_closure_arg_var() {
            let idx = closure_arg_number(node.name)?;

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
    use lpc_rs_errors::span::Span;
    use lpc_rs_function_support::program_function::ProgramFunction;
    use ustr::ustr;

    use super::*;
    use crate::{
        assert_regex,
        compiler::ast::{
            ast_node::AstNode, expression_node::ExpressionNode, var_init_node::VarInitNode,
        },
        interpreter::program::Program,
        test_support::empty_compilation_context,
    };

    #[tokio::test]
    async fn disallows_redefining_nomask_function() {
        let mut node = FunctionDefNode {
            return_type: LpcType::Void,
            name: ustr("duplicate"),
            parameters: vec![],
            flags: FunctionFlags::default(),
            body: vec![],
            span: None,
        };

        let mut context = empty_compilation_context();
        let mut program = Program::default();

        let prototype = FunctionPrototypeBuilder::default()
            .name("duplicate")
            .filename(Arc::new("duplicate".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(4))
            .flags(FunctionFlags::default().with_nomask(true))
            // If the span of the def is the same as the span of the prototype, that's _our_ prototype,
            // and we *are* allowed to define it, if it's a nomask function.
            // So we artificially set a different span here for this test.
            .span(Some(Span::new(3, 1..3)))
            .build()
            .unwrap();

        let func = ProgramFunction::new(prototype, 0);

        program
            .functions
            .insert(String::from("duplicate"), func.into());

        context.inherits.push(program);

        let mut walker = FunctionPrototypeWalker::new(context);
        let result = walker.visit_function_def(&mut node).await;

        if let Err(e) = result {
            assert_regex!(
                (*e).as_ref(),
                "attempt to redefine nomask function `duplicate`"
            );
        } else {
            panic!("didn't error?")
        }
    }

    #[tokio::test]
    async fn function_def_stores_the_prototype() {
        let mut walker = FunctionPrototypeWalker::default();
        let mut node = FunctionDefNode {
            return_type: LpcType::Mixed(false),
            name: ustr("marf"),
            flags: FunctionFlags::default(),
            parameters: vec![
                VarInitNode::new("foo", LpcType::Int(false)),
                VarInitNode::new("bar", LpcType::Mapping(true)),
            ],
            body: vec![],
            span: None,
        };

        let _ = walker.visit_function_def(&mut node).await;

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

    #[tokio::test]
    async fn closure_stores_the_prototype() {
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

        let _ = walker.visit_closure(&mut node).await;

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
