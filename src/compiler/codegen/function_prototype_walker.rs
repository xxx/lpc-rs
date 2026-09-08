use async_trait::async_trait;
use lpc_rs_core::{
    RegisterSize, call_namespace::CallNamespace, function_arity::FunctionArity, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result, lpc_error};
use lpc_rs_function_support::function_prototype::{FunctionKind, FunctionPrototypeBuilder};
use lpc_rs_utils::string::closure_arg_number;

use crate::compiler::{
    ast::{
        closure_node::ClosureNode, function_def_node::FunctionDefNode, var_init_node::VarInitNode,
        var_node::VarNode,
    },
    callee::Callee,
    codegen::tree_walker::{ContextHolder, Pass, TreeWalker, walk_closure, walk_function_def},
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
};

/// A walker to collect all of the function definitions. This runs early on to
/// allow for forward references.
#[derive(Debug, Default)]
pub struct FunctionPrototypeWalker {
    /// The compilation context
    context: CompilationContext,

    /// The highest `$N` referenced in the closure being walked.
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
}

impl ContextHolder for FunctionPrototypeWalker {
    #[inline]
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for FunctionPrototypeWalker {
    fn new(context: CompilationContext) -> Self {
        FunctionPrototypeWalker::new(context)
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for FunctionPrototypeWalker {
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        // `$N` names a position in the innermost closure only.
        let enclosing_max = std::mem::replace(&mut self.max_closure_arg_reference, 0);

        walk_closure(self, node).await?;

        let highest_positional =
            std::mem::replace(&mut self.max_closure_arg_reference, enclosing_max);

        let parameters = node.parameters.get_or_insert_default();
        let declared = RegisterSize::try_from(parameters.len())?;
        if highest_positional > declared && parameters.iter().any(|p| p.value.is_some()) {
            self.context.diagnostics.record(lpc_error!(
                node.span,
                "positional `${}` lies beyond a defaulted parameter",
                highest_positional
            ));
        }
        for position in (declared + 1)..=highest_positional {
            let mut positional = VarInitNode::new(&format!("${position}"), LpcType::Mixed(false));
            positional.span = node.span;
            parameters.push(positional);
        }

        let num_args = RegisterSize::try_from(parameters.len())?;
        let num_default_args =
            RegisterSize::try_from(parameters.iter().filter(|p| p.value.is_some()).count())?;
        let kind = FunctionKind::Closure;
        let arg_types = parameters.iter().map(|parm| parm.type_).collect::<Vec<_>>();
        let arg_spans = parameters.iter().flat_map(|n| n.span).collect::<Vec<_>>();

        self.context.function_prototypes.insert(
            node.name.to_owned(),
            FunctionPrototypeBuilder::default()
                .name(node.name.to_owned())
                .filename(self.context.source.input().clone())
                .return_type(node.return_type)
                .kind(kind)
                .arity(FunctionArity {
                    num_args,
                    num_default_args,
                })
                .arg_types(arg_types)
                .span(node.span)
                .arg_spans(arg_spans)
                .flags(node.flags)
                .ref_params(
                    parameters
                        .iter()
                        .map(|parm| parm.by_ref)
                        .collect::<Vec<_>>(),
                )
                .build()
                .expect("Failed to build function prototype"),
        );

        Ok(())
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let proto_opt = self
            .context
            .lookup_function_complete(node.name, &CallNamespace::default());

        // An object's own definition outranks a nomask simul efun (CD, FluffOS).
        if let Some(Callee::Local(prototype)) = proto_opt
            && prototype.flags.nomask()
        {
            let e = LpcError::new(format!(
                "attempt to redefine nomask function `{}`",
                node.name
            ))
            .with_span(node.span)
            .with_label("defined here", prototype.span);

            return Err(e);
        }

        let num_args = RegisterSize::try_from(node.parameters.len())?;
        // A fixed-count inherited function is redefined with its count, a
        // varargs one with any (CD); the parent's call sites then fit.
        if let Some(inherited) = self
            .context
            .lookup_function(node.name, &CallNamespace::Parent)
            && !inherited.flags.varargs()
            && !inherited.flags.ellipsis()
            && inherited.arity.num_args != num_args
        {
            let e = LpcError::new(format!(
                "incorrect number of arguments in redefinition of `{}`",
                node.name
            ))
            .with_span(node.span)
            .with_label("defined here", inherited.span);

            return Err(e);
        }

        // Store the prototype now, to allow for forward references.
        let num_default_args =
            RegisterSize::try_from(node.parameters.iter().filter(|p| p.value.is_some()).count())?;

        let arg_types = node
            .parameters
            .iter()
            .map(|parm| parm.type_)
            .collect::<Vec<_>>();
        self.context.function_prototypes.insert(
            node.name.to_owned(),
            FunctionPrototypeBuilder::default()
                .name(node.name.to_owned())
                .filename(self.context.source.input().clone())
                .return_type(node.return_type)
                .arity(FunctionArity {
                    num_args,
                    num_default_args,
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
                .ref_params(
                    node.parameters
                        .iter()
                        .map(|parm| parm.by_ref)
                        .collect::<Vec<_>>(),
                )
                .build()
                .expect("Failed to build function prototype"),
        );

        // walk the contents of the function, in case any closures are defined.
        walk_function_def(self, node).await
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

        program.functions.insert(ustr("duplicate"), func.into());

        context.inherits.push(program);

        let mut walker = FunctionPrototypeWalker::new(context);
        let result = walker.visit_function_def(&mut node).await;

        if let Err(e) = result {
            assert_regex!(
                e.message(),
                "attempt to redefine nomask function `duplicate`"
            );
        } else {
            panic!("didn't error?")
        }
    }

    /// Redefine an inherited `g` of `inherited_args` parameters and
    /// `inherited_flags` with one of `own_args` parameters.
    async fn redefine(
        inherited_args: RegisterSize,
        inherited_flags: FunctionFlags,
        own_args: usize,
    ) -> Result<()> {
        let mut node = FunctionDefNode {
            return_type: LpcType::Void,
            name: ustr("g"),
            parameters: (0..own_args)
                .map(|i| VarInitNode::new(&format!("p{i}"), LpcType::Int(false)))
                .collect(),
            flags: FunctionFlags::default(),
            body: vec![],
            span: None,
        };
        let mut context = empty_compilation_context();
        let mut program = Program::default();
        let prototype = FunctionPrototypeBuilder::default()
            .name("g")
            .filename(Arc::new("parent".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(inherited_args))
            .flags(inherited_flags)
            .span(Some(Span::new(3, 1..3)))
            .build()
            .unwrap();
        program
            .functions
            .insert(ustr("g"), ProgramFunction::new(prototype, 0).into());
        context.inherits.push(program);
        let mut walker = FunctionPrototypeWalker::new(context);
        walker.visit_function_def(&mut node).await
    }

    #[tokio::test]
    async fn a_redefinition_keeps_the_inherited_parameter_count() {
        let public = FunctionFlags::default();
        assert!(redefine(1, public, 1).await.is_ok());
        for (inherited, own) in [(0, 1), (1, 0), (1, 2)] {
            let e = redefine(inherited, public, own).await.unwrap_err();
            assert_eq!(
                e.message(),
                "incorrect number of arguments in redefinition of `g`"
            );
        }
    }

    #[tokio::test]
    async fn a_private_inherited_function_holds_its_count_too() {
        let e = redefine(0, FunctionFlags::from(&["private"][..]), 1)
            .await
            .unwrap_err();
        assert_regex!(
            e.message(),
            "incorrect number of arguments in redefinition of `g`"
        );
    }

    #[tokio::test]
    async fn a_varargs_inherited_function_takes_any_count() {
        let varargs = FunctionFlags::from(&["varargs"][..]);
        assert!(redefine(1, varargs, 0).await.is_ok());
        assert!(redefine(1, varargs, 2).await.is_ok());
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
                .ref_params(vec![false, false])
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
                .kind(FunctionKind::Closure)
                .arity(FunctionArity::new(4))
                .arg_types(vec![
                    LpcType::Int(false),
                    LpcType::Mapping(true),
                    LpcType::Mixed(false),
                    LpcType::Mixed(false),
                ])
                .ref_params(vec![false, false, false, false])
                .build()
                .expect("Failed to build function prototype"),
        )
    }
}
