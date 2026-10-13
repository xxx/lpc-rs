//! Retains each authorized source between interface collection and body compilation.

use std::{collections::HashMap, sync::Arc};

use lpc_rs_core::{lpc_path::LpcPath, visibility::Visibility};
use lpc_rs_errors::{LpcError, Result, lpc_error};
use ustr::ustr;

use super::{
    Compiler, CompilerBuilder,
    ast::{inherit_node::InheritNode, program_node::ProgramNode},
    codegen::{
        capture_walker::CaptureWalker,
        class_walker::ClassWalker,
        efun_override_walker::EfunOverrideWalker,
        function_prototype_walker::FunctionPrototypeWalker,
        inheritance_walker::InheritanceWalker,
        scope_walker::ScopeWalker,
        semantic_check_walker::SemanticCheckWalker,
        tree_walker::{ContextHolder, apply},
    },
    compilation_context::CompilationContext,
    diagnostics,
    program_interface::{InterfaceFunction, ProgramInterface},
    source::CompilerSource,
};

pub(super) struct PreparedProgram {
    node: ProgramNode,
    context: CompilationContext,
    parents: Vec<Self>,
    pub interface: Arc<ProgramInterface>,
}

impl PreparedProgram {
    pub async fn parse(
        compiler: &Compiler,
        source: Arc<CompilerSource>,
        code: &str,
        warning: Option<LpcError>,
    ) -> Result<Self> {
        Box::pin(async move {
            let (mut node, mut context) = compiler.parse_source(source, code, warning).await?;
            if let Some(auto) = &compiler.config.auto_inherit_file {
                let path = LpcPath::new_in_game(auto.as_str(), "/", &*compiler.config.lib_dir);
                if path.source_file() != context.source.input().source_file() {
                    node.inherits.insert(
                        0,
                        InheritNode {
                            visibility: Default::default(),
                            path: ustr(auto),
                            namespace: None,
                            span: None,
                        },
                    );
                }
            }
            let mut parents = Vec::with_capacity(node.inherits.len());
            for inherit in &node.inherits {
                let walker = InheritanceWalker::new(context);
                let path = walker.resolve(inherit).await?;
                context = walker.into_context();
                let parent_compiler = CompilerBuilder::default()
                    .config(context.config.clone())
                    .code_pool(context.code_pool.clone())
                    .definition_cache(context.definition_cache.clone())
                    .inherit_depth(context.inherit_depth + 1)
                    .gate(context.gate.clone())
                    .source_reader(context.source_reader.clone())
                    .diagnostic_sources(context.diagnostic_sources.clone())
                    .simul_efuns(context.simul_efuns.clone())
                    .build()?;
                let (source, code) = parent_compiler.read_file(path.source_file()).await?;
                let warning = code
                    .latin1
                    .then(|| diagnostics::latin1_warning(source.name().as_str(), None));
                let parent = Self::parse(&parent_compiler, source, &code.text, warning).await?;
                if parent.context.pragmas.no_inherit() && inherit.span.is_some() {
                    return Err(lpc_error!(
                        inherit.span,
                        "`pragma #no_inherit` is set on {}",
                        parent.interface.filename
                    ));
                }
                if context
                    .inherited_interfaces
                    .iter()
                    .any(|held| held.filename == parent.interface.filename)
                {
                    return Err(lpc_error!(
                        inherit.span,
                        "`{}` is already being inherited from",
                        parent.interface.filename
                    ));
                }
                if let Some(namespace) = inherit.namespace {
                    context
                        .inherit_names
                        .insert(namespace.to_string(), parents.len());
                }
                context.inherited_interfaces.push(parent.interface.clone());
                parents.push(parent);
            }
            if context.gate.is_some() {
                context = apply::<EfunOverrideWalker>(&mut node, context, true)
                    .await?
                    .into_context();
            }
            context = apply::<CaptureWalker>(&mut node, context, true)
                .await?
                .into_context();
            context = apply::<ClassWalker>(&mut node, context, true)
                .await?
                .into_context();
            context = apply::<FunctionPrototypeWalker>(&mut node, context, false)
                .await?
                .into_context();
            let interface = Arc::new(ProgramInterface::collect(&node, &context));
            Ok(Self {
                node,
                context,
                parents,
                interface,
            })
        })
        .await
    }

    pub async fn analyze(
        self,
        receiver: &HashMap<String, InterfaceFunction>,
    ) -> Result<(ProgramNode, CompilationContext)> {
        Box::pin(async move {
            let Self {
                mut node,
                mut context,
                parents,
                interface,
            } = self;
            let mut available = receiver.clone();
            // A private import freezes its targets before a descendant can supply a same-named function.
            for (name, function) in interface.receiver_functions() {
                if function.visibility == Visibility::Private || !available.contains_key(&name) {
                    available.insert(name, function);
                }
            }
            context.inherit_names.clear();
            context.inherited_interfaces.clear();
            let mut walker = InheritanceWalker::new(context);
            for (parent, inherit) in parents.into_iter().zip(&node.inherits) {
                let (node, context) = parent.analyze(&available).await?;
                let compiled = Compiler::generate(node, context).await?;
                walker.link(inherit, compiled)?;
            }
            context = walker.into_context();
            context.receiver_functions = available
                .into_iter()
                .filter(|(_, function)| !function.prototype.flags.private())
                .map(|(name, function)| (name, function.prototype))
                .collect();
            context = apply::<ScopeWalker>(&mut node, context, false)
                .await?
                .into_context();
            context = apply::<SemanticCheckWalker>(&mut node, context, true)
                .await?
                .into_context();
            Ok((node, context))
        })
        .await
    }
}

#[cfg(test)]
mod tests;
