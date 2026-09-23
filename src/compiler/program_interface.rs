//! Declaration metadata collected before inherited bodies are checked.

use std::{collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use lpc_rs_core::{class::ClassScope, lpc_path::LpcPath, mangle::Mangle, visibility::Visibility};
use lpc_rs_function_support::function_prototype::FunctionPrototype;

use super::{
    ast::{ast_node::AstNode, program_node::ProgramNode},
    compilation_context::CompilationContext,
};

#[derive(Debug, Clone)]
pub(crate) struct InterfaceFunction {
    pub prototype: FunctionPrototype,
    pub visibility: Visibility,
    pub hidden: bool,
}

#[derive(Debug)]
pub(crate) struct ProgramInterface {
    pub filename: Arc<LpcPath>,
    pub class_scope: Arc<ClassScope>,
    functions: IndexMap<String, InterfaceFunction>,
}

impl ProgramInterface {
    pub fn collect(node: &ProgramNode, context: &CompilationContext) -> Self {
        let mut functions = IndexMap::new();
        for (parent, inherit) in context.inherited_interfaces.iter().zip(&node.inherits) {
            for (name, function) in &parent.functions {
                let mut imported = function.clone();
                imported.hidden |= function.visibility == Visibility::Private;
                imported.visibility = function.visibility.restricted_by(inherit.visibility);
                functions.insert(name.clone(), imported);
            }
        }
        for declaration in &node.body {
            if let AstNode::FunctionDef(function) = declaration {
                let prototype = context.function_prototypes[function.name.as_str()].clone();
                functions.insert(
                    prototype.mangle(),
                    InterfaceFunction {
                        visibility: prototype.flags.visibility(),
                        prototype,
                        hidden: false,
                    },
                );
            }
        }
        Self {
            filename: context.source.program_path().clone(),
            class_scope: context.class_scope.clone(),
            functions,
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&InterfaceFunction> {
        self.functions
            .values()
            .rev()
            .find(|function| function.prototype.name == name)
    }

    pub fn has_basename(&self, name: &str) -> bool {
        self.filename
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|filename| filename.strip_suffix(".c").unwrap_or(filename) == name)
    }

    pub fn receiver_functions(&self) -> HashMap<String, InterfaceFunction> {
        self.functions
            .values()
            .filter(|function| !function.hidden)
            .map(|function| (function.prototype.name.to_string(), function.clone()))
            .collect()
    }
}
