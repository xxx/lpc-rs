use crate::{ast::inherit_node::InheritNode, codegen::tree_walker::{ContextHolder, TreeWalker}, compilation_context::CompilationContext, LpcError, Result};
use crate::compiler::Compiler;
use crate::util::path_maker::LpcPath;

/// A walker to handle compiling and linking inherited files.
#[derive(Debug, Default)]
pub struct InheritanceWalker {
    /// The compilation context
    context: CompilationContext,
}

impl InheritanceWalker {
    pub fn new(context: CompilationContext) -> Self {
        Self { context }
    }
}

impl ContextHolder for InheritanceWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for InheritanceWalker {
    fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        if let Some(namespace) = &node.namespace {
            if self.context.inherit_names.contains_key(namespace) {
                return Err(
                    LpcError::new(format!("inheritance namespace `{}` is already defined", namespace))
                        .with_span(node.span)
                )
            }
        }

        let cwd = match self.context.filename.cwd() {
            LpcPath::Server(_) => {
                self
                    .context
                    .filename
                    .as_in_game(self.context.config.lib_dir())
                    .into_owned()
            }
            LpcPath::InGame(x) => x
        };

        let full_path = LpcPath::new_in_game(
            &node.path,
            cwd,
            self.context.config.lib_dir()
        );

        let compiler = Compiler::new(self.context.config.clone());

        match compiler.compile_in_game_file(&full_path, node.span) {
            Ok(program) => {
                if self.context.inherits.iter().any(|x| x.filename == program.filename) {
                    let err = LpcError::new(
                        format!("`{}` is already being inherited from", program.filename)
                    ).with_span(node.span);

                    self.context.errors.push(err.clone());

                    return Err(err);
                }

                // TODO: move CompilationContext's `inherits` into a DAG, and check for cycles

                if let Some(namespace) = &node.namespace {
                    self.context.inherit_names.insert(
                        namespace.clone(),
                        self.context.inherits.len()
                    );
                }

                self.context.inherits.push(program);

                Ok(())
            }
            Err(e) => {
                let err = LpcError::from(e);
                self.context.errors.push(err.clone());
                Err(err)
            }
        }
    }
}
