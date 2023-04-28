use async_trait::async_trait;
use lpc_rs_core::{lpc_path::LpcPath, EFUN};
use lpc_rs_errors::{lpc_error, LpcError, Result};

use crate::compiler::{
    ast::inherit_node::InheritNode,
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
    CompilerBuilder,
};

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

    fn validate(&self, node: &InheritNode) -> Result<()> {
        let depth = self.context.inherit_depth;

        if depth >= self.context.config.max_inherit_depth {
            let err = lpc_error!(node.span, "maximum inheritance depth reached");

            return Err(err);
        }

        if let Some(namespace) = &node.namespace {
            if namespace.as_str() == EFUN {
                return Err(
                    lpc_error!(node.span, "inheritance namespace `efun` is reserved")
                );
            }

            if self.context.inherit_names.contains_key(namespace.as_str()) {
                return Err(lpc_error!(
                    node.span,
                    "inheritance namespace `{}` is already defined",
                    namespace
                ));
            }
        }

        Ok(())
    }
}

impl ContextHolder for InheritanceWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

#[async_trait]
impl TreeWalker for InheritanceWalker {
    async fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        self.validate(node)?;

        let cwd = match self.context.filename.cwd() {
            LpcPath::Server(_) => self
                .context
                .filename
                .as_in_game(self.context.config.lib_dir.as_str())
                .into_owned(),
            LpcPath::InGame(x) => x,
        };

        let full_path =
            LpcPath::new_in_game(&*node.path, cwd, self.context.config.lib_dir.as_str());

        let depth = self.context.inherit_depth;
        let compiler = CompilerBuilder::default()
            .config(self.context.config.clone())
            .inherit_depth(depth + 1)
            .build()?;

        match compiler.compile_in_game_file(&full_path, node.span).await {
            Ok(program) => {
                if program.pragmas.no_inherit() {
                    return Err(lpc_error!(
                        node.span,
                        "`pragma #no_inherit` is set on {}",
                        program.filename
                    ));
                }

                if self
                    .context
                    .inherits
                    .iter()
                    .any(|x| x.filename == program.filename)
                {
                    let err: Box<LpcError> = lpc_error!(
                        node.span,
                        "`{}` is already being inherited from",
                        program.filename
                    );

                    self.context.errors.push(err.clone());

                    return Err(err);
                }

                if let Some(namespace) = &node.namespace {
                    self.context
                        .inherit_names
                        .insert(namespace.to_owned(), self.context.inherits.len());
                }

                self.context.num_globals += program.num_globals;
                // subtract one, because r0 is for return values, but will get added
                // for each inherited file due to the recursive nature of inheritance
                // compilation.
                self.context.num_init_registers += program.num_init_registers() - 1;
                self.context.inherited_functions.extend(
                    program
                        .functions
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone())),
                );
                self.context
                    .strings
                    .extend(program.strings.into_iter().map(|x| x.1));

                self.context.inherits.push(program);

                Ok(())
            }
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::compiler::compilation_context::CompilationContextBuilder;

    fn walker() -> InheritanceWalker {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code/")
            .build()
            .unwrap();

        let context = CompilationContextBuilder::default()
            .filename(LpcPath::InGame("test.c".into()))
            .config(config)
            .build()
            .unwrap();

        InheritanceWalker::new(context)
    }

    mod test_visit_inherit {
        use claims::assert_ok;
        use ustr::ustr;

        use super::*;

        #[tokio::test]
        async fn test_sets_up_the_data() {
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/grandparent.c"),
                namespace: None,
                span: None,
            };

            let result = walker.visit_inherit(&mut node).await;

            assert_ok!(result);
            assert_eq!(walker.context.inherits.len(), 1);
        }

        #[tokio::test]
        async fn test_disallows_duplicate_namespace() {
            let mut walker = walker();

            walker
                .context
                .inherit_names
                .insert("grandparent".to_string(), 0);

            let mut node = InheritNode {
                path: ustr("/grandparent.c"),
                namespace: Some(ustr("grandparent")),
                span: None,
            };

            let result = walker.visit_inherit(&mut node).await;

            assert_eq!(
                result.unwrap_err().to_string(),
                "inheritance namespace `grandparent` is already defined"
            );
        }

        #[tokio::test]
        async fn test_disallows_no_inherit_pragma() {
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/no_inherit.c"),
                namespace: None,
                span: None,
            };

            let result = walker.visit_inherit(&mut node).await;

            assert_eq!(
                result.unwrap_err().to_string(),
                "`pragma #no_inherit` is set on /no_inherit.c"
            );
        }

        #[tokio::test]
        async fn test_disallows_efun_namespace() {
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/grandparent.c"),
                namespace: Some(ustr("efun")),
                span: None,
            };

            let result = walker.visit_inherit(&mut node).await;

            assert_eq!(
                result.unwrap_err().to_string(),
                "inheritance namespace `efun` is reserved"
            );
        }
    }
}
