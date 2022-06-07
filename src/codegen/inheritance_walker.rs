use crate::{
    ast::inherit_node::InheritNode,
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
    compiler::Compiler,
    core::EFUN,
    util::path_maker::LpcPath,
    LpcError, Result,
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

    fn valid_inherit(&self, node: &InheritNode) -> Result<()> {
        let depth = self.context.inherit_depth;

        if depth >= self.context.config.max_inherit_depth() {
            let err = LpcError::new("maximum inheritance depth reached").with_span(node.span);

            return Err(err);
        }

        if let Some(namespace) = &node.namespace {
            if namespace == EFUN {
                return Err(
                    LpcError::new("inheritance namespace `efun` is reserved").with_span(node.span)
                );
            }

            if self.context.inherit_names.contains_key(namespace) {
                return Err(LpcError::new(format!(
                    "inheritance namespace `{}` is already defined",
                    namespace
                ))
                .with_span(node.span));
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

impl TreeWalker for InheritanceWalker {
    fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
        self.valid_inherit(node)?;

        let cwd = match self.context.filename.cwd() {
            LpcPath::Server(_) => self
                .context
                .filename
                .as_in_game(self.context.config.lib_dir())
                .into_owned(),
            LpcPath::InGame(x) => x,
        };

        let full_path = LpcPath::new_in_game(&node.path, cwd, self.context.config.lib_dir());

        let depth = self.context.inherit_depth;
        let compiler = Compiler::new(self.context.config.clone()).with_inherit_depth(depth + 1);

        match compiler.compile_in_game_file(&full_path, node.span) {
            Ok(program) => {
                if self
                    .context
                    .inherits
                    .iter()
                    .any(|x| x.filename == program.filename)
                {
                    let err = LpcError::new(format!(
                        "`{}` is already being inherited from",
                        program.filename
                    ))
                    .with_span(node.span);

                    self.context.errors.push(err.clone());

                    return Err(err);
                }

                if let Some(namespace) = &node.namespace {
                    self.context
                        .inherit_names
                        .insert(namespace.clone(), self.context.inherits.len());
                }

                self.context.num_globals += program.num_globals;
                self.context.num_init_registers += program.num_init_registers;

                self.context.inherits.push(program);

                Ok(())
            }
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::config::Config;

    fn walker() -> InheritanceWalker {
        let config = Config::default().with_lib_dir("./tests/fixtures/code/");

        let context = CompilationContext::new("test.c", config.into());

        InheritanceWalker::new(context)
    }

    mod test_visit_inherit {
        use super::*;
        use claim::{assert_err, assert_ok};

        #[test]
        fn test_sets_up_the_data() {
            let mut walker = walker();

            let mut node = InheritNode {
                path: "/grandparent.c".to_string(),
                namespace: None,
                span: None,
            };

            let result = walker.visit_inherit(&mut node);

            assert_ok!(result);
            assert_eq!(walker.context.inherits.len(), 1);
        }

        #[test]
        fn test_disallows_duplicate_namespace() {
            let mut walker = walker();

            walker
                .context
                .inherit_names
                .insert("grandparent".to_string(), 0);

            let mut node = InheritNode {
                path: "/grandparent.c".to_string(),
                namespace: Some("grandparent".to_string()),
                span: None,
            };

            let result = walker.visit_inherit(&mut node);

            assert_err!(
                result,
                "inheritance namespace `grandparent` is already defined"
            );
        }

        #[test]
        fn test_disallows_efun_namespace() {
            let mut walker = walker();

            let mut node = InheritNode {
                path: "/grandparent.c".to_string(),
                namespace: Some("efun".to_string()),
                span: None,
            };

            let result = walker.visit_inherit(&mut node);

            assert_err!(result, "inheritance namespace `efun` is reserved");
        }
    }
}
