use lpc_rs_core::{lpc_path::LpcPath, EFUN};
use lpc_rs_errors::{LpcError, Result};
use qcell::QCellOwner;

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
            let err = LpcError::new("maximum inheritance depth reached").with_span(node.span);

            return Err(err);
        }

        if let Some(namespace) = &node.namespace {
            if namespace.as_str() == EFUN {
                return Err(
                    LpcError::new("inheritance namespace `efun` is reserved").with_span(node.span)
                );
            }

            if self.context.inherit_names.contains_key(namespace.as_str()) {
                return Err(LpcError::new(format!(
                    "inheritance namespace `{namespace}` is already defined"
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
    fn visit_inherit(&mut self, node: &mut InheritNode, cell_key: &mut QCellOwner) -> Result<()> {
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

        match compiler.compile_in_game_file(&full_path, node.span, cell_key) {
            Ok(program) => {
                if program.pragmas.no_inherit() {
                    return Err(LpcError::new(format!(
                        "`pragma #no_inherit` is set on {}",
                        program.filename
                    ))
                    .with_span(node.span));
                }

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
                        .insert(namespace.to_owned(), self.context.inherits.len());
                }

                self.context.num_globals += program.num_globals;
                self.context.num_init_registers += program.num_init_registers;
                println!("inheriting functions: {:?}", program.functions.keys());
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
        use claim::assert_ok;
        use ustr::ustr;

        use super::*;

        #[test]
        fn test_sets_up_the_data() {
            let mut cell_key = QCellOwner::new();
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/grandparent.c"),
                namespace: None,
                span: None,
            };

            let result = walker.visit_inherit(&mut node, &mut cell_key);

            assert_ok!(result);
            assert_eq!(walker.context.inherits.len(), 1);
        }

        #[test]
        fn test_disallows_duplicate_namespace() {
            let mut cell_key = QCellOwner::new();
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

            let result = walker.visit_inherit(&mut node, &mut cell_key);

            assert_eq!(
                result.unwrap_err().to_string(),
                "inheritance namespace `grandparent` is already defined"
            );
        }

        #[test]
        fn test_disallows_no_inherit_pragma() {
            let mut cell_key = QCellOwner::new();
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/no_inherit.c"),
                namespace: None,
                span: None,
            };

            let result = walker.visit_inherit(&mut node, &mut cell_key);

            assert_eq!(
                result.unwrap_err().to_string(),
                "`pragma #no_inherit` is set on /no_inherit.c"
            );
        }

        #[test]
        fn test_disallows_efun_namespace() {
            let mut cell_key = QCellOwner::new();
            let mut walker = walker();

            let mut node = InheritNode {
                path: ustr("/grandparent.c"),
                namespace: Some(ustr("efun")),
                span: None,
            };

            let result = walker.visit_inherit(&mut node, &mut cell_key);

            assert_eq!(
                result.unwrap_err().to_string(),
                "inheritance namespace `efun` is reserved"
            );
        }
    }
}
