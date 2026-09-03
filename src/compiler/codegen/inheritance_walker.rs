use async_trait::async_trait;
use lpc_rs_core::{EFUN, RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::{LpcError, Result, lpc_error, span::Span};

use crate::compiler::{
    Compiled, CompilerBuilder,
    ast::inherit_node::InheritNode,
    codegen::tree_walker::{ContextHolder, Pass, TreeWalker},
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
};
use crate::interpreter::program::Region;

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
                return Err(lpc_error!(
                    node.span,
                    "inheritance namespace `efun` is reserved"
                ));
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

impl Pass for InheritanceWalker {
    fn new(context: CompilationContext) -> Self {
        InheritanceWalker::new(context)
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

/// The block each of `imported`'s regions lands on: a program the child
/// already holds keeps its block, a new one takes the next slots.
fn place(
    layout: &mut Vec<Region>,
    num_globals: &mut RegisterSize,
    imported: &[Region],
    span: Option<Span>,
) -> Result<Vec<RegisterSize>> {
    let mut targets = Vec::with_capacity(imported.len());
    for region in imported {
        match layout.iter().find(|held| held.filename == region.filename) {
            Some(held) if held.count != region.count => {
                return Err(lpc_error!(
                    span,
                    "inherited two different versions of `{}`",
                    region.filename
                ));
            }
            Some(held) => targets.push(held.base),
            None => {
                let base = *num_globals;
                *num_globals += region.count;
                layout.push(Region {
                    base,
                    ..region.clone()
                });
                targets.push(base);
            }
        }
    }
    Ok(targets)
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

        let lib_dir = self.context.config.lib_dir.as_str();
        // An out-of-root path collapses to an empty in-game form, so the
        // error names the directive's own text.
        self.context
            .config
            .validate_in_game_path(&full_path, node.span)
            .map_err(|_| {
                lpc_error!(
                    node.span,
                    "attempt to inherit a file outside the root: `{}`",
                    node.path
                )
            })?;
        if let Some(gate) = &self.context.gate {
            let configured = self
                .context
                .config
                .auto_inherit_file
                .map(|auto| {
                    LpcPath::new_in_game(auto.as_str(), "/", lib_dir).source_file()
                        == full_path.source_file()
                })
                .unwrap_or(false);
            if !configured {
                let parent = full_path
                    .source_file()
                    .as_in_game(lib_dir)
                    .display()
                    .to_string();
                let child = self
                    .context
                    .filename
                    .as_in_game(lib_dir)
                    .display()
                    .to_string();
                if !gate.inherit(&parent, &child).await? {
                    return Err(lpc_error!(
                        node.span,
                        "inherit \"{}\": permission denied",
                        parent
                    ));
                }
            }
        }

        let depth = self.context.inherit_depth;
        let compiler = CompilerBuilder::default()
            .config(self.context.config.clone())
            .inherit_depth(depth + 1)
            .gate(self.context.gate.clone())
            .build()?;

        match compiler.compile_in_game_file(&full_path, node.span).await {
            Ok(Compiled { program, warnings }) => {
                // Before `place` adds this parent's programs to the layout, or
                // every group reads as held.
                let held = &self.context.layout;
                self.context.inherited_warnings.extend(
                    warnings
                        .into_iter()
                        .filter(|w| held.iter().all(|r| r.filename != w.filename)),
                );

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
                    let err: LpcError = lpc_error!(
                        node.span,
                        "`{}` is already being inherited from",
                        program.filename
                    );

                    return Err(self.context.diagnostics.fail(err));
                }

                if let Some(namespace) = &node.namespace {
                    self.context
                        .inherit_names
                        .insert(namespace.to_owned(), self.context.inherits.len());
                }

                let mut program = program;
                let targets = place(
                    &mut self.context.layout,
                    &mut self.context.num_globals,
                    &program.layout,
                    node.span,
                )
                .map_err(|e| self.context.diagnostics.fail(e))?;
                program.relocate_globals(&targets);
                self.context.inherited_functions.extend(
                    program
                        .functions
                        .iter()
                        .map(|(name, function)| (*name, function.clone())),
                );
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

    mod test_place {
        use std::sync::Arc;

        use ustr::ustr;

        use super::*;

        fn region(filename: &str, base: RegisterSize, count: RegisterSize) -> Region {
            Region {
                filename: Arc::new(LpcPath::InGame(filename.into())),
                base,
                count,
                init: ustr(""),
            }
        }

        #[test]
        fn a_held_program_keeps_its_block_and_a_new_one_follows() {
            let mut layout = vec![region("/gp.c", 0, 3), region("/left.c", 3, 2)];
            let mut num_globals = 5;
            let imported = [region("/gp.c", 0, 3), region("/right.c", 3, 1)];

            let targets = place(&mut layout, &mut num_globals, &imported, None).unwrap();

            assert_eq!(targets, [0, 5]);
            assert_eq!(num_globals, 6);
            let held: Vec<_> = layout
                .iter()
                .map(|r| (r.filename.to_string(), r.base))
                .collect();
            assert_eq!(
                held,
                [
                    ("/gp.c".to_string(), 0),
                    ("/left.c".to_string(), 3),
                    ("/right.c".to_string(), 5)
                ]
            );
        }

        #[test]
        fn two_versions_of_one_program_are_rejected() {
            let mut layout = vec![region("/gp.c", 0, 3)];
            let err = place(&mut layout, &mut 3, &[region("/gp.c", 0, 4)], None).unwrap_err();
            assert_eq!(
                err.to_string(),
                "inherited two different versions of `/gp.c`"
            );
        }
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
        async fn a_program_reached_through_two_parents_warns_once() {
            let mut walker = walker();

            for path in ["/warns_left.c", "/warns_right.c"] {
                let mut node = InheritNode {
                    path: ustr(path),
                    namespace: None,
                    span: None,
                };
                assert_ok!(walker.visit_inherit(&mut node).await);
            }

            let groups: Vec<_> = walker
                .context
                .inherited_warnings
                .iter()
                .map(|w| (w.filename.to_string(), w.warnings.len()))
                .collect();
            assert_eq!(
                groups,
                [
                    ("/warns.c".to_string(), 2),
                    ("/warns_left.c".to_string(), 0),
                    ("/warns_right.c".to_string(), 0),
                ]
            );
            assert!(walker.context.diagnostics.errors().is_empty());
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
